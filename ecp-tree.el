;;; ecpt.el --- Eglot Clojure Project Tree -*- lexical-binding: t; -*-

;; Author: Jakub Zika <zikajk@protonmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (eglot "1.12.29") (hierarchy "0.7.0") (jarchive "0.11.0")
;; Keywords: convenience, tools, languages
;; URL: http://github.com/zikajk/ecp-tree

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ecp-tree provides an interactive project tree for Clojure projects using eglot.
;; It allows navigating and interacting with project entities such as files,
;; namespaces, and functions directly from Emacs.

;; Installation:
;; Load ecp-tree.el in your Emacs and require it:
;; (require 'ecp-tree)

;; Usage:
;; M-x ecp-tree-show to display the project tree.
;; M-x ecp-tree-clear-cache to delete the cache, works only with ecp-tree-cache-enabled set to true.

;;; Code:

(require 'hierarchy)
(require 'cl-lib)

(defgroup ecp-tree nil
  "Customization group for the ECP-TREE (Eglot Clojure Project Tree) package.
This group contains settings for adjusting the behavior and appearance of the ECP-TREE project tree."
  :group 'clojure)

(defgroup ecp-tree-node-style nil
  "Customization options related to the appearance of nodes within the ECP-TREE tree.
   This includes icons, positions, and other style-related settings."
  :group 'ecp-tree)

(defcustom ecp-tree--project-icon "☆"
  "Icon (string) used to represent a project node in the ECP-TREE tree."
  :type 'string
  :group 'ecp-tree-node-style)

(defcustom ecp-tree--folder-icon "📁"
  "Icon (string) used to represent a folder node in the ECP-TREE tree."
  :type 'string
  :group 'ecp-tree-node-style)

(defcustom ecp-tree--library-icon "📕"
  "Icon (string) used to represent a library node in the ECP-TREE tree."
  :type 'string
  :group 'ecp-tree-node-style)

(defcustom ecp-tree--jar-icon "🫙"
  "Icon (string) used to represent a JAR (Java Archive) node in the ECP-TREE tree.
This icon (string) helps users quickly identify JAR files among other project resources."
  :type 'string
  :group 'ecp-tree-node-style)

(defcustom ecp-tree--namespace-icon "📁"
  "Icon (string) used to represent a namespace node in the ECP-TREE tree."
  :type 'string
  :group 'ecp-tree-node-style)

(defcustom ecp-tree--class-icon "⚧️"
  "Icon (string) used to represent a class node in the ECP-TREE tree."
  :type 'string
  :group 'ecp-tree-node-style)

(defcustom ecp-tree--method-icon "🔢"
  "Icon (string) used to represent a method node in the ECP-TREE tree."
  :type 'string
  :group 'ecp-tree-node-style)

(defcustom ecp-tree--variable-icon "📶"
  "Icon (string) used to represent a variable node in the ECP-TREE tree."
  :type 'string
  :group 'ecp-tree-node-style)

(defcustom ecp-tree--interface-icon "ℹ️"
  "Icon (string) used to represent an interface node in the ECP-TREE tree."
  :type 'string
  :group 'ecp-tree-node-style)

(defcustom ecp-tree-cache-enabled nil
  "Enable or disable caching for node's children in the ECP-TREE.
When set to true, children of nodes are cached to improve performance in navigating the project tree."
  :type 'boolean
  :group 'ecp-tree)

(defcustom ecp-tree-sort-nodes 't
  "Enable to sort children nodes alphabetically. The first branch is never sorted."
  :type 'boolean
  :group 'ecp-tree)

(defcustom ecp-tree-skip-external-deps '()
  "Enable to skip analysis of external deps. This can usually save a lot of time."
  :type 'boolean
  :group 'ecp-tree)

(defcustom ecp-tree-type-icon-pos 'start
  "Determines the position of the icon (string) in relation to the node text.
Can be set to 'start to place the icons before the node text, or 'end to place it after."
  :type '(choice (const :tag "Start" start)
                 (const :tag "End" end))
  :group 'ecp-tree-node-style)

(defcustom ecp-tree-debug-enabled '()
  "Enable or disable debug messages for the ECP-TREE.
When set to true, ECP-TREE will log additional debug information useful for development or troubleshooting."
  :type 'boolean
  :group 'ecp-tree)

(defun ecp-tree-toggle-debug ()
  "Toggle the debug mode for ECP-TREE."
  (interactive)
  (setq ecp-tree-debug-enabled (not ecp-tree-debug-enabled))
  (message "ECP-TREE Debug mode %s" (if ecp-tree-debug-enabled "enabled" "disabled")))

(defun ecp-tree--debug-message (format-string &rest args)
  "Display a debug message if debugging is enabled for ECP-TREE.
FORMAT-STRING and ARGS are as in `format'."
  (when ecp-tree-debug-enabled
    (let ((debug-message (apply 'format format-string args)))
      ;; Output to a dedicated debug buffer.
      (with-current-buffer (get-buffer-create "*ECP-TREE Debug*")
        (goto-char (point-max))
        (insert (format "%s\n" debug-message))))))

(defun ecp-tree-type-to-icon (n)
  (pcase n
    (1 ecp-tree--project-icon)
    (2 ecp-tree--folder-icon)
    (3 ecp-tree--library-icon)
    (4 ecp-tree--jar-icon)
    (5 ecp-tree--namespace-icon)
    (6 ecp-tree--class-icon)
    (7 ecp-tree--method-icon)
    (8 ecp-tree--variable-icon)
    (9 ecp-tree--interface-icon)))

(defvar ecp-tree--children-cache (make-hash-table :test 'equal))

(defun ecp-tree--node-name (node)
  (let* ((name (plist-get node :name))
	 (detail (plist-get node :detail))
	 (type (plist-get node :type))
	 (annt (ecp-tree-type-to-icon type))
	 (label (if detail
		    (format "%s %s" name (propertize detail 'face 'eglot-parameter-hint-face))
		  name)))
    (if (eq ecp-tree-type-icon-pos 'start)
	(concat annt " " label)
      (if (eq ecp-tree-type-icon-pos 'end)
	  (concat label " " annt)
	label))))

(defun ecp-tree--node-in-hierarchy-p (hierarchy new-node)
  (let ((items (hierarchy-items hierarchy)))
    (cl-some (lambda (element)
	       (and (listp element) (equal element new-node)))
	     items)))

(defun ecp-tree--get-children (hierarchy node use-cache)
  "Get children for NODE, optionally using cache based on USE-CACHE.
HIERARCHY is the tree structure to which NODE belongs."
 (ecp-tree--debug-message "Getting children for: %s" node)
  (when (listp node)
    (let ((nodes (plist-get node :nodes)))
      (when (not (and ecp-tree-skip-external-deps (= 4 (plist-get node :type))))
	(if nodes
            (append nodes nil)		; If nodes are directly available, return them.
          (if use-cache
              (let ((cached (gethash node ecp-tree--children-cache)))
		(if cached
                    cached		; Return cached result if available.
                  ;; Fetch, cache, and return the children if not in cache.
                  (ecp-tree--fetch-and-maybe-cache-children node hierarchy use-cache)))
            ;; Fetch without caching.
            (ecp-tree--fetch-and-maybe-cache-children node hierarchy nil)))))))

(defun ecp-tree--fetch-and-maybe-cache-children (node hierarchy use-cache)
  "Fetch children nodes for NODE and conditionally update cache based on USE-CACHE.
HIERARCHY is the context."
  (let* ((children-nodes
          (seq-remove
           (lambda (child-node) (ecp-tree--node-in-hierarchy-p hierarchy child-node))
           (append
	    (plist-get
	     (eglot--request (eglot-current-server) :clojure/workspace/projectTree/nodes node)
	     :nodes)
	    nil)))
	(sorted-nodes (if ecp-tree-sort-nodes
			(sort children-nodes (lambda (first second) (string< (plist-get first :name) (plist-get second :name))))
			children-nodes)))
    (ecp-tree--debug-message "Found children: %s" sorted-nodes)
    (when use-cache
      (puthash node sorted-nodes ecp-tree--children-cache))
    sorted-nodes))

(defun ecp-tree--open-file-or-jar (node type)
  "Open the file or jar associated with NODE, if it exists.
NODE is expected to be a plist containing a :uri key with the file's path.
TYPE specifies whether to open a 'file or a 'jar."
  (ecp-tree--debug-message "Opening %s for: %s" (if (eq type 'jar) "jar" "file") node)
  (let* ((uri (plist-get node :uri))
         (path (if (eq type 'jar)
                   (replace-regexp-in-string "jar:file://" "" uri)
                 (eglot-uri-to-path uri))))
    (if (and path (file-exists-p path))
        (progn
          (select-window (get-mru-window (selected-frame) nil :not-selected))
          (find-file path))
      (message "File does not exist: %s" path))))

(defun ecp-tree--visit-node-uri-on-pos (node)
  (ecp-tree--open-file-or-jar node 'file)
  (let ((pos (eglot--lsp-position-to-point (plist-get (plist-get node :range) :start) t)))
    (goto-char pos)))

(defun ecp-tree--find-method (node)
  (ecp-tree--debug-message "Finding method for: %s" node)
  (ecp-tree--visit-node-uri-on-pos node))

(defun ecp-tree--find-variable (node)
  (ecp-tree--debug-message "Finding variable for: %s" node)
  (ecp-tree--visit-node-uri-on-pos node))

(defun ecp-tree--undefined-action (node)
  (ecp-tree--debug-message "Undefined action for node: %s" node))

(defun ecp-tree--open-external-clj (node)
  (if (require 'jarchive nil 'noerror)
      (progn
        (ecp-tree--debug-message "Opening external CLJ file via Jarchive.el for: %s" node)
        (find-file (plist-get node :uri)))
    (message "Jarchive package is required to open %s" (plist-get node :uri))))


(defun ecp-tree--click-action (node)
  (let ((nt (plist-get node :type)))
    (cond
     ((= 1 nt) (ecp-tree--undefined-action node))
     ((= 2 nt) (ecp-tree--undefined-action node))
     ((= 3 nt) (ecp-tree--undefined-action node))
     ((= 4 nt) (ecp-tree--open-file-or-jar node 'jar))
     ((= 5 nt) (ecp-tree--open-file-or-jar node 'file))
     ;; TODO - add support for class decompiler, see
     ;; https://github.com/clojure-lsp/clojure-lsp/blob/a77e910998277ede4badc5266b5e321da449c1c0/lib/src/clojure_lsp/feature/java_interop.clj
     ((= 6 nt) (ecp-tree--undefined-action node))
     ((= 7 nt) (ecp-tree--find-method node))
     ((= 8 nt) (ecp-tree--find-variable node))
     ((= 9 nt) (ecp-tree--undefined-action node)))))

(defun ecp-tree-show ()
  "Display the project tree using eglot's project structure.
This function queries the eglot server for the current project's structure and displays it in a hierarchical tree view.
It allows users to interact with nodes representing different entities such as files, namespaces, and methods."
  (interactive)
  (if (and (featurep 'eglot) (featurep 'hierarchy))
      (let* ((project-name (eglot-project-nickname (eglot-current-server)))
	     (buffer-name (format "*%s-ecp-tree*" project-name))
	     (buffer (get-buffer-create buffer-name))
	     (hierarchy (hierarchy-new))
	 (root
	  (eglot--request (eglot-current-server) :clojure/workspace/projectTree/nodes '())))
    (hierarchy-add-tree
     hierarchy
     root
     nil
     (lambda (node) (ecp-tree--get-children hierarchy node ecp-tree-cache-enabled)))
    (switch-to-buffer
     (hierarchy-tree-display
      hierarchy
      (lambda (node _)
	(let ((name (ecp-tree--node-name node)))
	  (hierarchy-labelfn-indent
	   (insert-text-button name
			       'action (lambda (_)
					 (ecp-tree--click-action node))))))
      buffer)))
      (error "ECPT requires Emacs 29.1 or newer with built-in 'eglot' and 'hierarchy' packages")))

(defun ecp-tree-clear-cache ()
  "Clear the cache. Does nothing when `ecp-tree-cache-enabled` is nil."
  (interactive)
  (clrhash ecp-tree--children-cache))

(provide 'ecp-tree)

;;; ecp-tree.el ends here

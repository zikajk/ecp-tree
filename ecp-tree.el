;;; ecpt.el --- Eglot Clojure Project Tree -*- lexical-binding: t; -*-

;; Author: Jakub Zika <zikajk@protonmail.com>
;; Version: 0.2.0
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

;;; Code:
;; NOTE - disable eglot issues
(require 'hierarchy)
(require 'cl-lib)

(defgroup ecp-tree nil
  "Customization group for the ECP-TREE (Eglot Clojure Project Tree) package.
For adjusting the behavior and appearance of the ECP-TREE project tree."
  :group 'clojure)

(defgroup ecp-tree-node-style nil
  "Options related to the appearance of nodes within the ECP-TREE tree.
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

(defcustom ecp-tree--caller-icon "📞"
  "Icon used to represent a caller node in the ECP-TREE tree."
  :type 'string
  :group 'ecp-tree-node-style)

(defcustom ecp-tree--call-site-icon "↪️"
  "Icon used to represent a call site node in the ECP-TREE tree."
  :type 'string
  :group 'ecp-tree-node-style)

(defcustom ecp-tree-sort-nodes 't
  "Enable to sort children nodes alphabetically.  The first branch is never sorted."
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
  "Select icon based on type of node - N."
  (pcase n
    (1 ecp-tree--project-icon)
    (2 ecp-tree--folder-icon)
    (3 ecp-tree--library-icon)
    (4 ecp-tree--jar-icon)
    (5 ecp-tree--namespace-icon)
    (6 ecp-tree--class-icon)
    (7 ecp-tree--method-icon)
    (8 ecp-tree--variable-icon)
    (9 ecp-tree--interface-icon)
    (10 ecp-tree--caller-icon)
    (11 ecp-tree--call-site-icon)))

;;
;; End of customizations
;;
(defconst ecp-tree--project-type 1)
(defconst ecp-tree--folder-type 2)
(defconst ecp-tree--library-type 3)
(defconst ecp-tree--jar-type 4)
(defconst ecp-tree--namespace-type 5)
(defconst ecp-tree--method-type 7)
(defconst ecp-tree--variable-type 8)
(defconst ecp-tree--interface-type 9)
(defconst ecp-tree--caller-type 10)
(defconst ecp-tree--call-site-type 11)


(defun vector->plist (vector)
  (append vector nil))

(defun ecp-tree--format-node-name (icon label uri-hint)
  "Format the node name with the given icon, label, and URI hint."
  (if (eq ecp-tree-type-icon-pos 'start)
      (concat icon " " label " " (and uri-hint (concat " " uri-hint)) "")
    (if (eq ecp-tree-type-icon-pos 'end)
        (concat label " " icon  (and uri-hint (concat " " uri-hint)) "")
      (concat label " " (and uri-hint (concat " " uri-hint)))
      label)))

(defun count-references-for-server (server uri position)
  "Get reference count from SERVER for URI at LINE,COLUMN"
  (let* ((params (list :textDocument (list :uri uri)
                       :position position))
         (response (eglot--request server
                                   :textDocument/references
                                   params
                                   :timeout 2.0)))
    (number-to-string (length response))))

(defun ecp-tree--node-name (node server)
  "Construct name based on provided NODE."
  (let* ((name (plist-get node :name))
	 (detail (plist-get node :detail))
	 (type (plist-get node :type))
	 (icon (ecp-tree-type-to-icon type))
	 (uri (plist-get node :uri))
	 (label (if detail
		    (format "%s %s" name (propertize detail 'face 'eglot-parameter-hint-face))
		  name)))
    (if (or (= type ecp-tree--method-type)
	    (= type ecp-tree--variable-type))
	(let* ((reference-no (propertize
			      (count-references-for-server server uri (plist-get (plist-get node :range) :start))
			      'face 'eglot-parameter-hint-face)))
	  (ecp-tree--format-node-name icon name reference-no))
      (ecp-tree--format-node-name icon label ""))))

(defun ecp-tree--prepare-call-hierarchy (node server)
  "Prepare call hierarchy items for NODE."
  (let* ((uri (plist-get node :uri))
         (range (plist-get node :range))
         (start-pos (plist-get range :start))
         (params (list :textDocument (list :uri uri)
                       :position start-pos)))
    (eglot--request server :textDocument/prepareCallHierarchy params :immediate 't)))

(defun ecp-tree--get-incoming-calls (item server)
  "Fetch incoming calls for ITEM."
  (let ((incoming-calls (eglot--request server :callHierarchy/incomingCalls (list :item item) :immediate 't)))
    (ecp-tree--debug-message "Incoming calls: %s" incoming-calls)
    incoming-calls))


(defun ecp-tree--get-children (node eglot-server node-type)
  "Get children for NODE in running EGLOT-SERVER."
  (ecp-tree--debug-message "Getting children for: %s" node)
  (if (listp node)
    (let ((nodes (plist-get node :nodes)))
      (if nodes
          (vector->plist nodes)		; If nodes are directly available, return them.
        (ecp-tree--fetch-children node eglot-server)))
    (ecp-tree--fetch-children node eglot-server)))

(defun ecp-tree--fetch-children (node eglot-server)
  "Fetch children nodes for NODE in running EGLOT-SERVER."
  (condition-case err
      (let* ((children-nodes
              (vector->plist
	       (plist-get
		(eglot--request eglot-server :clojure/workspace/projectTree/nodes node :immediate 't)
		:nodes)))
	     (sorted-nodes (if ecp-tree-sort-nodes
			       (sort children-nodes (lambda (a b)
						      (string< (plist-get a :name)
							       (plist-get b :name))))
			     children-nodes)))
	(ecp-tree--debug-message "Found children: %s" sorted-nodes)
	sorted-nodes)
    (error (ecp-tree--debug-message "Fetch failed: %s" err)
	   nil)))

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
  "Visit uri in NODE on a specific location."
  (ecp-tree--open-file-or-jar node 'file)
  (let ((pos (eglot--lsp-position-to-point (plist-get (plist-get node :range) :start) t)))
    (goto-char pos)))

(defun ecp-tree--find-method (node)
  "Visit method in NODE."
  (ecp-tree--debug-message "Finding method for: %s" node)
  (ecp-tree--visit-node-uri-on-pos node))

(defun ecp-tree--find-variable (node)
  "Visit variable in NODE."
  (ecp-tree--debug-message "Finding variable for: %s" node)
  (ecp-tree--visit-node-uri-on-pos node))

(defun ecp-tree--undefined-action (node)
  "Do nothing - here NODE is used for debugging purposes only."
  (ecp-tree--debug-message "Undefined action for node: %s" node))

(defun ecp-tree--open-external-clj (node)
  "Open external clj(s) library in NODE."
  (if (require 'jarchive nil 'noerror)
      (progn
        (ecp-tree--debug-message "Opening external CLJ file via Jarchive.el for: %s" node)
        (find-file (plist-get node :uri)))
    (message "Jarchive package is required to open %s" (plist-get node :uri))))

;; TODO - add support for class decompiler, see
;; https://github.com/clojure-lsp/clojure-lsp/blob/a77e910998277ede4badc5266b5e321da449c1c0/lib/src/clojure_lsp/feature/java_interop.clj
(defun ecp-tree--click-action (node server)
  "Run action based on NODE's type."
  (let ((nt (plist-get node :type)))
    (cl-case nt
      ((1 2 3 6 9) (ecp-tree--undefined-action node))
      (4 (ecp-tree--open-file-or-jar node 'jar))
      (5 (ecp-tree--open-file-or-jar node 'file))
      (7 (ecp-tree--find-method node))
      (8 (ecp-tree--find-variable node))
      (10 (ecp-tree--visit-caller-definition node))
      (11 (ecp-tree--visit-call-site node))
      (t (ecp-tree--visit-node-uri-on-pos node)))))

(defun ecp-tree--visit-caller-definition (node)
  "Visit the caller's definition from NODE."
  (ecp-tree--visit-node-uri-on-pos node))

(defun ecp-tree--visit-call-site (node)
  "Visit specific call site location from NODE."
  (ecp-tree--visit-node-uri-on-pos node))

(defun ecp-tree--get-call-hierarchy-children (node server)
  "Get call hierarchy children for NODE."
  (let* ((base-items (ecp-tree--prepare-call-hierarchy node server))
         (incoming-calls (cl-loop for item across base-items
                                  append (vector->plist (ecp-tree--get-incoming-calls item server))))
         (caller-nodes (mapcar (lambda (call)
                                 (let* ((from (plist-get call :from))
                                        (ranges (plist-get call :fromRanges))
					(name (plist-get from :name)))
                                   (list :name name
                                         :type ecp-tree--caller-type
                                         :uri (plist-get from :uri)
                                         :range (plist-get from :selectionRange)
                                         :call-ranges ranges)))
                               incoming-calls)))
    (ecp-tree--debug-message "Call hierarchy children: %s" caller-nodes)
    caller-nodes))

(defun ecp-tree--node-expansion (eglot-server node)
  (if (= ecp-tree--method-type (plist-get node :type))
      (ecp-tree--get-call-hierarchy-children node eglot-server)
    (ecp-tree--get-children node eglot-server (plist-get node :type))))

(defun ecp-tree-show ()
  "Display the project tree using eglot's project structure.
This function queries the eglot server for the current project's structure
and displays it in a hierarchical tree view.
It allows users to interact with nodes representing different entities
such as files, namespaces, and methods."
  (interactive)
  (if (and (featurep 'eglot) (featurep 'hierarchy))
      (let* ((ecs (eglot--current-server-or-lose))
	     (project-name (eglot-project-nickname ecs))
	     (buffer-name (format "*%s-ecp-tree*" project-name))
	     (buffer (get-buffer-create buffer-name))
	     (hierarchy (hierarchy-new))
	     (root
	      (eglot--request ecs :clojure/workspace/projectTree/nodes '())))
	(ecp-tree--debug-message "Initializing ecp-tree!")
	(hierarchy-add-tree
	 hierarchy
	 root
	 nil
	 (lambda (node) (ecp-tree--node-expansion ecs node))
	 nil
	 't)
	(switch-to-buffer
	 (hierarchy-tree-display
	  hierarchy
	  (lambda (node _)
	    (let ((name (ecp-tree--node-name node ecs)))
	      (hierarchy-labelfn-indent
	       (insert-text-button name
				   'action (lambda (_)
					     (ecp-tree--click-action node ecs))))))
	  buffer)))
    (error "ECPT requires Emacs 29.1 or newer with built-in 'eglot' and 'hierarchy' packages")))
(provide 'ecp-tree)

;;; ecp-tree.el ends here

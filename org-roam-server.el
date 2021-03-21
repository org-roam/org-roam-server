;;; org-roam-server.el --- Org Roam Database Visualizer -*- lexical-binding: t; -*-

;; Author: Göktuğ Karakaşlı <karakasligk@gmail.com>
;; URL: https://github.com/goktug97/org-roam-server
;; Version: 1.1.3
;; Package-Requires: ((org-roam "1.2.1") (org "9.3") (emacs "26.1") (dash "2.17.0") (simple-httpd "1.5.1") (s "1.12.0") (f "0.20.0"))

;; MIT License

;; Copyright (c) 2020 Göktuğ Karakaşlı

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; A web application to visualize the org-roam database.
;; Use M-x org-roam-server-mode RET to enable the global mode.
;; It will start a web server on http://127.0.0.1:8080

(require 'json)
(require 'subr-x)
(require 'url)

(require 'f)
(require 's)
(require 'dash)
(require 'simple-httpd)

(require 'org)

(require 'ox)
(require 'ox-html)

(require 'org-roam)
(require 'org-roam-graph)
(require 'org-roam-buffer)
(require 'org-roam-link)

;;; Code:

(defvar org-roam-server-data nil)

(defvar org-roam-server-current-buffer (window-buffer))

(defvar org-roam-server-token)

(defvar org-roam-server-root
  (concat (file-name-directory
           (f-full (or
                    load-file-name
                    buffer-file-name)))
          "."))

(defvar org-roam-server-db-last-modification
  (float-time
   (file-attribute-modification-time
    (file-attributes org-roam-db-location))))

(defgroup org-roam-server nil
  "org-roam-server customizable variables."
  :group 'org-roam)

(defcustom org-roam-server-host "127.0.0.1"
  "Server host.
http://`org-roam-server-host`:`org-roam-server-port`."
  :group 'org-roam-server
  :type 'string)

(defcustom org-roam-server-port 8080
  "Server port.
http://127.0.0.1:`org-roam-server-port`."
  :group 'org-roam-server
  :type 'integer)

(defcustom org-roam-server-network-label-wrap-length 20
  "Maximum character length of labels in the network for each line."
  :group 'org-roam-server
  :type 'integer)

(defcustom org-roam-server-network-label-truncate t
  "Truncate label if it exceeds `org-roam-server-label-truncate-length`."
  :group 'org-roam-server
  :type 'boolean)

(defcustom org-roam-server-network-label-truncate-length 60
  "Maximum character length of labels in the network."
  :group 'org-roam-server
  :type 'integer)

(defcustom org-roam-server-network-arrows nil
  "If the type is given, enable arrows in the network.
Available types are to, middle, and from.
Types can be concatanated using commas."
  :group 'org-roam-server
  :type 'string)

(defcustom org-roam-server-network-vis-options nil
  "Options to be passed directly to vis.Network, in JSON format.
e.g. (json-encode (list (cons 'physics (list (cons 'enabled json-false)))))
or { \"physics\": { \"enabled\": false } }"
  :group 'org-roam-server
  :type 'string)

(defcustom org-roam-server-style nil
  "The CSS that can be used to customize the application."
  :group 'org-roam-server
  :type 'string)

(defcustom org-roam-server-authenticate nil
  "Enable authentication."
  :group 'org-roam-server
  :type 'boolean)

(defcustom org-roam-server-export-inline-images t
  "Show inline images in the viewer when `t`."
  :group 'org-roam-server
  :type 'boolean)

(defcustom org-roam-server-serve-files nil
  "Serve local files when `t`."
  :group 'org-roam-server
  :type 'boolean)

(defcustom org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
  "Supported file extensions to be served."
  :group 'org-roam-server
  :type 'list)

(defcustom org-roam-server-network-poll t
  "Poll the network changes if it is set to `t`.
If you have a large network and experience lag
in your system, you may want to set this to `nil`.
If set to `nil` the changes can be reloaded using
the reload button."
  :group 'org-roam-server
  :type 'boolean)

(defcustom org-roam-server-export-style
  (format "<style>%s</style>"
          (with-temp-buffer
            (insert-file-contents
             (concat org-roam-server-root
                     "/assets/org.css"))
            (buffer-string)))
  "HTML export style in css format."
  :group 'org-roam-server
  :type 'string)

(defcustom org-roam-server-extra-node-options nil
  "Additional options for nodes.
A list suitable for `json-encode', e.g. (list (cons 'shape \"box\")),
or a custom function with one argument NODE producing such a list.
In the first case options are applied to all nodes."
  :group 'org-roam-server
  :type '(choice
          (list :tag "Argument to json-encode")
          (function :tag "Custom function")))

(defcustom org-roam-server-extra-edge-options nil
  "Additional options for edges.
A list suitable for `json-encode', e.g. (list (cons 'width 3)),
or a custom function with one argument EDGE producing such a list.
In the first case options are applied to all edges."
  :group 'org-roam-server
  :type '(choice
          (list :tag "Argument to json-encode")
          (function :tag "Custom function")))

(defcustom org-roam-server-default-include-filters "null"
  "Options to set default include filters, in JSON format.
e.g. (json-encode (list (list (cons 'id \"test\") (cons 'parent \"tags\"))))
or [{ \"id\": \"test\", \"parent\" : \"tags\"  }]"
  :group 'org-roam-server
  :type 'string)

(defcustom org-roam-server-default-exclude-filters "null"
  "Options to set default exclude filters, in JSON format.
e.g. (json-encode (list (list (cons 'id \"test\") (cons 'parent \"tags\"))))
or [{ \"id\": \"test\", \"parent\" : \"tags\"  }]"
  :group 'org-roam-server
  :type 'string)

(defcustom org-roam-server-link-auto-replace t
  "When non-nil, replace Org-roam's roam links on export."
  :group 'org-roam-server
  :type 'boolean)

(define-obsolete-variable-alias 'org-roam-server-label-wrap-length
  'org-roam-server-network-label-wrap-length "org-roam-server 1.0.3")
(define-obsolete-variable-alias 'org-roam-server-label-truncate
  'org-roam-server-network-label-truncate "org-roam-server 1.0.3")
(define-obsolete-variable-alias 'org-roam-server-label-truncate-length
  'org-roam-server-network-label-truncate-length "org-roam-server 1.0.3")

(define-obsolete-variable-alias 'org-roam-server-webserver-supported-extensions
  'org-roam-server-served-file-extension "org-roam-server 1.0.5")

(make-obsolete 'org-roam-server-enable-access-to-local-files
               "the files are served with the org-roam-server instead of an external server."
               "org-roam-server 1.0.5")

(make-obsolete 'org-roam-server-webserver-address
               "the files are served with the org-roam-server instead of an external server."
               "org-roam-server 1.0.5")

(make-obsolete 'org-roam-server-webserver-prefix
               "the files are served with the org-roam-server instead of an external server."
               "org-roam-server 1.0.5")

(defun org-roam-server-random-token (length)
  "Create a random token with length of `LENGTH`."
  (with-temp-buffer
    (dotimes (_ length)
      (insert
       (let ((x (random 36)))
         (if (< x 10) (+ x ?0) (+ x (- ?a 10))))))
    (buffer-string)))

(defun org-roam-server-concat-or-regexp-tokens (tokens)
  "Concatenate TOKENS into OR regexp."
  (let ((result (car tokens)))
    (dolist (token (cdr tokens))
      (setq result (format "%s\\|%s" result token)))
    result))

(defun org-roam-server-html-servlet (file)
  "Export the FILE to HTML and create a servlet for it."
  `(defservlet* ,(intern (concat (org-roam--path-to-slug file) ".html")) text/html (token)
     (if org-roam-server-authenticate
         (if (not (string= org-roam-server-token token))
             (httpd-error httpd-current-proc 403)))
     (let ((html-string))
       (org-roam--with-temp-buffer ,file
         (setq-local org-export-with-sub-superscripts nil)
         (setq-local org-html-style-default org-roam-server-export-style)
         (when org-roam-server-link-auto-replace
           (org-roam-link-replace-all))

         ;; Handle served files
         (if org-roam-server-serve-files
             (let* ((file-string (buffer-string))
                    (-regexp (format "\\[\\[\\(file:\\)\\(.*\\.\\(%s\\)\\)\\]\\(\\[.*\\]\\)?\\]"
                                     (org-roam-server-concat-or-regexp-tokens
                                      org-roam-server-served-file-extensions)))
                    (positions (s-matched-positions-all -regexp file-string))
                    (matches (s-match-strings-all -regexp file-string)))
               (dolist (match matches)
                 (eval (org-roam-server-export-file-servlet match))
                 (let ((path (elt match 2))
                       (link (elt match 0)))
                   (if (file-exists-p path)
                       (setq file-string
                             (s-replace link (format "[[file:%s]%s]"
                                                     (secure-hash 'sha256 path)
                                                     (elt match 4))
                                        file-string)))))
               (erase-buffer)
               (insert file-string)
               (if positions
                   (dolist (p positions)
                     (save-excursion
                       (goto-char (car p))
                       (previous-line)
                       (insert "#+ATTR_HTML: :target _blank"))))))

         ;; Handle images
         (if org-roam-server-export-inline-images
             (let* ((file-string (buffer-string))
                    (matches (s-match-strings-all "\\[\\[\\(file:\\)\\(.*\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\)\\)\\]\\(\\[.*\\]\\)?\\]" file-string)))
               (dolist (match matches)
                 (let ((path (elt match 2))
                       (link (elt match 0)))
                   (unless (file-name-absolute-p path)
                     (setq path (concat (file-name-directory ,file) path)))
                   (setq path (f-full path))
                   (if (file-exists-p path)
                       (setq file-string
                             (s-replace link (format "[[image:%s]]" path) file-string)))))
               (erase-buffer)
               (insert file-string)))
         (setq html-string (org-export-as 'html)))
       (insert html-string))))

(defun org-roam-server-capture-servlet ()
  "Create a servlet for the recently captured `org-roam` file.
This is added as a hook to `org-capture-after-finalize-hook'."
  (when (and (not org-note-abort))
    (if-let ((file (org-roam-capture--get :file-path)))
        (eval (org-roam-server-html-servlet file)))))

(defun org-roam-server-visjs-json (node-query)
  "Convert `org-roam` NODE-QUERY db query to the visjs json format."
  (org-roam-db--ensure-built)
  (org-roam--with-temp-buffer nil
    (let* ((-compare-fn (lambda (x y) (string= (car x) (car y))))
           (nodes (-distinct (org-roam-db-query node-query)))
           (edges-query
            `[:with selected :as [:select [file] :from ,node-query]
                    :select :distinct [dest source] :from links
                    :where (and (in dest selected) (in source selected))])
           (edges-cites-query
            `[:with selected :as [:select [file] :from ,node-query]
                    :select :distinct [file source] :from links
                    :inner :join refs :on (and (= links:dest refs:ref)
                                               (= links:type "cite")
                                               (= refs:type "cite"))
                    :where (and (in file selected) (in source selected))])
           (edges       (org-roam-db-query edges-query))
           (edges-cites (org-roam-db-query edges-cites-query))
           (graph (list (cons 'nodes (list))
                        (cons 'edges (list)))))
      (dotimes (idx (length nodes))
        (let* ((file (xml-escape-string (car (elt nodes idx))))
               (title (or (cadr (elt nodes idx))
                          (org-roam--path-to-slug file)))
               (tags (elt (elt nodes idx) 2)))
          (push (append (list (cons 'id (org-roam--path-to-slug file))
                              (cons 'title title)
                              (cons 'tags tags)
                              (cons 'label (s-word-wrap
                                            org-roam-server-network-label-wrap-length
                                            (if org-roam-server-network-label-truncate
                                                (s-truncate
                                                 org-roam-server-network-label-truncate-length
                                                 title)
                                              title)))
                              (cons 'url (concat "org-protocol://roam-file?file="
                                                 (url-hexify-string file)))
                              (cons 'path file))
                        (pcase org-roam-server-extra-node-options
                          ('nil nil)
                          ((pred functionp)
                           (funcall org-roam-server-extra-node-options (elt nodes idx)))
                          ((pred listp)
                           org-roam-server-extra-node-options)
                          (wrong-type
                           (error "Wrong type of org-roam-server-extra-node-options: %s"
                                  wrong-type))))
                (cdr (elt graph 0)))))
      (dolist (edge edges)
        (let* ((title-source (org-roam--path-to-slug (elt edge 0)))
               (title-target (org-roam--path-to-slug (elt edge 1))))
          (push (remove nil (append (list (cons 'from title-source)
                                          (cons 'to title-target)
                                          (cons 'arrows org-roam-server-network-arrows))
                                    (pcase org-roam-server-extra-edge-options
                                      ('nil nil)
                                      ((pred functionp)
                                       (funcall org-roam-server-extra-edge-options edge))
                                      ((pred listp)
                                       org-roam-server-extra-edge-options)
                                      (wrong-type
                                       (error "Wrong type of org-roam-server-extra-edge-options: %s"
                                              wrong-type)))))
                (cdr (elt graph 1)))))
      (dolist (edge edges-cites)
        (let* ((title-source (org-roam--path-to-slug (elt edge 0)))
               (title-target (org-roam--path-to-slug (elt edge 1))))
          (push (remove nil (list (cons 'from title-source)
                                  (cons 'to title-target)
                                  (cons 'arrows org-roam-server-network-arrows)))
                (cdr (elt graph 1)))))
      (json-encode graph))))

(defun org-roam-server-update-current-buffer ()
  "Save the current buffer in a variable to serve to the server."
  (setq org-roam-server-current-buffer (window-buffer)))

(defun org-roam-server-export-server-id (link description format)
  "Custom export setting for backlinks.
Use LINK as an id in the <a> html tag instead
of href as it is used in another window.
The setting only applies to HTML FORMAT.
DESCRIPTION is the shown attribute to the user."
  (let ((desc (or description link)))
    (pcase format
      (`html
       (format "<a name=\"backlink\" id=\"%s\" href=\"javascript:void(0)\">%s</a>"
               link desc))
      (_ link))))

(defun org-roam-server-inline-image-servlet (link)
  "Create servlet in the server for the given image LINK."
  (let ((ext (cadr (s-match (cdar org-html-inline-image-rules) link))))
    `(defservlet* ,(intern (secure-hash 'sha256 link)) ,(httpd-get-mime ext) (token)
       (if org-roam-server-authenticate
           (if (not (string= org-roam-server-token token))
               (httpd-error httpd-current-proc 403)))
       (set-buffer-multibyte nil)
       (insert-file-contents-literally ,link))))

(defun org-roam-server-export-file-servlet (file)
  "Create servlet in the server for the given FILE LINK."
  `(defservlet* ,(intern (secure-hash 'sha256 (elt file 2)))
     ,(httpd-get-mime (elt file 3)) (token)
     (if org-roam-server-authenticate
         (if (not (string= org-roam-server-token token))
             (httpd-error httpd-current-proc 403)))
     (if org-roam-server-serve-files
         (progn
           (set-buffer-multibyte nil)
           (insert-file-contents-literally ,(elt file 2)))
       (httpd-error httpd-current-proc 403))))

(defun org-roam-server-export-file-id (link description format)
  "Append token to the file links.
`org-roam-server-token` is appended to the LINK.
The setting only applies to HTML FORMAT.
DESCRIPTION is the shown attribute to the user."
  (let ((desc (or description link)))
    (pcase format
      (`html
       (when (org-roam--org-roam-file-p link)
         (let ((html-link (concat (file-name-sans-extension link) ".html")))
           (if org-roam-server-authenticate
               (format "<a href=%s?token=%s>%s</a>"
                       html-link org-roam-server-token desc)
             (format "<a href=%s>%s</a>" html-link desc))))))))

(defun org-roam-server-export-image-id (link description format)
  "Create servlet for the image links.
`org-roam-server-token` is appended to the LINK.
The setting only applies to HTML FORMAT.
DESCRIPTION is the shown attribute to the user if the image is not rendered."
  (let ((desc (or description link)))
    (pcase format
      (`html
       (eval (org-roam-server-inline-image-servlet link))
       (let ((html-link (secure-hash 'sha256 link)))
         (if org-roam-server-authenticate
             (format "<img src=%s?token=%s alt=%s />"
                     html-link org-roam-server-token desc)
           (format "<img src=%s alt=%s />" html-link desc)))))))

;;;###autoload
(define-minor-mode org-roam-server-mode
  "Start the http server and serve org-roam files."
  :lighter ""
  :global t
  :group 'org-roam-server
  :init-value nil
  (cond
   (org-roam-server-mode
    (if org-roam-server-authenticate
        (progn
          (setq org-roam-server-token (org-roam-server-random-token 64))
          (switch-to-buffer-other-window "*org-roam-server*")
          (with-current-buffer "*org-roam-server*"
            (erase-buffer)
            (insert "* Org Roam Server\n")
            (insert "`org-roam-server-authenticate` is enabled.")
            (insert "To open the web application please go to;\n")
            (insert
             (url-recreate-url
              (url-parse-make-urlobj
               "http" nil nil
               org-roam-server-host
               org-roam-server-port
               (format "?token=%s" org-roam-server-token)
               nil nil t)))
            (org-mode)
            (outline-show-all))))
    (add-hook 'post-command-hook #'org-roam-server-find-file-hook-function)
    (add-hook 'org-capture-after-finalize-hook #'org-roam-server-capture-servlet)
    (org-link-set-parameters "server" :export #'org-roam-server-export-server-id)
    (org-link-set-parameters "file" :export #'org-roam-server-export-file-id)
    (org-link-set-parameters "image" :export #'org-roam-server-export-image-id)
    (setq-local httpd-port org-roam-server-port)
    (setq-local httpd-host org-roam-server-host)
    (setq httpd-root org-roam-server-root)
    (httpd-start)
    (let ((-compare-fn (lambda (x y) (string= (car x) (car y))))
          (node-query `[:select [file title] :from titles
                                ,@(org-roam-graph--expand-matcher 'file t)]))
      (org-roam--with-temp-buffer nil
        (let ((nodes (-distinct (org-roam-db-query node-query))))
          (dotimes (idx (length nodes))
            (let ((file (xml-escape-string (car (elt nodes idx)))))
              (if (org-roam--org-roam-file-p file)
                  (eval (org-roam-server-html-servlet file)))))))))
   (t
    (remove-hook 'post-command-hook #'org-roam-server-find-file-hook-function t)
    (remove-hook 'org-capture-after-finalize-hook #'org-roam-server-capture-servlet)
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (remove-hook 'post-command-hook #'org-roam-server-update-current-buffer t)))
    (httpd-stop))))

(defun org-roam-server-find-file-hook-function ()
  "If the current visited file is an `org-roam` file, update the current buffer."
  (when (org-roam--org-roam-file-p)
    (setq org-roam-last-window (get-buffer-window))
    (add-hook 'post-command-hook #'org-roam-server-update-current-buffer nil t)
    (org-roam-server-update-current-buffer)))

(defservlet* current-buffer-data text/event-stream (token)
  (if org-roam-server-authenticate
      (if (not (string= org-roam-server-token token))
          (httpd-error httpd-current-proc 403)))
  (insert (format "data: %s\n\n"
                  (if (org-roam--org-roam-file-p
                       (buffer-file-name org-roam-server-current-buffer))
                      (car (last
                            (split-string
                             (org-roam--path-to-slug
                              (buffer-name org-roam-server-current-buffer))
                             "/")))
                    ""))))

(defservlet* roam-data text/event-stream (force token)
  (if org-roam-server-authenticate
      (if (not (string= org-roam-server-token token))
          (httpd-error httpd-current-proc 403)))
  (let ((node-query `[:select [titles:file titles:title tags] :from titles
                              :left :outer :join tags :on (= titles:file tags:file)
                              ,@(org-roam-graph--expand-matcher 'titles:file t)]))
    (if force
        (insert (format "data: %s\n\n"
                        (org-roam-server-visjs-json node-query)))
      (when (and org-roam-server-network-poll
                 (> (abs (- org-roam-server-db-last-modification
                            (float-time
                             (file-attribute-modification-time
                              (file-attributes org-roam-db-location)))))
                    1e-6))
        (let ((data (org-roam-server-visjs-json node-query)))
          (setq org-roam-server-db-last-modification
                (float-time
                 (file-attribute-modification-time
                  (file-attributes org-roam-db-location))))
          (insert (format "data: %s\n\n" data)))))))

(defservlet* network-vis-options application/json (token)
  (if org-roam-server-authenticate
      (if (not (string= org-roam-server-token token))
          (httpd-error httpd-current-proc 403)))
  (insert (or org-roam-server-network-vis-options "{}")))

(defservlet* server-css text/css (token)
  (if org-roam-server-authenticate
      (if (not (string= org-roam-server-token token))
          (httpd-error httpd-current-proc 403)))
  (insert (or org-roam-server-style ":empty")))

(defservlet* default-filters application/json (token)
  (if org-roam-server-authenticate
      (if (not (string= org-roam-server-token token))
          (httpd-error httpd-current-proc 403)))
  (insert (format "{\"include\" : %s, \"exclude\": %s}"
                  org-roam-server-default-include-filters
                  org-roam-server-default-exclude-filters)))

(defun org-roam-server-insert-title (title)
  "Insert the TITLE as `org-document-title`."
  (insert (propertize title 'font-lock-face 'org-document-title)))

(defun org-roam-server-insert-citelinks (file)
  "Insert citation backlinks for the FILE."
  (if-let* ((ref (with-temp-buffer
                   (insert-file-contents file)
                   (org-roam--extract-ref)))
            (org-ref-p (require 'org-ref nil t)) ; Ensure that org-ref is present
            (key-backlinks (org-roam--get-backlinks (cdr ref)))
            (grouped-backlinks (--group-by (nth 0 it) key-backlinks)))
      (progn
        (insert (let ((l (length key-backlinks)))
                  (format "\n\n* %d %s\n"
                          l (org-roam-buffer--pluralize "Cite backlink" l))))
        (dolist (group grouped-backlinks)
          (let ((file-from (car group))
                (bls (cdr group)))
            (insert (format "** [[server:%s][%s]]\n"
                            (car (last (split-string file-from "/")))
                            (org-roam-db--get-title file-from)))
            (dolist (backlink bls)
              (pcase-let ((`(_ _ ,props) backlink))
                (if-let ((content (funcall org-roam-buffer-preview-function file-from (plist-get props :point))))
                    (insert (s-trim
                             (s-replace "\n" " "
                                        (s-replace
                                         (format "file:%s" (f-full org-roam-directory))
                                         "server:" content)))))
                (insert "\n\n"))))))
    (insert "\n\n* No cite backlinks!")))

(defun org-roam-server-insert-backlinks (file)
  "Insert the backlinks string for the FILE."
  (if file
      (if-let* ((backlinks (org-roam--get-backlinks file))
                (grouped-backlinks (--group-by (nth 0 it) backlinks)))
          (progn
            (insert (let ((l (length backlinks)))
                      (format "\n\n* %d %s\n"
                              l (org-roam-buffer--pluralize "Backlink" l))))
            (dolist (group grouped-backlinks)
              (let* ((file-from (car group))
                     (props (mapcar (lambda (row) (nth 2 row)) (cdr group)))
                     (props (seq-sort-by (lambda (p) (plist-get p :point)) #'< props)))
                (insert (format "** [[server:%s][%s]]\n"
                                (car (last (split-string file-from "/")))
                                (org-roam-db--get-title file-from)))
                (dolist (prop props)
                  (if-let ((content (funcall org-roam-buffer-preview-function file-from (plist-get prop :point))))
                      (insert (s-trim
                               (s-replace "\n" " "
                                          (s-replace
                                           "file:"
                                           "server:" content)))))
                  (insert "\n\n")))))
        (insert "\n\n* No backlinks!"))))

(defservlet* org-roam-buffer text/html (path label token)
  (if org-roam-server-authenticate
      (if (not (string= org-roam-server-token token))
          (httpd-error httpd-current-proc 403)))
  (if (and path label)
      (let ((source-org-roam-directory org-roam-directory)
            (html-string))
        (with-temp-buffer
          (erase-buffer)
          (setq-local org-roam-directory source-org-roam-directory)
          (setq-local default-directory source-org-roam-directory)
          (setq-local org-html-style-default org-roam-server-export-style)
          (setq-local org-export-with-toc nil)
          (setq-local org-export-with-section-numbers nil)
          (setq-local org-export-with-sub-superscripts nil)
          (org-roam-server-insert-title label)
          (org-roam-server-insert-backlinks path)
          (org-roam-server-insert-citelinks path)
          (setq html-string (org-export-as 'html)))
        (insert html-string))))

(provide 'org-roam-server)

;;; org-roam-server.el ends here

;;; org-roam-server.el --- Org Roam Database Visualizer -*- lexical-binding: t; -*-

;; Author: Göktuğ Karakaşlı <karakasligk@gmail.com>
;; URL: https://github.com/goktug97/org-roam-server
;; Version: 1.0.1
;; Package-Requires: ((org-roam "1.1.1") (org "9.3") (emacs "26.1") (simple-httpd "1.5.1"))

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

(require 'simple-httpd)
(require 'json)
(require 'subr-x)

(require 'org)

(require 'ox)
(require 'ox-html)

(require 'org-roam)
(require 'org-roam-graph)
(require 'org-roam-buffer)

;;; Code:
(defvar org-roam-server-export-style)

(defvar org-roam-server-data nil)

(defvar org-roam-server-current-buffer (window-buffer))

(defun org-roam-server-update-current-buffer ()
  "Save the current buffer in a variable to serve to the server."
  (setq org-roam-server-current-buffer (window-buffer)))

(defvar org-roam-server-root
  (concat (file-name-directory
           (file-truename (or
                           load-file-name
                           buffer-file-name)))
          "."))
(setq httpd-root org-roam-server-root)

(defun org-roam-server-html-servlet (file)
  "Export the FILE to HTML and create a servlet for it."
  `(defservlet* ,(intern (concat (org-roam--path-to-slug file) ".html")) text/html ()
     (let ((html-string))
       (with-temp-buffer
         (setq-local org-export-with-sub-superscripts nil)
         (setq-local org-html-style-default org-roam-server-export-style)
         (insert-file-contents ,file)
         (setq html-string (org-export-as 'html)))
       (insert html-string))))

(defun org-roam-server-capture-servlet ()
  "Create a servlet for the recently captured `org-roam` file.
This is added as a hook to `org-capture-after-finalize-hook'."
  (when (and (not org-note-abort)
             (eq (org-roam-capture--get :capture-fn)
                 'org-roam-insert))
    (let ((file (org-roam-capture--get :file-path)))
      (eval (org-roam-server-html-servlet file)))))

(defun org-roam-server-visjs-json (node-query)
  "Convert `org-roam` NODE-QUERY db query to the visjs json format."
  (org-roam-db--ensure-built)
  (org-roam--with-temp-buffer
    (let* ((nodes (org-roam-db-query node-query))
           (edges-query
            `[:with selected :as [:select [file] :from ,node-query]
                    :select :distinct [to from] :from links
                    :where (and (in to selected) (in from selected))])
           (edges-cites-query
            `[:with selected :as [:select [file] :from ,node-query]
                    :select :distinct [file from] :from links
                    :inner :join refs :on (and (= links:to refs:ref)
                                               (= links:type "cite")
                                               (= refs:type "cite"))
                    :where (and (in file selected) (in from selected))])
           (edges       (org-roam-db-query edges-query))
           (edges-cites (org-roam-db-query edges-cites-query))
           (graph (list (cons 'nodes (list))
                        (cons 'edges (list)))))
      (dotimes (idx (length nodes))
        (let* ((file (xml-escape-string (car (elt nodes idx))))
               (title (or (caadr (elt nodes idx))
                          (org-roam--path-to-slug file))))
          (push (list (cons 'id (org-roam--path-to-slug file))
                      (cons 'label (xml-escape-string title))
                      (cons 'url (concat "org-protocol://roam-file?file="
                                         (url-hexify-string file)))
                      (cons 'path file))
                (cdr (elt graph 0)))))
      (dolist (edge edges)
        (let* ((title-source (org-roam--path-to-slug (elt edge 0)))
               (title-target (org-roam--path-to-slug (elt edge 1))))
          (push (list (cons 'from title-source)
                      (cons 'to title-target)
                      ;(cons 'arrows "to")
                      )
                (cdr (elt graph 1)))))
      (dolist (edge edges-cites)
        (let* ((title-source (org-roam--path-to-slug (elt edge 0)))
               (title-target (org-roam--path-to-slug (elt edge 1))))
          (push (list (cons 'from title-source)
                      (cons 'to title-target)
                      ;(cons 'arrows "to")
                      )
                (cdr (elt graph 1)))))
      (json-encode graph))))


;;;###autoload
(define-minor-mode org-roam-server-mode
  "Start the http server and serve org-roam files."
  :lighter ""
  :global t
  :init-value nil
  (cond
   (org-roam-server-mode
    (setq org-roam-server-export-style
          (format "<style>%s</style>"
                  (with-temp-buffer
                    (insert-file-contents
                     (concat org-roam-server-root
                             "/assets/org.css"))
                    (buffer-string))))
    (add-hook 'post-command-hook #'org-roam-server-find-file-hook-function)
    (add-hook 'org-capture-after-finalize-hook #'org-roam-server-capture-servlet)
    (httpd-start)
    (let ((node-query `[:select [file titles] :from titles
                                ,@(org-roam-graph--expand-matcher 'file t)]))
      (org-roam--with-temp-buffer
        (let ((nodes (org-roam-db-query node-query)))
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

(defservlet* current-buffer-data text/event-stream ()
  (insert (format "data: %s\n\n"
                  (if (org-roam--org-roam-file-p
                       (buffer-file-name org-roam-server-current-buffer))
                      (car (last
                            (split-string
                             (org-roam--path-to-slug
                              (buffer-name org-roam-server-current-buffer))
                             "/")))
                    ""))))

(defservlet* roam-data text/event-stream (force)
  (let* ((node-query `[:select [file titles]
                               :from titles
                               ,@(org-roam-graph--expand-matcher 'file t)])
         (data (org-roam-server-visjs-json node-query)))
    (when (or force (not (string= data org-roam-server-data)))
      (setq org-roam-server-data data)
      (insert (format "data: %s\n\n" org-roam-server-data)))))

(org-link-set-parameters "server" :export #'org-roam-server-export-server-id)

(defun org-roam-server-export-server-id (link description format)
  "Custom export setting for backlinks.
Use LINK as an id in the <a> html tag instead
of href as it is used in another window.
The setting only applies to HTML FORMAT.
DESCRIPTION is the shown attribute to the user."
  (let ((desc (or description link)))
    (pcase format
      (`html (format "<a name=\"backlink\" id=\"%s\" href=\"javascript:void(0)\">%s</a>" link desc))
      (_ link))))

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
                            (org-roam--get-title-or-slug file-from)))
            (dolist (backlink bls)
              (pcase-let ((`(_ _ ,props) backlink))
                (insert (s-trim
                         (s-replace "\n" " "
                                    (s-replace
                                     (format "file:%s" (file-truename org-roam-directory))
                                     "server:" (plist-get props :content)))))
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
              (let ((file-from (car group))
                    (bls (cdr group)))
                (insert (format "** [[server:%s][%s]]\n"
                                (car (last (split-string file-from "/")))
                                (org-roam--get-title-or-slug file-from)))
                (dolist (backlink bls)
                  (pcase-let ((`(_ _ ,props) backlink))
                    (insert (s-trim
                             (s-replace "\n" " "
                                        (s-replace
                                         (format "file:%s" (file-truename org-roam-directory))
                                         "server:" (plist-get props :content)))))
                    (insert "\n\n"))))))
        (insert "\n\n* No backlinks!"))))

(defservlet* org-roam-buffer text/html (path label)
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

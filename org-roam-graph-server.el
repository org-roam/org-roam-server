;; Author: Göktuğ Karakaşlı <karakasligk@gmail.com>
;; URL: https://github.com/goktug97/org-roam-graph-server
;; Version: 1.0.0
;; Package-Requires: ((org-roam) (simple-httpd "1.5.1"))

;;; Commentary:
;; A web application to visualize the org-roam database in an interactive graph.

(require 'simple-httpd)
(require 'json)

(require 'ox)
(require 'ox-html)

(require 'org-roam)
(require 'org-roam-graph)
(require 'org-roam-buffer)

(defvar org-roam-graph-current-buffer (window-buffer))
(defun org-roam-graph-update-current-buffer ()
    (setq org-roam-graph-current-buffer
          (window-buffer)))

(defvar org-roam-graph-server-root (concat (file-name-directory (or load-file-name buffer-file-name)) "."))
(setq httpd-root org-roam-graph-server-root)

(defun file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-roam-html-servlet (file)
  `(defservlet ,(intern (concat (org-roam--path-to-slug file) ".html")) text/html (path)
     (let ((html-string))
       (with-temp-buffer
         (setq-local
          org-html-style-default
          (format "<style>%s</style>"
                  (file-contents
                   (concat org-roam-graph-server-root
                           "/assets/org.css"))))
         (insert-file-contents ,file)
         (setq html-string (org-export-as 'html)))
       (insert html-string))))

(defun org-roam-graph--json (node-query)
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
                                               (= links:type "cite"))
                    :where (and (in file selected) (in from selected))])
           (edges       (org-roam-db-query edges-query))
           (edges-cites (org-roam-db-query edges-cites-query))
           (graph (list (cons 'nodes (list))
                        (cons 'edges (list)))))
      (dotimes (idx (length nodes))
        (let* ((file (xml-escape-string (car (elt nodes idx))))
               (title (or (caadr (elt nodes idx))
                          (org-roam--path-to-slug file)))
               (shortened-title (s-truncate org-roam-graph-max-title-length title)))
          (push (list (cons 'id (org-roam--path-to-slug file))
                      (cons 'label (xml-escape-string title))
                      (cons 'url (concat "org-protocol://roam-file?file="
                                         (url-hexify-string file))))
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

(define-minor-mode org-roam-graph-server-mode
  :lighter ""
  :global t
  :init-value nil
  (cond
   (org-roam-graph-server-mode
    (add-hook 'post-command-hook #'org-roam-graph-find-file-hook-function)
    (httpd-start)
    (let ((node-query `[:select [file titles] :from titles
                                ,@(org-roam-graph--expand-matcher 'file t)]))
      (org-roam--with-temp-buffer
        (let ((nodes (org-roam-db-query node-query)))
          (dotimes (idx (length nodes))
            (let ((file (xml-escape-string (car (elt nodes idx)))))
              (if (org-roam--org-roam-file-p file)
                  (eval (org-roam-html-servlet file)))))))))
   (t
    (remove-hook 'post-command-hook #'org-roam-graph-find-file-hook-function t)
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (remove-hook 'post-command-hook #'org-roam-graph-update-current-buffer t)))
    (httpd-stop))))

(defun org-roam-graph-find-file-hook-function ()
  (when (org-roam--org-roam-file-p)
    (setq org-roam-last-window (get-buffer-window))
    (add-hook 'post-command-hook #'org-roam-graph-update-current-buffer nil t)
    (org-roam-graph-update-current-buffer)))

(defservlet current-roam-buffer text/event-stream (path)
  (insert (format "data:%s\n\n"
                  (if (org-roam--org-roam-file-p
                       (buffer-file-name org-roam-graph-current-buffer))
                      (car (last
                            (split-string
                             (org-roam--path-to-slug
                              (buffer-name org-roam-graph-current-buffer))
                             "/")))
                    ""))))

(defservlet graph-data text/event-stream (path)
  (let* ((node-query `[:select [file titles]
                               :from titles
                               ,@(org-roam-graph--expand-matcher 'file t)]))
    (insert (format "data:%s\n\n" (org-roam-graph--json node-query)))))

(provide 'org-roam-graph-server)

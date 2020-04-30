(require 'simple-httpd)
(require 'json)
(require 'org-roam)
(require 'org-roam-graph)
(require 'org-roam-buffer)

(defvar org-roam-graph-current-buffer (window-buffer))
(defun org-roam-graph-update-current-buffer ()
    (setq org-roam-graph-current-buffer
          (window-buffer)))

(setq httpd-root (concat (file-name-directory (or load-file-name buffer-file-name)) "."))

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
                    :inner :join refs :on (and ;(= links:to refs:ref)
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
        (print edge)
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
    (httpd-start))
   (t
    (remove-hook 'post-command-hook #'org-roam-graph-find-file-hook-function t)
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (org-link-set-parameters "file" :face 'org-link)
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

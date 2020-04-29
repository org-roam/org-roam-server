(require 'simple-httpd)
(require 'json)
(require 'org-roam-graph)

(defun org-roam-graph--json (node-query)
  "Build the json string for NODE-QUERY.
The Org-roam database titles table is read, to obtain the list of titles.
The links table is then read to obtain all directed links, and formatted
into a json."
  (org-roam-db--ensure-built)
  (org-roam--with-temp-buffer
    (let* ((nodes (org-roam-db-query node-query))
           (edges-query
            `[:with selected :as [:select [file] :from ,node-query]
                    :select :distinct [to from] :from links
                    :where (and (in to selected) (in from selected))])
           (edges-cites-query
            `[:with selected :as [:select [file] :from ,node-query]
                    :select :distinct [file from]
                    :from links :inner :join refs :on (and (= links:to refs:ref)
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
                      (cons 'x (* 100.0 (cos (/ (* 2.0 idx pi) (length nodes)))))
                      (cons 'y (* 100.0 (sin (/ (* 2.0 idx pi) (length nodes)))))
                      ;; (cons 'size 30)
                      (cons 'url "123"))
                (cdr (elt graph 0)))))
      (let ((idx 0))
        (dolist (edge edges)
          (let* ((title-source (org-roam--path-to-slug (elt edge 0)))
                 (title-target (org-roam--path-to-slug (elt edge 1))))
            (push (list (cons 'id (format "e%d" idx))
                        (cons 'source title-source)
                        (cons 'target title-target))
                  (cdr (elt graph 1)))
            (incf idx 1)))
        (dolist (edge edges-cites)
          (let* ((title-source (org-roam--path-to-slug (elt edge 0)))
                 (title-target (org-roam--path-to-slug (elt edge 1))))
            (push (list (cons 'id (format "e%d" idx))
                        (cons 'source title-source)
                        (cons 'target title-target))
                  (cdr (elt graph 1)))
            (incf idx 1))))
      (json-encode graph))))

(defun org-roam-graph-server-start ()
  (interactive)
  (httpd-start))

(defun org-roam-graph-server-stop ()
  (interactive)
  (httpd-stop))

;; (defservlet graph text/html (path)
;;   (httpd-send-file
;;    httpd-current-proc
;;    (concat (file-name-directory httpd-root) "graph.html")))

(defservlet graph-data text/event-stream (path)
  (let* ((node-query `[:select [file titles]
                               :from titles
                               ,@(org-roam-graph--expand-matcher 'file t)]))
    (insert (format "data:%s\n\n" (org-roam-graph--json node-query)))))

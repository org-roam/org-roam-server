org-roam-graph-server
===================================

![Graph](https://raw.githubusercontent.com/goktug97/org-roam-graph-server/master/org-roam-graph.png)

## Requirements

- [simple-httpd](https://github.com/skeeto/emacs-web-server/)

## use-package

```bash
git clone https://github.com/goktug97/org-roam-graph-server
```

```elisp
(use-package org-roam-graph-server
  :after org-roam
  :ensure nil
  :load-path <path-to-org-roam-graph-server-folder>)
```

## Usage

Use `M-x org-roam-graph-server-start RET` to start the server and goto http://127.0.0.1:8080/

To stop it: `M-x org-roam-graph-server-stop RET`

## Clickable Graph
The graph uses org-roam protocol which means if you click one of the nodes
it opens the org file. For it to work, you should set up the org-roam protocol;
https://org-roam.readthedocs.io/en/develop/roam_protocol/

Also make sure the emacs server is started; `M-x server-start RET`

## License
org-roam-graph-server is licensed under the MIT License.

For Javascript libraries refer to;
https://github.com/jquery/jquery/blob/master/LICENSE.txt
https://github.com/visjs/vis-network

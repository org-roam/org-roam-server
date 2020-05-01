org-roam-graph-server
===================================

![Graph](https://raw.githubusercontent.com/goktug97/org-roam-graph-server/master/org-roam-graph-server.png)

## Requirements

- [simple-httpd](https://github.com/skeeto/emacs-web-server/)

## use-package

```bash
git clone https://github.com/goktug97/org-roam-graph-server
```

```elisp
(use-package org-roam-graph-server
  :ensure nil
  :load-path <path-to-org-roam-graph-server-folder>)
```

## Usage

Use `M-x org-roam-graph-server-mode RET` to enable the global mode. 
It will start a web server on http://127.0.0.1:8080/

## Clickable Graph
The graph uses org-roam protocol which means if you click one of the nodes
it opens the org file. For it to work, you should set up the org-roam protocol;
https://org-roam.readthedocs.io/en/develop/roam_protocol/

Also make sure the emacs server is started; `M-x server-start RET`

## Contribution
- I don't know Javascript, I just smash my keyboard until it works. So if someone,
proficient in Javascript, thinks the code looks really bad, please clean it up
and open a pull request.
- I think the grouping code
https://github.com/goktug97/org-roam-graph-server/blob/6b33e9d54c6babdfec826283d8c268628803f3dc/index.html#L115-L149
is not good. So if anyone has a better idea for grouping please fix it and open a pull request.

## License
org-roam-graph-server is licensed under the MIT License.

For Javascript libraries please refer to;
- https://github.com/jquery/jquery/blob/master/LICENSE.txt
- https://github.com/visjs/vis-network
- https://github.com/twbs/bootstrap/blob/master/LICENSE

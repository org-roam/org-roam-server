org-roam-server
===================================
![License](https://img.shields.io/github/license/org-roam/org-roam-server)
![Release](https://img.shields.io/github/v/release/org-roam/org-roam-server)
[![MELPA](https://melpa.org/packages/org-roam-server-badge.svg)](https://melpa.org/#/org-roam-server)

![Graph](https://raw.githubusercontent.com/goktug97/org-roam-server/master/org-roam-server.gif)

## Installation

`org-roam-server` is on MELPA. You can directly install from there.

```elisp
(use-package org-roam-server
  :ensure t)
```

## Usage

Use `M-x org-roam-server-mode RET` to enable the global mode. 
It will start a web server on http://127.0.0.1:8080/.

Although it will automatically reload if there is a change in the
database, sometimes it fails to establish a connection. If it does not
reload after `org-roam-build-cache`, you can do it manually by
clicking the `reload` button which will reload the data, rebuild the
graph and refresh connections.

## Org-Roam Protocol
The graph utilizes org-roam protocol. Which means if you click on one of the nodes,
it will open the corresponding file in Emacs. For this future to work, org-roam protocol
should be configured in the system. 

[Configuring Org-Roam Protocol](https://org-roam.readthedocs.io/en/develop/roam_protocol/)

Also make sure the emacs server is started; `M-x server-start RET`

## License
org-roam-server is licensed under the MIT License.

For Javascript and CSS libraries please refer to;
- https://github.com/jquery/jquery/blob/master/LICENSE.txt
- https://github.com/visjs/vis-network
- https://github.com/twbs/bootstrap/blob/master/LICENSE
- https://github.com/gongzhitaao/orgcss
- https://github.com/select2/select2

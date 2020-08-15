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
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))
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
The graph utilizes org-roam protocol which means if you click on one
of the nodes, it will open the corresponding file in Emacs. For this
feature to work, org-roam protocol should be configured in the system.

[Configuring Org-Roam Protocol](https://www.orgroam.com/manual/Installation-_00281_0029.html#Installation-_00281_0029)

Also make sure the emacs server is started; `M-x server-start RET`


## Expose Local files

![localfiles](https://raw.githubusercontent.com/goktug97/org-roam-server/master/local-files.gif)

If you want to expose your local files to be accessible through the Preview
modal page you can check perform the following actions:

```elisp
(setq org-roam-server-enable-access-to-local-files t
      org-roam-server-webserver-prefix "/home"
      org-roam-server-webserver-address "127.0.0.1:8887/"
      org-roam-server-webserver-supported-extensions '("pdf" "mp4" "ogv"))
```

There are security reasons your browse disable this feature to be used,
however if you are aware of them and want to expose only in your local
network your files, you can use a Web Server like `python3 -m http.server
8887` and expose a specific *folder*.

This *folder* needs to be setup in the `org-roam-server-webserver-prefix`
and the address of the webserver configured at
`org-roam-server-webserver-address` variable.

Remember to keep your Web Server active in the background while navigating in your Roam Graph.

Be safe, but also be happy.

## License
org-roam-server is licensed under the MIT License.

For Javascript and CSS libraries please refer to;
- https://github.com/jquery/jquery/blob/master/LICENSE.txt
- https://github.com/visjs/vis-network
- https://github.com/twbs/bootstrap/blob/master/LICENSE
- https://github.com/gongzhitaao/orgcss
- https://github.com/select2/select2
- https://github.com/minhur/bootstrap-toggle/blob/master/LICENSE

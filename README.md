# yankel

Yankel is a brave attempt to provide worthy [BabelWeb2](https://github.com/Vivena/babelweb2) client for Emacs.

## Installation

Put `yankel.el` in your load-path and add

    (require 'yankel)

in emacs init file.

## Usage

Launch Yankel with

    M-x yankel RET

then enter address of BabelWeb server.

Use `C-d`, `C-o` and `C-m` to disconnect, reconnect and switch router respectively.

Yankel require [websocket.el](https://github.com/ahyatt/emacs-websocket).
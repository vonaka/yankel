;;; yankel.el --- babelweb2 emacs client

;; Copyright (C) 2018 Fedor Ryabinin

;; Author: Fedor Ryabinin

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; BabelWeb2 <https://github.com/Vivena/babelweb2> is arguably best
;; monitoring tool for Babel <https://www.irif.fr/~jch/software/babel/>,
;; but it has at least one major drawback -- the JavaScript.

;; Yankel is a brave and unsuccessful attempt to provide worthy
;; BabelWeb2 client.

;; It lacks more than a half of functionality of the original JS client,
;; still, it is written in Emacs-Lisp and, hence, better.

;;; Code:

(require 'json)
(require 'websocket)

(defgroup karl-yankel nil
  "BabelWeb2 Emacs Client."
  :prefix "karl-"
  :prefix "yankel-")

(defcustom yankel-mode-hook nil
  "Hook run upon starting Yankel."
  :group 'karl-yankel
  :type 'hook)

(defcustom yankel-buffer-prefix "BabelWeb"
  "Prefix used for the name of Yankel buffer."
  :group 'karl-yankel
  :type 'string)

(defcustom yankel-default-server "http://localhost:8080"
  "Default BabelWeb Server."
  :group 'karl-yankel
  :type 'string)

(defcustom yankel-cell-horizontal-char ?\-
  "Character of table's horizontal border line."
  :group 'karl-yankel
  :type 'character)

(defcustom yankel-cell-vertical-char ?\|
  "Character of table's vertical border line."
  :group 'karl-yankel
  :type 'character)

(defcustom yankel-cell-intersection-char ?\+
  "Character of table's cell corner."
  :group 'karl-yankel
  :type 'character)

(defcustom yankel-color-unreachable
  '(:background "black" :foreground "white")
  "Color of unreachable route."
  :group 'karl-yankel
  :type 'list)

(defcustom yankel-color-installed
  '(:background "green" :foreground "black")
  "Color of installed route."
  :group 'karl-yankel
  :type 'list)

(defcustom yankel-color-uninstalled
  '(:background "red" :foreground "black")
  "Color of uninstalled route."
  :group 'karl-yankel
  :type 'list)

(defcustom yankel-color-wired-link
  '(:background "yellow" :foreground "black")
  "Color of wired linked neighbour."
  :group 'karl-yankel
  :type 'list)

(defcustom yankel-color-lossless-wireless
  '(:background "orange" :foreground "black")
  "Color of wireless connected neighbour."
  :group 'karl-yankel
  :type 'list)

(defvar yankel-server-info nil)
(defvar yankel-babel-state (make-hash-table))
(defvar yankel-current-router "unknown")
(defvar yankel-websocket)
(defvar yankel-on-message-mutex (make-mutex))

(make-variable-buffer-local 'yankel-server-info)
(make-variable-buffer-local 'yankel-babel-state)
(make-variable-buffer-local 'yankel-current-router)
(make-variable-buffer-local 'yankel-websocket)
(make-variable-buffer-local 'yankel-on-message-mutex)

(defvar yankel-mode-map
  (let ((map (make-sparse-keymap 'yankel-mode-map)))
    (define-key map "\C-d"  'yankel-disconnect)
    (define-key map "\C-o"  'yankel-reconnect)
    (define-key map "\C-m"  'yankel-switch-router)
    (define-key map [left]  'yankel-scroll-left)
    (define-key map [right] 'yankel-scroll-right)
    (define-key map [down]  'yankel-scroll-down)
    (define-key map [up]    'yankel-scroll-up)
    map))

(defun yankel-make-interface-table ()
  `(("interface" up ipv4 ipv6) .
    ,(make-hash-table)))

(defun yankel-make-neighbour-table ()
  `(("neighbour" address if reach rxcost txcost cost rtt) .
    ,(make-hash-table)))

(defun yankel-make-route-table ()
  `(("route" prefix metric refmetric id via if installed) .
    ,(make-hash-table)))

(defun yankel-make-xroute-table ()
  `(("xroute" prefix metric) .
    ,(make-hash-table)))

(defun yankel-table-name (table)
  (car (car table)))

(defun yankel-table-columns (table)
  (cdr (car table)))

(defun yankel-table-rows (table)
  (cdr table))

(defun yankel-update-babel-state (update)
  (let ((router (alist-get 'router update))
        (name (alist-get 'name update))
        (action (alist-get 'action update))
        (table (alist-get 'table update))
        (id (alist-get 'id update))
        (data (alist-get 'data update)))
    (when (string= yankel-current-router "unknown")
      (setq yankel-current-router router))
    (let ((c-router (gethash (intern router) yankel-babel-state)))
      (when (not c-router)
        (setq c-router
              `((name . ,name)
                (interface . ,(yankel-make-interface-table))
                (neighbour . ,(yankel-make-neighbour-table))
                (route . ,(yankel-make-route-table))
                (xroute . ,(yankel-make-xroute-table)))))
      (let ((c-table (alist-get (intern table) c-router)))
        (if (string= action "flush")
            (remhash (intern id) (cdr c-table))
          (puthash (intern id) data (yankel-table-rows c-table)))
        (puthash (intern router) c-router yankel-babel-state)))))

(defun yankel-parse-server-name (server)
  (let* ((protocol "") (host "") (port "")
         (server-info (split-string server ":\\(//\\)?"))
         (info-len (safe-length server-info))
         (last-info (car (last server-info))))
    (cond ((eq info-len 1)
           (setq host last-info))
          ((eq info-len 2)
           (if (string-match "\\`[0-9]+" last-info)
               (setq host (car server-info)
                     port (car (split-string last-info "[^0-9]")))
             (setq protocol (car server-info)
                   host last-info)))
          ((eq info-len 3)
           (setq protocol (car server-info)
                 host (nth 1 server-info)
                 port (car (split-string last-info "[^0-9]")))))
    `((protocol . ,protocol) (host . ,host) (port . ,port))))

(defun yankel-buffer-full-name (server)
  (concat yankel-buffer-prefix "[" server "]"))

(defun yankel-draw-table (table pos)
  (goto-char pos)
  (let ((widths (mapcar #'(lambda (column)
                            (cons column (string-width (symbol-name column))))
                        (yankel-table-columns table))))
    (maphash
     (lambda (id row)
       (dolist (column (yankel-table-columns table))
         (let* ((cell (prin1-to-string (alist-get column row) t))
                (old-width (alist-get column widths))
                (new-width (string-width cell)))
           (when (> new-width old-width)
             (setq widths `((,column . ,new-width) . ,widths))))))
     (yankel-table-rows table))
    (let* ((string-of-row
            (lambda (generate-string separator face-property)
              (let ((row-str separator))
                (dolist (column (yankel-table-columns table))
                  (let* ((str (funcall generate-string column))
                         (width (string-width str))
                         (column-width (alist-get column widths)))
                    (setq row-str
                          (concat row-str
                                  (propertize
                                   (concat " " str
                                           (make-string
                                            (- column-width width) ? ) " ")
                                   'face face-property)
                                  separator))))
                row-str)))
           (border
            (funcall string-of-row
                     (lambda (column)
                       (make-string (alist-get column widths)
                                    yankel-cell-horizontal-char))
                     (char-to-string yankel-cell-intersection-char) nil)))
      (insert border "\n"
              (funcall string-of-row 'symbol-name
                       (char-to-string yankel-cell-vertical-char) 'bold))
      (maphash
       (lambda (id row)
         (let ((color (yankel-color-me-your-color table id)))
           (insert "\n" border "\n"
                   (funcall string-of-row
                            (lambda (column)
                              (yankel-cell-to-string (alist-get column row)))
                            (char-to-string yankel-cell-vertical-char)
                            color))))
       (yankel-table-rows table))
      (insert "\n" border))))

(defun yankel-cell-to-string (cell)
  (let ((string (prin1-to-string cell t)))
    (cond ((string= string "nil") " ")
          ((string= string "t") "yep")
          ((string= string ":json-false") "nope")
          (t string))))

(defun yankel-color-me-your-color (table id)
  "Color me your color, baby. Color me your car."
  (let ((table-name (yankel-table-name table))
        (row (gethash id (yankel-table-rows table)))
        (color nil))
    (cond ((string= table-name "route")
           (setq color yankel-color-uninstalled)
           (cond ((= (alist-get 'metric row) 65535)
                  (setq color yankel-color-unreachable))
                 ((not (eq (alist-get 'installed row) :json-false))
                  (setq color yankel-color-installed))))
          ((string= table-name "neighbour")
           (let ((rxcost (alist-get 'rxcost row)))
             (cond ((<= rxcost 96)
                    (setq color yankel-color-wired-link))
                   ((<= rxcost 256)
                    (setq color yankel-color-lossless-wireless))
                   ((> rxcost 256)
                    (setq color yankel-color-unreachable))))))
    color))

(defun yankel-on-open ()
  (erase-buffer)
  (insert "wait a sec..."))

(defun yankel-on-message (frame)
  (with-mutex yankel-on-message-mutex
    (yankel-update-babel-state
     (json-read-from-string
      (websocket-frame-text frame)))
    (let (points)
      (dolist (window (get-buffer-window-list))
        (setq points `((,window . ,(window-point window)) . ,points)))
      (erase-buffer)
      (insert (propertize " Neighbours\n" 'face 'bold))
      (yankel-draw-table
       (alist-get 'neighbour (gethash (intern yankel-current-router)
                                      yankel-babel-state))
       (point))
      (insert (propertize "\n\n Routes\n" 'face 'bold))
      (yankel-draw-table
       (alist-get 'route (gethash (intern yankel-current-router)
                                  yankel-babel-state))
       (point))
      (insert (propertize "\n\n Redistributed routes\n" 'face 'bold))
      (yankel-draw-table
       (alist-get 'xroute (gethash (intern yankel-current-router)
                                   yankel-babel-state))
       (point))
      (yankel-top-secret)
      (dolist (window (get-buffer-window-list))
        (set-window-point window (cdr (assoc window points)))))))

(defun yankel-on-close ()
  (erase-buffer)
  (insert "Connection closed"))

(defun yankel-connect (buffer-name protocol host port)
  (let ((ws nil)
        (url (concat "ws" (when (string= protocol "https") "s")
                     "://" host ":" port "/ws")))
    (setq ws
          (websocket-open
           url
           :on-open (lambda (_websocket)
                      (let ((buffer (websocket-client-data _websocket)))
                        (when (get-buffer buffer)
                          (with-current-buffer buffer
                            (let ((inhibit-read-only t))
                              (yankel-on-open))))))
           :on-message (lambda (_websocket frame)
                         (let ((buffer (websocket-client-data _websocket)))
                           (when (get-buffer buffer)
                             (with-current-buffer buffer
                               (let ((inhibit-read-only t))
                                 (yankel-on-message frame))))))
           :on-close (lambda (_websocket)
                       (let ((buffer (websocket-client-data _websocket)))
                         (when (get-buffer buffer)
                           (with-current-buffer buffer
                             (let ((inhibit-read-only t))
                               (yankel-on-close))))))))
    (setf (websocket-client-data ws) buffer-name)
    ws))

(defun yankel-disconnect ()
  (interactive)
  (websocket-close yankel-websocket))

(defun yankel-reconnect ()
  (interactive)
  (yankel-disconnect)
  (let ((buffer-name (cdr (car yankel-server-info)))
        (protocol (alist-get 'protocol (cdr yankel-server-info)))
        (host (alist-get 'host (cdr yankel-server-info)))
        (port (alist-get 'port (cdr yankel-server-info))))
    (setq yankel-websocket (yankel-connect buffer-name protocol host port))))

(defun yankel-switch-router (router)
  (interactive
   (let (routers)
     (maphash
      (lambda (router table)
        (setq routers `(,(symbol-name router) . ,routers)))
      yankel-babel-state)
     (list (completing-read "Switch router: " routers nil t))))
  (setq yankel-current-router router))

(defun yankel-scroll-left ()
  (interactive)
  (move-to-column (max 0 (- (current-column) (window-body-width)))))

(defun yankel-scroll-right ()
  (interactive)
  (move-to-column (+ (current-column) (window-body-width))))

(defun yankel-scroll-down ()
  (interactive)
  (let ((column (current-column)))
    (forward-line (window-body-height))
    (move-to-column column)))

(defun yankel-scroll-up ()
  (interactive)
  (let ((column (current-column)))
    (forward-line (- (window-body-height)))
    (move-to-column column)))

(defun yankel-top-secret ()
  "Classified."
  (save-excursion
    (let ((width 0)
          (height (count-screen-lines))
          (line 0))
      (goto-char 0)
      (while (< line height)
        (goto-char (line-end-position))
        (setq width (max width (current-column)))
        (forward-line)
        (setq line (+ line 1)))
      (goto-char 0)
      (setq line 0)
      (while (< line height)
        (goto-char (line-end-position))
        (while (< (current-column) width)
          (insert " "))
        (forward-line)
        (setq line (+ line 1))))))

(put 'yankel-mode 'mode-class 'special)

(define-derived-mode yankel-mode nil "Karl-Yankel"
  "BabelWeb2 Emacs Client major mode."
  (use-local-map yankel-mode-map)
  (setq truncate-lines t)
  (setq show-trailing-whitespace nil))

;;;###autoload
(defun yankel (server)
  "BabelWeb2 Emacs Client."
  (interactive
   (let* ((prompt
           (concat "BabelWeb Server [default: " yankel-default-server "]: "))
          (server
           (read-no-blanks-input prompt)))
     (when (string= server "")
       (setq server yankel-default-server))
     (list server)))

  (condition-case nil
      (let* ((server-info (yankel-parse-server-name server))
             (buffer-name
              (yankel-buffer-full-name (concat (alist-get 'host server-info) ":"
                                               (alist-get 'port server-info))))
             (ws (yankel-connect buffer-name
                                 (alist-get 'protocol server-info)
                                 (alist-get 'host server-info)
                                 (alist-get 'port server-info))))
        (select-window (or (get-buffer-window buffer-name)
                           (selected-window)))
        (switch-to-buffer buffer-name)
        (yankel-mode)
        (setq buffer-read-only t
              cursor-type nil
              yankel-websocket ws
              yankel-server-info
              `((buffer-name . ,buffer-name) . ,server-info)))
    (error (message "Unable to connect"))))

(provide 'yankel)

;;; yankel.el ends here

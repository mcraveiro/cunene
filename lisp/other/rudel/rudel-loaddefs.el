(let* ((rudel-dir (file-name-directory (or #$
					   load-file-name
					   (buffer-file-name))))
       (subdirs   (mapcar
		   (lambda (subdir)
		     (concat rudel-dir subdir))
		   '("." "jupiter" "adopted" "socket" "tls" "xmpp" "telepathy" "obby" "infinote" "zeroconf"))))
  ;; Adjust load path. We need to have all Rudel subdirectories on
  ;; the load path.
  (dolist (subdir subdirs)
    (add-to-list 'load-path subdir)))

(require 'eieio)
(require 'cl)
(require 'rudel-backend)

;;; rudel-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rudel-unsubscribe rudel-publish-buffer rudel-subscribe
;;;;;;  rudel-change-color rudel-leave-session rudel-host-session
;;;;;;  rudel-join-session) "rudel" "rudel.el" (20352 29681))
;;; Generated autoloads from rudel.el

(autoload 'rudel-join-session "rudel" "\
Join the collaborative editing session described by INFO.
INFO is a property list that describes the collaborative editing
session in terms of properties like :host, :port
and :encryption. The particular properties and their respective
meanings depend on the used backend.

When called interactively, all data required to join a session
will be prompted for.

\(fn INFO)" t nil)

(autoload 'rudel-host-session "rudel" "\
Host a collaborative editing session described by INFO.
INFO is a property list that describes the collaborative editing
session to be created in terms of properties like :address, :port
and :encryption. The particular properties and their respective
meanings depend on the used backend.

When called interactively, all data required to host a session
will be prompted for.

\(fn INFO)" t nil)

(autoload 'rudel-leave-session "rudel" "\
Leave the current collaborative editing session.

\(fn)" t nil)

(autoload 'rudel-change-color "rudel" "\
Change the color associated with the local user.
Not all backends support this operation.

\(fn)" t nil)

(autoload 'rudel-subscribe "rudel" "\
Subscribe to DOCUMENT offered by a peer in a collaborative editing session.
When called interactively, DOCUMENT is prompted for interactively.

\(fn DOCUMENT)" t nil)

(autoload 'rudel-publish-buffer "rudel" "\
Make the BUFFER available for subscription to peers in a collaborative editing session.
If BUFFER is nil, the current buffer is used.

\(fn &optional BUFFER)" t nil)

(autoload 'rudel-unsubscribe "rudel" "\
Detaches BUFFER from the collaborative editing session.
The most recent version of the content will remain in the
buffer but not be affected by future changes from other
peers. If BUFFER is nil, the current is used.

\(fn &optional BUFFER)" t nil)

;;;***

;;;### (autoloads (rudel-backend-get-factory rudel-backend-get rudel-backend-factory)
;;;;;;  "rudel-backend" "rudel-backend.el" (20352 29681))
;;; Generated autoloads from rudel-backend.el

(eieio-defclass-autoload 'rudel-backend-factory 'nil "rudel-backend" "Factory class that holds an object for each known backend\ncategory. Objects manage backend implementation for one backend\ncategory each.")

(defmethod rudel-get-factory :static ((this rudel-backend-factory) category) "Return the factory responsible for CATEGORY.\nIf there is no responsible factory, create one and return it." (with-slots (factories) this (or (gethash category factories) (puthash category (rudel-backend-factory category) factories))))

(defmethod rudel-add-backend ((this rudel-backend-factory) name class &optional replace) "Add factory class CLASS with name NAME to THIS.\nif REPLACE is non-nil, replace a registered implementation of the\nsame name." (with-slots (backends) this (when (or (not (gethash name backends)) replace) (puthash name class backends))))

(autoload 'rudel-backend-get "rudel-backend" "\
A shortcut for getting backend NAME of category CATEGORY.
The returned backend is of the form (NAME . OBJECT).

\(fn CATEGORY NAME)" nil nil)

(autoload 'rudel-backend-get-factory "rudel-backend" "\
A shortcut for getting the factory object for CATEGORY.

\(fn CATEGORY)" nil nil)

;;;***

;;;### (autoloads (rudel-infinote-backend) "rudel-infinote" "infinote/rudel-infinote.el"
;;;;;;  (20352 29681))
;;; Generated autoloads from infinote/rudel-infinote.el

(eieio-defclass-autoload 'rudel-infinote-backend '(rudel-protocol-backend) "rudel-infinote" "")

(rudel-add-backend (rudel-backend-get-factory 'protocol) 'infinote 'rudel-infinote-backend)

(eval-after-load 'rudel-zeroconf '(rudel-zeroconf-register-service "_infinote._tcp" 'xmpp 'infinote))

;;;***

;;;### (autoloads (global-rudel-minor-mode global-rudel-mode-line-publish-state-mode
;;;;;;  rudel-mode-line-publish-state-minor-mode global-rudel-header-subscriptions-mode
;;;;;;  rudel-header-subscriptions-minor-mode) "rudel-mode" "rudel-mode.el"
;;;;;;  (20352 29681))
;;; Generated autoloads from rudel-mode.el

(autoload 'rudel-header-subscriptions-minor-mode "rudel-mode" "\
Toggle Rudel header subscriptions minor mode.

This mode displays users subscribed to the document associated
with the buffer in the header-line. Depending on the kind of
session, additional information like connection status,
encryption or activity indication may be displayed with each
user.

If ARG is null, toggle Rudel header subscriptions mode.
If ARG is a number greater than zero, turn on Rudel header
subscriptions mode; otherwise, turn it off.

\(fn &optional ARG)" t nil)

(defvar global-rudel-header-subscriptions-mode nil "\
Non-nil if Global-Rudel-Header-Subscriptions mode is enabled.
See the command `global-rudel-header-subscriptions-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-rudel-header-subscriptions-mode'.")

(custom-autoload 'global-rudel-header-subscriptions-mode "rudel-mode" nil)

(autoload 'global-rudel-header-subscriptions-mode "rudel-mode" "\
Toggle Rudel-Header-Subscriptions minor mode in every possible buffer.
With prefix ARG, turn Global-Rudel-Header-Subscriptions mode on if and only if
ARG is positive.
Rudel-Header-Subscriptions minor mode is enabled in all buffers where
`rudel-header-subscriptions-minor-mode' would do it.
See `rudel-header-subscriptions-minor-mode' for more information on Rudel-Header-Subscriptions minor mode.

\(fn &optional ARG)" t nil)

(autoload 'rudel-mode-line-publish-state-minor-mode "rudel-mode" "\
Toggle Rudel mode line publish state minor mode.

This mode displays an indicator of the buffer's state with
respect to an associated Rudel document in the mode line. If the
buffer has an attached document, the string \"P\" is displayed
after the remote file indicator. Otherwise, the string \"-\" is
displayed.

If ARG is null, toggle Rudel mode line publish state minor mode.
If ARG is a number greater than zero, turn on Rudel minor mode
line publish state mode; otherwise, turn it off.

\(fn &optional ARG)" t nil)

(defvar global-rudel-mode-line-publish-state-mode nil "\
Non-nil if Global-Rudel-Mode-Line-Publish-State mode is enabled.
See the command `global-rudel-mode-line-publish-state-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-rudel-mode-line-publish-state-mode'.")

(custom-autoload 'global-rudel-mode-line-publish-state-mode "rudel-mode" nil)

(autoload 'global-rudel-mode-line-publish-state-mode "rudel-mode" "\
Toggle Rudel-Mode-Line-Publish-State minor mode in every possible buffer.
With prefix ARG, turn Global-Rudel-Mode-Line-Publish-State mode on if and only if
ARG is positive.
Rudel-Mode-Line-Publish-State minor mode is enabled in all buffers where
`rudel-mode-line-publish-state-minor-mode' would do it.
See `rudel-mode-line-publish-state-minor-mode' for more information on Rudel-Mode-Line-Publish-State minor mode.

\(fn &optional ARG)" t nil)

(defvar global-rudel-minor-mode nil "\
Non-nil if Global-Rudel minor mode is enabled.
See the command `global-rudel-minor-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-rudel-minor-mode'.")

(custom-autoload 'global-rudel-minor-mode "rudel-mode" nil)

(autoload 'global-rudel-minor-mode "rudel-mode" "\
Toggle global Rudel minor mode (No modeline indicator).

If ARG is null, toggle global Rudel mode.
If ARG is a number greater than zero, turn on global Rudel mode;
otherwise, turn it off.

\\{rudel-minor-keymap}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rudel-obby-backend) "rudel-obby" "obby/rudel-obby.el"
;;;;;;  (20352 29681))
;;; Generated autoloads from obby/rudel-obby.el

(eieio-defclass-autoload 'rudel-obby-backend '(rudel-protocol-backend) "rudel-obby" "Main class of the Rudel obby backend. Creates obby client\nconnections and creates obby servers.")

(rudel-add-backend (rudel-backend-get-factory 'protocol) 'obby 'rudel-obby-backend)

(eval-after-load 'rudel-zeroconf '(rudel-zeroconf-register-service "_lobby._tcp" 'start-tls 'obby))

;;;***

;;;### (autoloads (rudel-configured-sessions-backend rudel-ask-protocol-backend)
;;;;;;  "rudel-session-initiation" "rudel-session-initiation.el"
;;;;;;  (20352 29681))
;;; Generated autoloads from rudel-session-initiation.el

(eieio-defclass-autoload 'rudel-ask-protocol-backend '(rudel-session-initiation-backend) "rudel-session-initiation" "This fallback backend can \"discover\" sessions by letting the\nuser select a suitable backend and asking for connect information\nrequired by the chosen backend.")

(rudel-add-backend (rudel-backend-get-factory 'session-initiation) 'ask-protocol 'rudel-ask-protocol-backend)

(eieio-defclass-autoload 'rudel-configured-sessions-backend '(rudel-session-initiation-backend) "rudel-session-initiation" "This fallback backend can \"discover\" sessions the user has\nconfigured using customization.")

(rudel-add-backend (rudel-backend-get-factory 'session-initiation) 'configured-sessions 'rudel-configured-sessions-backend)

;;;***

;;;### (autoloads (rudel-tcp-backend) "rudel-socket" "socket/rudel-socket.el"
;;;;;;  (20352 29681))
;;; Generated autoloads from socket/rudel-socket.el

(eieio-defclass-autoload 'rudel-tcp-backend '(rudel-transport-backend) "rudel-socket" "TCP transport backend.\nThe transport backend is a factory for TCP transport objects.")

(rudel-add-backend (rudel-backend-get-factory 'transport) 'tcp 'rudel-tcp-backend)

;;;***

;;;### (autoloads (rudel-speedbar) "rudel-speedbar" "rudel-speedbar.el"
;;;;;;  (20352 29681))
;;; Generated autoloads from rudel-speedbar.el

(autoload 'rudel-speedbar "rudel-speedbar" "\
Show connected users and available documents of Rudel session in speedbar.

\(fn)" t nil)

;;;***

;;;### (autoloads (rudel-telepathy-backend) "rudel-telepathy" "telepathy/rudel-telepathy.el"
;;;;;;  (20352 29681))
;;; Generated autoloads from telepathy/rudel-telepathy.el

(eieio-defclass-autoload 'rudel-telepathy-backend '(rudel-transport-backend) "rudel-telepathy" "Class rudel-telepathy-backend ")

(rudel-add-backend (rudel-backend-get-factory 'transport) 'telepathy 'rudel-telepathy-backend)

;;;***

;;;### (autoloads (rudel-start-tls-backend) "rudel-tls" "tls/rudel-tls.el"
;;;;;;  (20352 29681))
;;; Generated autoloads from tls/rudel-tls.el

(eieio-defclass-autoload 'rudel-start-tls-backend '(rudel-transport-backend) "rudel-tls" "STARTTLS transport backend.\nThe transport backend is a factory for transport objects that\nsupport STARTTLS behavior.")

(rudel-add-backend (rudel-backend-get-factory 'transport) 'start-tls 'rudel-start-tls-backend)

;;;***

;;;### (autoloads (rudel-xmpp-backend) "rudel-xmpp" "xmpp/rudel-xmpp.el"
;;;;;;  (20352 29681))
;;; Generated autoloads from xmpp/rudel-xmpp.el

(eieio-defclass-autoload 'rudel-xmpp-backend '(rudel-transport-backend) "rudel-xmpp" "Transport backend works by transporting XMPP messages through\nXMPP connections.")

(rudel-add-backend (rudel-backend-get-factory 'transport) 'xmpp 'rudel-xmpp-backend)

;;;***

;;;### (autoloads nil "rudel-xmpp-tunnel" "xmpp/rudel-xmpp-tunnel.el"
;;;;;;  (20352 29681))
;;; Generated autoloads from xmpp/rudel-xmpp-tunnel.el

(rudel-add-backend (rudel-backend-get-factory 'transport) 'xmpp 'rudel-xmpp-tunnel-backend)

;;;***

;;;### (autoloads (rudel-zeroconf-backend rudel-zeroconf-register-service)
;;;;;;  "rudel-zeroconf" "zeroconf/rudel-zeroconf.el" (20352 29681))
;;; Generated autoloads from zeroconf/rudel-zeroconf.el

(autoload 'rudel-zeroconf-register-service "rudel-zeroconf" "\
Add an entry for TYPE with TRANSPORT-BACKEND and PROTOCOL-BACKEND to the list of service types.
TRANSPORT-BACKEND is the name of the transport backend handling
the service type TYPE.
PROTOCOL-BACKEND is the name of the protocol backend handling the
service type TYPE.

\(fn TYPE TRANSPORT-BACKEND PROTOCOL-BACKEND)" nil nil)

(eieio-defclass-autoload 'rudel-zeroconf-backend '(rudel-session-initiation-backend) "rudel-zeroconf" "")

(rudel-add-backend (rudel-backend-get-factory 'session-initiation) 'zeroconf 'rudel-zeroconf-backend)

;;;***

;;;### (autoloads nil nil ("adopted/adopted-compound.el" "adopted/adopted-delete.el"
;;;;;;  "adopted/adopted-insert.el" "adopted/adopted-nop.el" "adopted/adopted-operation.el"
;;;;;;  "adopted/adopted.el" "infinote/rudel-infinote-client.el"
;;;;;;  "infinote/rudel-infinote-display.el" "infinote/rudel-infinote-document.el"
;;;;;;  "infinote/rudel-infinote-errors.el" "infinote/rudel-infinote-group-directory.el"
;;;;;;  "infinote/rudel-infinote-group-document.el" "infinote/rudel-infinote-group-text-document.el"
;;;;;;  "infinote/rudel-infinote-group.el" "infinote/rudel-infinote-node-directory.el"
;;;;;;  "infinote/rudel-infinote-node.el" "infinote/rudel-infinote-state.el"
;;;;;;  "infinote/rudel-infinote-text-document.el" "infinote/rudel-infinote-user.el"
;;;;;;  "infinote/rudel-infinote-util.el" "jupiter/jupiter-compound.el"
;;;;;;  "jupiter/jupiter-delete.el" "jupiter/jupiter-insert.el" "jupiter/jupiter-nop.el"
;;;;;;  "jupiter/jupiter-operation.el" "jupiter/jupiter.el" "obby/rudel-obby-client.el"
;;;;;;  "obby/rudel-obby-debug.el" "obby/rudel-obby-display.el" "obby/rudel-obby-errors.el"
;;;;;;  "obby/rudel-obby-server.el" "obby/rudel-obby-state.el" "obby/rudel-obby-util.el"
;;;;;;  "rudel-chat.el" "rudel-color.el" "rudel-compat.el" "rudel-compile.el"
;;;;;;  "rudel-debug.el" "rudel-display.el" "rudel-errors.el" "rudel-hooks.el"
;;;;;;  "rudel-icons.el" "rudel-interactive.el" "rudel-operations.el"
;;;;;;  "rudel-operators.el" "rudel-overlay.el" "rudel-protocol.el"
;;;;;;  "rudel-state-machine.el" "rudel-transport-util.el" "rudel-transport.el"
;;;;;;  "rudel-util.el" "rudel-xml.el" "xmpp/rudel-xmpp-debug.el"
;;;;;;  "xmpp/rudel-xmpp-sasl.el" "xmpp/rudel-xmpp-state.el" "xmpp/rudel-xmpp-tls.el"
;;;;;;  "xmpp/rudel-xmpp-util.el") (20352 29845 597472))

;;;***

(provide 'rudel-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rudel-loaddefs.el ends here

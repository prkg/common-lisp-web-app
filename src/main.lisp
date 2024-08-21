(in-package #:cl-user)

(defpackage #:cl-web-app-example
  (:use #:cl)
  (:local-nicknames
   (#:s #:spinneret)
   (#:json #:com.inuoe.jzon))
  (:export #:main))

(in-package #:cl-web-app-example)

(defvar *server*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew "ws-" spinneret:*unvalidated-attribute-prefixes* :test #'equal)
  (pushnew "hx-" spinneret:*unvalidated-attribute-prefixes* :test #'equal)
  (pushnew "_" spinneret:*unvalidated-attribute-prefixes* :test #'equal)
  (setf (gethash :ws-send spinneret:*boolean-attributes*) t))

(defvar *connections* (make-hash-table))

(defmacro with-page ((&key title) &body body)
  `(s:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:link :rel "stylesheet" :href (format nil "/static/tailwind.css?~a" 1))
       (:script :src "https://unpkg.com/htmx.org@1.9.4")
       (:script :src "https://unpkg.com/hyperscript.org@0.9.12")
       (:script :src "https://unpkg.com/htmx.org@1.9.12/dist/ext/ws.js"))
      (:body.min-h-screen.flex.justify-center.min-h-screen
       :hx-ext "ws"
       :ws-connect "/chat"        
       (:main.w-full.flex.min-h-screen	
	,@body)))))

(defun frag/chat-form ()
  (s:with-html
    (:form#form
     :ws-send t
     (:input :name "chat_message" :autofocus t))))

(defun frag/chat-message (user message)
  (s:with-html
    (:div#chat_messages
     :hx-swap-oob "beforeend"
     (:div.flex
      (:div.font-bold (format nil "~a: " user))
      (:div message)))))

(defun page/index ()
  (with-page (:title "Home")
    (s:with-html
      (:div#chat
       (:div#chat_messages)
       (frag/chat-form)
       ))))

(defun ws-broadcast (message)
  (loop :for con :being :the :hash-key :of *connections* :do
    (wsd:send con message)))

(defun ws-broadcast-from (con message)
  (ws-broadcast
   (s:with-html-string
     (frag/chat-message (gethash con *connections*) message)
     (frag/chat-form))))

(defun ws-handle-new-connection (con)
  (setf (gethash con *connections*)
	(format nil "user-~a" (random 100000)))
  (wsd:send con "Welcome"))

(defun ws-handle-close-connection (con)
  (ws-broadcast-from con "Left the chat")
  (remhash con *connections*))

(defun ws-on-message (con)
  (lambda (msg)
    (let ((message (json:parse msg :key-fn #'alexandria:make-keyword)))
      (ws-broadcast-from con (gethash :|chat_message| message)))))

(defun ws-server (env)
  (let ((ws (wsd:make-server env)))
    (wsd:on :open ws
	    (lambda ()
	      (format t "New connection established~%")
	      (ws-handle-new-connection ws)))
    (wsd:on :message ws (ws-on-message ws))
    (wsd:on :close ws
	    (lambda (&key code reason)
	      (format t "Disconnected ~a ~a ~%" code reason)
	      (ws-handle-close-connection ws)))
    (wsd:on :error ws
	    (lambda (error)
	      (format *error-output* "Error Event [~S]~%" error)))
    (lambda (responder)
      (declare (ignore responder))
      (wsd:start-connection ws))))

(defparameter *app*
  (lambda (env)
    (declare (ignore env))
    (setf spinneret::*print-pretty* :t)
    `(200 (:content-type "text/html") (,(page/index)))))

(defun start-server ()
  (setf *server*
	(clack:clackup
	 (lack:builder
	  (:mount "/chat" #'ws-server)
	  (:static
	   :path "/static/"
	   :root (truename #P"./public"))
	  *app*)
	 :server :hunchentoot
	 :address "0.0.0.0"
	 :use-default-middlewares nil)))

(defun stop-server ()
  (clack:stop *server*))

(defun main ()
  (start-server))

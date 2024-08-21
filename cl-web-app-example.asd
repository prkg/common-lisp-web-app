(defsystem "cl-web-app-example"
  :version "0.0.1"
  :description "common lisp web-app example"
  :author ""
  :license ""
  :serial t
  
  :depends-on (#:clack-handler-hunchentoot
	       #:clack
	       #:lack-middleware-static
	       #:lack-middleware-mount
	       #:alexandria
	       #:websocket-driver
	       #:spinneret
	       #:com.inuoe.jzon)
  :components ((:module "src"
		:components ((:file "main"))))
  :in-order-to ((test-op (test-op "cl-web-app-example/tests"))))


(defsystem "cl-web-app-example/tests"
  :description "test system for cl-web-app-example"
  :author ""
  :license ""
  :depends-on (#:rove
	       #:cl-web-app-example)
  :components ((:module "tests"
		:components
		((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))


(in-package :pyffi)

(pyth-on)

(py-require "uno")

(defpyfun "uno.getComponentContext" ())

(defpyslot "ServiceManager")
(defpyslot "Text")

;can't yet find out appropriate class, the following is lame
(defpymethod "createInstanceWithContext")
(defpymethod "resolve")
(defpymethod "getCurrentComponent")
(defpymethod "createTextCursor")
(defpymethod "insertString")

(defun greet-uno (ascii-string)
  (let* ((lc (uno.getcomponentcontext))
	 (resolver (createinstancewithcontext 
		    (servicemanager lc)
		    "com.sun.star.bridge.UnoUrlResolver" lc))
	 (ctx (resolve resolver "uno:socket,host=localhost,port=2002;urp;StarOffice.ComponentContext"))
	 (desktop (createinstancewithcontext 
		   (servicemanager ctx)
		   "com.sun.star.frame.Desktop" ctx))
	 (model (getcurrentcomponent desktop))
	 (text (text model))
	 (cursor (createtextcursor text)))
    (insertstring text cursor ascii-string 0)))
	 	
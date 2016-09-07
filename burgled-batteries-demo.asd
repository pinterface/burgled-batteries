(defsystem :burgled-batteries-demo
  :name "burgled-batteries-demo"
  :description "burgled-batteries demo"
  :author "pinterface <pix@kepibu.org>"
  :maintainer "mmontone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components
  ((:module "todo-app"
    :pathname "t/todo-app"
    :components
    ((:file "package")
     (:file "todo-list"))
    :serial t))
  :depends-on (#:burgled-batteries))

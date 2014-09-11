(in-package :python-cffi.test)

(lift:deftestsuite burgled-batteries ()
  ()
  (:setup (burgled-batteries:startup-python))
  (:teardown (burgled-batteries:shutdown-python)))

(defun run-tests ()
  (lift:run-tests :suite 'burgled-batteries)) 

(load "utility-opengl.lisp")

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue"  )))

(defparameter *a* 0)

(defgeneric update ())

(defmethod update ()
  (setf *a* (+ *a* 1))
  (if (>= *a* 360)
      (setf *a* 0)))

(defparameter *texture0* nil)
(defparameter *texture1* nil)
(defparameter *texture2* nil)
(defparameter *texture3* nil)
(defparameter *texture4* nil)

(defgeneric draw ())

(defmethod draw :before ()
  (gl:viewport 0 0 1024 600)
  (gl:matrix-mode :projection)
  (gl:clear-color 0 0.3 0.8 0.7)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)
  (glu:perspective 30 (/ 1024 600) 0.001 10000)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod draw :after ()
  (sdl:update-display))

(defmethod draw ()
  (with-3d-view (0 0 5 0 0 0 0 1 0)
    (with-translate (1 1 0)
      (with-rotate (*a* 1 1 1)
        (texture-octahedron 0.9 *texture3*)))
    (with-rotate (*a* 1 1 1)
      (dolist ( i '(0.3 -0.3))
        (dolist ( j '(0.3 -0.3))
          (dolist ( k '(0.3 -0.3))
            (with-translate ((* i 1.8) (* j 1.8) (* k 1.8))
              (texture-cube 0.5 *texture2*))
            (with-translate (i j k)
              (texture-cube 0.3 *texture2*)))))
      (texture-cube 0.3 *texture2*)))

    ;;以下2d表示のため
  (with-2d-view (0 1024 600 0)
    (with-translate (800 300 0)
      (with-rotate (*a* 0 0 1)
        (draw-texture-at 0 0 1 *texture0*)))
    (with-translate (200 300 0)
      (with-rotate (*a* 0 0 -1)
        (draw-texture-at 0 0 0.5 *texture0*)))
    (draw-texture-at 400 400 1 *texture1*))
  )

(defun main-loop ()
  (sdl:with-init ()
    (glut:init)
    (sdl:window 1024 600
                :flags '(sdl:sdl-doublebuf sdl:sdl-opengl))
    (setf (sdl:frame-rate) 60)
    ;;(gl:shader-source 1 1 1 1)
    (setf *texture0* (load-a-texture "texture/feedlike.png"))
    (setf *texture1* (load-a-texture "texture/lisplogo_alien_256.png"))
    (setf *texture2* (load-a-texture "texture/wood.png"))
    (setf *texture3* (load-a-texture "texture/Lightning.png"))
    (setf *texture4* (load-a-texture "texture/iwa.png"))
    ;; cl-opengl needs platform specific support to be able to load GL
    ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
    ;;(setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
                       (when (sdl:key= key :sdl-key-escape)
                         (sdl:push-quit-event)))
      (:idle ()
             (update)
             ;; this lets slime keep working while the main loop is running
             ;; in sbcl using the :fd-handler swank:*communication-style*
             ;; (something similar might help in some other lisps, not sure which though)
             #+(and sbcl (not sb-thread)) (restartable
                                           (sb-sys:serve-all-events 0))
             (restartable (draw))))))

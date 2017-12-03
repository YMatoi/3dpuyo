;; 2012-02-24
(ql:quickload :cl-opengl)
(ql:quickload :cl-glut)
(ql:quickload :cl-glu)
(ql:quickload :lispbuilder-sdl)
;;(ql:quickload :png)
(ql:quickload :lispbuilder-sdl-image)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@body))

(defmacro with-translate ((x y z) &body body)
  `(progn
     (gl:translate ,x ,y ,z)
     ,@body
     (gl:translate (- ,x) (- ,y) (- ,z))))

(defmacro with-rotate ((a x y z) &body body)
  `(progn
     (gl:rotate ,a ,x ,y ,z)
     ,@body
     (gl:rotate (- ,a) ,x ,y ,z)))

(defmacro set-default-color ()
  `(gl:color 1 1 1)) ;;白

(defmacro with-color ((r g b) &body body)
  `(progn
     (gl:color ,r ,g ,b) ;;色を設定して
     ,@body ;;本体
     (set-default-color))) ;;色をデフォルトに戻す

(defmacro with-material-pre ((&rest rest))
  (cons 'progn (mapcar #'(lambda (s) (cons 'gl:material s)) rest)))

(defmacro set-default-material ()
  (with-material-pre ((:front :specular '(0 0 0 1))
                      (:front :shininess 128))))

(defmacro with-material ((&rest rest) &body body)
  `(progn (with-material-pre ,rest)
          ,@body
          (set-default-material)))

(defmacro vertex-ex (&rest rest)
  (cons 'progn (mapcar #'(lambda (s) (cons 'gl:vertex s)) rest)))

(defmacro with-push-matrix (() &body body)
  `(progn (gl:push-matrix)
          ,@body
          (gl:pop-matrix)))

(defmacro with-2d-view ((a b c d) &body body)
  `(progn (gl:push-attrib :enable-bit)
          (gl:matrix-mode :projection)
          (with-push-matrix ()
            (gl:load-identity)
            (glu:ortho-2d ,a ,b ,c ,d)      ;;完全な2d
            (gl:matrix-mode :modelview)
            (with-push-matrix ()
              (gl:load-identity)
              ,@body)
            (gl:matrix-mode :projection))
          (gl:pop-attrib)
          (gl:matrix-mode :modelview)))

(defun bind-texture (loaded-texture)
  (when (car loaded-texture)
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d (car loaded-texture))))

(defun load-a-texture (filename)
  (let ((texture (car (gl:gen-textures 1)))
        (image (sdl-image:load-image filename)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-env :texture-env :texture-env-mode :modulate)
    (sdl-base::with-pixel (pix (sdl:fp image))
      ;; we should probably be a bit more intelligent about this, but this
      ;; handles some common cases
      (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
                              (3 :rgb)
                              (4 :rgba))))
        ;; we should also handle this properly, by adjusting the
        ;; settings of gl:pixel-store
        (assert (and (= (sdl-base::pixel-pitch pix)
                        (* (sdl:width image) (sdl-base::pixel-bpp pix)))
                     (zerop (rem (sdl-base::pixel-pitch pix) 4))))
        (gl:tex-image-2d :texture-2d 0 :rgba
                         (sdl:width image) (sdl:height image)
                         0
                         texture-format
                         :unsigned-byte (sdl-base::pixel-data pix))))
    (list texture (sdl:width image) (sdl:height image))))

(defun draw-texture-at (x y n loaded-texture &optional (r 255) (g 255) (b 255))
  (bind-texture loaded-texture)
  (gl:color r g b)
  (let ((w (/ (* (second loaded-texture) n) 2))
        (h (/ (* (third loaded-texture) n) 2)))
    (gl:with-primitive :quads
      (gl:tex-coord 0 0)
      (gl:vertex (- x w) (- y h) 0)
      (gl:tex-coord 1 0)
      (gl:vertex (+ x w) (- y h) 0)
      (gl:tex-coord 1 1)
      (gl:vertex (+ x w) (+ y h) 0)
      (gl:tex-coord 0 1)
      (gl:vertex (- x w) (+ y h) 0))))

(defun texture-triangle (l loaded-texture &optional (r 1) (g 1) (b 1)) ;;正三角形に内接する円の中心が基準
  (bind-texture loaded-texture)
  (gl:color r g b)
  (gl:with-primitive :triangles
    (gl:normal 0 0 1)
    (gl:tex-coord 0 0)
    (gl:vertex 0 (* 2 l) 0)
    (gl:tex-coord 1 0)
    (gl:vertex (* l 1.7320508) (- l) 0)
    (gl:tex-coord 1 1)
    (gl:vertex (* l -1.7320508) (- l) 0)))

(defun texture-octahedron (n loaded-texture &optional (r 1) (g 1) (b 1))
  (bind-texture loaded-texture)
  (gl:color r g b)
  (let (( p (/ n 2 1.4142))
        ( m (/ n -2 1.4142)))
    (gl:with-primitive :triangles
      (gl:normal 1 1 1)
      (gl:tex-coord 0 0)
      (gl:vertex 0 0 p)
      (gl:tex-coord 1 0)
      (gl:vertex p 0 0)
      (gl:tex-coord 1 1)
      (gl:vertex 0 p 0)

      (gl:normal 1 1 -1)
      (gl:tex-coord 0 0)
      (gl:vertex p 0 0)
      (gl:tex-coord 1 0)
      (gl:vertex 0 0 m)
      (gl:tex-coord 1 1)
      (gl:vertex 0 p 0)
      
      (gl:normal -1 1 -1)
      (gl:tex-coord 0 0)
      (gl:vertex 0 0 m)
      (gl:tex-coord 1 0)
      (gl:vertex m 0 0)
      (gl:tex-coord 1 1)
      (gl:vertex 0 p 0)
      
      (gl:normal -1 1 1)
      (gl:tex-coord 0 0)
      (gl:vertex m 0 0)
      (gl:tex-coord 1 0)
      (gl:vertex 0 0 p)
      (gl:tex-coord 1 1)
      (gl:vertex 0 p 0) ;; ここまで上

      (gl:normal 1 -1 1)
      (gl:tex-coord 0 0)
      (gl:vertex 0 0 p)
      (gl:tex-coord 1 0)
      (gl:vertex p 0 0)
      (gl:tex-coord 1 1)
      (gl:vertex 0 m 0)

      (gl:normal 1 -1 -1)
      (gl:tex-coord 0 0)
      (gl:vertex p 0 0)
      (gl:tex-coord 1 0)
      (gl:vertex 0 0 m)
      (gl:tex-coord 1 1)
      (gl:vertex 0 m 0)
      
      (gl:normal -1 -1 -1)
      (gl:tex-coord 0 0)
      (gl:vertex 0 0 m)
      (gl:tex-coord 1 0)
      (gl:vertex m 0 0)
      (gl:tex-coord 1 1)
      (gl:vertex 0 m 0)
      
      (gl:normal -1 -1 1)
      (gl:tex-coord 0 0)
      (gl:vertex m 0 0)
      (gl:tex-coord 1 0)
      (gl:vertex 0 0 p)
      (gl:tex-coord 1 1)
      (gl:vertex 0 m 0))))



(defun texture-cube (l loaded-texture &optional (r 1) (g 1) (b 1))
  (let ((n (/ l 2)))
    (bind-texture loaded-texture)
    (gl:color r g b)
    (gl:with-primitive :polygon
      (gl:normal 0 0 1)
      (gl:tex-coord  0 0)
      (gl:vertex (- n) (- n) n)
      (gl:tex-coord  1 0)
      (gl:vertex n (- n) n)
      (gl:tex-coord  1 1)
      (gl:vertex n n n)
      (gl:tex-coord  0 1)
      (gl:vertex (- n) n n))
    (gl:with-primitive :polygon
      (gl:normal 0 0 -1)
      (gl:tex-coord  0 0)
      (gl:vertex (- n) (- n) (- n))
      (gl:tex-coord  1 0)
      (gl:vertex n (- n) (- n))
      (gl:tex-coord  1 1)
      (gl:vertex n n (- n))
      (gl:tex-coord  0 1)
      (gl:vertex (- n) n (- n)))
    (gl:with-primitive :polygon
      (gl:normal 0 1 0)
      (gl:tex-coord  0 0)
      (gl:vertex (- n) n (- n))
      (gl:tex-coord  1 0)
      (gl:vertex n n (- n))
      (gl:tex-coord  1 1)
      (gl:vertex n n n)
      (gl:tex-coord  0 1)
      (gl:vertex (- n) n n))
    (gl:with-primitive :polygon
      (gl:normal 0 -1 0)
      (gl:tex-coord  0 0)
      (gl:vertex (- n) (- n) (- n))
      (gl:tex-coord  1 0)
      (gl:vertex n (- n) (- n))
      (gl:tex-coord  1 1)
      (gl:vertex n (- n) n)
      (gl:tex-coord  0 1)
      (gl:vertex (- n) (- n) n))
    (gl:with-primitive :polygon
      (gl:normal 1 0 0)
      (gl:tex-coord  0 0)
      (gl:vertex n (- n) (- n))
      (gl:tex-coord  1 0)
      (gl:vertex n n (- n))
      (gl:tex-coord  1 1)
      (gl:vertex n n n)
      (gl:tex-coord  0 1)
      (gl:vertex n (- n) n))
    (gl:with-primitive :polygon
      (gl:normal -1 0 0)
      (gl:tex-coord  0 0)
      (gl:vertex (- n) (- n) (- n))
      (gl:tex-coord  1 0)
      (gl:vertex (- n) n (- n))
      (gl:tex-coord  1 1)
      (gl:vertex (- n) n n)
      (gl:tex-coord  0 1)
      (gl:vertex (- n) (- n) n))))

(defmacro with-3d-view ((cx cy cz lx ly lz ux uy uz) &body body)
  ;;引数はglu:look-atと同じで座標、視線上の・、上方向ベクトル
  `(progn
     (gl:shade-model :smooth)
     (gl:color-material :front-and-back :ambient-and-diffuse)
     (gl:enable :color-material)
     (gl:light :light0 :position '(1 1 1 1))

     ;(gl:enable :point-smooth)
     ;(gl:enable :line-smooth)
     ;(gl:enable :polygon-smooth)
     (gl:enable :blend)
     (gl:blend-func :src-alpha :one-minus-src-alpha)
     (gl:hint :polygon-smooth-hint :nicest)

     ;(gl:clear :color-buffer-bit :depth-buffer-bit)
     (gl:enable :lighting :light0 :depth-test)
     (glu:look-at ,cx ,cy ,cz
                  ,lx ,ly ,lz
                  ,ux ,uy ,uz)

     (with-push-matrix ()
       ,@body
       )
     (gl:disable :lighting :light0 :depth-test)))

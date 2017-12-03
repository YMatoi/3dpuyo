;;3dバージョンのぷよぷよ (環)

(ql:quickload :alexandria)

(defun array-2d= (a b) ;;二次元配列を比較する
  (let ((a-d (array-dimensions a))
        (b-d (array-dimensions b)))
    (cond ((not (equal a-d b-d )) nil)
          (t
           (let ((xmax (cadr a-d))
                 (ymax (car a-d))
                 (check t))
             (dotimes (i xmax)
               (dotimes (j ymax)
                 (if (not (equal (aref a j i) (aref b j i)))
                     (setf check nil))))
             check)))))

(defun make-board-init (y x max) ;;初期状態を作成する(連鎖の確認用
  (let ((array (make-array (list y x) :initial-element 0)))
    (dotimes (i y)
      (dotimes (j x)
        (setf (aref array i j) (+ (random max) 1))))
    array))

(defmacro access-board (board y x) ;;二次元配列を循環させるためのアクセス関数。
  `(aref ,board
         (mod ,y (array-dimension ,board 0))
         (mod ,x (array-dimension ,board 1))))

(let ((n 0)
      (temp-board nil))
  (defun init-count (board)
    (setf n 0)
    (setf temp-board (alexandria:copy-array board)))
  (defun count-rec (y x)
    (let ((id (access-board temp-board y x)))
      (incf n)
      (setf (access-board temp-board y x) nil)
      (if (equal id (access-board temp-board (+ y 1) x)) (count-rec (+ y 1) x))
      (if (equal id (access-board temp-board (- y 1) x)) (count-rec (- y 1) x))
      (if (equal id (access-board temp-board y (+ x 1))) (count-rec y (+ x 1)))
      (if (equal id (access-board temp-board y (- x 1))) (count-rec y (- x 1)))))
  (defun count-puyo (board y x)
    (init-count board)
    (count-rec y x)
    n))

(defun delete-puyo (board)
  (let ((xmax (array-dimension board 1))
        (ymax (array-dimension board 0))
        (next (alexandria:copy-array board)))
    (dotimes (y ymax)
      (dotimes (x xmax)
        (if (>= (count-puyo board y x) 4)
            (setf (access-board next y x) 0))))
    next))

(let ((temp-board nil))
  (defun init-down (board)
    (setf temp-board (alexandria:copy-array board)))
  (defun down-puyo-1 (y x)
    (cond ((< (- y 1) 0) nil)
          ((not (equal (access-board temp-board y x) 0)) nil)
          ((equal (access-board temp-board (- y 1) x) 0) nil)
          (t
           (progn
             (setf (access-board temp-board y x) (access-board temp-board(- y 1) x))
             (setf (access-board temp-board (- y 1) x) 0)
             t))))
  (defun down-puyo-rec (check)
    (let ((xmax (array-dimension temp-board 1))
          (ymax (array-dimension temp-board 0))
          (c nil))
      (if (not check)
          temp-board
          (progn (dotimes (y ymax)
                   (dotimes (x xmax)
                     (if (down-puyo-1 y x) (setf c t))))
                 (down-puyo-rec c)))))
  (defun down-puyo (board)
    (init-down board)
    (down-puyo-rec t)))

;;操作中のぷよ
(let ((a-x 0) (a-y 1) (state 0) (a 1) (b 1))
  (defun init-my-puyo (n)
    (setf a-x 0 a-y 1 state 2
          a (+ (random n) 1)
          b (+ (random n) 1)))
  (defun a-c ()
    a)
  (defun b-c ()
    b)
  (defun a-x ()
    a-x)
  (defun a-y ()
    a-y)
  (defun a-x+ ()
    (incf a-x))
  (defun a-x- ()
    (decf a-x))
  (defun a-x= (a)
    (setf a-x a))
  (defun b-x (&optional (a-x a-x))
    (cond ((= state 0) a-x)
          ((= state 2) a-x)
          ((= state 1) (+ a-x 1))
          ((= state 3) (- a-x 1))))
  (defun b-y (&optional (a-x a-x))
    (cond ((= state 1) a-y)
          ((= state 3) a-y)
          ((= state 0) (+ a-y 1))
          ((= state 2) (- a-y 1))))
  (defun state+= (a)
    (setf state (mod (+ state a) 4)))
  (defun check-my-puyo (board)
    (cond ((not (= (access-board board (a-y) (a-x)) 0)) nil)
          ((not (= (access-board board (b-y) (b-x)) 0)) nil)
          ((>= (a-y) *ymax*) nil)
          ((>= (b-y) *ymax*) nil)
          (t t)))
  (let ((check 1))
    (defun change-state-my-puyo (board)
      (state+= check)
      (if (not (check-my-puyo board))
          (progn
            (setf check (* check -1))
            (state+= check)))))
  '(defun change-state-my-puyo (board)
    (state+= 1)
    (if (not (check-my-puyo board))
        (progn
          (state+= -1)
          (state+= -1)
          (if (not (check-my-puyo board))
              (state+= 1)))))
  (defun enter-my-puyo (board)
    (setf (access-board board (a-y)(a-x)) a
          (access-board board (b-y)(b-x)) b)
    (init-my-puyo 3))
  (defun down-my-puyo (board)
    (setf a-y (+ a-y 1))
    (if (not (check-my-puyo board))
        (progn
          (setf a-y (- a-y 1))
          (enter-my-puyo board))))
  )
(init-my-puyo 3)

;;三次元に可視化する
(load "3d.lisp")

(defparameter *board* (make-board-init 12 21 3))
(defparameter *next-board* (alexandria:copy-array *board*))
(defparameter *xmax* (array-dimension *board* 1))
(defparameter *ymax* (array-dimension *board* 0))
(defparameter *angle* 0)

(let ((c nil))
  (defun pushed-a ()
    (if (sdl:get-key-state :sdl-key-a)
        (progn
          (setf c t)
          nil)
        (if c
            (progn
              (setf c nil)
              t)
            nil))))

(let ((c nil))
  (defun pushed-w ()
    (if (sdl:get-key-state :sdl-key-w)
        (progn
          (setf c t)
          nil)
        (if c
            (progn
              (setf c nil)
              t)
            nil))))

(let ((c nil))
  (defun pushed-s ()
    (if (sdl:get-key-state :sdl-key-s)
        (progn
          (setf c t)
          nil)
        (if c
            (progn
              (setf c nil)
              t)
            nil))))

(let ((c nil))
  (defun pushed-d ()
    (if (sdl:get-key-state :sdl-key-d)
        (progn
          (setf c t)
          nil)
        (if c
            (progn
              (setf c nil)
              t)
            nil))))

(let ((time 0)
      (state 0))
  (defmethod update ()
    (incf time)
    (if (pushed-d) (progn
                     (incf *angle*)
                     (a-x= (- *angle*))
                     (if (not (check-my-puyo *board*)) (decf *angle*))))
    (if (pushed-a) (progn
                     (decf *angle*)
                     (a-x= (- *angle*))
                     (if (not (check-my-puyo *board*)) (incf *angle*))))
    (if (pushed-w) (progn
                     (a-x= (- *angle*))
                     (change-state-my-puyo *board*)))
    (if (pushed-s) (progn
                     (a-x= (- *angle*))
                     (down-my-puyo *board*)))
    (if (> time 60)
        (progn
          (cond ((= state 0) (setf *next-board* (delete-puyo *board*)))
                ((= state 1) (setf *next-board* (down-puyo *board*))))
          (setf state (- 1 state))
          (setf time 0)
          (setf *board* *next-board*)))))

(defun color-box (a)
  (cond ((= a 1) (texture-cube 1 *texture2* 1 0 0))
        ((= a 2) (texture-cube 1 *texture2* 0 1 0))
        ((= a 3) (texture-cube 1 *texture2* 1 1 1))))

(defmethod draw ()
  (with-3d-view (0 -15 -30 0 5 0 0 -1 0)
    (with-rotate (0 0 1 0)
      (with-translate (0 (a-y) -4)
        (color-box (a-c))))
    (with-rotate ((* (/ 360 *xmax*) (b-x 0)) 0 1 0)
      (with-translate (0 (b-y) -4)
        (color-box (b-c))))
    (dotimes (x *xmax*)
      (with-rotate ((* (/ 360 *xmax*) (+ x)) 0 1 0)
        (with-translate (0 *ymax* -4)
          (cond  ((= x 0) (texture-cube 1 *texture4*))
                 (t
                  (texture-cube 1 *texture4* 0.5 0.5 0.5))))))
    (dotimes (y *ymax*)
      (dotimes (x *xmax*)
        (with-rotate ((* (/ 360 *xmax*) (+ x *angle*)) 0 1 0)
          (with-translate (0 y -4)
            (let ((a (access-board *board* y x)))
              (color-box a))))))))



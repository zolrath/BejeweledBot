(ns bejewed.core)

(def offset [422 427])  ;; set to top-left pixel of game window
(def grid-size 40) ;; set width/height of each cell in grid

(def color-map (hash-map -15368450 :blue -15307265 :blue -1 :blue
                         -655313 :red -917468 :red
                         -67297 :yellow -1248 :yellow -4945124 :yellow -67072 :yellow -5602816 :yellow
                         -14900709 :green -14771968 :green -14640128 :green
                         -1441555 :purple -1769224 :purple -1703687 :purple
                         -2236963 :white -2500135 :white -14632192 :white
                         -2008292 :orange -2338560 :orange -291260 :orange -289969 :orange))

(defn get-color-at [x y]
  (.getRGB (.getPixelColor (java.awt.Robot.) x y)))

(defn get-gem-color [x y]
  (let [x-pos (+ 422 (+ 20 (* x grid-size)))
        y-pos (+ 427 (+ 20 (* y grid-size)))]
    (get-color-at x-pos y-pos)))

(defn make-grid []
  (let [grid (for [y (range 8)
               x (range 8)]
               (color-map (get-gem-color x y)))]
    (vec grid)))

(defn click-point
  ([[x y]] (click-point [x y] 20))
  ([[orig-x orig-y] sleep-time]
     (let [[x y] (map + [orig-x orig-y] offset)]
       (dorun [(println orig-x orig-y)
               (.mouseMove (java.awt.Robot.) x y)
               (Thread/sleep 20)
               (.mousePress (java.awt.Robot.)
                            (.. java.awt.event.InputEvent BUTTON1_MASK))
               (Thread/sleep sleep-time)
               (.mouseRelease (java.awt.Robot.)
                              (.. java.awt.event.InputEvent BUTTON1_MASK))
               ]))))

(defn click-coord [x y]
  (let [x-pos (+ 20 (* x grid-size))
        y-pos (+ 20 (* y grid-size))]
    (click-point [x-pos y-pos])))

(defn click-coords [seq]
  (doseq [[x y] seq] (click-coord x y)))

(defn get-coord [x y board]
  (if (and (<= x 7) (<= y 7))
    (let [coord (+ x (* y 8))]
      (get board coord))
    (throw (Exception. "Invalid coordinate."))))

(defmacro find-gem [x xf nx y yf ny board]
  `(let [x# ~x
         y# ~y]
     (get-coord (~xf x# ~nx) (~yf y# ~ny) ~board)))

(defn next-gem [x y n b]
  (find-gem x + n y + 0 b))
(defn up-gem [x y n b]
  (find-gem x + 0 y - n b))
(defn down-gem [x y n b]
  (find-gem x + 0 y + n b))
(defn get-gem [x xf nx y yf ny b]
  (find-gem x xf nx y yf ny b))


(defn right-match [x y board]
  "x-xx"
  (if (<= x 4)
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (next-gem x y 2 board))
               (= gem (next-gem x y 3 board)))
        (click-coords [[x y] [(+ x 1) y]])))))

(defn left-match [x y board]
  "xx-x"
  (if (<= x 4)
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (next-gem x y 1 board))
               (= gem (next-gem x y 3 board)))
        (click-coords [[(+ x 3) y] [(+ x 2) y]])))))

(defn down-match [x y board]
  "x
   -
   x
   x"
  (if (<= y 4)
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (down-gem x y 2 board))
               (= gem (down-gem x y 3 board)))
        (click-coords [[x y] [x (+ y 1)]])))))

(defn up-match [x y board]
  "x
   x
   -
   x"
  (if (<= y 4)
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (down-gem x y 1 board))
               (= gem (down-gem x y 3 board)))
        (click-coords [[x (+ y 3)] [x (+ y 2)]])))))


(defn top-middle-match [x y board]
  "-x-
   x-x"
  (if (and (<= x 6)
           (<= y 6))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x - 1 y + 1 board))
               (= gem (get-gem x + 1 y + 1 board)))
        (click-coords [[x y] [x (+ y 1)]])))))

(defn top-left-match [x y board]
  "x--
   -xx"
  (if (and (<= x 5)
           (<= y 6))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x + 1 y + 1 board))
               (= gem (get-gem x + 2 y + 1 board)))
        (click-coords [[x y] [x (+ y 1)]])))))

(defn top-right-match [x y board]
  "--x
   xx-"
  (if (and (<= x 7)
           (<= y 6))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x - 1 y + 1 board))
               (= gem (get-gem x - 2 y + 1 board)))
        (click-coords [[x y] [x (+ y 1)]])))))

(defn bottom-middle-match [x y board]
  "x-x
   -x-"
  (if (and (<= x 6)
           (>= x 1)
           (>= y 1))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x - 1 y - 1 board))
               (= gem (get-gem x + 1 y - 1 board)))
        (click-coords [[x y] [x (- y 1)]])))))

(defn bottom-left-match [x y board]
  "-xx
   x--"
  (if (and (<= x 5)
           (>= y 1))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x + 1 y - 1 board))
               (= gem (get-gem x + 2 y - 1 board)))
        (click-coords [[x y] [x (- y 1)]])))))

(defn bottom-right-match [x y board]
  "xx-
   --x"
  (if (and (<= x 6)
           (>= x 2)
           (<= y 6)
           (>= y 1))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x - 1 y - 1 board))
               (= gem (get-gem x - 2 y - 1 board)))
        (click-coords [[x y] [x (- y 1)]])))))

(defn left-side-top-match [x y board]
  "x-
   -x
   -x"
  (if (and (<= x 6)
           (<= y 5))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x + 1 y + 1 board))
               (= gem (get-gem x + 1 y + 2 board)))
        (click-coords [[x y] [(+ x 1) y]])))))

(defn left-side-middle-match [x y board]
  "-x
   x-
   -x"
  (if (and (<= x 6)
           (<= y 6))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x + 1 y - 1 board))
               (= gem (get-gem x + 1 y + 1 board)))
        (click-coords [[x y] [(+ x 1) y]])))))

(defn left-side-bottom-match [x y board]
  "-x
   -x
   x-"
  (if (and (<= x 6))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x + 1 y - 1 board))
               (= gem (get-gem x + 1 y - 2 board)))
        (click-coords [[x y] [(+ x 1) y]])))))

(defn right-side-top-match [x y board]
  "-x
   x-
   x-"
  (if (and (>= x 1)
           (<= y 5))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x - 1 y + 1 board))
               (= gem (get-gem x - 1 y + 2 board)))
        (click-coords [[x y] [(- x 1) y]])))))

(defn right-side-middle-match [x y board]
  "x-
   -x
   x-"
  (if (and (>= x 1)
           (<= y 6))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x - 1 y - 1 board))
               (= gem (get-gem x - 1 y + 1 board)))
        (click-coords [[x y] [(- x 1) y]])))))

(defn right-side-bottom-match [x y board]
  "x-
   x-
   -x"
  (if (and (>= x 1))
    (if-let [gem (get-coord x y board)]
      (if (and (= gem (get-gem x - 1 y - 1 board))
               (= gem (get-gem x - 1 y - 2 board)))
        (click-coords [[x y] [(- x 1) y]])))))

(defn match-cond [x y board]
  (cond
   (right-match x y board) (str "RIIIGHT")
   (left-match x y board) (str "LEEEEFT")
   (down-match x y board) (str "dooown")
   (up-match x y board) (str "uppppppp")
   (top-middle-match x y board) (str "tm")
   (top-left-match x y board) (str "tl")
   (top-right-match x y board) (str "tr")
   (bottom-middle-match x y board) (str "bm")
   (bottom-left-match x y board) (str "bl")
   (bottom-right-match x y board) (str "br")
   (left-side-top-match x y board) true
   (left-side-middle-match x y board) true
   (left-side-bottom-match x y board) true
   (right-side-top-match x y board) true
   (right-side-middle-match x y board) true
   (right-side-bottom-match x y board) true
   :else (str "Noooope")))

(defn match-all [board]
  (doseq [x (range 7 -1 -1)
          y (range 7 -1 -1)]
    (match-cond x y board)))


(defn game-loop []
  (loop [n 0]
    (if (< n 30)
      (do (match-all (make-grid))
          (Thread/sleep 1000)
          (recur (inc n))))))

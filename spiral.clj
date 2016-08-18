(ns basic.core)
(import '(java.awt.image VolatileImage))
(import '(java.awt GraphicsConfiguration GraphicsEnvironment Color BasicStroke))
(import '(javax.swing JDialog JScrollPane JLabel ImageIcon ))

(def image 
  (let [ge (GraphicsEnvironment/getLocalGraphicsEnvironment)
        gc (.getDefaultConfiguration (.getDefaultScreenDevice ge))
        vi-drawing (.createCompatibleVolatileImage gc 1229 691)]
    vi-drawing))

(def image-label (-> image ImageIcon. JLabel.))
(def g (.createGraphics image))
(def dialog
  (let [d (javax.swing.JFrame.)
        cp (doto (.getContentPane d)
             (.setLayout (java.awt.GridLayout. 1 1))
             (.add image-label))]
    (doto d
      (.setSize 1229 691)
      (.setVisible true))))


(defn poly-spiral
  [x0 y0 interval sides num]
  (take num (map rest (iterate (fn [[i x y]]
                     (let [theta (* 2 Math/PI i (/ sides))
                           r (* interval i)
                           x2 (+ x0 (* r (Math/cos theta)))
                           y2 (+ y0 (* r (Math/sin theta)))]
                       [(inc i) x2 y2])) [1 x0 y0]))))

(defn draw-poly-spiral
  [xor yor interval sides num]
  (.setPaint g (Color. (int (* 128 (rand)))
                       (int (* 128 (rand)))
                       (int (* 128 (rand)))))  
  (.fillRect g 0 0 (.getWidth image) (.getHeight image))
  (.setPaint g (Color. (int (+ 127 (* 128 (rand))))
                       (int (+ 127 (* 128 (rand))))
                       (int (+ 127 (* 128 (rand))))))
  (doseq [[[x0 y0] [x1 y1]] (partition 2 (interleave (poly-spiral xor yor interval sides num)
                                                     (rest (poly-spiral xor yor interval sides num))))]
    (.drawLine g x0 y0 x1 y1))
  (.repaint image-label))

(defn start
  [a]
  (loop [fns [identity reverse]]
    (doseq [i ((first fns) (range a))]
      (draw-poly-spiral (int (* 0.5 (.getWidth image)))
                        (int (* 0.5 (.getHeight image)))
                        1.3 (inc (/ i a)) 500)
      (Thread/sleep (int (/ 1000 60))))
    (recur (reverse fns))))

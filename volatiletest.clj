(ns volatiletest)
(import '(java.awt.image VolatileImage BufferedImage))
(import '(java.awt GraphicsConfiguration GraphicsEnvironment Color BasicStroke))
(import '(java.awt.event ActionListener MouseAdapter))
(import '(java.awt.geom Ellipse2D$Double Path2D$Double))
(import '(javax.imageio ImageIO))
(import '(java.io File))
(import '(javax.swing JDialog JScrollPane JLabel ImageIcon JPopupMenu JMenuItem))

(def mouse-button (atom nil))

(def vertex-to-move (atom nil))
(add-watch vertex-to-move  :aoeuaoeu
	   (fn [k v old new]
	     (if (not= old new) (println (str "movable vertex set to " new)))))
     
(def polylines (atom []))
(add-watch polylines :aoeuaoeu
	   (fn [k v old new]
	     (println "active polylines: ")
	     (doseq [aoeu new]
	       (println aoeu))))

(def vertices (atom []))
(add-watch vertices :aoeuaoeu
	   (fn [k v old new]
	     (if (not= old new) (println (str "active vertices: " new)))))

(defn reset-state
  []
  (reset! mouse-button nil)
  (reset! vertex-to-move nil)
  (reset! polylines [])
  (reset! vertices []))

(defn- image-repainter
  [g base-image]
  (.drawImage g base-image 0 0 nil)
  (.setPaint g Color/RED)
  (.setStroke g (BasicStroke. 2))
  (doseq [polyline @polylines]
    (loop [[[x1 y1] [x2 y2] & rem] (interleave polyline (rest polyline))]
      (if (and x1 x2 y1 y2)
	(.drawLine g x1 y1 x2 y2))
      (if rem (recur rem))))
  (.setStroke g (BasicStroke. 1))
  (case (count @vertices)
	1
	(let [[x y] (first @vertices)
	      circle (Ellipse2D$Double. x y 4 4)]
	  (doto g
	    (.setPaint Color/BLACK)
	    (.fill circle)
	    (.setPaint Color/WHITE)
	    (.draw circle)))
	0 nil
	(let [circles (mapv (fn [[x y]] (Ellipse2D$Double. x y 4 4)) @vertices)
	      [[x y] & p-rest] @vertices
	      path2d (Path2D$Double.)]
	  (loop [[circle & rem] circles i 0]
	    (doto g
	      (.setPaint Color/BLACK)
	      (.fill circle)
	      (.setPaint (if (= i @vertex-to-move) Color/BLUE Color/WHITE))
	      (.draw circle))
	    (if rem (recur rem (inc i))))
	  (.moveTo path2d (double x) (double y))
	  (loop [[[x y] & rem] p-rest]
	    (.lineTo path2d (double x) (double y))
	    (if rem (recur rem)))
	  (.setPaint g Color/WHITE)
	  (.draw g path2d))))
  
  
(defn vertex-adder
  [me g base-image]
  (let [[mx my] (map #(% (bean me)) [:x :y])
	vertex-under-mouse (first (filter (fn [[x y]] (<= (Math/sqrt (+ (* (- mx x) (- mx x)) (* (- my y) (- my y)))) 7)) @vertices))]
    (if vertex-under-mouse
      nil
      (do 
	(swap! vertices conj [mx my])
	(image-repainter g base-image)))))

(defn- set-vertex-to-move
  [me g base-image]
  (let [[mx my] (map #(% (bean me)) [:x :y])
	vertex (first (filter (fn [[i [x y]]] (<= (Math/sqrt (+ (* (- mx x) (- mx x)) (* (- my y) (- my y)))) 7)) (map-indexed vector @vertices)))]
    (if vertex
      (reset! vertex-to-move (first vertex))
      (reset! vertex-to-move nil))
    (image-repainter g base-image)))
    
(defn vertex-mover
  [me g base-image]
  (let [[mx my] (map #(% (bean me)) [:x :y])]
    (when @vertex-to-move
      (swap! vertices assoc @vertex-to-move [mx my])
      (image-repainter g base-image))))

(defn path-finisher
  [me g base-image]
  (if (>= (count @vertices) 2)
    (swap! polylines conj @vertices))
  (reset! vertices [])
  (image-repainter g base-image))

(defn polyline-under-mouse
  [me g base-image]
  (let [[mx my] (map #(% (bean me)) [:x :y])
	temp-vertices (atom nil)]
    (if @polylines
      (doseq [polyline (shuffle @polylines)]
	(loop [[[x1 y1] [x2 y2] & rem] (interleave polyline (rest polyline)) min-dist 500]
	  (let [a (- mx x1) b (- my y1) c (- x2 x1) d (- y2 y1)
		dot (+ (* a c) (* b d))
		len-sq (+ (* c c) (* d d))
		param (if (zero? len-sq) -1 (/ dot len-sq))
		[xx yy] (cond (< param 0)
			      [x1 y1]
			      (> param 1)
			      [x2 y2]
			      :else [(+ x1 (* param c)) (+ y1 (* param d))])
		[dx dy] [(- mx xx) (- my yy)]
		d (Math/sqrt (+ (* dx dx) (* dy dy)))]			      
			  
	    (if rem
	      (recur rem (min d min-dist))
	      (do (println [x1 y1 x2 y2 mx my d])
		  (if (<= (min d min-dist) 5)
		    (reset! temp-vertices polyline))))))))
    (when @temp-vertices
      (if (>= (count @vertices) 2)
	(swap! polylines conj @vertices))
      (reset! vertices @temp-vertices)
      (reset! polylines (into [] (remove #(= % @temp-vertices) @polylines))))
    (image-repainter g base-image)))
	

(defn create-equation-editing-dialog
  [parent-dialog]
  "buhhhh")

(defn display-image
  [path]
  (reset-state)
  (let [bufferedimage (ImageIO/read (File. path)) ;;to show an image
	[w h] (map #(% (bean bufferedimage)) [:width :height])
	ge (GraphicsEnvironment/getLocalGraphicsEnvironment)
	gc (.getDefaultConfiguration (.getDefaultScreenDevice ge))
	vi-drawing (.createCompatibleVolatileImage gc w h) ;;to draw on top of the underlying image
	vi-drawing-g (.getGraphics vi-drawing)
	vi-image (.createCompatibleVolatileImage gc w h)
	vi-image-g (.getGraphics vi-image)
	dialog (JDialog.)
	content-pane (.getContentPane dialog)
	scrollpane (JScrollPane.)
	image-label (JLabel. (ImageIcon. vi-drawing))
	menu-launcher-thread (atom nil)
	menu-launched (atom nil)

	launch-menu (fn [me]
		      (let [menu (JPopupMenu.)
			    [mx my] (map #(% (bean me)) [:x :y])
			    line-editor (doto (JMenuItem. "Edit line under point")
					  (.addActionListener
					   (proxy [ActionListener] []
					     (actionPerformed [ae]
							      (polyline-under-mouse me vi-drawing-g vi-image)
							      (.repaint dialog)
							      (.setVisible menu false)
							      (reset! menu-launched nil)))))
			    equation-editor (doto (JMenuItem. "Edit transformation equations")
					      (.addActionListener
					       (proxy [ActionListener] []
						 (actionPerformed [ae]
								  (create-equation-editing-dialog dialog)
								  (.setVisible menu false)
								  (reset! menu-launched nil)))))]
			(doto menu
			  (.add line-editor)
			  (.add equation-editor)
			  (.pack)
			  (.setLocation mx my)
			  (.setVisible true))))
			    

	start-menu-launcher (fn [me]
			      (let [t (Thread. (fn []
						 (reset! menu-launched false)
						 (loop [i (System/currentTimeMillis)
							j (System/currentTimeMillis)]
						   (when (not (Thread/interrupted))
						     (if (>= (- i j) 250)
						       (do (reset! menu-launched true)
							   (launch-menu me))
						       (recur (System/currentTimeMillis) j))))))]
				(.start t)

				(reset! menu-launcher-thread t)))
	mouseadapter (proxy [MouseAdapter] []
		       (mouseClicked [me]
				     (if (not @menu-launched)
				       (case (.getButton me)
					     1
					     (vertex-adder me vi-drawing-g vi-image)
					     3
					     (if (not @menu-launched)
					       (path-finisher me vi-drawing-g vi-image))
					     nil))
				     (.repaint dialog))
		       (mousePressed [me]
				     (reset! mouse-button (.getButton me))
				     (case @mouse-button
					   1 (set-vertex-to-move me vi-drawing-g vi-image)
					   3 (start-menu-launcher me)
					   nil)
				     (.repaint dialog))
		       (mouseReleased [me]
				      (reset! vertex-to-move nil)
				      (image-repainter vi-drawing-g vi-image)
				      (if @menu-launcher-thread
					(.interrupt @menu-launcher-thread))
				      (.repaint dialog))
		       (mouseDragged [me]
				     (case @mouse-button
					   1 (do (vertex-mover me vi-drawing-g vi-image)
						 (.repaint dialog))
					   nil)))
	]

    (.drawImage vi-image-g bufferedimage 0 0 nil)
    (.drawImage vi-drawing-g bufferedimage 0 0 nil)

    (.setViewportView scrollpane image-label)
    (.add content-pane scrollpane)

    (.addMouseListener image-label mouseadapter)
    (.addMouseMotionListener image-label mouseadapter)
    

    (doto dialog
      (.setSize 500 500)
      (.setVisible true))))
	
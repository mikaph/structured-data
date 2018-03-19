(ns structured-data)

(defn
  do-a-thing
  [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn
  spiff
  [v]
  (+ (get v 0) (get v 2)))

(defn
  cutify
  [v]
  (conj v "<3"))

(defn
  spiff-destructuring
  [[v1 _ v3]]
  (+ v1 v3))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn
  width
  [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
  (- x2 x1)))

(defn
  height
  [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
  (- y2 y1)))

(defn
  square?
  [rectangle]
  (= (height rectangle) (width rectangle)))

(defn
  area
  [rectangle]
  (* (height rectangle) (width rectangle)))

(defn
  contains-point?
  [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
  (and (<= x1 xp x2) (<= y1 yp y2) )))

(defn
  contains-rectangle?
  [outer inner]
  (let [[bottom-left top-right] inner]
  (and (contains-point? outer bottom-left) (contains-point? outer top-right))))

(defn
  title-length
  [book]
  (count (:title book)))

(defn
  author-count
  [book]
  (count (:authors book)))

(defn
  multiple-authors?
  [book]
  (< 1 (author-count book)))

(defn
  add-author
  [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn
  alive?
  [author]
  (not (contains? author :death-year)))

(defn
  element-lengths
  [collection]
  (map count collection))

(defn
  second-elements
  [collection]
  (let [sec (fn [coll] (get coll 1))]
  (map sec collection)))

(defn
  titles
  [books]
  (map :title books))

(defn
  monotonic?
  [x]
  (or (apply <= x) (apply >= x)))

(defn
  stars
  [n]
  (apply str (repeat n "*")))

(defn
  toggle
  [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn
  contains-duplicates?
  [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))


(defn old-book->new-book
  [book]
  (assoc book :authors (set (:authors book))))

(defn
  has-author?
  [book author]
  (contains? (:authors book) author))

(defn
  authors
  [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names
  [books]
  (set (map :name (apply clojure.set/union (map :authors books)))))


(defn
  author->string
  [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
  (if (contains? author :birth-year) (str name " (" birth-year " - " death-year ")") (str name)) ))


(defn
  authors->string
  [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn
  book->string
  [book]
  (str (:title book) ", written by " (authors->string (authors #{book}))))


(defn
  books->string
  [books]
  (if (empty? books)
    "No books."
    (str (count books) " book" (if (> (count books) 1) "s") ". " (apply str (interpose ". " (map book->string books))) ".")))


(defn
  books-by-author
  [author books]
  (filter (fn [x] (has-author? x author)) books))


(defn
  author-by-name
  [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))


(defn
  living-authors
  [authors]
  (filter alive? authors))


(defn
  has-a-living-author?
  [book]
  (not (empty? (living-authors (authors #{book})))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

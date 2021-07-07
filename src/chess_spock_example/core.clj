(ns chess-spock-example.core
  (:require [spock.swi :as spock])
  (:gen-class))

(def pawn-moves
  '[(moves [(position (piece black pawn) 1 :col) (game :_ :board :_) :result]
           [(not (memberchk (position :_ 3 :col) :board))
            (not (memberchk (position :_ 2 :col) :board))
            (= :result [3 :col])])
    (moves [(position (piece white pawn) 6 :col) (game :_ :board :_) :result]
           [(not (memberchk (position :_ 4 :col) :board))
            (not (memberchk (position :_ 5 :col) :board))
            (= :result [4 :col])])
    (moves [(position (piece black pawn) :row :col) (game :_ :board :_) :result]
           [(is :new-row (+ :row 1))
            (not (memberchk (position :_ :new-row :col) :board))
            (= :result [:new-row :col])])
    (moves [(position (piece white pawn) :row :col) (game :_ :board :_) :result]
           [(is :new-row (- :row 1))
            (not (memberchk (position :_ :new-row :col) :board))
            (= :result [:new-row :col])])

    (moves [(position (piece black pawn) :row :col) (game :_ :board :_) :result]
           [(is :new-row (+ :row 1))
            (or (is :new-col (+ :col 1)) (is :new-col (- :col 1)))
            (memberchk (position (piece white :_) :new-row :new-col) :board)
            (= :result [:new-row :new-col])])
    (moves [(position (piece white pawn) :row :col) (game :_ :board :_) :result]
           [(is :new-row (- :row 1))
            (or (is :new-col (+ :col 1)) (is :new-col (- :col 1)))
            (memberchk (position (piece black :_) :new-row :new-col) :board)
            (= :result [:new-row :new-col])])])

(def gen-moves
  '[(combine [[] :_ :res] [(= :res []) !])
    (combine [:_ [] :res] [(= :res []) !])
    (combine [[:f1 & :r1] [:f2 & :r2] :res]
             [(= :first [:f1 :f2])
              (combine :r1 :r2 :rest)
              (= :res [:first & :rest])])
    (gen-lists [:num :list]
               [(numlist :num 7 [:_ & :before])
                (numlist 0 :num :a)
                (reverse :a [:_ & :after])
                (or (= :list :before)
                    (= :list :after))])
    (simple-moves [[:row :col] rook :list]
           [(= :rows [:row :row :row :row :row :row :row :row])
            (gen-lists :col :v-cols)
            (combine :rows :v-cols :list)])
    (simple-moves [[:row :col] rook :list]
           [(= :cols [:col :col :col :col :col :col :col :col])
            (gen-lists :row :v-rows)
            (combine :v-rows :cols :list)])
    (simple-moves [[:row :col] bishop :list]
           [(gen-lists :col :v-cols)
            (gen-lists :row :v-rows)
            (combine :v-rows :v-cols :list)])
    (simple-moves [:pos queen :list] [(simple-moves :pos bishop :list)])
    (simple-moves [:pos queen :list] [(simple-moves :pos rook :list)])
    (simple-moves [:pos king :list] [(simple-moves :pos queen :res)
                                     (nth0 0 :res :elem)
                                     (= :list [:elem])])])

(def rules-moves
  '[(valid-pos [[:row :col]] [(>= :row 0)
                              (< :row 8)
                              (>= :col 0)
                              (< :col 8)])
    (moves [(position (piece :color knight) :row :col) (game :_ :board :_) :result]
           [(permutation [2 1] [:dx :dy])
            (member :mx [1 -1])
            (member :my [1 -1])
            (is :ncol (+ :col (* :mx :dx)))
            (is :nrow (+ :row (* :my :dy)))
            (not (memberchk (position (piece :color :_) :nrow :ncol) :board))
            (= :result [:nrow :ncol])])
    (cut [:_ :_ [] :result] [(= :result [])])
    (cut [:board :color [[:row :col] & :rst] :result]
         [(memberchk (position (piece :ncolor :_) :row :col) :board)
          (and (or (-> (= :ncolor :color) (= :result []))
                   (= :result [[:row :col]]))
               !)])
    (cut [:board :color [:fst & :rst] :result]
         [(cut :board :color :rst :res)
          (= :result [:fst & :res])])
    (moves [(position (piece :color :kind) :row :col) (game :_ :board :_) :result]
           [(simple-moves [:row :col] :kind :list)
            (not (member :kind ['pawn 'knight]))
            (cut :board :color :list :moves)
            (member :result :moves)])])

(def moves
  '[(other-player [black :other] [(= :other white)])
    (other-player [white :other] [(= :other black)])
    (in-check [:player :board]
              [(memberchk (position (piece :player king) :k-row :k-col) :board)
               (other-player :player :other)
               (member :piece :board)
               (= :piece (position (piece :other :_) :_ :_))
               (moves :piece (game :_ :board :_) [:k-row :k-col])])
    (move [:pos (game :_ :board :_) :result]
          [(member :pos :board)
           (subtract :board [:pos] :without-me)
           (not (= :board :without-me))
           (moves :pos (game :_ :board :_) [:new-row :new-col])
           (valid-pos [:new-row :new-col])
           (subtract :without-me [(position :_ :new-row :new-col)] :without)
           (= :pos (position :piece :_ :_))
           (union :without [(position :piece :new-row :new-col)] :result)
           (= :pos (position (piece :player :_) :_ :_))
           (not (in-check :player :result))])])

(def finish
  '[(finished [:player :board :end]
              [(in-check :player :board)
               (not (and (member :piece :board)
                         (= :piece (position (piece :player :_) :_ :_))
                         (move :piece (game :_ :board :_) :new-board)))
               (= :end checkmate)])
    (finished [:player :board :end]
              [(not (and (member :piece :board)
                         (= :piece (position (piece :player :_) :_ :_))
                         (move :piece (game :_ :board :_) :_)))
               (= :end stalemate)])])

(def rules (concat pawn-moves gen-moves rules-moves moves finish))

(defn- piece [color kind row col]
  (list 'position
        (list 'piece color kind) row col))

(def initial-board
  (let [pieces ['[rook knight bishop queen king bishop knight rook] (repeat 8 'pawn)]
        pieces (concat (map (fn [p] ['black p]) (apply concat pieces))
                       (map (fn [p] ['white p]) (apply concat (reverse pieces))))
        board (concat (for [row (range 2), col (range 8)] [row col])
                      (for [row (range 6 8), col (range 8)] [row col]))]
    (mapv (fn [[color kind] [row col]]
            (piece color kind row col))
          pieces
          board)))

(def pieces '{"K" king
              "Q" queen
              "R" rook
              "B" bishop
              "N" knight
              "P" pawn})

(defn- translate-row-col [rowcol]
  (cond
    (nil? rowcol) :_
    (re-matches #"\d+" rowcol) (- 8 (Integer/parseInt rowcol))
    :else (- (int (first rowcol)) 97)))

(defn- chess-notation [string player]
  (let [[beg end] (re-seq #"[KQRBN]?[a-h][1-8]" string)
        [beg end] (if (nil? end)
                    [nil beg]
                    [beg end])
        kind (pieces (re-find #"[KQRBN]" (or beg end)) 'pawn)
        [_ start-col start-row] (some->> beg (re-find #"[KQRBN]?([a-h])?([1-8])?"))
        [_ end-col end-row] (re-find #"[KQRBN]?([a-h])?([1-8])?" end)]
    {:start (piece player
                   kind
                   (translate-row-col start-row)
                   (translate-row-col start-col))
     :end (piece player kind
                 (translate-row-col end-row)
                 (translate-row-col end-col))}))

(defn- move-piece [rules board {:keys [start end]}]
  (let [moves
        (spock/solve {:rules rules :bind {:board board
                                          :pos start
                                          :end end}}
                     '(and (move :pos (game :_ :board :_) :final-board)
                           (member :end :final-board)))]
    (when (-> moves count (not= 1))
      (throw (ex-info "Not a valid move" {:possibilities (count moves)})))
    (-> moves first :final-board)))

(defn- play [rules {:keys [board player]} movement]
  (let [notation (chess-notation movement player)
        new-board (move-piece rules board notation)
        new-player (if (= player 'black) 'white 'black)
        [{:keys [finish]}] (spock/solve {:rules rules
                                         :bind {:player new-player
                                                :board new-board}}
                                        '(finished :player :board :finish))]
    (when finish
      (throw (ex-info "GAME FINISHED" {:finish finish
                                       :player new-player})))
    {:board new-board
     :player new-player}))

#_
(with-open [r (spock/with-rules rules)]
  (let [play (partial play r)]
    (-> {:board initial-board :player 'white}
        ; (play "e4")
        ; (play "e5")
        ; (play "Qf3")
        ; (play "Na6")
        ; (play "Bc4")
        ; (play "Nb4")
        ; (play "Qf7")
        :board)))

#_
(def chess-example
  [(piece 'black 'rook 0 3)
   (piece 'black 'rook 0 7)
   (piece 'white 'rook 3 0)
   (piece 'white 'queen 4 4)
   (piece 'white 'queen 4 7)
   (piece 'rook 'queen 7 0)
   (piece 'white 'queen 7 7)])

#_
(with-open [r (spock/with-rules rules)]
  [chess-example
   (move-piece r chess-example
               (chess-notation "Qh4e1" 'white))])

#_
(defn gen-random-chessboard []
  (let [pieces (concat (repeat 8 'pawn)
                       '[bishop bishop queen king knight knight rook rook])
        pieces (for [piece pieces, color '[black white]] [color piece])
        board (for [row (range 8), col (range 8)] [row col])
        full-board (mapv (fn [[color kind] [row col]]
                           (piece color kind row col))
                         pieces
                         (shuffle board))]
    (def last-full-board full-board)
    (time
     (with-open [r (spock/with-rules rules)]
       (spock/solve {:rules r
                     :bind {:board full-board}}
                    '(finished white :board :ended))))))

#_
last-full-board
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

#_
(let [board [(piece 'white 'queen 4 3)
             (piece 'black 'queen 4 5)
             (piece 'white 'king 4 4)
             (piece 'white 'bishop 5 4)
             (piece 'white 'knight 6 4)]]
  (with-open [r (spock/with-rules rules)]
    (try
      (cons board
            (mapv :result
                  (spock/solve {:rules r
                                :bind {:board board
                                       :pos (piece 'white 'king 4 4)}}
                               '(move :pos (game :_ :board :_) :result)))))))

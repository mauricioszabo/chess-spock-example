(ns chess-spock-example.commons)
  ; (:require [spock.swi :as spock]
  ;           [chess-spock-example.core :as chess]))

(def html-for-chess
  '(let [board (->> ?state
                    (map (fn [[_ [_ color kind] row col]]
                           [[row col] [color kind]]))
                    (into {}))
         board (mapv (fn [row]
                       (mapv (fn [col] (board [row col]))
                             (range 8)))
                     (range 8))
         pieces {'[black pawn] "\u265F"
                 '[black rook] "\u265C"
                 '[black knight] "\u265E"
                 '[black bishop] "\u265D"
                 '[black queen] "\u265B"
                 '[black king] "\u265A"
                 '[white rook] "\u2656"
                 '[white knight] "\u2658"
                 '[white bishop] "\u2657"
                 '[white queen] "\u2655"
                 '[white king] "\u2654"
                 '[white pawn] "\u2659"}
         w-style {:width "35pt" :height "35pt"
                  :background-color "gray"
                  :font-size "20pt"
                  :text-align "center"}
         b-style (assoc w-style :background-color "lightgray")
         tds (cycle [[:td {:style w-style}] [:td {:style b-style}]])
         light-tds (take 8 (rest tds))
         black-tds (take 8 tds)
         make-row (fn [row-idx col-idx col html-col]
                    (conj html-col [:a {:href "#" :style {:color "black"}
                                        :on-click (fn [_] (editor/run-callback
                                                           :on-copy
                                                           (pr-str
                                                            (list 'position
                                                                  (apply list 'piece col)
                                                                  row-idx col-idx))))}
                                    (str (pieces col))]))
         board (map (fn [row-idx row html-row] (map (partial make-row row-idx)
                                                    (range)
                                                    row
                                                    html-row))
                    (range)
                    board
                    (cycle [light-tds black-tds]))
         html-board (map (fn [tds idx] [:tr [:td (str (- 8 idx))] [:<> tds]]) board (range 8))]
     [:div
      [:table
       [:tbody
        [:tr
         [:td] [:td "a"] [:td "b"] [:td "c"] [:td "d"] [:td "e"] [:td "f"] [:td "g"] [:td "h"]]
        [:<> html-board]]]]))

(def html-for-boards
  (list 'let ['html (list 'fn '[?state] html-for-chess)]
        '[:div.rows
          [:div.cols
           [:a {:href "#" :on-click (fn [_]
                                      (swap! ?state-atom update :idx #(cond-> % (> % 0) dec)))}
            "<"]
           "  Moves (" (:idx ?state) ") "
           [:a {:href "#" :on-click (fn [_]
                                      (swap! ?state-atom update :idx
                                             #(cond-> %
                                                      (< % (dec (count (:boards ?state))))
                                                      inc)))}
            ">"]]
          (html (nth (:boards ?state) (:idx ?state)))]))

(defn chessboard [board]
  {:html html-for-chess
   :state board})

(defn moves [state]
  {:html html-for-boards
   :state {:boards state :idx 0}})

(defn slide [contents]
  (let [conts (cond-> contents (not (vector? contents)) vector)]
    {:state {:idx 0
             :fs? false
             :slides conts}
     :html '[:<>
             [:style "@keyframes example {}"
              "@keyframes fade {
                from {opacity: 0;}
                to {opacity: 1;}
              }"
              "div.fs div {
                animation-name: fade;
                animation-duration: 0.5s;
              }"
              "pre { margin-bottom: 4em;}"
              "div.fs {font-size: 2em;
                   padding: 2em;
                   background-color: #222;}
           div.fs pre code {font-size: 3.5em !important;}"]
             [:div.rows.fs {:id "fs"
                            :class "native-key-bindings"
                            :tabIndex "-1"
                            :style {:animation-name "example"
                                    :animation-duration "0.1s"}
                            :onClick (fn [e]
                                       (when (< (:idx ?state) (-> ?state :slides count dec))
                                         (swap! ?state-atom update :idx inc)))
                            :onContextMenu (fn [e]
                                             (when (> (:idx ?state) 0)
                                               (swap! ?state-atom update :idx dec)))
                            :on-animation-end (fn [e]
                                                (when-not (:fs? ?state)
                                                  (swap! ?state-atom assoc :fs? true)
                                                  (.requestFullscreen (.-target e))))}
              (let [{:keys [slides idx]} ?state
                    contents (get slides idx)]
                [:div.pre [:div/md contents]])]]}))

(def prolog-slides ["# Spock - Prolog in Clojure
*Maurício Eduardo Chicupo Szabo*"

                    "# About:Me
* Author of Chlorine & Clover
* Living on Montevideo (we accept clojurians wanting to live here :D)
* Searching for people to help on reviving the idea of a hackable text editor :D"

                    "# A brief reminder of Prolog

* Imperative: I command the computer
* Functional: Here's some data, get me a result
* Logic: This thing here is true
  * No \"orders\" nor \"transformations\"
  * Equality, and backtracking (No **cons** and **member** and **insert**)"
                    "# Prolog - Facts

```prolog
person(mauricio, 22, [clojure, prolog]).
person(ariovaldo, 27, [ruby, perl]).
```

# Prolog - Rules

```prolog
programmer_of(Language, Name) :-
  person(Name, _, Languages),
  member(Language, Languages).
```"
                    "# Elements
* Atom - mauricio, clojure, prolog, ruby, perl
* Var - Language, Languages, Name
* Struct - person(name, age, list)
* List - [clojure, prolog]
"
                    "# ->Clojure
* Atom -> symbols - 'mauricio, 'clojure, 'prolog, 'ruby, 'perl
* Var -> keywords - :language, :languages, :name
* Struct -> list - '(person name age list)
* List -> Vector - '[clojure prolog]
"
                    "# What Prolog?
* TuProlog - Java Native, *REALLY slow*
* SWI - Prolog - Needs FFI to run JPL

Demo?
"])

(def chess-slides
  ["# Let's do a chess board

In Clojure, we would do something like:

```clojure
(def board
  [[:black/rook :black/knight ...]
   [:black/pawn :black/pawn ...]])
```"
    "# Let's do a chess board

In Prolog, we want to assert facts. There's the \"impure\" way:

```prolog
assert ( position( piece(black, rook), 0, 0 ) )
assert ( position( piece(black, knight), 0, 1 ) )
```
... or in Spock ...
```clojure
(spock/with-rules '[[position [(piece black rook) 0 0]]
                    [position [(piece black knight) 0 1]]])
```"
    "# Let's do a chess board

Or we can use a _list of facts_

```prolog
Board = [
  position( piece(black, rook), 0, 0 ),
  position( piece(black, knight), 0, 1 )
], move(
  Piece,
  game(LastBoard, Board, Flags),
  NewBoard
).
```
... in Spock ...
```clojure
(spock/solve '(and (= :board [(position (piece black rook) 0 0)
                              (position (piece black knight) 0 0)])
                   (move :piece (game :last-board :board :flags))
                   :new-board))
```"])

(def closing-slides
  ["# The best of both worlds

* Clojure makes some very pragmatic decisions on how to add \"impure\" functional code and not look too alien
* In prolog, \"impure\" logic is still kinda weird
* So, the best of both worlds!"
   "# For example...
```clojure
(spock/solve {:bind {:board initial-board}}
             '(and (member :row [0 1 2 3 4 5 6 7])
                   (member :col [0 1 2 3 4 5 6 7])
                   (or (and (member (position (piece :color :piece) :row :col) :board)
                            (atomics_to_string [:color \"/\" :piece] :to-print-1)
                            (string_length :to-print-1 :size)
                            (or (and (> :size 10)
                                     (sub_string :to-print-1 0 10 :_ :to-print))
                                (and (or (= :size 10) (< :size 10))
                                     (= :to-print :to-print-1))))
                       (and (not (member (position :piece :row :col) :board))
                            (= :to-print \"          \")))
                   (write :to-print)
                   (or (and (= :col 7) nl)
                       (write \" | \"))))
```"
   "# To print this:
```
 | black/rook | black/knig | black/bish | black/quee | black/king | ...
 | black/pawn | black/pawn | black/pawn | black/pawn | black/pawn | ...
 |            |            |            |            |            | ...
 |            |            |            |            |            | ...
 |            |            |            |            |            | ...
 |            |            |            |            |            | ...
 | white/pawn | white/pawn | white/pawn | white/pawn | white/pawn | ...
 | white/rook | white/knig | white/bish | white/quee | white/king | ...
```"
   "# In Clojure:

```clojure
(let [pieces (->> initial-board
                  (map (fn [[_ [_ color piece] row col]]
                         [[row col] (keyword (str color) (str piece))]))
                  (into {}))]
  (doseq [row (range 8)]
    (doseq [col (range 8)
            :let [piece (pieces [row col] \"\")]]
      (apply print piece (repeat (- 14 (count (str piece)) ) \"\"))
      (when-not (= 7 col) (print \"| \")))
    (println)))
```
* Questions?"])

(defn prolog-res [res]
  (if (seq res)
    {:html '(into [:div.rows]
                  (map (fn [fact idx]
                         [:<>
                          [:div.title "Result " (inc idx)]
                          (if (= {} fact)
                            [:div.title "Yes"]
                            [:div/clj (walk/prewalk #(if (and (keyword? %)
                                                              (-> % name first (= "_")))
                                                       '_
                                                       %)
                                                    fact)])
                          [:div.space]])
                       ?state (range)))
     :state res}
    {:html [:div.error "No"]}))

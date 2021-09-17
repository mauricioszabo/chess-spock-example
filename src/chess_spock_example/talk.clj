(ns chess-spock-example.talk
  (:require [spock.swi :as spock]
            [chess-spock-example.core :as chess]))

(def html-for-chess
  '(let [board (->> ?state
                    (map (fn [[_ [_ color kind] row col]]
                           [[row col] (keyword (str color) (str kind))]))
                    (into {}))
         board (mapv (fn [row]
                       (mapv (fn [col] (board [row col]))
                             (range 8)))
                     (range 8))
         pieces {:black/pawn "\u265F"
                 :black/rook "\u265C"
                 :black/knight "\u265E"
                 :black/bishop "\u265D"
                 :black/queen "\u265B"
                 :black/king "\u265A"
                 :white/rook "\u2656"
                 :white/knight "\u2658"
                 :white/bishop "\u2657"
                 :white/queen "\u2655"
                 :white/king "\u2654"
                 :white/pawn "\u2659"}
         w-style {:width "35pt" :height "35pt"
                  :background-color "gray"
                  :font-size "20pt"
                  :text-align "center"}
         b-style (assoc w-style :background-color "lightgray")
         tds (cycle [[:td {:style w-style}] [:td {:style b-style}]])
         light-tds (take 8 (rest tds))
         black-tds (take 8 tds)
         make-row (fn [col html-col]
                    (conj html-col [:a {:href "#" :style {:color "black"}
                                        :on-click (fn [_] (editor/run-callback :on-copy
                                                                               (pr-str col)))}
                                    (str (pieces col))]))
         board (map (fn [row html-row] (map make-row row html-row))
                    board (cycle [light-tds black-tds]))
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

(defn- chessboard [board]
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
              "pre { margin-bottom: 10em;}"
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
* I'm from Brazil
* Living on Montevideo (we accept clojurians wanting to live here :D)
* Author of Chlorine & Clover
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
"])

(defn- prolog-res [res]
  (if (seq res)
    {:html (into [:div.rows]
                 (map (fn [fact]
                        (if (= {} fact)
                          [:div.title "Yes"]
                          [:div/clj fact])))
                 res)}
    {:html [:div.error "No"]}))

;;; -- CUT HERE --
(slide prolog-slides)

#_
(spock/solve '(= :age 20))

#_
(prolog-res
 (spock/solve '(length [9 7 3] 3)))

#_
(prolog-res
 (spock/solve '(member 7 [9 7 3])))

#_
(prolog-res
 (spock/solve '(and (length :list 4)
                    (member 7 :list))))

#_
(prolog-res
 (spock/solve '(and (length :l 3)
                    (member 5, :l)
                    (member 2 :l))))

#_
(with-open [r (spock/with-rules '[[person [mauricio 22 [clojure prolog]]]
                                  [person [ariovaldo 27 [ruby perl]]]])]
  (spock/solve {:rules r}
               '(and (person :name :_ :languages)
                     (member :language :languages)
                     (= :language clojure))))

; (slide rules-in-clj)

(def pawn-moves
  '[(moves [(position (piece black pawn) 1 :col) (game :_ :board :_) :result]
           [(not (memberchk (position :_ 3 :col) :board))
            (not (memberchk (position :_ 2 :col) :board))
            (= :result [3 :col])])])

#_
(let [board '[(position (piece white queen) 5 5)
              (position (piece black king) 0 0)
              (position (piece black pawn) 1 1)]]
 (with-open [r (spock/with-rules chess/rules)]
   (spock/solve {:rules r
                 :bind {:board board}}
                '(move (position (piece black pawn) 1 1)
                       (game :_ :board :_)
                       :result))))

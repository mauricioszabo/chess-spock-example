(ns chess-spock-example.talk
  (:require [spock.swi :as spock]
            [chess-spock-example.core :as chess]
            [chess-spock-example.commons :refer :all]))

(slide prolog-slides)

#_
(spock/solve '(= 20 20))

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
  (-> {:rules r}
      (spock/solve '(and (person :name :_ :languages)
                         (member :language :languages)
                         (= :language clojure)))
      prolog-res))

;; CHESS GAME!
(slide chess-slides)

#_
(let [board '[(position (piece black rook) 0 0)
              (position (piece black knight) 0 1)]]
  (->> (spock/solve {:bind {:board board}} '(member :member :board))))
       ; (mapv :member)
       ; chessboard))


;; Chessboard moves
; move -> moves -> simple-moves
; moves -> pawn, knight
; simple-moves -> king, rook, bishop, queen

#_
(with-open [rules (spock/with-rules chess/moves-rules)]
  (->>
   (spock/solve {:rules rules}
                '(and (gen-lists 5 :rows)))
                      ; (gen-lists 4 :cols)
                      ; (combine :rows :cols :result)))
   ; (mapv :result)
   prolog-res))

#_
(with-open [r (spock/with-rules chess/rules)]
  (->> '(and (= :res :board))
             ; (move :position (game :_ :board :_) :res))
       (spock/solve {:rules r :bind {:board chess/initial-board}})
       (mapv :res)
       moves))

#_
(with-open [r (spock/with-rules chess/rules)]
  (->> '(and (move (position (piece white pawn) 6 5) (game :_ :board :_) :step1))
             ; (move (position (piece black :_) :_ :_) (game :_ :step1 :_) :step2)
             ; (move (position (piece white :_) :_ :_) (game :_ :step2 :_) :step3)
             ; (move (position (piece black :_) :_ :_) (game :_ :step3 :_) :step4)
             ; (finished white :step4 checkmate))
       (spock/solve {:rules r :bind {:board chess/initial-board}})
       first
       ((juxt :board :step1 :step2 :step3 :step4))
       moves))

(slide closing-slides)

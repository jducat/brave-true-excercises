(ns brave-true-chapters.chapter10
  (:gen-class))
(require '[clojure.string :as s])

;----------------------------------------------------------------------------------------------------------------------------------
;;;; Excercise 1:
;; Create an atom with the initial value 0, use swap! to increment it a couple of times, and then dereference it.
;----------------------------------------------------------------------------------------------------------------------------------

(def test-atom (atom 0))

(swap! test-atom inc)
(swap! test-atom inc)

@test-atom

;----------------------------------------------------------------------------------------------------------------------------------
;;;; Excercise 2:
;; Create a function that uses futures to parallelize the task of downloading random quotes from
;; https://www.braveclojure.com/random-quote using (slurp "https://www.braveclojure.com/random-quote").
;; The futures should update an atom that refers to a total word count for all quotes. The function will take the number of quotes
;; to download as an argument and return the atom’s final value. Keep in mind that you’ll need to ensure that all futures have
;; finished before returning the atom’s final value. Here’s how you would call it and an example result:
;;
;; (quote-word-count 5)
;; => {"ochre" 8, "smoothie" 2}
;----------------------------------------------------------------------------------------------------------------------------------


(def url "https://www.braveclojure.com/random-quote")
(def n-cores 4) ;; number of cores on my machine
(def word-freq (atom {}))

(defn quote-word-count
  "chapter 9"
  [n-quotes]
  (doseq [calls (partition-all (/ n-quotes n-cores) (range n-quotes))] ; this splits up the input up into n new threads, being equal to the number of cores on my machine.
    (doseq [nth-call calls]
;;; AP - This has a subtle, and nasty bug. :-(
;;; You're blocking on @read-quote right after each future,
;;; and thus the doseq can't start the next future until each
;;; one has finished.  You can see this by inserting a print statement
;;; in your future like I've done below.  On my box, this printed:
;;;
;; Starting future: 16
;; Finished future:  16
;; Starting future: 200
;; Finished future:  200
;; Starting future: 951
;; Finished future:  951
;; Starting future: 333
;; Finished future:  333
;; Starting future: 322
;; Finished future:  322
;;
;; You need to get to a situation where it prints something more like:
;; Starting future: 16
;; Starting future: 200
;; Starting future: 322
;; Starting future: 333
;; Starting future: 951
;; Finished future:  16
;; Finished future:  200
;; Finished future:  322
;; Finished future:  333
;; Finished future:  951
;;
;; Do you understand why?  AP
      (let [read-quote (promise)
            id (rand-int 1000)]
        (future
          (println "Starting future:" id)
          (deliver read-quote (slurp url))
          (swap! word-freq (fn [current-state]
                             (merge-with + current-state (frequencies (s/split @read-quote #"\W+"))))))
        @read-quote
        (println "Finished future: " id))))
;;; AP - Try to not return global values from functions.
;;; Better is:
;;; (let [word-freq (atom {})]
;;;   (your code here)
;;;   @word-freq)
  @word-freq)

(quote-word-count 5)

;----------------------------------------------------------------------------------------------------------------------------------
;;;; Excercise 3:
;; Create representations of two characters in a game. The first character has 15 hit points out of a total of 40. The second
;; character has a healing potion in his inventory. Use refs and transactions to model the consumption of the healing potion and
;; the first character healing.
;----------------------------------------------------------------------------------------------------------------------------------


(def player-1 (ref {:hit 15
                    :inventory {}}))

(def player-2 (ref {:hit 0
                    :inventory {:healing-potion 1}}))

(defn use-potion
  [player-1 player-2]
  (dosync
   (alter player-1 update-in [:hit] - (:hit @player-1))
   (alter player-2 update-in [:inventory :healing-potion] - 1)))

(use-potion player-1 player-2)

;; I was going to put a validator on the potion to make sure they have one to remove, but instead I have
;; killed a third player - just for fun.... and provided a warning message

(defn is-dead?
  [{:keys [hit]}]
  ;; AP - Stylistic note: typically, a function with a name ending in a question mark
  ;; is considered a "predicate" and returns a true/false value.  It should never throw.
  ;; I would call this function something like `assert-alive!`
  (or (< hit 40)
      (throw (IllegalStateException. "You're dead! You have taken too many hits.. :("))))

(def player-3 (ref {:hit 0
                    :inventory {}}
                   :validator is-dead?))

(defn take-hit
  [player hit]
  (dosync
   (alter player update-in [:hit] + hit)))

(take-hit player-3 10)

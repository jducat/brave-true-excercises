(ns brave-true-chapters.chapter9
  (:gen-class))

;----------------------------------------------------------------------------------------------------------------------------------
;;;; Excercises 1 and 2:
;; 1. Write a function that takes a string as an argument and searches for it on Bing and Google using the slurp function. 
;; Your function should return the HTML of the first page returned by the search.
;; 2. Update your function so it takes a second argument consisting of the search engines to use.
;----------------------------------------------------------------------------------------------------------------------------------
(def default-search-engines ["https://www.bing.com/search?q%3D" "https://au.search.yahoo.com/search?p%3D"])

(defn online-search
  "chapter 9"
  ([search-term]
   (online-search search-term default-search-engines))
  ([search-term search-engines]
   (let [read-engine (promise)]
     (doseq [engine search-engines]
       (future (deliver read-engine (slurp (str engine search-term)))
               (println @read-engine))))))

(online-search "clojure")

;; ** bug - noticed that when you call the function with just one search engine (not a list of engines), it has an error as it then 
;; splits the one input out into characters in the doseq.
;; Icouldnt quickly figure out how to resolve this without using if statements etc. Is there an easy way to do this?

;----------------------------------------------------------------------------------------------------------------------------------
;;;; Excercise 3
;; 1. Create a new function that takes a search term and search engines as arguments, and returns a vector of the URLs from the 
;; first page of search results from each search engine.
;----------------------------------------------------------------------------------------------------------------------------------

(defn get-urls
  [source]
  (re-seq #"href=\"(https?://[=?.:/\w]+)" source))

;; not finished... but the regex above was tested - thanks Alain.


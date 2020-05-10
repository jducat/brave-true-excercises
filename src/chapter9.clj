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

;; AP - This appears to not return any value?  How do you get the results out?

(online-search "clojure") ;; => nil


;; ** bug - noticed that when you call the function with just one search engine (not a list of engines), it has an error as it then
;; splits the one input out into characters in the doseq.
;; Icouldnt quickly figure out how to resolve this without using if statements etc. Is there an easy way to do this?

;; AP - Your function signature is [search-term search-engines] so you need to
;; provide exactly 2 arguments.  So call it like this:
(online-search "clojure" ["https://www.bing.com/search?q%3D" "https://au.search.yahoo.com/search?p%3D"])

;; If you want to call like like this:
;(online-search2 "clojure" "https://www.bing.com/search?q%3D" "https://au.search.yahoo.com/search?p%3D")
;; You need to redefine the signagure as being [search-term & search-engines]
;; The & will wrap all the remaining arguments into a sequence.

;----------------------------------------------------------------------------------------------------------------------------------
;;;; Excercise 3
;; 1. Create a new function that takes a search term and search engines as arguments, and returns a vector of the URLs from the
;; first page of search results from each search engine.
;----------------------------------------------------------------------------------------------------------------------------------

(defn promised-request
  [term search-engine]
  (let [url (str search-engine term)
        request-promise (promise)]
    (future (deliver request-promise (slurp url)))
    request-promise))

(defn get-urls
  [source]
  (re-seq #"href=\"(https?://[=?.:/\w]+)" source))

(def a "class=\"fs_label\">Language</span><span class=\"sw_ddbk\"></span> </a></span><span class=\"ftrB\"><a class=\"ftrH\" id=\"h5096\" role=\"button\" aria-label=\"Filtered by Region\" aria-expanded=\"false\" aria-controls=\"d5096\" aria-haspopup=\"true\" href=\"javascript:\" h=\"ID=SERP,5393.1\"><span class=\"fs_label\">Region</span><span class=\"sw_ddbk\"></span> </a></span></div><ol id=\"b_results\"><li class=\"b_algo\"><h2 class=\" b_topTitle\"><a href=\"https://clojure.org/\"  h=\"ID=SERP,5136.1\">Clojure</a></h2><div class=\"b_caption\"><div class=\"b_attribution\" u=\"0N|5120|4829265696328671|hgcUoMfj8l2UGK_7rbG9WbWaHfZzuisZ\"><cite>https://<strong>clojure</strong>.org</cite><span class=\"c_tlbxTrg\"><span class=\"c_tlbxH\" H=\"BASE:CACHEDPAGEDEFAULT\" K=\"SERP,5137.1\"></span></span></div><p><strong>Clojure</strong> is a dynamic, general-purpose programming language, combining the approachability and interactive development of a scripting language with an efficient and robust infrastructure for multithreaded programming. <strong>Clojure</strong> is a compiled language, yet remains completely dynamic – every feature supported by <strong>Clojure</strong> is supported at runtime.</p></div><div class=\"b_vlist2col b_deep\"><ul><li><h3 class=\"deeplink_title\"><a href=\"https://clojure.org/about/rationale\" ")
(vec (flatten (get-urls a)))
;(deref (promised-request "clojure" "https://www.bing.com/search?q%3D"))

(defn search
  "Exercise 3"
  [term search-engines]
  (vec (flatten (map #(get-urls (deref %)) (map #(promised-request term %) search-engines)))))


(search "clojure" default-search-engines)

;; (defn -main [& args] (println :chapter9))

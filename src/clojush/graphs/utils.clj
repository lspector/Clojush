(ns clojush.graphs.utils
  (require [plumbing.graph :as graph]
           [plumbing.core :refer [fnk map-vals]]
           [plumbing.fnk.pfnk :as pfnk]
           [plumbing.map :as map]))

(def debug? (System/getenv "CLOJUSH_DEBUG_GRAPH"))

(defn debug [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn debug-fnk
  "Prints (to stderr) when any function is computed"
  [f ks]
  (pfnk/fn->fnk
   (fn [m]
     (debug "->" ks)
     (let [res (f m)]
      (debug "<-" ks)
      res))
   [(pfnk/input-schema f)
    (pfnk/output-schema f)]))


(def profile-file-name (System/getenv "CLOJUSH_FLAME_GRAPH_FILE"))

(defn now []
  (System/nanoTime))

(def profile-atom
  (atom {:started {nil (now)}
         :finished {}}))

(defn start! [ks]
  (swap! profile-atom assoc-in [:started ks] (now)))

(defn end!
  "record the ending of a function call.

  Remove the start time from :started and add the time elapsed to
  :finished. Also add it's duration to the start times of anything
  else which hasn't finished yet, so that those other parent functions
  don't count this time"
  [ks]
  (swap! profile-atom
    (fn [prof]
     (let [start-time (get-in prof [:started ks])
           total-time (- (now) start-time)]
        (-> prof
          (update :started dissoc ks)
          (update :finished update ks (fn [prev-time] (+ total-time (or prev-time 0))))
          (update :started (partial map-vals #(+ % total-time))))))))


(defn profile-fnk
  [f ks]
  (pfnk/fn->fnk
   (fn [m]
     (start! ks)
     (let [res (f m)]
      (end! ks)
      res))
   [(pfnk/input-schema f)
    (pfnk/output-schema f)]))

(defn flame-str [ks time]
  (str
    (clojure.string/join ";" (map name ks))
    " "
    time
    "\n"))

(defn end-profile! []
  (when profile-file-name
    (end! nil)
    (let [{:keys [started finished]} @profile-atom]
      (assert (empty? started))
      (spit profile-file-name "")
      (doseq [[ks time] finished]
        (spit profile-file-name (flame-str ks time) :append true)))))


(defn wrap-graph [prefix graph]
  (->> graph
    (map/map-leaves-and-path
      (fn [ks f]
        (let [combined-ks (cons prefix ks)]
          (cond-> f
            debug? (debug-fnk combined-ks)
            profile-file-name (profile-fnk combined-ks)))))))

(defn compile-graph [prefix graph]
  (->> graph
    (wrap-graph prefix)
    plumbing.graph/lazy-compile))

(ns clojush.graphs.config.log.text
  (:require [plumbing.core :refer [defnk]]
            [clj-random.core :as random]
            [clojush.util :refer [not-lazy]]

            [clojush.graphs.init.log.text :refer [print-params]]))

(defnk text
  [argmap
   registered-instructions
   clojush-version
   git-hash
   argmap-with-random-str]

  ((:problem-specific-initial-report argmap) argmap)
  (println  "Registered instructions:" registered-instructions)
  (println "Starting PushGP run.")
  (println "Clojush version = "
    (or clojush-version
        "version number unavailable"))
  ;; NOTES: - Last commit hash will only be correct if this code has
  ;;          been committed already.
  ;;        - GitHub link will only work if commit has been pushed
  ;;          to GitHub.
  (let [git-hash-str (if (empty? git-hash) "unavailable" git-hash)]
    (println "Hash of last Git commit =" git-hash-str)
    (println
      (str
        "GitHub link = https://github.com/lspector/Clojush/commit/"
        git-hash-str)))
  (print-params argmap-with-random-str))

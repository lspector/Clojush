(defproject clojush "3.25.0"
  :description "The Push programming language and the PushGP genetic programming
                system implemented in Clojure. See http://pushlanguage.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojars.etosch/cosmos "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/data.csv "0.1.4"]
                 [local-file "0.1.0"]
                 [clojure-csv "2.0.2"]
                 [org.clojure/data.json "0.2.6"]
                 [clj-random "0.1.8"]
                 ;; https://mvnrepository.com/artifact/org.apache.commons/commons-math3
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [cheshire "5.8.1"]
                 [prismatic/plumbing "0.5.5"]
                 [criterium "0.4.4"]
                 [net.totakke/libra "0.1.1"]
                 [quil "2.8.0"]]
  :plugins [[lein-codox "0.10.6"]
            [lein-shell "0.5.0"]
            [lein-gorilla "0.4.0"]
            [cider/cider-nrepl "0.21.1"]
            [lein-cloverage "1.0.13"]
            [net.totakke/lein-libra "0.1.2"]]
  :profiles {:text {:plugins [[venantius/ultra "0.5.4"]]}}
  :codox {:source-uri "http://github.com/lspector/Clojush/blob/master/{filepath}#L{line}"
          :namespaces [#"^(?!clojush\.problems)"]
          :output-path "doc"
          :metadata {:doc/format :markdown}}
  :ultra {:repl         false
          :stacktraces  false
          :tests        true}
  :libra {:bench-paths ["test/clojush/bench"]}
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :username :env
                              :sign-releases false
                              :password :env}]]
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["shell" "git" "commit" "-am" "Version ${:version} [ci skip]"]
                  ["vcs" "tag" "v" "--no-sign"] ; disable signing and add "v" prefix
                  ["deploy"]
                  ["change" "version" "leiningen.release/bump-version" "qualifier"]
                  ["shell" "git" "commit" "-am" "Version ${:version} [ci skip]"]
                  ["vcs" "push"]]
  ;; faster without defaults
  ;; https://plot.ly/~SaulShanabrook/11/?share_key=pNTJGA4S58MA29Uqskbr6j
  ;; https://push-language.hampshire.edu/t/improving-profiling-clojush-performance-results/904/13?u=saulshanabrook
  :jvm-opts ^:replace []
  :main clojush.core)

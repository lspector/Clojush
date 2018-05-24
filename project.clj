(defproject clojush "3.10.0-1-SNAPSHOT"
  :description "The Push programming language and the PushGP genetic programming
                system implemented in Clojure. See http://pushlanguage.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojars.etosch/cosmos "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.clojure/data.csv "0.1.3"]
                 [local-file "0.1.0"]
                 [clojure-csv "2.0.1"]
                 [org.clojure/data.json "0.2.6"]
                 [clj-random "0.1.7"]
                 ;; https://mvnrepository.com/artifact/org.apache.commons/commons-math3
                 [org.apache.commons/commons-math3 "3.2"]
                 [cheshire "5.7.1"]
                 [prismatic/plumbing "0.5.4"]]

  ; different compiled classes per profile
  :target-path "target/%s"
  :plugins [[lein-codox "0.9.1"]
            [lein-shell "0.5.0"]
            [lein-gorilla "0.4.0"]
            [cider/cider-nrepl "0.15.1"]
            [lein-cloverage "1.0.6"]]
  :aliases {"benchmark" ["with-profile" "+benchmark" "trampoline" "jmh"]
            "benchmark-sample" ["with-profile" "+benchmark" "trampoline" "run"]
            "benchmark-compare" ["with-profile" "+compare" "trampoline" "run"]}

  :profiles {:text {:plugins [[venantius/ultra "0.5.1"]]}
             :benchmark {:main clojush.bench.helpers/sample
                         :aot [clojush.bench.helpers]
                         :dependencies [[de.ruedigermoeller/fst "2.57"]]
                         :plugins [[lein-jmh "0.2.5"]]
                         ; increase stack size to deal with deserializing nested objects
                         :jvm-opts ["-Xss4m"]}
             :compare {:main clojush.bench.compare
                       :dependencies [[lein-jmh "0.2.5"]
                                      [jmh-clojure "0.2.1"]
                                      [clojure-future-spec "1.9.0-beta4"]]}}
  :codox {:source-uri "http://github.com/lspector/Clojush/blob/master/{filepath}#L{line}"
          :namespaces [#"^(?!clojush\.problems)"]
          :output-path "doc"
          :metadata {:doc/format :markdown}}
  :ultra {:repl         false
          :stacktraces  false
          :tests        true}
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

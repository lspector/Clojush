(defproject clojush "3.0.0-1-SNAPSHOT"
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
                 [prismatic/plumbing "0.5.4"]
                 [mvxcvi/puget "1.0.1"]]
  :plugins [[lein-codox "0.9.1"]
            [lein-shell "0.5.0"]
            [lein-gorilla "0.4.0"]
            [cider/cider-nrepl "0.14.0"]
            [lein-cloverage "1.0.6"]]
  :profiles {:text {:plugins [[venantius/ultra "0.5.1"]]}}
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
;;;;;;;;;; jvm settings for high performance, using most of the machine's RAM
;  :jvm-opts ~(let [mem-to-use
;                   (long (* (.getTotalPhysicalMemorySize
;                              (java.lang.management.ManagementFactory/getOperatingSystemMXBean))
;                            0.8))]
;               ^:replace [(str "-Xmx" mem-to-use)
;                          (str "-Xms" mem-to-use)
;                          "-server"
;                          "-XX:-TieredCompilation"
;                          "-XX:+AggressiveOpts"
;                          "-Djava.awt.headless=true"])
  ;;;;;;;;;; misc other jvm-opts
  ;:jvm-opts ["-Djava.awt.headless=true"]
  ;;"-XX:+UseG1GC"
  ;:jvm-opts ["-Xmx12g" "-Xms12g" "-XX:+UseParallelGC"]
  :main clojush.core)

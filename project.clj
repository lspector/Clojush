(defproject clojush "2.0.73-SNAPSHOT"
  :description "The Push programming language and the PushGP genetic programming
                system implemented in Clojure.
                See http://hampshire.edu/lspector/push.html"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojars.etosch/cosmos "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.clojure/data.csv "0.1.3"]
                 [local-file "0.1.0"]
                 [clojure-csv "2.0.1"]
                 [org.clojure/data.json "0.2.6"]
                 [clj-random "0.1.7"]]
  :plugins [[lein-codox "0.9.1"]
            [lein-shell "0.5.0"]
            [lein-gorilla "0.3.5"]
            [lein-cloverage "1.0.6"]]
  :codox {:source-uri "http://github.com/lspector/Clojush/blob/master/{filepath}#L{line}"
          :namespaces [#"^(?!clojush\.problems)"]
          :output-path "doc"
          :metadata {:doc/format :markdown}}
  :dev-dependencies [[lein-ccw "1.2.0"][lein-midje "3.1.3"]]
  :profiles {:dev {:dependencies [[midje "1.7.0"]]}}
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :username :env
                              :sign-releases false
                              :password :env}]]
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["shell" "git" "commit" "-am" "Version ${:version} [ci skip]"]
                  ["vcs" "tag" "v" "--no-sign"] ; disable signing and add "v" prefix
                  ["deploy"]
                  ["change" "version" "leiningen.release/bump-version"]
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

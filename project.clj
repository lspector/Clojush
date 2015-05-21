(defproject clojush "2.0.26"
  :description "The Push programming language and the PushGP genetic programming
                system implemented in Clojure.
                See http://hampshire.edu/lspector/push.html"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojars.etosch/cosmos "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [org.clojure/data.csv "0.1.2"]
                 [local-file "0.1.0"]
                 [clojure-csv "2.0.0-alpha2"]
                 [org.clojure/data.json "0.1.3"]
                 [clj-random "0.1.7"]
                 ;[incanter/incanter-core "1.5.2"]
                 ]
  :dev-dependencies [[lein-ccw "1.2.0"]]
  ;; the following, or a variant, may be helpful on big machines
  ;:jvm-opts ["-Xmx58g" "-Xms58g" "-XX:+UseParallelGC"]
  ;:jvm-opts ["-Xmx12g" "-Xms12g" "-XX:+UseParallelGC"]
  ;:jvm-opts ["-Xmx58g" "-Xms58g" "-XX:+UseParallelGC" "-Djava.awt.headless=true"]
  ;; the following should automatically take 80% of the machine's RAM and also 
  ;; turn on some other jvm settings for high performance
;  :jvm-opts ~(let [mem-to-use
;                   (long (* (.getTotalPhysicalMemorySize
;                              (java.lang.management.ManagementFactory/getOperatingSystemMXBean))
;                            0.8))]
;               ^:replace [(str "-Xmx" mem-to-use)
;                          (str "-Xms" mem-to-use)
;                          "-server"
;                          "-XX:-TieredCompilation"
;                          "-XX:+AggressiveOpts"])
  ;;"-XX:+UseG1GC"
  :main clojush.core)

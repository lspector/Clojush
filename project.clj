(defproject lspector/clojush "1.3.29"
            :description "The Push programming language and the PushGP genetic
                          programming system implemented in Clojure.
                          See http://hampshire.edu/lspector/push.html"
            :dependencies [[org.clojure/clojure "1.5.1"]
                           [org.clojars.etosch/cosmos "1.0.0"]
                           [org.clojure/math.numeric-tower "0.0.1"]
                           [local-file "0.1.0"]
                           [clojure-csv "2.0.0-alpha2"]
                           [org.clojure/data.json "0.1.3"]
                           ;[incanter/incanter-core "1.5.2"]
                           ]
            :dev-dependencies [[lein-ccw "1.2.0"]]
            ;; the following, or a variant, may be helpful on big machines
            ;:jvm-opts ["-Xmx58g" "-Xms58g" "-XX:+UseParallelGC"]
            ;:jvm-opts ["-Xmx58g" "-Xms58g" "-XX:+UseParallelGC" "-Djava.awt.headless=true"]
            :main clojush.core)

(defproject mw-explore "0.1.5-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [hiccup "1.0.5"]
                 [mw-engine "0.1.5-SNAPSHOT"]
                 [mw-parser "0.1.5-SNAPSHOT"]]
  :plugins [[lein-gorilla "0.3.6"]]
;;  :jvm-opts ["-XX:ThreadStackSize=8192k"]
  :jvm-opts ["-d64" "-Xmx12g"]
  )

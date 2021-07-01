(defproject chess-spock-example "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [link.szabo.mauricio/spock "0.1.0-SNAPSHOT"]]
  :main ^:skip-aot chess-spock-example.core
  :target-path "target/%s"

  :resource-paths ["resources/jpl.jar"]
  :jvm-opts [~(str "-Djava.library.path=resources/:" (System/getProperty "java.library.path"))]

  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})

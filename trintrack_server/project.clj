(defproject trintrack_server "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [io.pedestal/pedestal.service "0.5.9"]
                 [io.pedestal/pedestal.route "0.5.9"]
                 [io.pedestal/pedestal.jetty "0.5.9"]
                 [cheshire "5.11.0"]
                 [ring/ring-json "0.5.1"]
                 [clj-http "3.12.3"]]
  :repl-options {:init-ns trintrack-server.core})

(ns trintrack-server.test-client(:require [clj-http.client :as client]
                                          [cheshire.core :as json]))

(def base-url "http://localhost:9999")

(defn cadastrar-usuario []
  (let [dados {:nome "Fernando"
               :altura 1.75
               :peso 70
               :idade 30
               :sexo "M"}]
    (client/post (str base-url "/usuario")
                 {:headers {"Content-Type" "application/json"}
                  :body (json/generate-string dados)
                  :as :json})))

(defn cadastrar-alimento []
  (let [dados {:nome "Banana"
               :gramas 100
               :data "2025-05-29"}]
    (client/post (str base-url "/alimento")
                 {:headers {"Content-Type" "application/json"}
                  :body (json/generate-string dados)
                  :as :json})))

(defn cadastrar-treino []
  (let [dados {:nome "Corrida"
               :tempo 30
               :data "2025-05-29"}]
    (client/post (str base-url "/treino")
                 {:headers {"Content-Type" "application/json"}
                  :body (json/generate-string dados)
                  :as :json})))

(defn verificar-usuario []
  (client/get (str base-url "/verificar") {:as :json}))

;; Testes rápidos no REPL
(comment
  (cadastrar-usuario)
  (verificar-usuario)
  (cadastrar-alimento)
  (cadastrar-treino))

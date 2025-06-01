(ns trintrack-frontend.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def base-url "http://localhost:3000")

(defn ler-nome []
  (let [nome (read-line)]
    (if (str/blank? nome)
      (do (println "Nome não pode ser vazio.")
          (recur))
      nome)))

(defn formatar-data [texto]
  (if (re-matches #"\d{2}/\d{2}/\d{4}" texto)
    texto
    (do (println "Data inválida. Use o formato dd/mm/aaaa.")
        (recur (read-line)))))

(defn verificar-usuario-cadastrado []
  (let [res (client/get (str base-url "/usuario/existe") {:as :json})]
    (get-in res [:body :existe])))

(defn cadastro-usuario []
  (println "=== Cadastro do Usuário ===")

  (let [nome (do (println "Digite seu nome: ") (ler-nome))
        idade (do (println "Idade:") (Integer/parseInt (read-line)))
        altura (do (println "Altura (ex: 1.75):") (Double/parseDouble (read-line)))
        peso (do (println "Peso (kg):") (Double/parseDouble (read-line)))
        sexo (do (println "Sexo (M/F):") (read-line))
        usuario {:nome nome :idade idade :altura altura :peso peso :sexo sexo}]
    ;; Enviar para backend
    (client/post (str base-url "/usuario")
                   {:headers {"Content-Type" "application/json"}
                    :body (json/generate-string usuario)})
    (println "Usuário cadastrado com sucesso:" usuario)))

(defn cadastro-alimento []
  (println "=== Registrar Alimentação ===")
  (if (verificar-usuario-cadastrado)
    (let [nome-comida (do (println "Digite o que comeu: ") (ler-nome))
          quantidade (do (println "Quantidade (gramas):") (Integer/parseInt (read-line)))
          data (do (println "Data da refeição (dd/mm/aaaa):") (formatar-data (read-line)))
          alimento {:nome nome-comida :quantidade quantidade :data data}]
      (client/post (str base-url "/alimentacao")
                     {:headers {"Content-Type" "application/json"}
                      :body (json/generate-string alimento)})
      (println "Alimento registrado com sucesso: " alimento))
    (println "Nenhum usuário cadastrado. Cadastre o usuário primeiro.")))

(defn cadastro-treino []
  (println "=== Registrar Treino ===")
  (if (verificar-usuario-cadastrado)

    (let [nome-treino (do (println "Digite o que treinou: ") (ler-nome))
          tempo (do (println "Tempo (minutos):") (Integer/parseInt (read-line)))
          data (do (println "Data da refeição (dd/mm/aaaa):") (formatar-data (read-line)))
          treino {:nome nome-treino :tempo tempo :data data}]
      (client/post (str base-url "/exercicio")
                     {:headers {"Content-Type" "application/json"}
                      :body (json/generate-string treino)})
      (println "Treino registrado com sucesso: " treino))

    (println "Nenhum usuário cadastrado. Cadastre o usuário primeiro.")))

(defn trin-track []
  ;(println "Usuário cadastrado? " (verificar-usuario-cadastrado))
  (println "\nBem-vindo ao TRINTRACK - Seu App de Controle Corporal")
  (println "1 - Cadastro do usuário")
  (println "2 - Registrar alimentação")
  (println "3 - Registrar treino")
  ;(println "4 - Consultar saldo de perca do dia")
  ;(println "5 - Consultar saldo de ganho do dia")
  (println "6 - Encerrar")
  (print "Escolha uma opção: ") (flush)
  (let [opcao (read-line)]
    (cond
      (= opcao "1") (do (cadastro-usuario) (trin-track))
      (= opcao "2") (do (cadastro-alimento) (trin-track))
      (= opcao "3") (do (cadastro-treino) (trin-track))
      ;(= opcao "4") (trin-track)
      ;(= opcao "5") (trin-track)
      (= opcao "6") (println "Encerrando o sistema.")
      :else         (do (println "Opção inválida.") (trin-track)))))

(trin-track)
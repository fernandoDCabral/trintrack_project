(ns trintrack-frontend.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def base-url "http://localhost:3000")

;Verificações ⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_
(defn ler-nome []
  (let [nome (read-line)]
    (if (str/blank? nome)
      (do (println "Nome não pode ser vazio.")
          (recur))
      nome)))

(defn formatar-data []
  (let [data (read-line)]
   (if (re-matches #"\d{4}/\d{2}/\d{2}" data)
    data
    (do (println "Data inválida. Use o formato aaaa/mm/dd.")
        (recur)))))

(defn verificar-usuario-cadastrado []
  (let [res (client/get (str base-url "/usuario/existe") {:as :json})]
    (get-in res [:body :existe])))

(defn mostrar-dados-usuario []
  (let [res (client/get (str base-url "/usuario/dados") {:as :json})
        usuario (:body res)]
    (println "\nUsuário já cadastrado:")
    (println (str "Nome: " (:nome usuario)))
    (println (str "Idade: " (:idade usuario)))
    (println (str "Altura: " (:altura usuario)))
    (println (str "Peso: " (:peso usuario)))
    (println (str "Sexo: " (:sexo usuario)))
    (println "------------------------------------")))

;Verificações ⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_
;usado para treino/ alimentos ⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_

(defn imprimir-opcoes [opcoes idx]
  (let [idx (or idx 1)]
    (when (seq opcoes)
      (println (str idx " - " (first opcoes)))
      (recur (rest opcoes) (inc idx)))))

(defn calcular-calorias [calorias_g_p gramas_tempo variavel]
(let [resultado (* (double calorias_g_p) (/ gramas_tempo variavel))]
  (Double/parseDouble (format "%.2f" resultado))
  ))

;usado para treino/ alimentos ⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_
;usado para extrato ⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_

(defn imprimir-extrato-aux [registros i]
  (when (< i (count registros))
    (let [r (nth registros i)]
      (case (:tipo r)
        :alimento  (println (str "Data: " (:data r)
                                 ", Prato: " (:prato r)
                                 ", Calorias: " (:calorias r) " kcal"))
        :exercicio (println (str "Data: " (:data r)
                                 ", Atividade: " (:nome r)
                                 ", Calorias/h: " (:calorias-por-hora r)
                                 ", Tempo: " (:tempo r) "min"))))
    (recur registros (inc i))))

(defn consultar-extrato-por-periodo []
  (if (verificar-usuario-cadastrado)
    (do
      (println "=== Consultar Extrato por Período ===")
      (print "Digite a data de início (aaaa/mm/dd): ")
      (flush)
      (let [inicio (formatar-data)
            _ (print "Digite a data de fim (aaaa/mm/dd): ")
            _ (flush)
            fim  (formatar-data)
            resposta (client/get (str base-url "/extrato")
                                 {:query-params {"inicio" inicio "fim" fim}
                                  :as :json})
            corpo (:body resposta)
            alimentos (:alimentos corpo)
            exercicios (:exercicios corpo)

            todos (sort-by :data
                           (concat
                             (map #(assoc % :tipo :alimento) alimentos)
                             (map #(assoc % :tipo :exercicio) exercicios)))]
        (println "\n--- Extrato Consolidado ---")
        (if (seq todos)
          (imprimir-extrato-aux todos 0)
          (println "Nenhum dado encontrado nesse período."))))
    (println "Nenhum usuário cadastrado. Cadastre o usuário primeiro.")))

;usado para extrato ⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_
;usado para saldo ⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_
(defn imprimir-saldo-por-data [saldos idx]
  (let [saldos-lista (vec (sort-by key saldos))]
    (when (< idx (count saldos-lista))
      (let [[data saldo] (nth saldos-lista idx)]
        (println (str "Data: " data ", Saldo calórico: " saldo " kcal")))
      (recur saldos-lista (inc idx)))))

(defn consultar-saldo []
  (if (verificar-usuario-cadastrado)
    (do
      (println "=== Consultar Saldo por Período ===")
      (print "Digite a data de início (aaaa/mm/dd): ")
      (flush)
      (let [inicio (formatar-data)
            _ (print "Digite a data de fim (aaaa/mm/dd): ")
            _ (flush)
            fim (formatar-data)
            resposta (client/get (str base-url "/saldo")
                                 {:query-params {"inicio" inicio "fim" fim}
                                  :as :json})
            corpo (:body resposta)
            saldo (:saldo corpo)]
        (println "\n--- Saldo Calórico do periodo ---")
        (if (seq saldo)
          (imprimir-saldo-por-data (vec saldo) 0)
          (println "Nenhum dado de saldo encontrado nesse período."))))
    (println "Nenhum usuário cadastrado. Cadastre o usuário primeiro.")))

;usado para saldo ⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_
;usado para cadastros ⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_⍗_

(defn cadastro-usuario []
  (println "=== Cadastro do Usuário ===")
  (if (verificar-usuario-cadastrado)
    (mostrar-dados-usuario)
    (do
      (let [nome (do (println "Digite seu nome: ") (ler-nome))
            idade (do (println "Idade:") (Integer/parseInt (read-line)))
            altura (do (println "Altura (ex: 1.75):") (Double/parseDouble (read-line)))
            peso (do (println "Peso (kg):") (Double/parseDouble (read-line)))
            sexo (do (println "Sexo (M/F):") (read-line))
            usuario {:nome nome :idade idade :altura altura :peso peso :sexo sexo}]
        (client/post (str base-url "/usuario")
                     {:headers {"Content-Type" "application/json"}
                      :body (json/generate-string usuario)})
        (println "Usuário cadastrado com sucesso:")))))

(defn cadastro-alimento []
  (println "=== Registrar Alimentação ===")
  (if (verificar-usuario-cadastrado)
    (do
      (println "Digite o que comeu:")
      (let [entrada (ler-nome)
            resposta (client/get (str base-url "/alimentos/sugestoes")
                                 {:query-params {"filtro" entrada} :as :json})
            opcoes (:body resposta)]
        (if (empty? opcoes)
          (println "Nenhum alimento encontrado com esse nome.")
          (do
            (println (str "Alimentos relacionados a '" entrada "':"))
            (imprimir-opcoes opcoes 1)
            (print "Escolha o número do alimento: ")
            (flush)

            (let [indice (dec (Integer/parseInt (read-line)))
                  alimento-escolhido  (nth opcoes indice)
                  nome (:prato alimento-escolhido)
                  quantidade (do (println "Quantidade (gramas):") (Integer/parseInt (read-line)))
                  calorias_alimento (:calorias alimento-escolhido)
                  calorias (calcular-calorias calorias_alimento quantidade 100)
                  data (do (println "Data da refeição (aaaa/mm/dd):") (formatar-data))
                  alimento {:prato nome :calorias calorias :quantidade quantidade :data data }]
              (client/post (str base-url "/alimentacao")
                           {:headers {"Content-Type" "application/json"}
                            :body (json/generate-string alimento)})
              (println "Alimento registrado com sucesso:"))))))
    (println "Nenhum usuário cadastrado. Cadastre o usuário primeiro.")))

(defn cadastro-treino []
  (println "=== Registrar Treino ===")
  (if (verificar-usuario-cadastrado)
    (do
      (println "Digite o que treinou:")
      (let [entrada (ler-nome)
            resposta (client/get (str base-url "/treinos/sugestoes")
                                 {:query-params {"filtro" entrada} :as :json})
            opcoes (:body resposta)]
        (if (empty? opcoes)
          (println "Nenhum treino encontrado com esse nome.")
          (do
            (println (str "treinos relacionados a '" entrada "':"))
            (imprimir-opcoes opcoes 1)
            (print "Escolha o número do treino: ")
            (flush)
            (let [indice (dec (Integer/parseInt (read-line)))
                  treino-escolhido (nth opcoes indice)
                  nome (:nome treino-escolhido)
                  tempo (do (println "Tempo (minutos):") (Integer/parseInt (read-line)))
                  calorias-hora (:calorias-por-hora treino-escolhido)
                  calorias  (calcular-calorias calorias-hora tempo 60)
                  data (do (println "Data da treino (aaaa/mm/dd):") (formatar-data))
                  treino {:nome nome
                          :calorias-por-hora calorias
                          :tempo tempo
                          :data data}]
              (client/post (str base-url "/exercicio")
                           {:headers {"Content-Type" "application/json"}
                            :body (json/generate-string treino)})
              (println "Treino registrado com sucesso:"))
            ))))
    (println "Nenhum usuário cadastrado. Cadastre o usuário primeiro.")))

;usado para cadastros ⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_⍐_


(defn menu_opcoes []
  (println "1 - Cadastro do usuário")
  (println "2 - Registrar alimentação")
  (println "3 - Registrar treino")
  (println "4 - Consultar extrato")
  (println "5 - Consultar saldo")
  (println "6 - Encerrar")
  (print "Escolha uma opção: ")
  (flush)
  )

(defn submenu_opcoes [opcao]
  (cond
    (= opcao "1") (cadastro-usuario)
    (= opcao "2") (cadastro-alimento)
    (= opcao "3") (cadastro-treino)
    (= opcao "4") (consultar-extrato-por-periodo)
    (= opcao "5") (consultar-saldo)
    :else (println "Opção inválida.")
    )
  )

(defn trin-track []
  (println "Bem-vindo ao TRINTRACK - Seu App de Controle Corporal")
  (menu_opcoes)
  (let [opcao (read-line)]
    (if (= opcao "6")
      (println "Encerrando o sistema.")
      (do
        (submenu_opcoes opcao)
        (recur)
        )
      )
    )
  )

(trin-track)

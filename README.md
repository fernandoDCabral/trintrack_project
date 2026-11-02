Trintrack

Aplicação para acompanhamento de usuários, alimentação e exercícios físicos, organizada em duas partes: um frontend e um servidor (API) que responde na porta 3000.

Projeto em desenvolvimento. [PREENCHER: uma frase sobre o objetivo, por exemplo "ajudar o usuário a registrar o que come e como treina"]

Estrutura do repositório
trintrack_project/
├── trintrack-frontend/     # interface da aplicação
├── trintrack_server/       # servidor / API (http://localhost:3000)
└── Comandos para testar.txt
Tecnologias
Linguagem: Clojure

Pré-requisitos
JDK instalado
Leiningen ou Clojure CLI, conforme o servidor tenha project.clj ou deps.edn
Node.js, se o frontend usar shadow-cljs ou npm

Como rodar
1. Clonar o repositório
bash
git clone https://github.com/fernandoDCabral/trintrack_project.git
cd trintrack_project
2. Servidor
bash
cd trintrack_server
Comando para iniciar o servidor, ex. lein run (Leiningen) ou clojure -M:run (deps.edn)

O servidor fica disponível em http://localhost:3000.

3. Frontend
bash
cd trintrack-frontend

Rotas do servidor
Com o servidor rodando, as rotas abaixo podem ser testadas no navegador ou pelo terminal com curl.
Rota	Descrição
GET /usuario/dados	Dados dos usuários
GET /usuario/existe	Verifica se o usuário existe
GET /alimentacao/dados	Dados de alimentação
GET /alimentacao/existe	Verifica se o registro de alimentação existe
GET /exercicio/dados	Dados de exercícios
GET /exercicio/existe	Verifica se o registro de exercício existe

Exemplo:
bash
curl http://localhost:3000/usuario/dados

Próximos passos
Ainda falta faze "cadastro de exercícios pelo frontend"

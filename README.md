## Plataforma de Coworking

### Descrição do Projeto
Este sistema permite reservar, gerenciar e cancelar reservas de estações de trabalho ou salas dentro de um ambiente compartilhado. Ele possui dois tipo de usuários, o Administrador e usuários comuns do sistema.
O objetivo é facilitar o controle de uso dos espaços, evitando conflitos de horário e permitindo que tanto administradores quanto usuários comuns realizem operações de forma simples pelo terminal.

---

### Funcionalidades e Regras de Negócio

#### **Administrador**
**O Administrador pode realizar as seguintes funções**

- **Ver todas as estações** — Lista os espaços cadastrados.  
- **Filtrar por capacidade** — Permite ver estações para 1, 2 ou mais pessoas.  
- **Ver estações disponíveis** — Mostra espaços livres em uma data e horário.  
- **Cadastrar nova estação** — Adiciona uma nova estação informando andar, número e capacidade.  
- **Alterar estação** — Atualiza andar e número de uma estação existente.  
- **Ver e alterar usuários** — Lista todos os usuários e permite renomeá-los.  
- **Ver e cancelar reservas** — Pode ver as reservas e, caso queira, através do ID da reserva, pode excluir qualquer reserva do sistema.  
- **Salvar dados** — Grava manualmente os dados nos arquivos `.dat`.

#### **Usuário**
**O Usuário pode realizar as seguintes funções**

- **Cadastro automático** — Se o nome informado não existe, o sistema cadastra um novo usuário.  
- **Fazer reserva** — Informa data, horário e quantidade de pessoas. O sistema escolhe automaticamente o melhor espaço disponível.  
- **Cancelar reserva** — Cancela uma reserva existente pelo ID.  
- **Ver minhas reservas** — Exibe todas as reservas realizadas pelo usuário logado.  
- **Ver estações por capacidade** — Lista todas as estações ou filtra por número de pessoas.

#### **Regras de Negócio**
- Uma reserva **não pode ser feita se houver conflito de horário**.  
- Cada reserva está associada a um **usuário** e um **espaço**.  
- O sistema **lê os dados ao iniciar** e **salva automaticamente** após cada operação.  
- A senha do administrador é `1234`.  

---

### Tipos Algébricos Principais

```haskell
data Espaco = EstacaoTrabalho Id Int Int Int
data Usuario = Usuario Id String
data Reserva = Reserva Id Usuario Espaco Data Horario Horario
type Banco = ([Espaco], [Usuario], [Reserva])
```

- **Espaco** representa uma estação de trabalho, com andar, número e capacidade.  
- **Usuario** representa um usuário do sistema.  
- **Reserva** liga um usuário a um espaço em uma data e horário.  
- **Banco** mantém todas as listas de dados carregadas em memória.  

---

### Reuso de ideias e/ou códigos existentes

O desenvolvimento deste projeto se baseou em exemplos de projetos disponibilizados pelo professor, utilizados apenas como referência estrutural e de boas práticas.

---

### Estrutura do Projeto

```
├── Core.hs      # Lógica central: tipos e funções do sistema
├── FileIO.hs    # Entrada e saída de dados (salvar/carregar)
├── Main.hs      # Interface principal de menus
└── README.md
```

---

### Como Executar o Projeto

#### Instalar o compilador Haskell
O comílador pode ser baixado através do site (https://www.haskell.org/ghcup/)

#### 2️⃣ Compilar e executar
No terminal, dentro da pasta do projeto:
```bash
ghc Main.hs -o sistema
./sistema
```
ou, para rodar direto:
```bash
runghc Main.hs
```

module Main where

import Core
import FileIO
import System.IO
import Data.List
import Control.Exception (catch, IOException)

-- Função Principal
main :: IO ()
main = do
  putStrLn " Sistema de Reservas - Iniciando..."
  banco <- carregarBanco 
  menuPrincipal banco

-- Menu principal
menuPrincipal :: Banco -> IO ()
menuPrincipal banco = do
  putStrLn "\n--- Bem-vindo ao Sistema de Reservas ---"
  putStrLn "1) Administrador"
  putStrLn "2) Usuário"
  putStrLn "3) Sair"
  putStr "Escolha: "
  hFlush stdout
  op <- getLine
  case op of
    "1" -> checaAdm banco
    "2" -> entradaUsuario banco
    "3" -> putStrLn "Encerrando..."
    _   -> putStrLn "Opção inválida." >> menuPrincipal banco

-- Confere se a senha digitada é a do administrador 
checaAdm :: Banco -> IO ()
checaAdm banco = do
  putStrLn "Digite a senha do administrador: "
  hFlush stdout
  senha <- getLine
  if senha == "1234"
    then menuAdministrador banco
    else putStrLn "Senha incorreta." >> menuPrincipal banco

-- Entrada do usuário
entradaUsuario :: Banco -> IO ()
entradaUsuario banco@(espacos, usuarios, reservas) = do
  putStrLn "Digite seu nome: "
  hFlush stdout
  nomeUsuario <- getLine
  
  case buscarUsuarioPorNome nomeUsuario usuarios of
    Just (Usuario idU nomeCadastrado) -> do
      putStrLn $ "Olá, " ++ nomeCadastrado ++ "!"
      menuUsuario banco idU
    Nothing -> do
      let novoIdU = novoId (map getIdUsuario usuarios)
          novoUsuario = Usuario novoIdU nomeUsuario
          novoBanco = (espacos, usuarios++[novoUsuario], reservas)
      
      putStrLn "Usuários cadastrados atualmente:"
      mapM_ (\(Usuario i n) -> putStrLn (show i ++ " - " ++ n)) (usuarios++[novoUsuario])
      putStrLn $ "Seja bem vindo, " ++ nomeUsuario ++ "!"
      
      salvarBanco novoBanco
      menuUsuario novoBanco novoIdU

-- Menu do ADM
menuAdministrador :: Banco -> IO ()
menuAdministrador banco@(espacos, usuarios, reservas) = do
  putStrLn "\n--- Menu do Administrador ---"
  putStrLn "1) Ver todas as estações"
  putStrLn "2) Ver estações por quantidade de pessoas"
  putStrLn "3) Ver estações disponíveis por data/horário"
  putStrLn "4) Cadastrar nova estação"
  putStrLn "5) Alterar estação"
  putStrLn "6) Ver usuários cadastrados"
  putStrLn "7) Alterar usuário"
  putStrLn "8) Ver todas as reservas"
  putStrLn "9) Salvar dados"
  putStrLn "0) Voltar"
  putStr "Escolha: "
  hFlush stdout
  op <- getLine
  case op of
    "1" -> mapM_ (putStrLn . showEsp) espacos >> menuAdministrador banco
    "2" -> do
      putStrLn "Quantidade de pessoas (1,2 ou 3-5, 0 para todas): "
      q <- readLn
      let disp = if q==0 then espacos else filter (\e -> capacidade e == q || (q>=3 && q<=5 && capacidade e>=3 && capacidade e<=5)) espacos
      mapM_ (putStrLn . showEsp) disp
      menuAdministrador banco
    "3" -> do
      putStrLn "Data (DD-MM-AAAA): "
      dataReserva <- getLine
      putStrLn "Horário de entrada (HH:MM): "
      entrada <- getLine
      putStrLn "Horário de saída (HH:MM) ou 0 para dia inteiro: "
      saidaInput <- getLine
      let saida = if saidaInput == "0" then "23:59" else saidaInput
      let disp = espacosDisponiveis dataReserva entrada saida banco
      mapM_ (putStrLn . showEsp) disp
      menuAdministrador banco
    "4" -> do
      putStrLn "Andar da nova estação: "; andar <- readLn
      putStrLn "Número da nova estação: "; num <- readLn
      putStrLn "Capacidade (1,2 ou 3-5): "; cap <- readLn
      let novoBanco = cadastrarEstacao andar num cap banco
      salvarBanco novoBanco
      putStrLn "Nova estação cadastrada."
      menuAdministrador novoBanco
    "5" -> do
      putStrLn "ID da estação a alterar: "; idE <- readLn
      putStrLn "Novo andar: "; novoAndar <- readLn
      putStrLn "Novo número: "; novoNum <- readLn
      putStrLn "Nova capacidade (digite 0 para DESATIVAR): "; novaCap <- readLn
      case alterarEstacao idE novoAndar novoNum novaCap banco of
        Right b -> do
          salvarBanco b
          putStrLn "Alteração feita."
          menuAdministrador b
        Left err -> putStrLn err >> menuAdministrador banco
    "6" -> mapM_ (\(Usuario i n) -> putStrLn (show i ++ " - " ++ n)) usuarios >> menuAdministrador banco
    "7" -> do
      putStrLn "ID do usuário: "; idU <- readLn
      putStrLn "Novo nome: "; novoNome <- getLine
      case alterarUsuario idU novoNome banco of
        Right b -> do
          salvarBanco b
          putStrLn "Alteração feita."
          menuAdministrador b
        Left err -> putStrLn err >> menuAdministrador banco
    "8" -> mapM_ print reservas >> menuAdministradorReservas banco
    "9" -> salvarBanco banco >> menuAdministrador banco
    "0" -> menuPrincipal banco
    _   -> putStrLn "Opção inválida." >> menuAdministrador banco

-- Interface para o menu de cancelar reservas adm
menuAdministradorReservas :: Banco -> IO ()
menuAdministradorReservas banco = do
  putStrLn "1) Cancelar reservas"
  putStrLn "2) Voltar"
  op <- getLine
  case op of
   "1" -> do cancelarReservaAdministrador banco
   "2" -> menuAdministrador banco

-- Menu do usuário
menuUsuario :: Banco -> Id -> IO ()
menuUsuario banco idU = do
  putStrLn "\n--- Menu do Usuário ---"
  putStrLn "1) Fazer reserva"
  putStrLn "2) Cancelar reserva"
  putStrLn "3) Ver estações por quantidade de pessoas"
  putStrLn "4) Ver minhas reservas"
  putStrLn "5) Voltar"
  putStr "Escolha: "
  hFlush stdout
  op <- getLine
  case op of
    "1" -> fazerReservaUI banco idU
    "2" -> cancelarReservaUI banco idU
    "3" -> verDisponiveisPorQtdUI banco idU
    "4" -> verMinhasReservasUI banco idU
    "5" -> menuPrincipal banco
    _   -> putStrLn "Opção inválida." >> menuUsuario banco idU

-- Interface da reserva
fazerReservaUI :: Banco -> Id -> IO ()
fazerReservaUI banco idU = do
  putStrLn "Quantidade de pessoas (1,2 ou 3-5): "
  qnt <- readLn
  putStrLn "Data da reserva (DD-MM-AAAA): "
  dataReserva <- getLine
  putStrLn "Horário de entrada (HH:MM): "
  entrada <- getLine
  putStrLn "Horário de saída (HH:MM) ou 0 para dia inteiro: "
  saidaInput <- getLine
  let saida = if saidaInput == "0" then "23:59" else saidaInput
  
  case alocacaoAutomatica qnt dataReserva entrada saida banco of
    Nothing -> do
      putStrLn "\n Nenhuma estação disponível para essa quantidade e horário."
      menuUsuario banco idU
    Just espacoEscolhido -> do
      putStrLn "\n Sistema encontrou automaticamente a melhor sala:"
      putStrLn $ "  -> " ++ showEsp espacoEscolhido
      
      case fazerReserva idU (getEspacoId espacoEscolhido) dataReserva entrada saida banco of
        Right b -> do
          salvarBanco b
          putStrLn "\n Reserva realizada com sucesso!"
          menuUsuario b idU
        Left err -> do
          putStrLn $ "\n Erro: " ++ err
          menuUsuario banco idU

-- Interface de cancelar reserva
cancelarReservaUI :: Banco -> Id -> IO ()
cancelarReservaUI banco@(_, _, reservas) idU = do
  putStrLn "ID da reserva: "
  idR <- readLn
  let existe = find (\(Reserva i (Usuario uid _) _ _ _ _) -> i==idR && uid==idU) reservas
  case existe of
    Just _ -> do
      let novoBanco = cancelarReserva idR banco
      salvarBanco novoBanco
      putStrLn "Reserva cancelada."
      menuUsuario novoBanco idU
    Nothing -> putStrLn "Reserva não encontrada." >> menuUsuario banco idU

-- Interface para cancelar reserva Adm
cancelarReservaAdministrador :: Banco -> IO ()
cancelarReservaAdministrador banco@(_, _, reservas) = do
  putStrLn "ID da reserva: "
  idR <- readLn
  let existe = find (\(Reserva i _ _ _ _ _) -> i == idR) reservas
  case existe of
    Just _ -> do
      let novoBanco = cancelarReserva idR banco
      salvarBanco novoBanco
      putStrLn "Reserva cancelada."
      menuAdministrador novoBanco
    Nothing -> putStrLn "Reserva não encontrada." >> menuAdministrador banco

-- Interface de disponibilidade
verDisponiveisPorQtdUI :: Banco -> Id -> IO ()
verDisponiveisPorQtdUI banco idU = do
  putStrLn "Quantidade de pessoas (1,2 ou 3-5, 0 para todas): "
  q <- readLn
  let estDisp = if q==0 then fst3 banco else filter (\e -> capacidade e == q || (q>=3 && q<=5 && capacidade e>=3 && capacidade e<=5)) (fst3 banco)
  mapM_ (putStrLn . showEsp) estDisp
  menuUsuario banco idU

-- Interface reserva
verMinhasReservasUI :: Banco -> Id -> IO ()
verMinhasReservasUI banco@(_, _, reservas) idU = do
  let minhas = filter (\(Reserva _ (Usuario uid _) _ _ _ _) -> uid==idU) reservas
  if null minhas then putStrLn "Nenhuma reserva." else mapM_ print minhas
  menuUsuario banco idU
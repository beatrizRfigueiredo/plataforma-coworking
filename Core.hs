module Core (
    Id, Data, Horario,
    Espaco(..), Usuario(..), Reserva(..),
    Banco,
    
    gerarEstacoes,
    capacidade,
    temConflitoHorario,
    espacoDisponivel,
    espacosDisponiveis,
    alocacaoAutomatica,
    fazerReserva,
    cancelarReserva,
    cadastrarEstacao,
    alterarEstacao,
    alterarUsuario,
    getEspacoId,
    getReservaId,
    getIdUsuario,
    novoId,
    buscarUsuarioPorNome,
    showEsp,
    fst3
) where

import Data.Char (toLower)
import Data.List
import System.IO

type Id = Int
type Data = String 
type Horario = String

data Espaco = EstacaoTrabalho Id Int Int Int deriving (Eq)
data Usuario = Usuario Id String deriving (Eq, Show)
data Reserva = Reserva Id Usuario Espaco Data Horario Horario deriving (Eq)

-- Mostra os dados organizados
instance Show Espaco where
  show (EstacaoTrabalho _ andar num capacidade) = 
    "E." ++ show andar ++ "." ++ show num ++ " (" ++ show capacidade ++ " pessoa" ++ plural capacidade ++ ")"
    where plural 1 = ""; plural _ = "s"

instance Show Reserva where
  show (Reserva i (Usuario _ nome) espaco dataHora entrada saida) =
    "Reserva #" ++ show i ++ " - " ++ nome ++ " - " ++ show espaco ++ " - " ++ 
    dataHora ++ " " ++ entrada ++ "-" ++ saida

-- Banco de dados
type Banco = ([Espaco], [Usuario], [Reserva])

getEspacoId :: Espaco -> Id
getEspacoId (EstacaoTrabalho i _ _ _) = i

getReservaId :: Reserva -> Id
getReservaId (Reserva i _ _ _ _ _) = i

getIdUsuario :: Usuario -> Id
getIdUsuario (Usuario i _) = i

novoId :: [Id] -> Id
novoId [] = 1
novoId xs = maximum xs + 1

capacidade :: Espaco -> Int
capacidade (EstacaoTrabalho _ _ _ c) = c

--Verificações
temConflitoHorario :: Horario -> Horario -> Horario -> Horario -> Bool
temConflitoHorario entrada1 saida1 entrada2 saida2 =
  not (saida1 <= entrada2 || saida2 <= entrada1)

espacoDisponivel :: Espaco -> Data -> Horario -> Horario -> Banco -> Bool
espacoDisponivel espaco dataReserva entrada saida (_, _, reservas) =
  not (any conflito reservas)
  where
    conflito (Reserva _ _ e d ent sd) =
      e == espaco && d == dataReserva && temConflitoHorario entrada saida ent sd

espacosDisponiveis :: Data -> Horario -> Horario -> Banco -> [Espaco]
espacosDisponiveis dataReserva entrada saida banco@(espacos, _, _) =
  filter (\e -> espacoDisponivel e dataReserva entrada saida banco) espacos

-- Seleciona automaticamente um espaço disponível que tenha capacidade
alocacaoAutomatica :: Int -> Data -> Horario -> Horario -> Banco -> Maybe Espaco
alocacaoAutomatica qtdPessoas dataReserva entrada saida banco =
  let estDisp = filter (\e -> capacidade e >= qtdPessoas) 
                      (espacosDisponiveis dataReserva entrada saida banco)
      estOrdenadas = sortBy (\e1 e2 -> 
        case compare (capacidade e1) (capacidade e2) of
          EQ -> compare (getEspacoId e1) (getEspacoId e2)
          other -> other
        ) estDisp
  in if null estOrdenadas then Nothing else Just (head estOrdenadas)

-- Gerar estações
gerarEstacoes :: [Espaco]
gerarEstacoes = [EstacaoTrabalho ((a - 1) * 10 + e) a e cap | a <- [1,2], (e, cap) <- zip [1..10] (cycle [1,2,5])]

-- Operações da reserva
fazerReserva :: Id -> Id -> Data -> Horario -> Horario -> Banco -> Either String Banco
fazerReserva idU idEsp dataReserva entrada saida banco@(espacos, usuarios, reservas) =
  case (find (\u -> getIdUsuario u == idU) usuarios, find (\e -> getEspacoId e == idEsp) espacos) of
    (Just user, Just espaco) ->
      if espacoDisponivel espaco dataReserva entrada saida banco
        then Right (espacos, usuarios, reservas ++ [Reserva (novoId (map getReservaId reservas)) user espaco dataReserva entrada saida])
        else Left "Espaço já está reservado neste horário."
    _ -> Left "Usuário ou espaço não encontrado."

cancelarReserva :: Id -> Banco -> Banco
cancelarReserva idR (espacos, usuarios, reservas) =
  (espacos, usuarios, filter (\(Reserva i _ _ _ _ _) -> i /= idR) reservas)

-- Cadastra e altera estação 
cadastrarEstacao :: Int -> Int -> Int -> Banco -> Banco
cadastrarEstacao andar num cap (espacos, usuarios, reservas) =
  let novoIdEsp = novoId (map getEspacoId espacos)
      novaEstacao = EstacaoTrabalho novoIdEsp andar num cap
  in (espacos ++ [novaEstacao], usuarios, reservas)

alterarEstacao :: Id -> Int -> Int -> Int -> Banco -> Either String Banco
alterarEstacao idE novoAndar novoNum novoCap (espacos, usuarios, reservas) =
  if any (\e -> getEspacoId e == idE) espacos
    then
      let espacosAtualizados = map update espacos
          update e@(EstacaoTrabalho i _ _ _) = 
            if i==idE 
            then EstacaoTrabalho i novoAndar novoNum novoCap 
            else e
      in Right (espacosAtualizados, usuarios, reservas)
    else Left "Estação não encontrada."

-- Usuários
alterarUsuario :: Id -> String -> Banco -> Either String Banco
alterarUsuario idU novoNome (espacos, usuarios, reservas) =
  if any (\(Usuario i _) -> i == idU) usuarios
    then
      let usuariosAtualizados = map update usuarios
          update u@(Usuario i _) = if i==idU then Usuario i novoNome else u
      in Right (espacos, usuariosAtualizados, reservas)
    else Left "Usuário não encontrado."

buscarUsuarioPorNome :: String -> [Usuario] -> Maybe Usuario
buscarUsuarioPorNome nome usuarios = find (\(Usuario _ n) -> map toLower n == map toLower nome) usuarios

showEsp :: Espaco -> String
showEsp e = show (getEspacoId e) ++ " - " ++ show e

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
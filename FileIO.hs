module FileIO (
    salvarBanco,
    carregarBanco
) where

import System.Directory (doesFileExist)
import System.IO
import Core 

-- Arquivos
arquivoEspacos :: FilePath
arquivoEspacos = "espacos.dat"

arquivoUsuarios :: FilePath
arquivoUsuarios = "usuarios.dat"

arquivoReservas :: FilePath
arquivoReservas = "reservas.dat"

-- Converte uma estação em string para salvar em arquivo
espacoParaString :: Espaco -> String
espacoParaString (EstacaoTrabalho i andar num cap) =
  show i ++ "|" ++ show andar ++ "|" ++ show num ++ "|" ++ show cap
stringParaEspaco :: String -> Maybe Espaco
stringParaEspaco linha =
  case splitOn '|' linha of
    [i, andar, num, cap] -> Just (EstacaoTrabalho (read i) (read andar) (read num) (read cap))
    _ -> Nothing

-- Converte o usuário em string para salvar
usuarioParaString :: Usuario -> String
usuarioParaString (Usuario i nome) = show i ++ "|" ++ nome
stringParaUsuario :: String -> Maybe Usuario
stringParaUsuario linha =
  case splitOn '|' linha of
    [i, nome] -> Just (Usuario (read i) nome)
    _ -> Nothing

-- Converte uma reserva em string para salvar no arquivo
reservaParaString :: Reserva -> String
reservaParaString (Reserva idR (Usuario idU nomeU) (EstacaoTrabalho idE andar num cap) dataReserva entrada saida) =
  show idR ++ "|" ++ show idU ++ "|" ++ nomeU ++ "|" ++ 
  show idE ++ "|" ++ show andar ++ "|" ++ show num ++ "|" ++ show cap ++ "|" ++ 
  dataReserva ++ "|" ++ entrada ++ "|" ++ saida
stringParaReserva :: String -> Maybe Reserva
stringParaReserva linha =
  case splitOn '|' linha of
    [idR, idU, nomeU, idE, andar, num, cap, dataReserva, entrada, saida] -> 
      Just (Reserva (read idR) 
                    (Usuario (read idU) nomeU) 
                    (EstacaoTrabalho (read idE) (read andar) (read num) (read cap)) 
                    dataReserva entrada saida)
    _ -> Nothing

-- Divide uma string em partes com |
splitOn :: Char -> String -> [String]
splitOn delim str = case break (== delim) str of
  (a, []) -> [a]
  (a, _:b) -> a : splitOn delim b

-- Salvar os dados
salvarBanco :: Banco -> IO ()
salvarBanco (espacos, usuarios, reservas) = do
  writeFile arquivoEspacos (unlines $ map espacoParaString espacos)  
  writeFile arquivoUsuarios (unlines $ map usuarioParaString usuarios)  
  writeFile arquivoReservas (unlines $ map reservaParaString reservas)  
  putStrLn " Dados salvos com sucesso!"

-- Carregar os dados
carregarBanco :: IO Banco
carregarBanco = do

  -- Verifica se os arquivos existem
  existeEspacos <- doesFileExist arquivoEspacos
  existeUsuarios <- doesFileExist arquivoUsuarios
  existeReservas <- doesFileExist arquivoReservas
  
  -- Carregar espaços
  espacos <- if existeEspacos
    then do
      conteudo <- readFile arquivoEspacos
      let linhas = lines conteudo
      return [e | Just e <- map stringParaEspaco linhas]
    else return gerarEstacoes 
  
  -- Carregar usuários
  usuarios <- if existeUsuarios
    then do
      conteudo <- readFile arquivoUsuarios
      let linhas = lines conteudo
      return [u | Just u <- map stringParaUsuario linhas]
    else return [Usuario 1 "Alice", Usuario 2 "Bob", Usuario 3 "Carol"] 
  
  -- Carregar reservas
  reservas <- if existeReservas
    then do
      conteudo <- readFile arquivoReservas
      let linhas = lines conteudo
      return [r | Just r <- map stringParaReserva linhas]
    else return []
  
  if existeEspacos || existeUsuarios || existeReservas
    then putStrLn "Dados carregados dos arquivos!"
    else putStrLn "Iniciando com dados de primeira execução"
  
  return (espacos, usuarios, reservas)
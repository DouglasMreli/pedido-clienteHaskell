import Data.Char ()
import System.IO
import Text.ParserCombinators.Parsec ()
import GHC.IO.Handle.Text ()


type Cliente = (String,String,Integer)

clienteToString :: Cliente -> String
clienteToString (nome,rua,num) = nome ++ " " ++ rua ++ " " ++ show num

fulano :: Cliente
fulano = ("Fulano", "Rua a", 9999)
ciclano :: Cliente
ciclano = ("Ciclano", "Rua b", 8888)

clientes :: [Cliente]
clientes = [fulano, ciclano]

type Date = (Integer, Integer, Integer)

date :: Date
date = (14,9,2021)

dateToString :: Date -> String
dateToString (dia,mes,ano) = show dia ++ " " ++ show mes ++ " " ++ show ano

type Pedido = (Integer, Double, Cliente, Date)
pedidos :: [Pedido]
pedidos = [(123, 20.0, fulano, date)]

pedidoToString :: Pedido -> String
pedidoToString (num,preco,cliente,date) = show num ++ " " ++ show  preco ++ " " ++ clienteToString cliente ++ " " ++ dateToString date

type PedidoExpresso = (Integer, Double, Cliente, Date, Date)

dateAtrasado :: Date
dateAtrasado = (15,9,2021)
pedidoExpAtrasado :: PedidoExpresso
pedidoExpAtrasado = (555, 20.0*1.2, ciclano, date, dateAtrasado)

pedidosExp = [pedidoExpAtrasado]

entregueNoPrazo :: PedidoExpresso -> Bool
entregueNoPrazo (_, _, _, date, dateEntrega) = date == dateEntrega

foiEntregue :: Bool -> String
foiEntregue x
            | x = "Foi entregue no prazo"
            | otherwise = "Nao foi entregue no prazo"


escreverCliente :: [Cliente] -> IO ()
escreverCliente clientes = do
            arq <- openFile "cliente.txt" AppendMode
            hPrint arq clientes
            hFlush  arq
            hClose arq


escreverPedido :: [Pedido] -> IO ()
escreverPedido pedidos = do
    arq <- openFile "pedido.txt" AppendMode
    hPrint arq pedidos
    hFlush  arq
    hClose arq

escreverPedidoExp :: [PedidoExpresso] -> IO ()
escreverPedidoExp pedidosExp = do
    arq <- openFile "pedidoExpresso.txt" AppendMode
    hPrint arq pedidosExp
    hFlush  arq
    hClose arq




castCliente = read::String->[Cliente]

lerCliente :: IO [Cliente]
lerCliente = do
    conteudo <- readFile "cliente.txt"
    let x = castCliente conteudo
    return x 


castPedido = read::String->[Pedido]


lerPedido :: IO [Pedido]
lerPedido = do
    conteudo <- readFile "pedido.txt"
    let x = castPedido conteudo
    return x 


castPedidoExp = read::String->[PedidoExpresso]

lerPedidoExp :: IO [PedidoExpresso]
lerPedidoExp = do
    conteudo <- readFile "pedidoExp.txt"
    let x = castPedidoExp conteudo
    return x 

main :: IO ()
main = do
    print (map clienteToString clientes)
    print (map pedidoToString pedidos)
    print (foiEntregue (entregueNoPrazo pedidoExpAtrasado))

    escreverCliente clientes
    escreverPedido pedidos
    escreverPedidoExp pedidosExp
    

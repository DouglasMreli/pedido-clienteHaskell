{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import System.IO
import Text.XHtml.Frameset (body)


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


lerCLiente:: [Cliente] -> IO ()
lerCliente clientes = do
    arq <- readFile "cliente.txt" ReadMode
    hPrint arq clientes
    hFlush arq
    hClose arq


lerPedido:: [Pedido] -> IO ()
lerPedido pedidios = do
    arq <- readFile "pedido.txt" ReadMode
    hPrint arq pedidos
    hFlush arq
    hClose arq


main :: IO ()
main = do
    
    escreverCliente clientes
    escreverPedido pedidos
    lerCliente clientes
    lerPedido pedidos
  

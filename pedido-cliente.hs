
type Cliente = (String,String,Integer)

fulano = ("Fulano", "Rua a", 9999)
ciclano = ("Ciclano", "Rua b", 8888)

clientes :: [Cliente]
clientes = [fulano, ciclano]

type Data = (Integer, Integer, Integer)

date :: Data
date = (14,9,2021)

type Pedido = (Integer, Double, Cliente, Data)
pedidos :: [Pedido]
pedidos = [(123, 20.0, fulano, date)]
            

type PedidoExpresso = (Integer, Double, Cliente, Data, Data)

dateAtrasado = (15,9,2021)
pedidoExpAtrasado = (555, 20.0*1.2, ciclano, date, dateAtrasado)

entregueNoPrazo :: PedidoExpresso -> Bool
entregueNoPrazo (num, preco, ciclano, date, dateEntrega) = date == dateEntrega

foiEntregue :: Bool -> String
foiEntregue x
            | x = "Foi entregue no prazo" 
            | otherwise = "Nao foi entregue no prazo"

main :: IO ()
main = do
    
    print(pedidoExpAtrasado)
    let foientregue = foiEntregue (entregueNoPrazo pedidoExpAtrasado)
    print(foientregue)
    
    

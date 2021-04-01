--1) Implemente as funções recursivas vistas nas vídeo-aulas 9 e 10: conta_ch,
--conta, maior, primeiros, pertence, uniaoR.



conta_ch::[Char]->Int

conta_ch []=0
conta_ch (x:xs)=1+conta_ch xs

conta::[t]->Int
conta [x]=1
conta (x:xs)=1+conta xs

maior::[Int]->Int
maior [x]=x
maior (x:y:xs) |(x<y)= maior (y:xs)
               |otherwise= maior (x:xs)

primeiros:: Int->[t]->[t]
primeiros _ []=[]
primeiros 0 _ =[]
primeiros n (x:xs)=x:primeiros (n-1) xs


pertence:: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = if (a == x) then True
else pertence a z

--uniaoR:: [t]->[t]->[t]
--uniaoR [] l=l
--uniaoR (x:xs) l =if pertence x l then uniaoR xs l else x:uniaoR xs l

{-2) Escreva a função recursiva npares que recebe uma lista de inteiros e retorna a
quantidade de números pares pertencentes à lista.-}

npares::[Int]->Int
npares []=0
npares (x:xs)=if(even(x)) then 1+npares (xs) else npares xs

{-
3) Escreva a função recursiva produtorio que recebe uma lista de números e
retorna o produto de todos os seus elementos.


-}

produtorio::[Int]->Int
produtorio []=1
produtorio (x:xs)=x*produtorio xs


{-4) Escreva a função recursiva comprime a seguir que recebe uma lista de listas e
retorna uma lista contendo todos os elementos das sublistas.
> comprime [[1,2],[3,4,5],[],[6]]
[1,2,3,4,5,6]-}

comprime::[[t]]->[t]

comprime []=[]
comprime (x:xs)= x ++ (comprime xs)



[(nome,idade)]
(_,i):xs= minha_funcao(xs)
{-
5) Escreva a função recursiva tamanho a seguir que recebe uma lista polimórfica
(de qualquer tipo) e retorna a quantidade de elementos que ela possui.
> tamanho [1,3,5,7,9]

-}
tamanho::[t]->Int
tamanho []=0
tamanho (x:xs)=1+tamanho xs


{-
6) Escreva a função recursiva uniaoRec2 que faz a união de duas listas de modo
que mantenha todos os elementos da 1a lista na mesma ordem e no final
acrescenta os elementos da 2a lista que não estejam presentes na primeira.
> uniaoRec2 [1,2,3,4,5,6,7] [2,9,7,10,4]
[1,2,3,4,5,6,7,9,10]

-}


uniaoNRec2  :: Eq t => [t] -> [t] -> [t]
uniaoNRec2 l1 [] = l1
uniaoNRec2 l1 (g:r) = if elem g l1 then uniaoNRec2 l1 r else uniaoNRec2 (l1 ++ [g]) r 

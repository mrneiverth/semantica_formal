import Estado

data AExp = Num Int
  |Var String
  |Som AExp AExp
  |Sub AExp AExp
  |Mul AExp AExp
  deriving(Show)

data BExp =	TRUE
  | FALSE
  | Not BExp
	| And BExp BExp
  | Or  BExp BExp
	| Ig  AExp AExp
  | Leq AExp AExp -- inserido o leq
  deriving(Show)

data CExp = While BExp CExp
  | If BExp CExp CExp
  | Seq CExp CExp
  | DoWhile BExp CExp -- inserido o dowhile
  | For AExp AExp AExp CExp --inserido for
  | Atrib AExp AExp --inserido
  | Swap AExp AExp -- inserido Swap
  | Skip
  | Repeat CExp BExp
  | Do CExp BExp
  | DuplaAtrib AExp AExp AExp AExp --inserido dupla atrib
	deriving(Show)

  ------------------------exp ARITMEICAS ------------------
abigStep :: (AExp,Estado) -> (Int,Estado)
abigStep (Var x,s) = (procuraVar s x,s)
abigStep (Num n,s) = (n,s)
abigStep (Som e1 e2,s) = let (n1,s1) = abigStep (e1, s);
							(n2,s2) = abigStep (e2, s)
							in (n1+n2,s)
abigStep (Sub e1 e2,s) = let (n1, s1) = abigStep (e1, s);
							(n2, s2) = abigStep (e2, s)
							in (n1-n2,s)
abigStep (Mul e1 e2,s) = let (n1, s1) = abigStep (e1, s);
							(n2, s2) = abigStep (e2, s)
							in (n1*n2, s)

---------------- exp. booleanas -------------------------------
bbigStep :: (BExp,Estado) -> (Bool,Estado)
--true
bbigStep (TRUE,s) = (True,s)
--false
bbigStep (FALSE,s) = (False,s)
--not
bbigStep (Not b,s) = let (b1,s1) = bbigStep (b,s)
              in (not b1,s1)
--igual
bbigStep (Ig e1 e2, s) = let (n1,s1) = abigStep(e1,s);
							(n2,s2) = abigStep (e2,s)
							in (n1 == n2, s)
--and
bbigStep (And e1 e2, s) = let (b1, s1) = bbigStep(e1,s);
							(b2, s2) = bbigStep(e2,s)
							in (b1 && b2, s)
--or
bbigStep (Or e1 e2, s) = let (b1, s1) = bbigStep(e1, s);
							(b2, s2) = bbigStep(e2, s)
							in (b1 || b2, s)
--leq
bbigStep (Leq e1 e2, s) = let (n1,s1) = abigStep(e1,s);
							(n2,s2) = abigStep(e2,s)
							in (n1 <= n2, s)

-- abigStep :: (AExp,Estado) -> (Int,Estado)
-- abigStep (Var x,s) = (procuraVar s x,s)
-- abigStep (Num n,s) = (n,s)
-- abigStep (Som e1 e2,s)  = let	(n1,s1) = abigStep (e1, s)
-- 				(n2,s2) = abigStep (e2, s)
-- 					in (n1+n2,s)

--[implmentacao de sub e mul]
--sub
-- abigStep (Sub e1 e2,s)  = let 	(n1,s1) = abigStep(e1, s)
-- 				(n2,s2) = abigStep (e2, s)
-- 					in (n1-n2,s)
--mul
-- abigStep (Mul e1 e2,s)  = let 	(n1,s1) = abigStep(e1, s)
-- 				(n2,s2) = abigStep (e2, s)
-- 					in (n1*n2,s)
--[/implmentacao de sub e mul]


-- bbigStep :: (BExp,Estado) -> (Bool,Estado)
-- bbigStep (TRUE,s)  	= (True,s)
-- bbigStep (FALSE,s) 	= (False,s)
-- bbigStep (Not b,s) = case bbigStep (b,s) of
-- 		(True,_) -> (False, s)
--                 (False,_) -> (True, s)

--[implmentacao de ig and e or]
--ig
-- bbigStep (Ig e1 e2,s ) = let	(b1, s1) = abigStep(e1, s)
-- 				(b2, s2) = abigStep(e2, s)
-- 					in(b1==b2, s)
--and
-- bbigStep (And b1 b2,s )  = let	(n1, s1) = bbigStep(b1, s)
-- 				(n2, s2) = bbigStep(b2, s)
-- 					in(n1 && n2, s)
-- --or
-- bbigStep (Or b1 b2,s )  = let	(n1, s1) = bbigStep(b1, s)
-- 				(n2, s2) = bbigStep(b2, s)
-- 					in(n1 || n2, s)
--[/implmentacao de ig and e or]

cbigStep :: (CExp,Estado) -> (CExp,Estado)
cbigStep (Skip,s) = (Skip,s)

--[implmentacao de if, seq, atrib, while]
--if
cbigStep (If b c1 c2,s) = case bbigStep(b, s) of
                          (True,_) -> cbigStep(c1, s)
                          (False,_) -> cbigStep(c2, s)
-- Seq
cbigStep (Seq c1 c2,s)  = let	(com1, s1) = cbigStep (c1, s)
 				(com2, s2) = cbigStep(c2, s1)
					in (com2, s2)
--Atrib
cbigStep (Atrib (Var x) e,s) = let	(n1,s1) = abigStep(e,s)
					(s2)	= (mudaVar s1 x n1)
						in (Skip, s2)
--While
cbigStep (While b c, s) = case bbigStep(b,s) of
			(True, _) -> cbigStep(Seq (c) (While b c), s)
			(False, _) -> (Skip, s)
--[/implmentacao de if, seq, atrib, while]
--Repeat
cbigStep (Repeat c b, s) = let	(c1, s1) = cbigStep(c,s)
				in(case bbigStep(b, s1) of
				(True, _) -> (Skip, s1)
				(False, _) -> cbigStep(Repeat c b, s1))

--do while
cbigStep(Do c b, s) = let (c1, s1) = cbigStep(c,s)
                        in(case bbigStep(b,s1) of
                          (True,_) -> cbigStep(Do c b, s1)
                          (False,_)-> (Skip,s1))
--for
cbigStep (For (Var x) e1 e2 c,s) = cbigStep(Seq
					(Atrib (Var x) e1)
					(If (Leq e1 e2) (Seq c (For (Var x) (Som e1 (Num 1)) e2 c)) (Skip)), s)
--swap
cbigStep(Swap (Var x) (Var y), s) = (Skip, mudaVar (mudaVar s x (procuraVar s y)) y (procuraVar s x))

--doubleAtrib
cbigStep(DuplaAtrib (Var x) (Var y) e1 e2, s)= cbigStep(Seq (Atrib (Var x) e1)
                                                (Atrib (Var y) e2), s)

fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
               (While (Not (Ig (Var "x") (Num 1)))
                      (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                           (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

-- -- função incrementa
-- exIncrementa :: CExp
-- exIncrementa = Atrib (Var "x") (Som (Var "x")(Num 1))
--
-- -- do while
-- exDoWhile :: CExp
-- exDoWhile = DoWhile exIncrementa (Leq (Var "x")(Num 6))


--------------------------testes------------------------------

meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]

meuEstado1 :: Estado
meuEstado1 = [("x",3), ("y",2), ("z",1)]

meuEstadoXY :: Estado
meuEstadoXY = [("x",3), ("y",2)]

meuEstadoX :: Estado
meuEstadoX = [("x",8)]

exIg :: BExp
exIg = (Ig (Som (Num 2) (Num 2))(Mul (Num 2)(Num 2)))

exSom :: AExp
exSom = Som (Num 3) (Som (Var "x")(Var "y"))

--meuEstado
-- meuEstadoFat
meuEstadoFat :: Estado
meuEstadoFat = [("x",10), ("y",0), ("z",0)]
exFat :: CExp
exFat = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x")(Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y")(Var "x")))
                            (Atrib (Var "x") (Sub (Var "x")(Num 1))))))
-- meuEstado => (Skip,[("x",1),("y",6),("z",0)])
-- fat(10! == 3.628.800) => (Skip,[("x",1),("y",3628800),("z",0)])

--estadoXY
exSwap :: CExp
exSwap = (Swap (Var "x")(Var "y") )
--x 2 y 3

--estado xy (constantes atrib(5, -1)
exDuplaAtrib :: CExp
exDuplaAtrib = (DuplaAtrib (Var "x")(Var "y") (Som (Num 2)(Num 3)) (Sub (Num 2)(Num 3)))
--x=5, y=-1

--estadoX
exDo :: CExp
exDo = (Do (Atrib (Var "x") (Mul (Var "x")(Num 2)))  (Leq (Var "x")(Num 16)))
--32

--meuEstado1 (x=3, y=2, z=1)
exFor:: CExp
exFor = (For (Var "x") (Num 1)(Num 10)  (Seq (Atrib (Var "y")(Som (Var "y")(Num 1))) (Atrib (Var "z")(Var "x"))))
--(Skip,[("x",11),("y",12),("z",10)])

--exSom retorna 8
exAtrib :: CExp
exAtrib = Atrib (Var "x") exSom
-- x=8

--meuestado1
exSeq :: CExp
exSeq = (Seq (Seq (Atrib (Var "x")(Var "z")) (Atrib (Var "z")(Var "y")))
		(Atrib (Var "y") (Var "x")))
-- x=1, y=1, z=2

--fazer com Estado [("x",0), ("y",10)]
meuEstadoYZ :: Estado
meuEstadoYZ = [("z",0), ("y",25)]
exRepeat :: CExp
exRepeat = (Repeat ( Atrib (Var "z") (Som (Var "z")(Num 1))) (Ig (Var "z")(Var "y") ) )
--(Skip,[("z",25),("y",25)])

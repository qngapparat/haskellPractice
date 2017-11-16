import Data.List

type Id = String
data Term = Var Id | App Term Term | Abs Id Term

vars :: Term -> [Id]

vars (Var id) = [id]
vars (App t1 t2) = vars t1 `union` vars t2
vars (Abs id t1) = [id] `union` vars t1


fvs :: Term -> [Id]

--same as above
fvs (Var id) = [id]
fvs (App t1 t2) = fvs t1 `union` fvs t2

--but without bounded variables

fvs (Abs id t1) = fvs t1 \\ [id




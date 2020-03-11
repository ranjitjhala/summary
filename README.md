# summary

## Abstract State 

```
Loc   := l1, l2, l3 ...
ALoc  := l | ?l | #l | ?#l
Ref   := Loc | Ref.fld
Heap  := Id + Ref |-> Loc 
LMap  := Loc |-> {new, abs}
State := (Heap, LMap)
```

## Programs 

```
(Id)   v  := x,y,z 

(Lval) lv := v | v.f

(Stmt) s  := lv <- lv | v <- new(i) | s1; s2 | s1 + s2 | s* | return lv  

(Func) fn := f(x1,x2,...) { s }  

(Prog) p  := fn1, fn2, ...
```

## Transfer Function

```haskell
type State = (Map Id Node, [Node], [Edge]) 
type Node  = Locs
type Edge  = (Node, Field, Node) 
type Locs  = Set Loc

-- | Graph API
next :: (State, Node, Field) -> Node
```

```haskell
TF :: (State, Stmt) -> State
TF(S, x <- new(i)) = merge(S, x, get(S, x), {#l_i}) 
TF(s, x   <- y  )  = merge(S, x, get(S, x), get(S, y))
TF(s, x   <- y.f)  = merge(S, x, get(S, x), get(S, y.f)) 
TF(s, x.f <- y  )  = set  (S, f, get(S, x), get(S, y)) 
```

```haskell
merge :: (State, Id, Locs, Locs) -> State
merge(S, x, ls1, ls2) = S' [ x |-> ls ] 
  where 
    (S', ls)          = unify(S, ls1, ls2)

-- | unify(s, ls1, ls2) updates `s` by merging the merges the Loc
unify :: (State, Locs, Locs) -> (State, Locs)
unify(S, 0, ls)    = (S, ls)
unify(S, ls, 0)    = (S, ls)
unify(S, ls1, ls2) = -- compute union of ls1, ls2 -- recursively unify their "out-edges"
```

```haskell
-- | get(S, lv) returns the Node abstracting the points-to set of lv in S
get :: (State, LVal) -> Locs
get (S, x)   = S(x)
get (S, x.f) = next(S, S(x), f)
```

```haskell
-- | set(S, f, ls1, ls2) returns new graph with edge ls1 -f-> ls2
--   but if ls1 -f-> ls2' already, then we want to MERGE/UNIFY ls2, ls2'?
set :: (State, Field, Locs, Locs) -> State 
set(S, f, ls1, ls2) = S'[ls1 :-f-> ls2'']  
  where 
      ls2'          = next(S, ls1, f)   
      (S', ls2'')   = unify(S, ls2, ls2')
```


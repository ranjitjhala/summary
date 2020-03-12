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

(Stmt) s  := lv <- lv | v <- new(i) | x <- f(y1,...) | s1; s2 | s1 + s2 | s* | return lv  

(Func) fn := f(x1,x2,...) { s }  

(Prog) p  := fn1, fn2, ...
```

## Initial State

## Transfer Function (Inter)

```haskell
TF(S, x <- f(y1,...)) = ???
```

## Transfer Function (Intra)

### Graph API 

```haskell
-- | Nodes, Labels etc.
type Locs  = Set Loc
type Node  = Locs
type Edge  = (Node, Field, Node) 

data Label = Single | Many | New 

-- | Abstract State
data State = St 
  { roots :: Map Id Node
  , label :: Map Node [Label]
  , nodes :: [Node]
  , edges :: [Edge] 
  } 

-- | Graph API
next :: (State, Node, Field) -> Node
```

### Transfer Function

```haskell
TF :: (State, Stmt) -> State
TF(S, x <- new(i)) = merge(S, x, get(S, x), {#l_i}) 
TF(s, x   <- y  )  = merge(S, x, get(S, x), get(S, y))
TF(s, x   <- y.f)  = merge(S, x, get(S, x), get(S, y.f)) 
TF(s, x.f <- y  )  = set  (S, f, get(S, x), get(S, y)) 
```

### Merging Location-Sets

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

**TODO:** How to merge "labels" when we merge sets of locations at a node.

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


## TODO 

- write up summary-based analysis as "pseudocode"
- Plain recursion
- Recursion + list
- Static fields
- Lambdas
- Exceptions
- Missing methods (NO_BODIES) some "handwritten" summaries.
- Different kinds of aliasing at method inputs.

## Example 1: Propagating Taints across methods

```java
private void smash(Obj x) {
                            // [x -> lx]
  x.data = source(); @l       
                            // [x -> lx, lx.data -> @l]
}                           // SUMMARY: Top ===> TAINT(lx.data)

public void test2() {
  Obj o1 = new Obj (); @l   // [o1 -> l]
  Obj o2 = o1;              // [o2 -> l]
  smash(o1);                // as o1 -> l, from SUMMARY: TAINT(l.data)

  sink(o1.data);            // as o1 -> l, sink-finding as TAINT(l.data)
  sink(o2.data);            // as o2 -> l, sink-finding as TAINT(l.data)
}
```

```java
Box wrapper() { 
  Box b = new Box() @l;
  return b;                 // ret = new @l
}

void main() {

  v1 = wrapper();           // [v1 -> new @l1 ]
  
  v1.data = source;
	 
  sink(v1.data); // finding

  v2 = wrapper();           // [v2 -> new @l2 ]
  v2.data = "happy";
  sink(v2.data); // non-finding
}
```


## Example 2: Propagating "Deep" taints across methods

```java
private void smash(Obj x) {  // [x -> lx]
  T1 tmp1 = x.f1;            // [tmp1 -> lx.f1]
  T2 tmp2 = tmp1.f2;         // [tmp2 -> lx.f1.f2]
  T3 tmp3 = tmp2.f3;         // [tmp3 -> lx.f1.f2.f3]
  tmp3.data = source();      // TAINT(lx.f1.f2.f3.data)
}
                             // SUMMARY: Top ===> TAINT(lx.f1.f2.f3.data)

public void test2() {
  Obj o1 = new Obj ();@l     // o1 -> l
  Obj o2 = o1;               // o2 -> l

  smash(o1);                 // o1 -> l, from SUMMARY: TAINTED(l.f1.f2.f3.data)

  sink(o2.f1.f2.f3.data);    // as o2 -> l, sink-finding as TAINT(l.f1.f2.f3.data)
}
```

## Example 3: Context-Sensitive Propagation

```java
void put(Box b, String s){
  b.fld = s;                // [b -> lb, s -> ls, lb.fld -> ls]
}                           // SUMMARY: TAINT(s) ===> TAINT(lb.fld)


String get(Box b){
  return b.fld;             // [b -> lb]
}                           // SUMMARY: TAINT(lb.fld) ===> TAINT(ret)

void main(){
  Box b1   = new Box();     // b1 -> l1
  String s1 = secret();     // TAINT(s1)
  put(b1, s1);              // TAINT(l1.fld)      (due to b1 -> l1, lb is l1, SUMMARY: TAINT(s) ===> TAINT(lb.fld))

  Box b2 = new Box();
  String s2 = "yolo";
  put(b2, s2);

  String t1 = get(b1);      // TAINT(t1)          (due to b1 -> l1, lb is l1, SUMMARY: TAINT(lb.fld) ===> TAINT(ret))
  sink(t1);                 // sink-finding

  String t2 = get(b2);      // no taint on t2 as no taint on l2.fld
  sink(t2);                 // sink-non-finding



  b1 = new Box @l1
  
  b2 = new Box @l2

  b1.fld = source()

  if (cond)
    x = b1
  else 
    x = b2
  
  x.fld = "happy"

  // x -> ?l
  // x -> 
  
  x.fld = source()

}
```

## Example 4:

```java
void baz(Box b1, Box b2) {  // [b1 -> l, b2 -> l]
  b1.fld = source();        // TAINT(l.fld)
  sink(b2.fld);
}

void main(){
  Box b1 = new Box(); @m1
  baz(b1, b1);

  Box b2 = new Box(); @m2
  baz(b1, b2);
}
```


## Ex 5

```java
void bar(Box b){  
                    // [b -> lb]
  return b.yld;
                    // ret = lb.yld
}

void foo(Box b){
                    // [ b -> fb ]
  b.fld = bar(b);
                    // [ b -> fb, fb.fld -> fb.yld ]
}
```

## Ex6 

```java
Box insert(Box t, String s) {  // [ t -> lt, s -> ls]
  Box b = new Box() @ib;
                     // [ t -> lt, b -> new ib ]
  b.fld = s;
                     // [ t -> lt, b -> ib, ib.fld -> ls ]
  b.next = t;
                     // [ t -> lt, b -> ib, ib.fld -> ls, ib.next -> lt ]
  return b;
                     // ret = new ib
}

Box foo(String data){ // [data -> ldata]
  Box l = null;
                      // [ l -> 0 ]   
  while (*) { 
    l = insert(l, data);
                      // [ t -> lt, b -> ib, ib.fld -> ls, ib.next -> lt ]
                      // [ l -> new ?ib, ib.next -> ?ib, ?ib.fld -> ldata ]
  }
  return l;
}

void main() { 
  String s1 = source();
  Box l1 = foo(s1);
                      // [ l1 -> new ?ib1, ib1.next -> ?ib1, ?ib1.fld -> ls1 ]
  sink(l1.fld);  // sink-finding

  String s2 = "happy";
  Box l2 = foo(s2);
                      // [ l2 -> new ?ib2, ib2.next -> ?ib2, ?ib2.fld -> ls2 ]

  sink(l2.fld);  // sink-non-finding
}
```

// CONTEXT 1 [ o1 -> l1, o2 -> l2, l1.fld -> lcommon, l2.fld -> lcommon ]
// CONTEXT 2 [ o1 -> l1, o2 -> l2, l1.fld -> lc1, l2.fld -> lc2 ]             (l1.fld, l2.fld are DIFFERENT)
foo(o1, o2)

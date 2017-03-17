module RedBlackZipper exposing (..)

type Color
    = Red
    | Black
    | BBlack
    | NBlack

type LeafColor
    = LBlack
    | LBBlack

type Tree comparable v
    = Node Color comparable v (Tree comparable v) (Tree comparable v)
    | Empty LeafColor

type Context comparable v
    = RightContext Color comparable v (Tree comparable v) 
    | LeftContext Color comparable v (Tree comparable v)

type alias Breadcrumbs comparable v =
    List (Context comparable v)

type alias Zipper comparable v =
    ( Tree comparable v, Breadcrumbs comparable v )
       
new : Zipper comparable v
new =
  zipper empty
      
empty : Tree comparable v
empty =
  Empty LBlack
  
zipper : Tree comparable v->Zipper comparable v
zipper tree =
  (tree, [])
   
insert : comparable -> v->Zipper comparable v -> Zipper comparable v
insert key value zip =
  update key (always (Just value)) zip

remove : comparable -> Zipper comparable v -> Zipper comparable v
remove key zip =
  update key (always Nothing) zip

update : comparable -> (Maybe v -> Maybe v) -> Zipper comparable v -> Zipper comparable v
update key alter zip =
  let
    foundZip = goToDown key zip
  in
    case foundZip of
      (Node color k v left right, breadcrumbs)->
        case alter (Just v) of
          Just alteredV->
            (Node color k alteredV left right, breadcrumbs)
          Nothing->
            deleteImpl foundZip
              |> ensureBlackRoot
      (Empty _, breadcrumbs)->
        case alter Nothing of
          Just alteredV->
            (Node Red key alteredV empty empty, breadcrumbs)
              |> moveAndBalance
              |> ensureBlackRoot

          Nothing->
            foundZip

deleteImpl : Zipper comparable v -> Zipper comparable v
deleteImpl (tree,breadCrumbs) =
  case tree of
    Node Black _ _ (Empty _) (Empty _)-> (Empty LBBlack, breadCrumbs)
                                      |> (updateMaybe goUp)
                                      |> bubbleUp 
    Node Red _ _ (Empty _) (Empty _)->(Empty LBlack,breadCrumbs)
    Node Black _ _ (Node Red k v left right) (Empty _)->(Node Black k v left right,breadCrumbs)                                                        
    Node Black _ _ (Empty _) (Node Red k v left right)->(Node Black k v left right,breadCrumbs)
                                                        
    Node color k v (Node lColor lk lv lleft lright) (Node rcolor rk rv rleft rright)->
      let 
        ((newKey,newValue),newLeft) = removeMax (Node lColor lk lv lleft lright)
      in
        bubbleUp (Node color newKey newValue newLeft (Node rcolor rk rv rleft rright),breadCrumbs)
    _ ->Native.Debug.crash "delete case impossible!"

extractKeyValue : Tree comparable v->(comparable,v)
extractKeyValue tree =
  case tree of
    Node _ key value _ _ -> (key,value)
    _ ->Native.Debug.crash "max must have a key value!"
        
removeMax : Tree comparable v-> ((comparable,v),Tree comparable v)
removeMax tree =
  let 
    (maxTree, breadCrumbs) = zipper tree
                           |> moveToMax
                              
    (newTree,_) = (maxTree, breadCrumbs)
                |> deleteImpl
                |> goTop
  in
    (extractKeyValue maxTree, newTree)

bubbleUp : Zipper comparable v -> Zipper comparable v
bubbleUp zip =  
  case bubble zip of    
    Just bZip ->
      case goUp bZip of
        Just upperZip -> bubbleUp upperZip
        Nothing -> bZip
    _-> zip
            
bubble : Zipper comparable v -> Maybe (Zipper comparable v)
bubble (tree, breadCrumbs) =
  case tree of
    Node color key v left right->        
      if isBBlack left || isBBlack right then
        Just (updateMaybe  balanceZipper (Node (moreBlack color) key v (lessBlackTree left) (lessBlackTree right), breadCrumbs))
      else
        Nothing
    _->Nothing

  
moveToMax : Zipper comparable v -> Zipper comparable v
moveToMax zip =
  case zip of
    (Node color k v left (Empty _ ), breadcrumbs)->
      zip         
    (Node color k v left right, breadcrumbs)->
      moveToMax (right, (LeftContext color k v left)::breadcrumbs)
    _ ->Native.Debug.crash "Max should have been found earlier!"

isBBlack : Tree comparable v->Bool
isBBlack tree =
  case tree of
    Node BBlack _ _ _ _ ->True
    Empty LBBlack ->True
    _ ->False
    
moreBlack : Color -> Color
moreBlack color =
  case color of
    Black->BBlack
    Red->Black
    NBlack->Red
    _ ->Native.Debug.crash "More black case impossible!"
        
lessBlack : Color -> Color
lessBlack color =
  case color of
    BBlack->Black
    Black ->Red
    Red ->NBlack
    _ ->Native.Debug.crash "Less black case impossible!"
    
lessBlackTree : Tree comparable v-> Tree comparable v
lessBlackTree tree  =
  case tree of
    (Node color key value left right)->
      (Node (lessBlack color ) key value left right)
    Empty _-> Empty LBlack

ensureBlackRoot : Zipper comparable v->Zipper comparable v
ensureBlackRoot (tree, breadCrumbs) =
  case breadCrumbs of
    []-> (blacken tree,breadCrumbs)
    _->(tree, breadCrumbs)    
    
blacken : Tree comparable v -> Tree comparable v
blacken tree=
  case tree of 
    Node _ key v left right->
      Node Black key v left right      
    Empty _ ->
      Empty LBlack

redden : Tree comparable v -> Tree comparable v
redden t =
  case t of
    Empty _ ->
      Native.Debug.crash "leaf cannot be red"
    Node _ key v left right ->
      Node Red key v left right

      
goLeft : Zipper comparable v->Maybe (Zipper comparable v)
goLeft zip =
  case zip of
    (Node color k v left right, breadcrumbs)->
      Just (left, (RightContext color k v right)::breadcrumbs)
    _->Nothing  

goRight : Zipper comparable v->Maybe (Zipper comparable v)
goRight zip =
  case zip of
    (Node color k v left right, breadcrumbs)->
      Just (right, (LeftContext color k v left)::breadcrumbs)
    _->Nothing
    
goUp : Zipper comparable v->Maybe (Zipper comparable v)
goUp zip =           
  case zip of
    (tree, [])->Nothing
    (tree, (RightContext color k v right)::tail)->
      Just (Node color k v tree right,tail)
    (tree, (LeftContext color k v left)::tail)->
      Just (Node color k v left tree,tail)

goUpOrStay : Zipper comparable v->Zipper comparable v
goUpOrStay zip =
    case zip of
    (tree, [])->(tree, [])
    (tree, (RightContext color k v right)::tail)->
      (Node color k v tree right,tail)
    (tree, (LeftContext color k v left)::tail)->
      (Node color k v left tree,tail)

until : (x-> Maybe x) -> x -> x
until f x =
  case f x of
    Just newX -> until f newX
    Nothing-> x

updateMaybe : (x-> Maybe x) -> x -> x
updateMaybe f x =
  case f x of
    Just newX -> newX
    Nothing-> x
              
moveAndBalance : Zipper comparable v -> Zipper comparable v
moveAndBalance zip =
  case (goUpOrStay >> goUpOrStay >> balanceZipper) zip of
    Just x -> moveAndBalance x
    Nothing ->zip
  --until (goUpOrStay >> goUpOrStay >> balanceZipper) zip    

balanceZipper : Zipper comparable v -> Maybe (Zipper comparable v)
balanceZipper (tree, breadCrumbs) =
  case balance tree of
    Just x -> Just (x,breadCrumbs)
    Nothing-> Nothing
  
balance : Tree comparable v -> Maybe (Tree comparable v)
balance tree =
    case tree of
      Node color keyZ vZ (Node Red keyX vX a (Node Red keyY vY b c )) d->
        balancedZTree color keyX vX keyY vY keyZ vZ a b c d
         --Just (Node (lessBlack color) keyY vY (Node Black keyX vX a b )(Node Black keyZ vZ c d))
                         
      Node color keyZ vZ (Node Red keyY vY (Node Red keyX vX a b ) c) d->
        balancedZTree color keyX vX keyY vY keyZ vZ a b c d
         --Just (Node (lessBlack color) keyY vY (Node Black keyX vX a b )(Node Black keyZ vZ c d))
                
      Node color keyX vX a (Node Red keyY vY b (Node Red keyZ vZ c d ))->
        balancedZTree color keyX vX keyY vY keyZ vZ a b c d
         --Just (Node (lessBlack color) keyY vY (Node Black keyX vX a b )(Node Black keyZ vZ c d))
                            
      Node color keyX vX a (Node Red keyZ vZ (Node Red keyY vY b c) d)->
        balancedZTree color keyX vX keyY vY keyZ vZ a b c d
         --Just (Node (lessBlack color) keyY vY (Node Black keyX vX a b )(Node Black keyZ vZ c d) )            

      Node BBlack xk xv a (Node NBlack zk zv (Node Black yk yv b c) (Node Black _ _ _ _ as d)) ->
        let
          m = updateMaybe balance (Node Black zk zv c (redden d))
        in 
          Just (Node Black yk yv (Node Black xk xv a b) m)

      Node BBlack zk zv (Node NBlack xk xv (Node Black _ _ _ _ as a) (Node Black yk yv b c)) d ->
        let
          m = updateMaybe balance (Node Black xk xv (redden a) b)
        in 
          Just (Node Black yk yv m (Node Black zk zv c d))
        
      _->Nothing
         --Just tree  --TODO try goin,g up the tree with this!
         
balancedZTree : Color->comparable->v->comparable->v->comparable->v->Tree comparable v->Tree comparable v->Tree comparable v->Tree comparable v->Maybe (Tree comparable v)
balancedZTree col xk xv yk yv zk zv a b c d =
  Just (Node
          (lessBlack col)
          yk
          yv
          (Node Black xk xv a b)
          (Node Black zk zv c d))
  

         
goTo : comparable->Zipper comparable v->Zipper comparable v
goTo key zip =
  zip

get : comparable->Zipper comparable v->Maybe v
get key (tree, _) =
  getImpl key tree
    
getImpl : comparable->Tree comparable v->Maybe v
getImpl key tree =
  case tree of
    Empty _->Nothing
    Node _ k v left right->
      case compare key k of
        LT-> getImpl key left
        GT-> getImpl key right
        EQ->Just v        
    
goToDown : comparable->Zipper comparable v->Zipper comparable v
goToDown key zip =
  case zip of
    (Empty _, _)->zip
    (Node color k v left right, breadcrumbs)->
      case compare key k of        
        LT-> goToDown key (left, (RightContext color k v right)::breadcrumbs)
        GT-> goToDown key (right, (LeftContext color k v left)::breadcrumbs)
        EQ->zip    
             
goTop : Zipper comparable v->Zipper comparable v
goTop (tree, breadCrumb) =
  case breadCrumb of
    (RightContext color k v right)::tail->
      goTop (Node color k v tree right,tail)
    (LeftContext color k v left)::tail->
      goTop (Node color k v left tree,tail)
    []->(tree, breadCrumb)
    
foldr : (comparable -> v -> b -> b) -> b -> Zipper comparable v -> b
foldr f acc zip =
  Tuple.first (foldrImpl f (acc,zip))

foldl : (comparable -> v -> b -> b) -> b -> Zipper comparable v -> b
foldl f acc zip =
  Tuple.first (foldlImpl f (acc,zip))

foldrImpl : (comparable->v->b->b) -> (b,Zipper comparable v) -> (b,Zipper comparable v)
foldrImpl f (acc,(tree,breadCrumbs)) =
  case (tree,breadCrumbs) of
    ((Empty _), _)->
      (acc,(tree,breadCrumbs))
             
    (Node _ key v (Empty _) (Empty _), [])->
      (f key v acc, (tree,breadCrumbs) )
        
    (Node _ key v (Empty _) (Empty _), ((LeftContext pColor pKey pV pLeft)::tail))->
      -- we eat up the node ang go upLeft and delete the right child
      foldrImpl f (f key v acc,(Node pColor pKey pV pLeft empty,tail))                       
          
    (Node _ key v left (Empty _), breadCrumbs)->
      -- we eat up the node ang go upLeft
      foldrImpl f ((f key v acc),(left,breadCrumbs))
    
    (Node color key v left right, breadCrumbs)->
      foldrImpl f (acc,(right, (LeftContext color key v left)::breadCrumbs))     

foldlImpl : (comparable->v->b->b) -> (b,Zipper comparable v) -> (b,Zipper comparable v)
foldlImpl f (acc,(tree,breadCrumbs)) =
  case (tree,breadCrumbs) of
    ((Empty _), _)->
      (acc,(tree,breadCrumbs))
             
    (Node _ key v (Empty _) (Empty _), [])->
      (f key v acc, (tree,breadCrumbs) )
        
    (Node _ key v (Empty _) (Empty _), ((RightContext pColor pKey pV pRight)::tail))->
      -- we eat up the node ang go upRight and delete the left child
      foldlImpl f (f key v acc,(Node pColor pKey pV empty pRight,tail))                       
          
    (Node _ key v (Empty _) right , breadCrumbs)->
      -- we eat up the node ang go upRight
      foldlImpl f ((f key v acc),(right,breadCrumbs))
    
    (Node color key v left right, breadCrumbs)->
      foldlImpl f (acc,(left, (RightContext color key v right)::breadCrumbs))

each : (Color->comparable->v->b->b)->(b->b)->(b->b)->(b,Zipper comparable v)->(b,Zipper comparable v)
each f fTerm fUp (acc,(tree,breadCrumbs)) =
  case (tree,breadCrumbs) of
    ((Empty _), _)->
      (acc,(tree,breadCrumbs))

    (Node BBlack key v (Empty _) (Empty _), [])->
      (acc, (tree,breadCrumbs) )
        
    (Node color key v (Empty _) (Empty _), [])->
      (fTerm(f color key v acc), (tree,breadCrumbs) )
                
    (Node BBlack key v (Empty _) (Empty _), ((LeftContext pColor pKey pV lLeft)::tail))->
        each f fTerm fUp (fUp acc,(Node pColor pKey pV (Empty LBlack) (Empty LBlack) ,tail))

    (Node BBlack key v (Empty _) (Empty _), ((RightContext pColor pKey pV lRight)::tail))->
        each f fTerm fUp (fUp acc,(lRight ,(LeftContext BBlack pKey pV (Empty LBlack))::tail))
        
    (Node color key v (Empty _) (Empty _), ((LeftContext BBlack pKey pV lLeft)::tail))->          
      each f fTerm fUp (fUp(fTerm(f color key v acc)),(Node BBlack pKey pV (Empty LBlack) (Empty LBlack) ,tail))

    (Node color key v (Empty _) (Empty _), ((RightContext pColor pKey pV pRight)::tail))->
        each f fTerm fUp (fUp(fTerm(f color key v acc)),(pRight,(LeftContext BBlack pKey pV (Empty LBlack))::tail))
                  
    (Node color key v (Empty _) right , breadCrumbs)->
      each f fTerm fUp (fTerm(f color key v acc),(right,(LeftContext BBlack key v (Empty LBlack))::breadCrumbs))
    
    (Node color key v left right, breadCrumbs)->
      each f fTerm fUp ((f color key v acc),(left, (RightContext color key v right)::breadCrumbs))
      
map : (comparable -> v -> a)->Zipper comparable v->List a
map f zip =
  let
    accumulate k v acc =
      (f k v)::acc
  in
    foldr accumulate [] zip
    
keys : Zipper comparable v->List comparable
keys zip =
  let
    key k _ = k
  in
    map key zip

values : Zipper comparable v->List v
values zip =
  let
    value _ v = v
  in
    map value zip

toList : Zipper comparable v -> List (comparable,v)
toList zip =
  foldr (\key value list -> (key,value) :: list) [] zip

    
fromList : List (comparable,v) -> Zipper comparable v
fromList assocs =
  List.foldl (\(key,value) zip -> insert key value zip) new assocs

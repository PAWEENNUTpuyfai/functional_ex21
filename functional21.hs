newtype State s a =
    State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b)
      -> State s a -> State s b
    fmap f (State g) = State $ \s ->
        let (x, s') = g s
        in (f x, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State $ \s -> (x, s)

    (<*>) :: State s (a -> b) -> State s a
      -> State s b
    State h <*> State g = State $ \s ->
        let (f, s')   = h s
            (x', s'') = g s'
        in (f x', s'')

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b)
      -> State s b
    State h >>= f = State $ \s ->
        let (x, s') = h s
            State g = f x
        in g s'
        
{-
implement a queue data structure
type Queue a = ([a], [a], Int)
and the following operations:
size :: State (Queue a) Int
isEmpty :: State (Queue a) Bool
enqueue :: a -> State (Queue a) ()
dequeue :: State (Queue a) a
mkQueue :: [a] -> State (Queue a) ()
empty :: State (Queue a) () -- empty q
try to make all operations O(1)
hint: we can implement a queue using two stacks
-}

type Queue a = ([a], [a], Int) 

size :: State (Queue a) Int
size = State $ \(fs, bs, sz) -> (sz, (fs, bs, sz))


isEmpty :: State (Queue a) Bool
isEmpty = State $ \(fs, bs, sz) -> (sz == 0, (fs, bs, sz))


enqueue :: a -> State (Queue a) ()
enqueue x = State $ \(fs, bs, sz) -> ((), (fs, x:bs, sz+1))


dequeue :: State (Queue a) (Maybe a)
dequeue = State $ \(fs, bs, sz) ->
    case fs of
        (x:xs) -> (Just x, (xs, bs, sz-1)) 
        [] -> case reverse bs of
            (y:ys) -> (Just y, (ys, [], sz-1)) 
            []     -> (Nothing, ([], [], 0)) 


mkQueue :: [a] -> State (Queue a) ()
mkQueue xs = State $ \_ -> ((), (xs, [], length xs))

empty :: State (Queue a) ()
empty = State $ \_ -> ((), ([], [], 0))

--implement function joinState :: State s (State s a) -> State s a


joinState :: State s (State s a) -> State s a
joinState (State outer) = State $ \s ->
    let (innerState, s') = outer s         
        (result, s'') = runState innerState s' 
    in (result, s'')     
{-
--prove that state monad satisfies functor laws
--hint: for composition law, work from right to left
------------Answer--------------
--Identity Law: fmap id == id
fmap id (State g)       = State (\s -> let (x, s') = g s in (id x, s'))
                        = State (\s -> let (x, s') = g s in (x, s'))
                        = State (\s -> g s)
                        = State g ✔

--Composition Law:fmap (h . g) = fmap h . fmap g
fmap (h . g) (State f)      = State (\s -> let (x, s') = f s in ((h . g) x, s'))
fmap h . fmap g $ (State f) = State (\s -> let (x, s') = f s in (g x, s'))
                            = State (\s -> let (x, s') = f s in (h (g x), s'))
                            = State (\s -> let (x, s') = f s in ((h . g) x, s')) ✔

-}

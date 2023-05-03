module Week04.Monad2 where

threeInts :: Monad m => m Int -> m Int -> m Int-> m Int
threeInts mx my mz = 
    mx >>= \k ->
    my >>= \l ->
    mz >>= \m ->
    let s = k + l + m in return s        
    
    
-- Writing with the do notation

threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m 
    return s

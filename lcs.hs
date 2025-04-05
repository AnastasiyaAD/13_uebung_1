lcs :: String -> String -> Int
lcs a b = table !! m !! n
  where
    m = length a
    n = length b
    table = foldl -- iterate solve through matrix with size of string lengths initialy 0
      (\t (i, j) -> solve t i j)
      [[0 | _ <- [0..n]] | _ <- [0..m]]
      [(i, j) | i <- [1..m], j <- [1..n]]
    solve t i j = -- replace element i j
      take i t ++
      [take j (t !! i) ++ [
        if a !! (i - 1) == b !! (j - 1)
        then (t !! (i - 1) !! (j - 1)) + 1 -- keep char, hence length + 1
        else max (t !! (i - 1) !! j) (t !! i !! (j - 1)) -- drop char, hence other length
      ] ++
      drop (j + 1) (t !! i)] ++
      drop (i + 1) t

main :: IO ()
main = do
  print $ lcs "BANANAS" "ANALYSIS"
  print $ lcs "BIDIRECTIONAL-ANALYSIS" "BNANASS"
  print $ lcs "JOHANNES" "ANASTASIIA"

data Optional a = Valid a | Invalid deriving Show

(>=>) :: (a -> Optional b) -> (b -> Optional c) -> (a -> Optional c)
(>=>) f g = \x ->
    let y = f x
    in  case y of
        Valid z -> g z
        _ -> Invalid

return :: a -> Optional a
return x = Valid x

safe_root :: Double -> Optional Double
safe_root x = if (x >= 0) then Valid (sqrt(x)) else Invalid

safe_reciprocal :: Double -> Optional Double
safe_reciprocal x = if (x /= 0) then Valid (1/x) else Invalid

safe_root_reciprocal = safe_reciprocal >=> safe_root

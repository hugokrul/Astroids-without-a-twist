module Astroid where

-- import Graphics.Gloss
-- import Model

-- bigAstroid :: Picture
-- bigAstroid = scale 2 2 $ color white $ polygon [(17, 0), (33, -6), (32, -22), (23, -32), (12, -26), (0, -24), (2, -13), (1, -5), (7, 0)]

-- mediumAstroid :: Picture
-- mediumAstroid = color white $ polygon [(17, 0), (33, -6), (32, -22), (23, -32), (12, -26), (0, -24), (2, -13), (1, -5), (7, 0)]

-- smallAstroid :: Picture
-- smallAstroid = scale 0.5 0.5 $ color white $ polygon [(17, 0), (33, -6), (32, -22), (23, -32), (12, -26), (0, -24), (2, -13), (1, -5), (7, 0)]

-- bigAstroidHitBox :: [Point]
-- -- topleft, topright, bottomright, bottomleft
-- bigAstroidHitBox = [(0, 0), (66, 0), (66, -64), (0, -64)]

-- mediumAstroidHitBox :: [Point]
-- -- topleft, topright, bottomright, bottomleft
-- mediumAstroidHitBox = [(0, 0), (33, 0), (33, -32), (0, -32)]

-- smallAstroidHitBox :: [Point]
-- -- topleft, topright, bottomright, bottomleft
-- smallAstroidHitBox = [(0, 0), (16.5, 0), (16.5, -16), (0, -16)]

-- mkAStroid :: Astroid -> Float -> Picture
-- mkAStroid astroid@(Astroid (x, y) v d t s) time = case s of
--     Big -> translate posx posy bigAstroid
--     Medium -> translate posx posy mediumAstroid
--     Small -> translate posx posy smallAstroid
--     where
--         posx = x + dirx*v*(time-t)
--         posy = y + diry*v*(time-t)
--         (dirx, diry) = (sin dirAngleRad, cos dirAngleRad)
--         dirAngleRad = d*(pi/180)

-- getAstroids :: World -> [Astroid]
-- getAstroids w = case w of
--     (Play _ _ astroids) -> astroids
--     _ -> []

-- astroidPos :: World -> (Float, Float)
-- astroidPos w = case w of
--     (Play _ _ ((Astroid pos _ _ _ _):_)) -> pos
--     _ -> (-10000, -100000)

-- getAstroidsPicture :: World -> Float -> [Picture]
-- getAstroidsPicture w time = case w of
--     (Play _ _ []) -> []
-- getAstroidsPicture (Play p b (astroid:rest)) time = mkAStroid astroid time : getAstroidsPicture (Play p b rest) time

-- -- checkDeleteAstroid :: World -> World
-- -- checkDeleteAstroid (Play p b []) = Play p b []
-- -- checkDeleteAstroid (Play p b astroids@((Astroid pos@(x, y) vel dir life size):rest))
-- --     = case size of 
-- --         Big ->  | 
-- --                 | otherwise = Play p b astroids
-- --         Medium ->
-- --         Small ->

-- {-
-- checkDeleteBullet :: World -> Float -> World
-- checkDeleteBullet (Play p [] a) time = Play p [] a
-- checkDeleteBullet (Play p list@(bullet:rest) a) time
--     | (time - getBulletTime bullet) >= 3 = Play p rest a
--     | otherwise = Play p list a
-- -}
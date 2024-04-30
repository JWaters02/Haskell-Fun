module Films where

type AgeRating = Int
type FilmName = String

filmData :: [(AgeRating,FilmName)]
filmData =
  [ (12,"The Imitation Game"),
    (13,"Ada"),
    (12,"The Social Network"),
    (15,"Steve Jobs")
  ]

eligibleFilms :: AgeRating -> [FilmName]
eligibleFilms userAge =
  let eligibleFilmData = filter (\ (ageRating,name) -> userAge >= ageRating) filmData
      eligibleFilmNames = map (\ (ageRating,name) -> name) eligibleFilmData
   in eligibleFilmNames

getUserAge :: IO AgeRating
getUserAge = do
  putStr "Enter your age: "
  age <- readLn
  return age

main :: IO ()
main = do
  userAge <- getUserAge
  let filmNames = eligibleFilms userAge
  print filmNames

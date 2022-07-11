main :: IO()
main = do 

   print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] -- == "Bulgaria"

highestCapital :: [Country] -> Name
highestCapital countries = getCountry $ foldr1 (\ x@(a, b, c) y@(d, e, f) -> if elevations a countries > elevations d countries then x else y) vectors
   where
    vectors  = [(name, capital, cities) | (Country name capital cities) <- countries ]
    getCountry = (\(n, cap, cit) -> n)

elevations :: Name -> [Country] -> Int
elevations country countries = snd $ head [(c, el) | (City c el t) <- citiesList, c == getCapital]
    where
    citiesList = concat $ [ cts | (Country cntr cpt cts) <- countries]
    getCapital = snd $ head [(cntr, cpt) | (Country cntr cpt cts) <- countries, cntr == country]



type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
    deriving (Show, Eq)

data Country = Country Name Capital [City]
    deriving (Show, Eq)
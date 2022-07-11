main :: IO()
main = do 

    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

coldestCapital :: [Country] -> Name
coldestCapital countries = foldl1 (\c1 c2 -> if avg (avgT c1) <= avg (avgT c2) then c1 else c2) countryNames 
    where
        countryNames = [name | (Country name _ _) <- countries]

        avgT country = [temp | (Country name _ cs) <- countries, (City _ _ temp) <- cs, name == country]

        avg xs = sum xs / (fromIntegral $ length xs)
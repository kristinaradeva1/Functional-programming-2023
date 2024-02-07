main :: IO()
main = do
    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

highestCapital :: [Country] -> Name
highestCapital cs = fst $ foldr1 (\ x1@(cn1, el1) x2@(cn2, el2) -> if el1 > el2 then x1 else x2) [ (countryName, el) | (Country countryName capital cities) <- cs, (City cityName el _) <- cities, capital == cityName]
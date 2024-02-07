main :: IO()
main = do
    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

coldestCapital :: [(Country)] -> Name
coldestCapital cs = fst $ foldr1 (\ x1@(c1, t1) x2@(c2, t2) -> if t1 < t2 then x1 else x2) [(countryName, temperature) | (Country countryName cap cities) <- cs, (City cityName el temperature) <- cities]
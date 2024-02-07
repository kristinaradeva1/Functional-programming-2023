import Data.List
main :: IO()
main = do
    print $ getNameLengthColor db ((== 'Y'), (> 106)) == [("Pretty Woman",119),("The Man Who Wasn't There",116),("Logan's run",120),("Empire Strikes Back",111),("Star Trek",132),("Star Trek: Nemesis",116)]
    print $ getNameLengthColor db ((== 'Y'), (> 237)) == []
    print $ getNameLengthColor db ((== 'Y'), (> 238)) == []
    print $ getNameLengthColor db ((== 'N'), (< 133)) == [("Terms of Endearment",132)]
    print $ getNameLengthColor db ((== 'N'), (< 300)) == [("Terms of Endearment",132)]
    -------------------------------------------------------------------------------------------------------------------------
    print $ (getStudios db) [2001] == [("USA Entertainm.","Stephen Spielberg"),("Buzzfeed Entertainm.","Christian Baesler")]
    print $ (getStudios db) [2002] == [("Buzzfeed Entertainm.","Christian Baesler")]
    print $ (getStudios db) [1990] == [("Disney","Merv Griffin"),("Buzzfeed Entertainm.","Christian Baesler")]
    print $ (getStudios db) [1990, 2001, 1976] == [("Disney","Merv Griffin"),("USA Entertainm.","Stephen Spielberg"),("Buzzfeed Entertainm.","Christian Baesler")]
    print $ (getStudios db) [1979, 2002] == [("Buzzfeed Entertainm.","Christian Baesler")]

type Title = String
type Year = Int
type Length = Int
type InColor = Char
type StudioName = String
type Name = String
type ProducerID = Int
type Networth = Integer
type Movie = (Title, Year, Length, InColor, StudioName)
type Studio = (Name, ProducerID)
type MovieExec = (Name, ProducerID, Networth)
type MovieDB = ([Movie], [Studio], [MovieExec])

studios :: [Studio]
studios = [("Disney", 199),("USA Entertainm.", 222),("Fox", 333),("Paramount", 123),("MGM", 555),("Buzzfeed Entertainm.", 42)]

movieExecs :: [MovieExec]
movieExecs = [("George Lucas", 555, 200000000),("Ted Turner", 333,125000000),("Stephen Spielberg", 222, 100000000),("Merv Griffin",199, 112000000),("Calvin Coolidge", 123, 20000000),("Christian Baesler", 42, 420000)]

movies :: [Movie]
movies = [("Pretty Woman", 1990, 119, 'Y', "Disney"),("The Man Who Wasn't There", 2001, 116, 'Y', "USA Entertainm."),("Logan's run", 1976, 120, 'Y', "Fox"),("Star Wars", 1977, -1, 'N', "Fox"),("Star Wars 2", 1977, 238, 'N', "Fox"),("Empire Strikes Back", 1980, 111, 'Y', "Fox"),("Star Trek", 1979, 132, 'Y', "Paramount"),("Star Trek: Nemesis", 2002, 116, 'Y', "Paramount"),("Terms of Endearment", 1983, 132, 'N', "MGM"),("The Usual Suspects", 1995, 106, 'Y', "MGM"),("Gone With the Wind", 1938, 238, 'Y', "MGM"),("Gone With the Wind 2", 1938, 238, 'Y', "MGM"),("The Fellowship of the Ring", 2001, -1, 'Y', "USA Entertainm.")]

db :: MovieDB
db = (movies, studios, movieExecs)
-------------------------------------------------------------------------------------------------------------------------
-- Task 1
longestColorMovie :: MovieDB -> Length
longestColorMovie (movies, _, _) = snd $ foldr1 (\ m1@(name1, length1) m2@(name2, length2) -> if length1 > length2 then m1 else m2) [ (name, length) | (name, _, length, color, _) <- movies, color == 'Y'] 
-- обхождаме чрез foldr1 всички елементи два по два, за да намерим максималната дължина, след което взимаме името на цветния филм, чиято дължина е максимална

getNameLengthColor :: MovieDB -> ((Char -> Bool), (Int -> Bool)) -> [(Title, Length)]
getNameLengthColor db (f, g) = [ (name, length) | (name, year, length, color, studio) <- movies, length /= longestColorMovie db && length > 0 && f color && g length] 
-- връща списък от заглавията и дължините в минути на всички филми, които удовлетворяват двата предиката и имат дължина, която е известна и различна от тази на най-дългия цветен филм

-------------------------------------------------------------------------------------------------------------------------

-- Task 2
getAllStudios :: MovieDB -> [StudioName]
getAllStudios (_, studios, _) = [ movieName | (movieName, _) <- studios]

getStudiosWithZeroMovies :: MovieDB -> [StudioName]
getStudiosWithZeroMovies (movies, studios, _) = filter (\ zeroStudio -> notElem zeroStudio [ studioName | (studioName, _) <- studios, (movieName, movieYear, _, _, studioName2) <- movies, studioName == studioName2]) (getAllStudios db)

getStudiosAndProducersWithZeroMovies :: [StudioName] -> [(StudioName, Name)]
getStudiosAndProducersWithZeroMovies xs = [ (head xs, producer) | (producer, producerID, _) <- movieExecs, (studioName, producerID2) <- studios, head xs == studioName && producerID == producerID2]
-- с помощта на горните 3 фунции намирам студиата и техните продуценти, които не са продуцирали нито един филм
------------------------------------------------
getStudiosThatProducedInOneYearOnly :: MovieDB -> [(StudioName, Year)] 
getStudiosThatProducedInOneYearOnly (movies, _, _) = concat $ filter (\ x -> length x == 1) $ groupBy (\ x y -> fst x == fst y) $ nub $ nubBy (\ x y -> fst x == fst y && snd x == snd y) [ (studio, year) | (_, year, _, _, studio) <- movies]

getStudiosAndProducersThatProducedInOneYearOnly :: MovieDB -> [(StudioName, Name, Year)]
getStudiosAndProducersThatProducedInOneYearOnly db = helper db xs 
 where 
    xs = (getStudiosThatProducedInOneYearOnly db) 
    getProducer db producerID = head [producer | (producer, producerID2, _) <- movieExecs, producerID == producerID2] 
    helper db xs = [(studio, (getProducer db producerID), year) | (studio, year) <- xs, (studio1, producerID) <- studios, studio == studio1]
-- с помощта на тези 2 функции намирам студиата и продуцентите, които са продуцирали филми само в една и съща година

getStudios :: MovieDB -> ([Year] -> [(StudioName, Name)])
getStudios db = (\ ys -> [ (studio, producer) | (studio, producer, year) <- getStudiosAndProducersThatProducedInOneYearOnly db, elem year ys] ++ noMovieStudios) 
 where 
    noMovieStudios = getStudiosAndProducersWithZeroMovies (getStudiosWithZeroMovies db)
-- ys е списъкът от години, които е елементът на ламбда функцията
-- в нея конкатенираме 2 списъка - единият, който е студиата и продуцентите, работили в една година, като проверяваме дали тази година е част от списъка с години ys
--                               - вторият, който е студиата и продуцентите без нито един филм, които директно конкатенираме винаги, без допълнителна проверка
## Data : https://www.kaggle.com/aaronschlegel/seattle-pet-licenses
install.packages("tidyr")
# Load library
library("tidyr")

# Read csv file
file.csv <- read.csv("C:\\Users\\Caroline\\Google Drive\\ISMIN 2A\\LinkedIn Learning - Linux\\R\\seattle_pet_licenses.csv", header = T)
str(file.csv)

# Get the different specoes
species.freq <- table(file.csv$species) # Creates table from Species

totalOfRecords <- 66042

# Transform the amount into a percentage
species.freq.percentage <- species.freq * 100 / totalOfRecords

fbba <- c("blue", "red", "green")

# Creates barplot in new window
barplot(
        species.freq,
        col = fbba,
        ylim = c(0,50000),
        main = "Species of licensed pets in Seattle\nA survey of 66042 people",
        xlab = "Species",
        border = NA,
        )


# Get the different animal names
animalsName.freq <- table(file.csv$animal_s_name, exclude = "") # Creates table from Animal Names

barplot(
        animalsName.freq[order(animalsName.freq, decreasing = TRUE)][1:10],
        col = fbba,
        ylim = c(0, 600),
        main = "Most famous animal names\nA survey of 66042 animals",
        xlab = "Animal names",
        border = NA,
        )

# Get the cat names
catsNames <- table(file.csv$animal_s_name[file.csv$species == "Cat"], exclude = "") # Creates table from Cat Names

barplot(
        catsNames[order(catsNames, decreasing = TRUE)][1:10],
        col = fbba,
        ylim = c(0, 200),
        main = "Most famous cat names\nA survey of 15795 animals",
        xlab = "Cat names",
        border = NA,
        )

# Get the dog names
dogsNames <- table(file.csv$animal_s_name[file.csv$species == "Dog"], exclude = "") # Creates table from Dog Names

barplot(
        dogsNames[order(dogsNames, decreasing = TRUE)][1:10],
        col = fbba,
        ylim = c(0, 600),
        main = "Most famous dog names\nA survey of 15795 animals",
        xlab = "Dog names",
        border = NA,
        )

# Get the livestock names
livestocksNames <- table(file.csv$animal_s_name[file.csv$species == "Livestock"], exclude = "") # Creates table from Livestock Names

barplot(
        livestocksNames[order(livestocksNames, decreasing = TRUE)][1:10],
        col = fbba,
        ylim = c(0, 2),
        main = "Most famous livestock names\nA survey of 15795 animals",
        xlab = "Livestock names",
        border = NA,
        )

# Get the dog primary breeds
dogsPrimaryBreed <- table(file.csv$primary_breed[file.csv$species == "Dog"], exclude = "") 

barplot(
        dogsPrimaryBreed[order(dogsPrimaryBreed, decreasing = TRUE)][1:10],
        col = fbba,
        ylim = c(0, 7000),
        main = "Most famous dog breeds\nA survey of 15795 animals",
        xlab = "Dog Breeds",
        border = NA,
        cex.names = 0.75
        )

# Get the cat primary breeds
catsPrimaryBreed <- table(file.csv$primary_breed[file.csv$species == "Cat"], exclude = "") 

barplot(
        catsPrimaryBreed[order(catsPrimaryBreed, decreasing = TRUE)][1:10],
        col = fbba,
        ylim = c(0, 15000),
        main = "Most famous cat breeds\nA survey of 15795 animals",
        xlab = "Cat breeds",
        border = NA,
        cex.names = 0.75
        )

# Get the livestock primary breeds
livestocksPrimaryBreed <- table(file.csv$primary_breed[file.csv$species == "Livestock"], exclude = "")

barplot(
        livestocksPrimaryBreed[order(livestocksPrimaryBreed, decreasing = TRUE)][1:5],
        col = fbba,
        ylim = c(0, 50),
        main = "Most famous livestock breeds\nA survey of 15795 animals",
        xlab = "Livestock breeds",
        border = NA,
        )

# Animals / ZipCode
counts <- table(file.csv$zip_code)
barplot(
        counts[order(counts, decreasing = TRUE)][1:30],
        main = "Number of Pets / ZipCode",
        xlab = "ZipCode",
        ylim = c(0, 6000),
        cex.names = 0.75,
        )
)

# Livestocks / ZipCode
countsLivestock <- table(file.csv$zip_code[file.csv$species=="Livestock"])
barplot(
        countsLivestock[order(countsLivestock, decreasing = TRUE)][1:20],
        main = "Number of Livestock / ZipCode",
        xlab = "ZipCode",
        ylim=c(0,10),
        cex.names = 0.75,
        )
)

# Cats / ZipCode
countsCat <- table(file.csv$zip_code[file.csv$species == "Cat"])
barplot(
        countsCat[order(countsCat, decreasing = TRUE)][1:30],
        main = "Number of Cats / ZipCode",
        xlab = "ZipCode",
        ylim = c(0, 2500),
        cex.names = 0.75,
        )
)

# Dog / ZipCode
countsDog <- table(file.csv$zip_code[file.csv$species == "Dog"])
barplot(
        countsDog[order(countsDog, decreasing = TRUE)][1:30],
        main = "Number of Dogs / ZipCode",
        xlab = "ZipCode",
        ylim = c(0, 4000),
        cex.names = 0.75,
       )
)

# Pets / ZipCode
countsAnimalPerZipCode <- table(file.csv$species, file.csv$zip_code)
barplot(
        countsAnimalPerZipCode,
        main = "Number of Pets / ZipCode",
        xlab = "ZipCode",
        ylim = c(0, 4000),
        cex.names = 0.75,
        legend = rownames(countsAnimalPerZipCode),
        col = c("blue", "red", "green"),
        beside = TRUE)
)
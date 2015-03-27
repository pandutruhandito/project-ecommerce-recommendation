library(XML)
library(httr)
library(dplyr)
library(proxy)

#remove productLins #3250 -> homepage lazada
productLinks <- productLinks[-3250]

#prepare data model
urls <- c()
names <- c()
prices <- c()
sizes <- c()
weights <- c()
colors <- c()
layars <- c()
rams <- c()
hds <- c()
kameras <- c()
oss <- c()
processors <- c()


#iterate over productLinks
for (i in 1:length(productLinks)) {
    #get the url
    url <- productLinks[i]
    resp <- GET(url, user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/38.0.2125.104 Safari/537.36"))
    html <- content(resp, as="parsed")
    name <- xpathSApply(html, "//h1[@id='prod_title']", xmlValue)
    specVect <- xpathSApply(html, "//table[@class='specification-table']//td", xmlValue)
    harga <- specVect[match("Harga", specVect)+1]
    harga <- as.numeric(gsub("\\.","",substring(harga,4,nchar(harga))))
    size <- specVect[match("Ukuran (L x W x H cm)", specVect)+1]
    weight <- specVect[match("Berat (kg)", specVect)+1]
    color <- specVect[match("Warna", specVect)+1]
    layar <- specVect[match("Ukuran Layar (in) ", specVect)+1]
    ram <- specVect[match("RAM", specVect)+1]
    hd <- specVect[match("Hard Disk", specVect)+1]
    hd <- ifelse(is.na(hd), specVect[match("Kapasitas Penyimpanan", specVect)+1], NA)
    kamera <- specVect[match("Megapiksel", specVect)+1]
    os <- specVect[match("Sistem Operasi", specVect)+1]
    processor <- specVect[match("Kecepatan Processor", specVect)+1]
    
    if (status_code(resp) == 200) {
        #insert to vectors
        urls <- c(urls, url)
        names <- c(names, name)
        prices <- c(prices, harga)
        sizes <- c(sizes, size)
        weights <- c(weights, weight)
        colors <- c(colors, color)
        layars <- c(layars, layar)
        rams <- c(rams, ram)
        hds <- c(hds, hd)
        kameras <- c(kameras, kamera)
        oss <- c(oss, os)
        processors <- c(processors, processor)
    }
}

kameras <- as.numeric(kameras)
panjangs <- sapply(sizes, function(x) { as.numeric(gsub('^\\s|\\s$',"",tolower(gsub('x',"",substring(tolower(x),1,regexpr('x',tolower(x))[1]))))) })
lebars <- sapply(sizes, function(x) { as.numeric(gsub('^\\s|\\s$',"",tolower(gsub('x','',substring(tolower(x),regexpr('x',tolower(x))[1],regexpr('x( )?[^x]+$',tolower(x))[1]))))) })
tinggis <- sapply(sizes, function(x) { as.numeric(gsub('^\\s|\\s?(kg|cm|Cm|CM|Kg|KG|mm)$',"",tolower(gsub('x','',substring(tolower(x),regexpr('x( )?[^x]+$',tolower(x))[1],nchar(x)))))) })
colorSplit <- function(x) { 
    if (!is.na(x)) {
        if (regexpr(",",x)[1]>-1) { 
            tolower(gsub('^\\s|\\s$',"",unlist(strsplit(x,","))[1]))
        } else if (regexpr("/",x)[1]>-1)  {
            tolower(gsub('^\\s|\\s$',"",unlist(strsplit(x,"/"))[1]))
        } else if (regexpr("-",x)[1]>-1) {
            tolower(gsub('^\\s|\\s$',"",unlist(strsplit(x,"-"))[1]))
        } else if (regexpr(",|/|-",x)[1]==-1) {
            tolower(x)
        }    
    } else {
        NA
    }
}
 
colors <- as.character(sapply(colors, colorSplit))

# color categorizing
colorCat <- function(x) {
    if (is.na(x)) {
        "other"
    } else if (regexpr('black|hitam',tolower(x))[1]>-1) {
        "black"
    } else if (regexpr('putih|white',tolower(x))[1]>-1) {
        "white"
    } else if (regexpr('silver|platinum',tolower(x))[1]>-1) {
        "silver"
    } else if (regexpr('abu|gray|grey',tolower(x))[1]>-1) {
        "grey"
    } else if (regexpr('red|merah',tolower(x))[1]>-1) {
        "merah"
    } else if (regexpr('blue|biru',tolower(x))[1]>-1) {
        "blue"
    } else if (regexpr('hijau|green|ijo|lime|mint',tolower(x))[1]>-1) {
        "green"
    } else if (regexpr('kuning|yellow',tolower(x))[1]>-1) {
        "yellow"
    } else if (regexpr('ungu|violet|purple|purpel',tolower(x))[1]>-1) {
        "purple"
    } else if (regexpr('orange|oranye',tolower(x))[1]>-1) {
        "orange"
    } else if (regexpr('pink',tolower(x))[1]>-1) {
        "pink"
    } else {
        "other"
    }
}
colors <- unlist(sapply(colors, colorCat))

weights <- sapply(weights, function(x) { as.numeric(gsub(' ?k?g(ram)?','',tolower(x))) })
layars <- as.numeric(layars)
# rams <- as.numeric(rams)
# hds <- as.numeric(hds)

splitOs <- function(x) {
    if (is.na(x)) {
        "others"
    } else if (regexpr('android|kitkat|jelly ?bean',tolower(x))[1]>-1) {
        "android"
    } else if  (regexpr('ios',tolower(x))[1]>-1) {
        "ios"
    } else if  (regexpr('windows',tolower(x))[1]>-1) {
        "windows"
    } else if  (regexpr('java',tolower(x))[1]>-1) {
        "java"
    } else if  (regexpr('nokia',tolower(x))[1]>-1) {
        "nokia"
    } else if  (regexpr('blackberry',tolower(x))[1]>-1) {
        "blackberry"
    } else {
        "others"
    }
}

#os categorizing
oss <- sapply(oss,splitOs)


#create data frame
df <- data.frame(urls,prices,oss,layars,weights,kameras,unlist(colors)) #https://www.flickr.com/photos/94549009@N08/14153124106/
df.complete <- df[complete.cases(df),]
names(df.complete)[7] <- "colors"

#turn colors into dummy vars
color.mat <- df.complete$colors
color.mat <- model.matrix(~color.mat)[,-1]
colnames(color.mat) <- gsub("color.mat","",colnames(color.mat))

#turn os into dummy vars
os.mat <- df.complete$oss
os.mat <- model.matrix(~os.mat)[,-1]
colnames(os.mat) <- gsub("os.mat", "", colnames(os.mat))

#append color.mat to df.complete
df.complete <- cbind(df.complete, color.mat)
#append os.mat to df.complete
df.complete <- cbind(df.complete, os.mat)
#remove colors from df.complete
df.complete <- df.complete %>% select(-c(colors,oss))


#create matrix from df
mat.complete <- as.matrix(df.complete %>% select(-urls))
#normalize the matrix through columns
mat.complete <- apply(mat.complete,2,scale)
#distance matrix
#manhattan
dist.mat <- dist(mat.complete, method="Manhattan")
#pushes out rank based on ranking through rows. Make sure that self loop is identified as 
#0 for easiness, thus rank-1
dist.mat.rank <- t(apply(as.matrix(dist.mat),1,rank)-1)



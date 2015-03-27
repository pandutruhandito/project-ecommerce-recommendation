library(XML)

mainUrl <- "http://www.lazada.co.id/beli-handphone/?viewType=listView&page="
until <- 1000
productLinks <- c()

for (i in 1:until) {
    url <- paste0(mainUrl,i)
    html <- htmlTreeParse(url, useInternalNodes = T)
    products <- xpathSApply(html, "//div[@class='product-description']//a/@href")
    productLinks <- c(productLinks, products)
    
}

print(productLinks)
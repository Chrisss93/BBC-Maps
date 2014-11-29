require(stringr)
require(rworldmap)
require(plyr)
require(XML)
require(RCurl)
asia <- c("Afghanistan
Australia
          Bangladesh
          Bhutan
          Brunei
          Myanmar
          Cambodia
          China
          Cook Islands
          East Timor
          Fiji
          French Polynesia
          Guam
          Hong Kong
          India
          Indonesia
          Japan
          Kashmir
          Kazakhstan
          Kiribati
          North Korea
          South Korea
          Kyrgyzstan
          Laos
          Macau
          Malaysia
          The Maldives
          Marshall Islands
          Federated States of Micronesia
          Mongolia
          Nauru
          Nepal
          New Caledonia
          New Zealand
          Niue
          Northern Marianas
          Pakistan
          Palau
          Papua New Guinea
          Philippines
          Samoa
          Singapore
          Sri Lanka
          Solomon Islands
          Taiwan
          Tajikistan
          Thailand
          Tibet
          Tokelau
          Tonga
          Turkmenistan
          Tuvalu
          Uzbekistan
          Vanuatu
          Vietnam
          Xinjiang", "Hong Kong S.A.R.", "Macau S.A.R", "Maldives")
asia <- str_trim(unlist(strsplit(asia, "\n")))
africa <- c("Algeria
            Angola
            Benin
            Botswana
            Burkina Faso
            Burundi
            Cameroon
            Cape Verde
            Central African Republic
            Chad
            Comoros
            Democratic Republic of the Congo
            Republic of the Congo
            Djibouti
            Egypt
            Equatorial Guinea
            Eritrea
            Ethiopia
            Gabon
            Gambia
            Ghana
            Guinea
            Guinea Bissau
            Ivory Coast
            Kenya
            Lesotho
            Liberia
            Libya
            Madagascar
            Malawi
            Mali
            Mauritania
            Mauritius
            Morocco
            Mozambique
            Namibia
            Niger
            Nigeria
            Rwanda
            Sao Tome and Principe
            Senegal
            Seychelles
            Sierra Leone
            Somalia
            South Africa
            South Sudan
            Sudan
            Swaziland
            United Republic of Tanzania
            Togo
            Tunisia
            Uganda
            Zambia
            Zimbabwe", "Zaire", "Western Sahara", "Somaliland")
africa <- str_trim(unlist(strsplit(africa, "\n")))
europe <- c("Albania
            Andorra
            Armenia
            Austria
            Azerbaijan
            Belarus
            Belgium
            Bosnia and Herzegovina
            Bulgaria
            Croatia
            Cyprus
            Czech Republic
            Denmark
            Estonia
            Finland
            France
            Georgia
            Germany
            Greece
            Hungary
            Iceland
            Ireland
            Italy
            Latvia
            Liechtenstein
            Lithuania
            Luxembourg
            Macedonia
            Malta
            Moldova
            Monaco
            Montenegro
            Netherlands
            Norway
            Poland
            Portugal
            Romania
            Russia
            San Marino
            Republic of Serbia
            Slovakia
            Slovenia
            Spain
            Sweden
            Switzerland
            Turkey
            Ukraine
            United Kingdom
            Vatican", "Yugoslavia", "Greenland", "Sardinia",
            "Sicily", "Northern Cyprus", "Kosovo")
europe <- str_trim(unlist(strsplit(europe, "\n")))

middle_east <- c("Algeria
                 Bahrain
                 Egypt
                 Iran
                 Iraq
                 Israel
                 Jordan
                 Kuwait
                 Lebanon
                 Libya
                 Mauritania
                 Morocco
                 Oman
                 Qatar
                 Saudi Arabia
                 Sudan
                 Syria
                 Tunisia
                 United Arab Emirates
                 Yemen", "Gaza", "West Bank")
middle_east <- str_trim(unlist(strsplit(middle_east, "\n")))
latin_america <- c("Antigua and Barbuda
                   Argentina
                   The Bahamas
                   Barbados
                   Belize
                   Bolivia
                   Brazil
                   Chile
                   Colombia
                   Costa Rica
                   Cuba
                   Dominica
                   Dominican Republic
                   Ecuador
                   El Salvador
                   Grenada
                   Guatemala
                   Guyana
                   Haiti
                   Honduras
                   Jamaica
                   Mexico
                   Nicaragua
                   Panama
                   Paraguay
                   Peru
                   Suriname
                   Saint Kitts and Nevis
                   Saint Lucia
                   Saint Vincent and the Grenadines
                   Trinidad and Tobago
                   Uruguay
                   Venezuela", "Saint Helena", "Bermuda", "Cayman Islands", "French Guiana")
latin_america <- str_trim(unlist(strsplit(latin_america, "\n")))
north_america <- c("Canada", "Puerto Rico", "Saint Pierre and Miquelon",
                   "United States of America", "United States Virgin Islands", "American Samoa")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

get <- getURI("http://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations")
doc <- htmlParse(get)
superscript <- getNodeSet(doc, '//*[@id="mw-content-text"]/table/tr/td/sup')
removeNodes(superscript)
tab <- readHTMLTable(doc, stringsAsFactors = FALSE)[[1]]
free(doc)
doc <- NULL
tab$V4 <- NULL
colnames(tab) <- c("Country", "Tag", "Tag2")
clean <- function(x) {
  no_parenthesis <- gsub("\\(|\\)", "", x)
  commas <- gsub('\\/| or |\\;|\\"', "\\,", no_parenthesis)
  return(commas)
}
tab <- data.frame(apply(tab, 2, clean), stringsAsFactors = FALSE)
tab$Tag <- paste(tab$Tag, tab$Tag2, tab$Country, sep = ",")
tab$Tag2 <- NULL
addPluralPosessionStrings <- function(x) {
  tag_vector <- str_trim(unlist(strsplit(x,"\\,")))
  plural_string <- paste0(paste(tag_vector, "s", sep = ""), collapse = ",")
  posessive_string <-paste0(paste(tag_vector,"'s", sep = ""), collapse = ",")
  s_posessive_string <- paste0(paste(tag_vector, "'", sep = ""), collapse = ",")
  plural_posessive_string <-paste0(paste(tag_vector, "s'", sep = ""), collapse = ",")
  all_string <- paste(x, plural_string, posessive_string, s_posessive_string, plural_posessive_string, sep=",")
  return(all_string)
}
tab$Tag <- unlist(lapply(tab$Tag ,addPluralPosessionStrings))

newmap <- getMap()
polys <- slot(newmap, "polygons")
country_names <- unlist(lapply(polys, function(x){x@ID}))

extractCoordinates <- function(x) {
  coord_list <- lapply(polys, function(x) {
    coordinates <- as.data.frame(x@Polygons[[1]]@coords)
    coordinates$Country <- x@ID
    return(coordinates) })
  addNA <- function(x) {
    x[nrow(x)+1,] <- c(NA,NA,NA)
    return(x)}
  coord_df <- ldply(coord_list, addNA)
  coord_df <- coord_df[-nrow(coord_df),]
  colnames(coord_df) <- c("Longitude", "Latitude", "Country")
  return(coord_df)
}
#Extract only coordinates for the countries we have tags for
country_coords <- extractCoordinates(polys)
#Get info about countries
country_info <- newmap@data[c("ADMIN","ADM0_A3", "NAME_FORMA")]
country_info <- data.frame(apply(country_info, 2, as.character), stringsAsFactors=FALSE)
country_info$NAME_FORMA[country_info$NAME_FORMA == country_info$ADMIN] <- NA
colnames(country_info)[1] <- "Country"
country_coords <- join(country_coords, country_info, "Country")

test <- data.frame(Country = country_info$Country, Region=1, stringsAsFactors = FALSE)
test$Region[test$Country %in% asia] <- "asia"
test$Region[test$Country %in% europe] <- "europe"
test$Region[test$Country %in% latin_america] <- "latin_america"
test$Region[test$Country %in% africa] <- "africa"
test$Region[test$Country %in% north_america] <- "us_and_canada"
test$Region[test$Country %in% middle_east] <- "middle_east"
test$Region[test$Country == "United Kingdom"] <- "uk"
test$Region.proper <- mapvalues(test$Region,from = c("asia", "europe", "latin_america", "africa", 
                                                     "us_and_canada","middle_east","uk"),
                                to = c("Asia", "Europe", "Latin America", "Africa", "North America",
                                       "Middle-East", "United Kingdom"))

#Give additional tags to UK, USA
#Find shp lat/long files for Scotland, England, Northern Ireland, Wales instead of just broad UK

old_tag_names <- c("Dem. Republic of the Congo", "Micronesia, Federated States of", "Hong Kong",
                   "Côte d'Ivoire", "Macau", "Burma", "St. Helena", "St. Kitts and Nevis",
                   "St. Lucia", "Serbia", "Bahamas", "St. Vincent and the Grenadines", "Tanzania",
                   "Surinam", "United States", "Saint-Pierre and Miquelon", "People's Republic of China",
                   "São Tomé and Príncipe", "Republic of Macedonia", "Guinea-Bissau")
new_tag_names <- c("Democratic Republic of the Congo", "Federated States of Micronesia", 
                   "Hong Kong S.A.R.", "Ivory Coast", "Macau S.A.R", "Myanmar", 
                   "Saint Helena", "Saint Kitts and Nevis", "Saint Lucia",
                   "Republic of Serbia", "The Bahamas", "Saint Vincent and the Grenadines",
                   "United Republic of Tanzania", "Suriname", "United States of America",
                   "Saint Pierre and Miquelon", "China", "Sao Tome and Principe", "Macedonia",
                   "Guinea Bissau")
tab$Country <- mapvalues(tab$Country, old_tag_names, new_tag_names)
tab$Tag[tab$Country %in% new_tag_names] <- paste(unlist(lapply(tab$Country[tab$Country %in% new_tag_names],
                                                               addPluralPosessionStrings)),
                                                 tab$Tag[tab$Country %in% new_tag_names], sep=",")
tab[nrow(tab)+seq(5),] <- c("Gaza", "West Bank", "Northern Cyprus", "Somaliland", 
                            "United States Virgin Islands", 
                            paste("Gaza","Gaza's", tab$Tag[tab$Country=="Israel"], sep=","),
                            paste("West Bank", "West Bank's", tab$Tag[tab$Country=="Israel"], sep=","),
                            tab$Tag[tab$Country=="Cyprus"],
                            paste("Somaliland", "Somaliland's", tab$Tag[tab$Country=="Somalia"], sep=","),
                            paste("Virgin Islands",  tab$Tag[tab$Country=="United States of America"], sep=","))
joined <- join(test, tab, "Country")
joined <- joined[!(joined$Region==1|is.na(joined$Tag)),]
joined$Color <- mapvalues(joined$Region,from = unique(joined$Region),
                          to = gg_color_hue(length(unique(joined$Region))))

country_df <- join(country_coords, joined, "Country")
country_unique <- country_df[!(duplicated(country_df$Country)|is.na(country_df$Country)),]

scrapeBBC <- function(address) {
  get <- getURI(address)
  doc <- htmlParse(get)
  country_profile <- getNodeSet(doc,'//*[@id="container-country-profiles"]//a')
  country_language <- getNodeSet(doc,'//*[@class="languages"]//a')
  if(!is.null(country_profile)) {removeNodes(country_profile)}
  if(!is.null(country_language)) {removeNodes(country_language)}
  article_name <- xpathSApply(doc, '//*[@id="main-content"]//a', xmlValue)
  article_attributes <- xpathSApply(doc, '//*[@id="main-content"]//a', xmlAttrs)
  article1 <- ldply(article_attributes, function(x){x[names(x) == "href"] })
  article1$title <- article_name
  
  article_text_node <- getNodeSet(doc, '//*[@id="main-content"]//p')
  keep_nodes <- sapply(article_text_node, function(x){
    string <- xmlValue(x, trim = TRUE)
    if(nchar(string) > 0) TRUE else FALSE
  })
  findArticle <- function(node) {
    parent <- xmlAncestors(node, count = 2)[[1]]
    article <- xmlAttrs(parent[[2]][[1]])
    if(is.null(article)) {
      if(!is.null(parent[[2]][[2]])) {
        article <- xmlAttrs(parent[[2]][[2]])
      } else {return(NA)}
    }
    return(article[names(article) == "href"])
  }
  article2 <- ldply(article_text_node[keep_nodes], findArticle)
  text2 <- xpathSApply(doc, '//*[@id="main-content"]//p', xmlValue)
  article2$text <- text2[nchar(text2) > 0]
  article2 <- article2[!is.na(article2$href),]
  free(doc)
  
  article <- join(article1, article2, "href")
  article <- article[!grepl("http", article$href),]
  article$href <- paste("http://www.bbc.com", article$href, sep = "")
  article$title <- str_trim(article$title)
  article$text <- str_trim(article$text)
  article$region <- address
  return(article)
}
reportString <- function(selected) {
  country <- country_unique[country_unique$Country == selected,]
  return(tags$div(tags$strong(selected), 
                  tags$strong(paste("[",country$ADM0_A3,"]", sep="")),
                  if(is.na(country$NAME_FORMA)) {NULL} else{
                    tags$div(country$NAME_FORMA)},
                  tags$div(tags$strong("Region:",span(country$Region.proper,
                                                      style=paste("color",country$Color,sep=":"))))))
}
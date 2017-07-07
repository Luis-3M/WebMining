closeAllConnections()
rm(list=ls())
# R packages
library(rvest)
library(arules)
library(XML)
# Searcher function
searcher <- function(query, limit) {
  type <- "tt"
  imdbURL <- "http://www.imdb.com"
  movieList <- html_session(imdbURL)
  form <- html_form(movieList)[[1]]
  form <- set_values(form, q = query, s = type)
  response <- submit_form(movieList, form)
  responseURL <- response$url
  moviesUrlList <- paste(responseURL,"&ttype=ft&ref_=fn_ft",sep = "")
  
  movName <- read_html(moviesUrlList)
  moviesName <- movName %>% 
    html_nodes(".result_text") %>%
    html_text(trim=T)
  movID <- read_html(moviesUrlList)
  moviesID <- movID %>% 
    html_nodes(".result_text") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  #Build URL for each movie and get all its info !
  f <- "movies_info.txt"
  if(!file.exists(f)) {
    file.create(f, showWarnings = T)
  } else {
    file.remove(f)
  }
  index <- 1
  queryResult <- c()
  for(id in moviesID) {
    movieURL <- read_html(paste(imdbURL,id,sep = ""))
    #Movie's description
    description <- movieURL %>%
      html_nodes(xpath = '//div[@class="summary_text"]/text()[1]') %>%
      html_text(trim=T)
    #Movie's director
    directors <- movieURL %>%
      html_nodes(xpath = '//span[@itemprop="director"]') %>%
      html_text(trim=T)
    #Movie's creators
    creators <- movieURL %>%
      html_nodes(xpath = '//span[@itemprop="creator"]') %>%
      html_text(trim=T)
    #Movie's cast
    cast <- movieURL %>%
      html_nodes("#titleCast .itemprop span") %>%
      html_text(trim=T)
    #RESULT LIST
    if(!missing(limit)) {
      if(index <= limit) {
        write("NAME", file = f, append = T)
        write(moviesName[index], file = f, append = T)
        write("DESCRIPTION", file = f, append = T)
        write(description, file = f, append = T)
        write("DIRECTORS", file = f, append = T)
        write(directors, file = f, append = T)
        write("CREATORS", file = f, append = T)
        write(creators, file = f, append = T)
        write("CAST", file = f, append = T)
        write(cast, file = f, append = T)
        write("--------------------------", file = f, append = T)
        queryResult[[index]] = list(moviesName[index], description, directors, creators, cast)
      }
      else {
        break
      }
    } else {
      write("NAME", file = f, append = T)
      write(moviesName[index], file = f, append = T)
      write("DESCRIPTION", file = f, append = T)
      write(description, file = f, append = T)
      write("DIRECTORS", file = f, append = T)
      write(directors, file = f, append = T)
      write("CREATORS", file = f, append = T)
      write(creators, file = f, append = T)
      write("CAST", file = f, append = T)
      write(cast, file = f, append = T)
      write("--------------------------", file = f, append = T)
      queryResult[[index]] = list(moviesName[index], description, directors, creators, cast)
    }
    index <- index + 1
  }
  if(!missing(limit)) {
    if(limit > length(queryResult)){
      sprintf("Max limit allowed: %d",length(queryResult))
    } else {
      for(i in 1:limit) {
        print(queryResult[[i]])
      }
    }
  } else {
    print(queryResult)
  }
}
#search function-----
#' rsquare_search function
#' 
#' This function accesses the foursquare search venues api and returns a json response and/or a dataframe
#' @param simplify This returns a tidy dataframe, defaults to TRUE, in BETA, parsing list fix in development.
#' @keywords rsquare
#' @export
#' @examples
#' rsquare_search("40.75,-73.98","key/token","20161028",2000, simplify = TRUE)

rsquare_search = function(latlon,token,date,radius, simplify = TRUE){
  if(simplify == TRUE) {
    
    w=paste("https://api.foursquare.com/v2/venues/search?ll=",latlon,"&radius=",radius,"&oauth_token=",token,"&v=",date,sep="")
    print(w)
    u= RCurl::getURL(w)
    test= jsonlite::fromJSON(u)
    
    #pull data
    for(n in 1:length(test$response$venues)) {
      locationid = test$response$venues['id']
      locationname = test$response$venues['name']
      location= test$response$venues$location['address']
      postal_code= test$response$venues$location['postalCode']
      lat = test$response$venues$location['lat']
      long = test$response$venues$location['lng']
      checkinscount = test$response$venues$stats['checkinsCount']
      userscount = test$response$venues$stats['usersCount']
      herenow = test$response$venues$hereNow['count']
      phone = test$response$venues$contact['phone']
      
      #handle categories
      z = 1:length(test$response$venues$categories)
      z = 
        lapply(z, function(y){
          q = 
            tryCatch(
              as.data.frame(test$response$venues$categories[y])[,c('id','name','shortName')],
              error = function(e)
                data.frame(id = NA,name = NA,shortName = NA)
            )
          names(q) = c('category_id','category')
          q
        }) 
      z = rbindlist(z, fill = T, use.names = T)
      
      #bind all
      search_api = as.data.frame(cbind(locationid, locationname, location, postal_code,
                                       lat, long,checkinscount,userscount,herenow,phone,z
      ))
      names(search_api) = tolower(names(search_api))
      search_api
    }
    #add extras
    search_api$query_date = paste(date)
    search_api$pulled_date = Sys.time()
    search_api$x_query = paste(latlon)
    search_api$type_api = 'search'
    search_api$radius = paste(radius)
    #time = gsub("[[:punct:]]", "", Sys.time())
    #filename = paste(time,"search_api",".csv", sep="") 
    #print(filename)
    return(search_api)
  }
  else {
    
    w=paste("https://api.foursquare.com/v2/venues/search?ll=",latlon,"&radius=",radius,"&oauth_token=",token,"&v=",date,sep="")
    print(w)
    u=RCurl::getURL(w)
    test=jsonlite::fromJSON(u)
    return(test)
  }
}

#rsquare_explore----
#' rsquare_explore function
#' 
#' This function accesses the foursquare explore api and returns a json response and/or a dataframe
#' @param simplify This returns a tidy dataframe, defaults to TRUE
#' @keywords rsquare
#' @export
#' @examples
#' rsquare_explore("40.75,-73.98","key/token","20161028",2000, simplify = TRUE)


rsquare_explore = function(latlon,token,date,radius, simplify = TRUE){
  
  if(simplify == TRUE) {
    
    w=paste("https://api.foursquare.com/v2/venues/explore?ll=",latlon,"&radius=",radius,"&oauth_token=",token,"&v=",date,sep="")
    print(w)
    u= RCurl::getURL(w)
    test= jsonlite::fromJSON(u)
    {locationid =""
      locationname=""
      location =""
      lat=""
      long=""
      categories = ""
      checkinscount = ""
      userscount = ""
      beenhere=""
      herenow=""}
    for(n in 1:length(test$response$groups$items)) {
      locationid = test$response$groups$items[[n]]$venue$id
      locationname = test$response$groups$items[[n]]$venue$name
      location= test$response$groups$items[[n]]$venue$location$address
      lat = test$response$groups$items[[n]]$venue$location$lat
      long = test$response$groups$items[[n]]$venue$location$lng
      categories= test$response$groups$items[[n]]$venue$categories
      checkinscount = test$response$groups$items[[n]]$venue$stats$checkinsCount
      userscount = test$response$groups$
        items[[n]]$venue$stats$usersCount
      beenhere = test$response$groups$items[[n]]$venue$beenHere$count
      herenow = test$response$groups$items[[n]]$venue$hereNow$count
      
      explored_api = as.data.frame(cbind(locationid, locationname, location, lat, long, 
                                         checkinscount,userscount, beenhere, herenow))
      print(explored_api)
      categories = jsonlite::rbind_pages(categories)
      categories = categories[, c("id", "name")]
      explored_api = as.data.frame(cbind(explored_api,categories))
    }
    
    explored_api$query_date = paste(date)
    explored_api$pulled_date = Sys.time()
    explored_api$x_query = paste(latlon)
    explored_api$type_api = 'explored'
    explored_api$radius = paste(radius)
    time = gsub("[[:punct:]]", "", Sys.time())
    filename = paste(time,"explored_api",".csv", sep="") 
    print(filename)
    return(explored_api)
  }
  else {
    
    
    w=paste("https://api.foursquare.com/v2/venues/explore?ll=",latlon,"&radius=",radius,"&oauth_token=",token,"&v=",date,sep="")
    print(w)
    u=RCurl::getURL(w)
    test=jsonlite::fromJSON(u)
    return(test)
  }
}





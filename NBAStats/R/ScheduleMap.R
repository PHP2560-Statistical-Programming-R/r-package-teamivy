#NBA schedule map

schedule_map<-function(date){
  #USA map
  usa<-map_data("usa")
  usa_map<-ggplot() + 
    geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "yellow", color = "blue") + 
    coord_fixed(1.3)
  
  #position of arenas
  labs <- data.frame(
    long = c(-122.203092,-118.267233,-81.688141,-122.666119,-71.062151,-86.155528,
             -90.050495,-87.917068,-90.079220,-81.399844,-75.172037,-112.070156,
             -84.392168,-87.674089,-121.499845,-74.016235,-96.815228,-83.055133,
             -80.374820,-97.515083,-111.901235,-80.839322,-105.007694,-73.993407,
             -98.437370,-95.362091,-118.267233,-93.275882,-77.020959,-79.426987,
             -90.079220,-122.353806,-74.016235,-90.079220,-80.839235),
    lat = c(37.750427, 34.043169,41.496762, 45.532069, 42.366437,39.764087,
            35.138423,43.043929,29.952573,28.635790,39.901613,33.445462,
            33.757064,41.880867,38.580936,40.659398,32.791563,42.341150,
            25.675423,35.463528,40.768442,35.225210,39.749438,40.750651,
            29.427211,29.750895,34.043169,44.979680,38.898251,43.634139,
            29.952573,47.622216,40.659398,29.952573,35.225668),
    Home = c("Golden State Warriors", "Los Angeles Lakers", "Cleveland Cavaliers", "Portland Trail Blazers","Boston Celtics","Indiana Pacers","Memphis Grizzlies","Milwaukee Bucks","New Orleans Pelicans","Orlando Magic","Philadelphia 76ers","Phoenix Suns","Atlanta Hawks","Chicago Bulls","Sacramento Kings","Brooklyn Nets","Dallas Mavericks","Detroit Pistons","Miami Heat","Oklahoma City Thunder","Utah Jazz","Charlotte Hornets","Denver Nuggets","New York Knicks","San Antonio Spurs","Houston Rockets","Los Angeles Clippers","Minnesota Timberwolves","Washington Wizards","Toronto Raptors","New Orleans Hornets","Seattle SuperSonics","New Jersey Nets","New Orleans/Oklahoma City Hornets","Charlotte Bobcats"),
    stringsAsFactors = FALSE
  ) 
  
  #make sure the right season
  m<-as.numeric(substr(date,6,7))
  if(m>9&&m<13){
    today<-schedule_table(as.numeric(substr(date, 1, 4))+1)
  }else if(m<7&&m>0){
    today<-schedule_table(as.numeric(substr(date, 1, 4)))
  }else{
    return(usa_map)
  }
  
  #find the schedule for the date
  today<-schedule_table(as.numeric(substr(date, 1, 4))+1)
  today<-today %>%
    filter(Date==date) %>%
    left_join(labs, by="Home")
  
  #show cities that have games on map
  map<-usa_map+ 
    geom_point(data = today, aes(x = long, y = lat), color = "black", size = 3) +
    geom_point(data = today, aes(x = long, y = lat), color = "red", size = 2)+
    geom_text_repel(data=today, aes(x=long, y=lat,label = Home),alpha=1,size=3)
  
  return(map)
}
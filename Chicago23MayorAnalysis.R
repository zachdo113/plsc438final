rm(list=ls())

library(sf)
library(MASS)
library(googlesheets4)

chicago_data <- st_read('/Users/zsdon/Desktop/Politics/DecisionDeskHQ/ChicagoPrecincts/geo_export_5346d2d5-da47-4724-97ab-5b10de3fcea2.shp')
vote_data <- read.csv('/Users/zsdon/Desktop/Politics/DecisionDeskHQ/April23ChicagoResult.csv')

library(tidyverse)
library(stringr)

dem_data <- read.csv('/Users/zsdon/Downloads/ChicagoPrecinctData.csv')
dem_data <- dem_data %>% filter(District==1)
vote_data_prim <- read.csv('/Users/zsdon/Downloads/chicago_precincts_by_candidate.csv')

dem_data$GEOID23 <- sprintf("%05d", dem_data$GEOID23)

dem_data$ward <- str_sub(dem_data$GEOID23,1,2)
dem_data$precinct <- str_sub(dem_data$GEOID23,4,5)

dem_data <- dem_data %>% dplyr::select(Dem_2020_Pres,Rep_2020_Pres,ward,precinct,Total_2020_VAP,Hispanic_2020_VAP,Asian_2020_VAP,Black_2020_VAP)

vote_data_prim$ward <- word(vote_data_prim$ward,2,2)
dem_data$ward <- as.numeric(dem_data$ward)
table(dem_data$ward)
dem_data$precinct <- as.numeric(dem_data$precinct)

dem_data$code <- paste(dem_data$ward, dem_data$precinct, sep = "_")
vote_data_prim$code <- paste(vote_data_prim$ward, vote_data_prim$precinct, sep = "_")

comb_data <- full_join(dem_data, vote_data_prim, by="code")
comb_data <- comb_data %>% filter(!(paul.vallas==0))

comb_data$gop_per <- (comb_data$Rep_2020_Pres) / (comb_data$Rep_2020_Pres+comb_data$Dem_2020_Pres)
comb_data$others <- (comb_data$votes - comb_data$paul.vallas - comb_data$brandon.johnson) / (comb_data$votes)

comb_data$X..5 <- (as.numeric(substr(comb_data$X..5, 1, nchar(comb_data$X..5)-1)))/100

vote_data$code <- paste(vote_data$Ward, vote_data$Precinct, sep = "_")

colnames(vote_data) <- c("Precinct","AprilVotes","JohnsonAprilVotes","Johnson_April_Per","VallasAprilVotes","Vallas_April_Per","Ward","code")

comb_data_final <- full_join(comb_data,vote_data,by="code")

chicago_data$code <-  paste(chicago_data$ward, chicago_data$precinct, sep = "_")
chicago_data <- chicago_data %>% dplyr::select(geometry,code)

comb_data_final <- full_join(comb_data_final,chicago_data,by="code")
comb_data_final <- comb_data_final %>% filter(!is.na(Dem_2020_Pres)) %>% filter(VallasAprilVotes > 0)

#INTERESTING DATA EXPLORATION, FIGURES FOR APPENDIX
plot(comb_data_final$geometry,col=colorRampPalette(c('red4','red','white','blue','navy'))(100)[
  cut(comb_data_final$JohnsonAprilVotes/(comb_data_final$VallasAprilVotes+comb_data_final$JohnsonAprilVotes),breaks=c(0,seq(0.15,0.85,length=98),1))])

title(main="Vallas (Red) vs. Johnson (Blue) Runoff Results")

plot(comb_data_final$geometry,col=colorRampPalette(c('white','black'))(100)[
  cut(comb_data_final$gop_per,breaks=c(0,seq(0.1,0.7,length=98),1))])

title(main="Trump 2020 Vote Share (Dark)")

plot(comb_data_final$geometry,col=colorRampPalette(c('white','black'))(100)[
  cut(comb_data_final$Hispanic_2020_VAP/(comb_data_final$Hispanic_2020_VAP+comb_data_final$Total_2020_VAP),breaks=c(0,seq(0.1,0.7,length=98),1))])

title(main="Hispanic 2020 VAP Share (Dark)")

plot(comb_data_final$geometry,col=colorRampPalette(c('white','black'))(100)[
  cut(comb_data_final$Black_2020_VAP/(comb_data_final$Black_2020_VAP+comb_data_final$Total_2020_VAP),breaks=c(0,seq(0.1,0.7,length=98),1))])

title(main="Black 2020 VAP Share (Dark)")

plot(comb_data_final$geometry,col=colorRampPalette(c('white','black'))(100)[
  cut(comb_data_final$Asian_2020_VAP/(comb_data_final$Asian_2020_VAP+comb_data_final$Total_2020_VAP),breaks=c(0,seq(0.1,0.7,length=98),1))])

title(main="Asian 2020 VAP Share (Dark)")

comb_data_final$AprilVotes <- comb_data_final$JohnsonAprilVotes + comb_data_final$VallasAprilVotes
comb_data_final$votes
comb_data_final$AprilVotes <- as.numeric(gsub(",", "", comb_data_final$AprilVotes))

comb_data_final$turnout_diff <- comb_data_final$AprilVotes/comb_data_final$votes
citywide_diff <- weighted.mean(comb_data_final$turnout_diff,comb_data_final$AprilVotes)
comb_data_final$turnout_diff <-  comb_data_final$turnout_diff/citywide_diff

#NOT THAT INTERESTING DATA EXPLORATION
# plot(comb_data_final$geometry,col=colorRampPalette(c('yellow','white','purple'))(100)[
#   cut(comb_data_final$turnout_diff,breaks=c(0,seq(0.75,1.25,length=98),1))])
# 
# title(main="High (Purple) vs. Low (Yellow) Turnout Change")

comb_data_final$White_2020_VAP <- comb_data_final$Total_2020_VAP - (comb_data_final$Asian_2020_VAP + comb_data_final$Hispanic_2020_VAP + comb_data_final$Black_2020_VAP)

#CITYWIDE TURNOUT MODELING
turnout_model_white <- lm(AprilVotes ~ 0 + White_2020_VAP + Hispanic_2020_VAP + Black_2020_VAP + White_2020_VAP*Hispanic_2020_VAP + Hispanic_2020_VAP*Black_2020_VAP, data = comb_data_final)
summary(turnout_model_white)

turnout_model_asian <- lm(AprilVotes ~ 0 + Asian_2020_VAP, data = comb_data_final, weights = Asian_2020_VAP^2)
summary(turnout_model_asian)

comb_data_final


#CITYWIDE GOODMAN EI
comb_data_final$hisp <- (comb_data_final$Hispanic_2020_VAP*.118 / (comb_data_final$Hispanic_2020_VAP*.118 + comb_data_final$White_2020_VAP*.408 + comb_data_final$Black_2020_VAP*.237 + comb_data_final$Asian_2020_VAP*.296))
comb_data_final$white <- ((comb_data_final$White_2020_VAP*.408 / (comb_data_final$Hispanic_2020_VAP*.118 + comb_data_final$White_2020_VAP*.408 + comb_data_final$Black_2020_VAP*.237 + comb_data_final$Asian_2020_VAP*.296)) + .05) * .95
comb_data_final$black <- (comb_data_final$Black_2020_VAP*.237 / (comb_data_final$Hispanic_2020_VAP*.118 + comb_data_final$White_2020_VAP*.408 + comb_data_final$Black_2020_VAP*.237 + comb_data_final$Asian_2020_VAP*.296))
comb_data_final$asian <- (comb_data_final$Asian_2020_VAP*.296 / (comb_data_final$Hispanic_2020_VAP*.118 + comb_data_final$White_2020_VAP*.408 + comb_data_final$Black_2020_VAP*.237 + comb_data_final$Asian_2020_VAP*.296))

comb_data_final$hisp <- comb_data_final$hisp / (comb_data_final$hisp + comb_data_final$white + comb_data_final$black + comb_data_final$asian)
comb_data_final$white <- comb_data_final$white / (comb_data_final$hisp + comb_data_final$white + comb_data_final$black + comb_data_final$asian)
comb_data_final$black <- comb_data_final$black / (comb_data_final$hisp + comb_data_final$white + comb_data_final$black + comb_data_final$asian)
comb_data_final$asian <- comb_data_final$asian / (comb_data_final$hisp + comb_data_final$white + comb_data_final$black + comb_data_final$asian)

comb_data_final$Vallas_April_Per <- substr(comb_data_final$Vallas_April_Per, 1, nchar(comb_data_final$Vallas_April_Per) - 1)
comb_data_final$Vallas_April_Per <- as.numeric(comb_data_final$Vallas_April_Per)

comb_data_final$Johnson_April_Per <- substr(comb_data_final$Johnson_April_Per, 1, nchar(comb_data_final$Johnson_April_Per) - 1)
comb_data_final$Johnson_April_Per <- as.numeric(comb_data_final$Johnson_April_Per)

model_hisp <- lm(Johnson_April_Per ~ hisp, data = comb_data_final)
summary(model_hisp)
plot(comb_data_final$hisp, comb_data_final$Johnson_April_Per)

model_black <- lm(Johnson_April_Per ~ black, data = comb_data_final)
summary(model_black)
plot(comb_data_final$black, comb_data_final$Johnson_April_Per)

model_white <- lm(Johnson_April_Per ~ white, data = comb_data_final)
summary(model_white)
plot(comb_data_final$white, comb_data_final$Johnson_April_Per)

model_asian <- lm(Johnson_April_Per ~ asian, data = comb_data_final)
summary(model_asian)
plot(comb_data_final$asian, comb_data_final$Johnson_April_Per)

sum_black <- sum(coef(model_black)[1:2]); sum_hisp <- sum(coef(model_hisp)[1:2]); sum_white <- sum(coef(model_white)[1:2]); sum_asian <- sum(coef(model_asian)[1:2])
results_df <- data.frame(avg_sum_black = sum_black, avg_sum_white = sum_white, avg_sum_hisp = sum_hisp, avg_sum_asian = sum_asian)

model_hisp <- lm(Johnson_April_Per ~ hisp + black + white + asian, data = comb_data_final)
summary(model_hisp)
plot(comb_data_final$hisp, comb_data_final$Johnson_April_Per)

model_black <- lm(Johnson_April_Per ~ black + hisp + white + asian, data = comb_data_final)
summary(model_black)
plot(comb_data_final$black, comb_data_final$Johnson_April_Per)

model_white <- lm(Johnson_April_Per ~ white + black + hisp + asian, data = comb_data_final)
summary(model_white)
plot(comb_data_final$white, comb_data_final$Johnson_April_Per)

model_asian <- lm(Johnson_April_Per ~ asian + white + black + hisp, data = comb_data_final)
summary(model_asian)
plot(comb_data_final$asian, comb_data_final$Johnson_April_Per)

sum_black <- sum(coef(model_black)[1:2]); sum_hisp <- sum(coef(model_hisp)[1:2]); sum_white <- sum(coef(model_white)[1:2]); sum_asian <- sum(coef(model_asian)[1:2])
results_df2 <- data.frame(avg_sum_black = sum_black, avg_sum_white = sum_white, avg_sum_hisp = sum_hisp, avg_sum_asian = sum_asian)
results_df <- rbind(results_df, results_df2)



##GEOGRAPHIC KERNAL ANALYSIS
comb_data_final_copy <- comb_data_final

if (!inherits(comb_data_final, "sf")) {
  comb_data_final <- st_as_sf(comb_data_final)
}

comb_data_final <- st_transform(comb_data_final, crs = 32616)
comb_data_final$geometry <- st_make_valid(comb_data_final$geometry)
comb_data_final$coordinates <- st_centroid(comb_data_final$geometry)
comb_data_final$precinct_id <- seq(1, nrow(comb_data_final))

#print(comb_data_final$coordinates)



get_nearby_precincts <- function(target_precinct_id, radius) {
  target_geometry <- st_geometry(comb_data_final[comb_data_final$precinct_id == target_precinct_id, ])
  distances <- st_distance(target_geometry, st_geometry(comb_data_final), by_element = TRUE)
  nearby_indices <- which(distances <= radius)
  nearby_precincts <- comb_data_final[nearby_indices, ]
  
  return(nearby_precincts)
}


get_nearby_precinct_data <- function(target_precinct_id, radius, comb_data_final) {
  target_precinct <- comb_data_final[comb_data_final$precinct_id == target_precinct_id, ]
  if (inherits(target_precinct, "sf") && nrow(target_precinct) == 1) {
    target_geometry <- st_geometry(target_precinct)
    distances <- st_distance(target_geometry, st_geometry(comb_data_final), by_element = FALSE)
    nearby_indices <- which(distances <= radius)
    nearby_precinct_data <- comb_data_final[nearby_indices, c("Johnson_April_Per", "hisp", "white", "black", "asian","AprilVotes")]
    
    return(nearby_precinct_data)
  } else {
    warning("Target precinct ID not found or multiple entries exist.")
    return(NULL)
  }
}


radius <- units::set_units(5000, "meters")

comb_data_final$nearby_data <- lapply(comb_data_final$precinct_id, function(id) {
  get_nearby_precinct_data(id, radius, comb_data_final)
})

####SUCCESFUL LOCAL GEOGRAPHGIC KERNAL CODE

perform_regressions_for_precinct <- function(precinct_data) {
  if (is.null(precinct_data) || nrow(precinct_data) == 0) {
    return(list(sum_black = NA, sum_white = NA, sum_hisp = NA, sum_asian = NA,
                black_ppl = NA, white_ppl = NA, hisp_ppl = NA, asian_ppl = NA))
  }
  
  model_black <- lm(Johnson_April_Per ~ black, data = precinct_data)
  sum_black <- max(min(sum(coef(model_black)),100),0)
  black_ppl <- weighted.mean(precinct_data$black, precinct_data$AprilVotes)
  
  model_white <- lm(Johnson_April_Per ~ white, data = precinct_data)
  sum_white <- min(max(sum(coef(model_white)),0),100)
  white_ppl <- weighted.mean(precinct_data$white, precinct_data$AprilVotes)
  
  model_hisp <- lm(Johnson_April_Per ~ hisp, data = precinct_data)
  sum_hisp <- min(max(sum(coef(model_hisp)),0),100)
  hisp_ppl <- weighted.mean(precinct_data$hisp, precinct_data$AprilVotes)
  
  model_asian <- lm(Johnson_April_Per ~ asian, data = precinct_data)
  sum_asian <- min(max(sum(coef(model_asian)),0),100)
  asian_ppl <- weighted.mean(precinct_data$asian, precinct_data$AprilVotes)
  
  return(list(sum_black = sum_black, sum_white = sum_white, sum_hisp = sum_hisp, sum_asian = sum_asian,
              black_ppl = black_ppl, white_ppl = white_ppl, hisp_ppl = hisp_ppl, asian_ppl = asian_ppl))
}

comb_data_final$regression_results <- lapply(comb_data_final$nearby_data, perform_regressions_for_precinct)

average_regression_sums <- function(regression_results) {
  avg_sum_black <- weighted.mean(sapply(regression_results, function(x) x$sum_black), 
                                 sapply(regression_results, function(x) x$black_ppl), na.rm = TRUE)
  avg_sum_white <- weighted.mean(sapply(regression_results, function(x) x$sum_white), 
                                 sapply(regression_results, function(x) x$white_ppl), na.rm = TRUE)
  avg_sum_hisp <- weighted.mean(sapply(regression_results, function(x) x$sum_hisp), 
                                sapply(regression_results, function(x) x$hisp_ppl), na.rm = TRUE)
  avg_sum_asian <- weighted.mean(sapply(regression_results, function(x) x$sum_asian), 
                                sapply(regression_results, function(x) x$asian_ppl), na.rm = TRUE)
  
  return(data.frame(avg_sum_black = avg_sum_black, avg_sum_white = avg_sum_white, avg_sum_hisp = avg_sum_hisp, avg_sum_asian = avg_sum_asian))
}
comb_data_final$regression_results

overall_avg_sums <- average_regression_sums(comb_data_final$regression_results)

results_df <- rbind(results_df, overall_avg_sums)

perform_regressions_for_precinct <- function(precinct_data) {
  if (is.null(precinct_data) || nrow(precinct_data) == 0) {
    return(list(sum_black = NA, sum_white = NA, sum_hisp = NA, sum_asian = NA,
                black_ppl = NA, white_ppl = NA, hisp_ppl = NA, asian_ppl = NA))
  }
  
  model_black <- lm(Johnson_April_Per ~ black + hisp + white + asian, data = precinct_data)
  sum_black <- max(min(sum(coef(model_black)[1:2]),100),0)
  black_ppl <- weighted.mean(precinct_data$black, precinct_data$AprilVotes)
  
  model_white <- lm(Johnson_April_Per ~ white + black + hisp + asian, data = precinct_data)
  sum_white <- min(max(sum(coef(model_white)[1:2]),0),100)
  white_ppl <- weighted.mean(precinct_data$white, precinct_data$AprilVotes)
  
  model_hisp <- lm(Johnson_April_Per ~ hisp + black + white + asian, data = precinct_data)
  sum_hisp <- min(max(sum(coef(model_hisp)[1:2]),0),100)
  hisp_ppl <- weighted.mean(precinct_data$hisp, precinct_data$AprilVotes)
  
  model_asian <- lm(Johnson_April_Per ~ asian + black + white + hisp, data = precinct_data)
  sum_asian <- min(max(sum(coef(model_asian)[1:2]),0),100)
  asian_ppl <- weighted.mean(precinct_data$asian, precinct_data$AprilVotes)
  
  return(list(sum_black = sum_black, sum_white = sum_white, sum_hisp = sum_hisp, sum_asian = sum_asian,
              black_ppl = black_ppl, white_ppl = white_ppl, hisp_ppl = hisp_ppl, asian_ppl = asian_ppl))
}

comb_data_final$regression_results <- lapply(comb_data_final$nearby_data, perform_regressions_for_precinct)

average_regression_sums <- function(regression_results) {
  avg_sum_black <- weighted.mean(sapply(regression_results, function(x) x$sum_black), 
                                 sapply(regression_results, function(x) x$black_ppl), na.rm = TRUE)
  avg_sum_white <- weighted.mean(sapply(regression_results, function(x) x$sum_white), 
                                 sapply(regression_results, function(x) x$white_ppl), na.rm = TRUE)
  avg_sum_hisp <- weighted.mean(sapply(regression_results, function(x) x$sum_hisp), 
                                sapply(regression_results, function(x) x$hisp_ppl), na.rm = TRUE)
  avg_sum_asian <- weighted.mean(sapply(regression_results, function(x) x$sum_asian), 
                                sapply(regression_results, function(x) x$asian_ppl), na.rm = TRUE)
  
  return(data.frame(avg_sum_black = avg_sum_black, avg_sum_white = avg_sum_white, avg_sum_hisp = avg_sum_hisp, avg_sum_asian = avg_sum_asian))
}


overall_avg_sums <- average_regression_sums(comb_data_final$regression_results)

results_df <- rbind(results_df, overall_avg_sums)

# THE CODE WAS A FAILED ATTEMPT TO PERFORM THE KERNAL REGRESSION
# #comb_data_final$nearby_data
# 
# aggregate_nearby_data <- function(nearby_data) {
#   if (is.null(nearby_data) || nrow(nearby_data) == 0) {
#     return(data.frame(white = NA, black = NA, hisp = NA, asian = NA, average_support = NA))
#   }
# 
#   aggregated_data <- data.frame(
#     white = mean(nearby_data$white, na.rm = TRUE),
#     black = mean(nearby_data$black, na.rm = TRUE),
#     hisp = mean(nearby_data$hisp, na.rm = TRUE),
#     asian = mean(nearby_data$asian, na.rm = TRUE),
#     average_support = mean(nearby_data$Johnson_April_Per, na.rm = TRUE)
#   )
# 
#   return(aggregated_data)
# }
# 
# 
# comb_data_final$aggregated_nearby_data <- lapply(comb_data_final$nearby_data, aggregate_nearby_data)
# head(comb_data_final$aggregated_nearby_data)
# 
# aggregated_data_all <- do.call(rbind, comb_data_final$aggregated_nearby_data)
# #head(aggregated_data_all)
# 
# model_black <- lm(average_support ~ black, data = aggregated_data_all)
# model_hisp <- lm(average_support ~ hisp, data = aggregated_data_all)
# model_white <- lm(average_support ~ white, data = aggregated_data_all)
# summary(model_black)
# 
# sum_black <- sum(coef(model_black)[1:2]); sum_hisp <- sum(coef(model_hisp)[1:2]); sum_white <- sum(coef(model_white)[1:2])
# results_df2 <- data.frame(avg_sum_black = sum_black, avg_sum_white = sum_white, avg_sum_hisp = sum_hisp)
# results_df <- rbind(results_df, results_df2)
# 
# model_black <- lm(average_support ~ black + hisp + white, data = aggregated_data_all)
# model_hisp <- lm(average_support ~ hisp + black + white, data = aggregated_data_all)
# model_white <- lm(average_support ~ white + black + hisp, data = aggregated_data_all)
# summary(model_black)
# 
# sum_black <- sum(coef(model_black)[1:2]); sum_hisp <- sum(coef(model_hisp)[1:2]); sum_white <- sum(coef(model_white)[1:2])
# results_df2 <- data.frame(avg_sum_black = sum_black, avg_sum_white = sum_white, avg_sum_hisp = sum_hisp)
# results_df <- rbind(results_df, results_df2)

# model_black <- lm(average_support ~ black + hisp + white + black*hisp + white*hisp + black*white, data = aggregated_data_all)
# model_hisp <- lm(average_support ~ hisp + black + white + black*hisp + white*hisp + black*white, data = aggregated_data_all)
# model_white <- lm(average_support ~ white + black + hisp + black*hisp + white*hisp + black*white, data = aggregated_data_all)
# summary(model_hisp)
# 
# sum_black <- sum(coef(model_black)[1:2]); sum_hisp <- sum(coef(model_hisp)[1:2]); sum_white <- sum(coef(model_white)[1:2])
# results_df2 <- data.frame(avg_sum_black = sum_black, avg_sum_white = sum_white, avg_sum_hisp = sum_hisp)
# results_df <- rbind(results_df, results_df2)

#plot(aggregated_data_all$hisp, aggregated_data_all$average_support,
#     xlim = c(0, 1),
#     xlab = "Hispanic Population Proportion",
#     ylab = "Average Support")



##NEIGHBORHOOD REGRESSION
comb_data_final <- comb_data_final_copy

#27 is kind of a problem; doesnt fit at all clearly into any of them, the rest are easy
northside <- c(1,40,44,46,47,48,49)
wealthy <- c(2,32,34,39,42,43)
middle <- c(11,13,19,23,27,36,38,41,45,50)
black_ic <- c(3,4,5,6,7,8,9,16,17,18,20,21,24,28,29,37)
hisp_ic <- c(10,12,14,15,22,25,26,30,31,33,35)

nor_dat <- comb_data_final %>% filter(ward.x %in% northside)
wea_dat <- comb_data_final %>% filter(ward.x %in% wealthy)
mid_dat <- comb_data_final %>% filter(ward.x %in% middle)
bla_dat <- comb_data_final %>% filter(ward.x %in% black_ic)
his_dat <- comb_data_final %>% filter(ward.x %in% hisp_ic)

comb_data_final <- comb_data_final %>%
  mutate(
    neighborhood = case_when(
      ward.x %in% northside  ~ "Northside",
      ward.x %in% wealthy    ~ "Wealthy",
      ward.x %in% middle     ~ "Middle Class",
      ward.x %in% black_ic   ~ "Inner City Black",
      ward.x %in% hisp_ic    ~ "Inner City Hispanic",
      TRUE                   ~ "None"  # Default case if none of the above conditions are met
    )
  )

comb_data_final$neighborhood <- as.factor(comb_data_final$neighborhood)

color_palette <- c("Inner City Black" = "lightblue",
                   "Inner City Hispanic" = "red",
                   "Middle Class" = "green",
                   "Northside" = "yellow",
                   "Wealthy" = "purple")

ggplot(data = comb_data_final) +
  geom_sf(aes(geometry = geometry, fill = neighborhood)) +
  scale_fill_manual(values = color_palette) +
  labs(title = "Five Political Neighborhoods of Chicago",
       fill = "Neighborhood") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

summary_df <- comb_data_final %>% 
              group_by(neighborhood) %>%
              summarize(
                votes = sum(AprilVotes),
                voters = sum(Total_2020_VAP),
                turnout = round(votes/voters,3),
                biden_per = round(sum(Dem_2020_Pres) / sum(Dem_2020_Pres + Rep_2020_Pres),3),
                johnson_per = round(sum(JohnsonAprilVotes) / sum(VallasAprilVotes+JohnsonAprilVotes),3),
                white_per = round(weighted.mean(white, AprilVotes), 2),
                black_per = round(weighted.mean(black, AprilVotes), 2),
                hisp_per = round(weighted.mean(hisp, AprilVotes), 2),
                asian_per = round(weighted.mean(asian, AprilVotes), 2)) %>%
              arrange(-votes) %>%
              mutate(proportion = round(votes / sum(votes),2))
print(summary_df)


gt_table <- summary_df %>%
  gt() %>%
  tab_header(title = "Election Summary by Neighborhood") %>%
  cols_label(
    neighborhood = "Neighborhood",
    votes = "Total Votes",
    voters = "Total VAP",
    turnout = "Turnout",
    biden_per = "Biden%",
    johnson_per = "Johnson%",
    white_per = "White Voter %",
    black_per = "Black Voter %",
    hisp_per = "Hispanic Voter %",
    asian_per = "Asian Voter %",
    proportion = "Percent of City"
  ) %>%
  data_color(
    columns = vars(white_per, black_per, hisp_per, asian_per, proportion),
    colors = scales::col_numeric(
      palette = c("white", "black"),
      domain = c(0, 1)
    )
  ) %>%
  data_color(
    columns = vars(biden_per, johnson_per),
    colors = scales::col_numeric(
      palette = c("darkred","white", "darkblue"),
      domain = c(0, 1)
    )
  )

print(gt_table)

black_share <- weighted.mean(summary_df$black_per,summary_df$proportion)
white_share <- weighted.mean(summary_df$white_per,summary_df$proportion)
hisp_share <- weighted.mean(summary_df$hisp_per,summary_df$proportion)
asian_share <- weighted.mean(summary_df$asian_per,summary_df$proportion)


#TONS OF REGRESSIONS, RACE BY NEIGHBORHOOD

#hisp
model_hisp_nor <- lm(Johnson_April_Per ~ hisp, data = nor_dat)
est_model_hisp_nor <- max(min(sum(coef(model_hisp_nor)[1:2]),100),0)
  
model_hisp_wea <- lm(Johnson_April_Per ~ hisp, data = wea_dat)
est_model_hisp_wea <- max(min(sum(coef(model_hisp_wea)[1:2]),100),0)
  
model_hisp_mid <- lm(Johnson_April_Per ~ hisp, data = mid_dat)
est_model_hisp_mid <- max(min(sum(coef(model_hisp_mid)[1:2]),100),0)
  
model_hisp_bla <- lm(Johnson_April_Per ~ hisp, data = bla_dat)
est_model_hisp_bla <- max(min(sum(coef(model_hisp_bla)[1:2]),100),0)

model_hisp_his <- lm(Johnson_April_Per ~ hisp, data = his_dat)
est_model_hisp_his <- max(min(sum(coef(model_hisp_his)[1:2]),100),0)

hisp_voters <- summary_df$hisp_per * summary_df$votes
hisp_est <- (est_model_hisp_bla*hisp_voters[1] + est_model_hisp_mid*hisp_voters[2] + est_model_hisp_nor*hisp_voters[3] + est_model_hisp_his*hisp_voters[4] + est_model_hisp_wea*hisp_voters[5]) / sum(hisp_voters)

#black
model_black_nor <- lm(Johnson_April_Per ~ black, data = nor_dat)
est_model_black_nor <- max(min(sum(coef(model_black_nor)[1:2]),100),0)

model_black_wea <- lm(Johnson_April_Per ~ black, data = wea_dat)
est_model_black_wea <- max(min(sum(coef(model_black_wea)[1:2]),100),0)

model_black_mid <- lm(Johnson_April_Per ~ black, data = mid_dat)
est_model_black_mid <- max(min(sum(coef(model_black_mid)[1:2]),100),0)

model_black_bla <- lm(Johnson_April_Per ~ black, data = bla_dat)
est_model_black_bla <- max(min(sum(coef(model_black_bla)[1:2]),100),0)

model_black_his <- lm(Johnson_April_Per ~ black, data = his_dat)
est_model_black_his <- max(min(sum(coef(model_black_his)[1:2]),100),0)

black_voters <- summary_df$black_per * summary_df$votes
black_est <- (est_model_black_bla*black_voters[1] + est_model_black_mid*black_voters[2] + est_model_black_nor*black_voters[3] + est_model_black_his*black_voters[4] + est_model_black_wea*black_voters[5]) / sum(black_voters)

#asian
model_asian_nor <- lm(Johnson_April_Per ~ asian, data = nor_dat)
est_model_asian_nor <- max(min(sum(coef(model_asian_nor)[1:2]),100),0)

model_asian_wea <- lm(Johnson_April_Per ~ asian, data = wea_dat)
est_model_asian_wea <- max(min(sum(coef(model_asian_wea)[1:2]),100),0)

model_asian_mid <- lm(Johnson_April_Per ~ asian, data = mid_dat)
est_model_asian_mid <- max(min(sum(coef(model_asian_mid)[1:2]),100),0)

model_asian_bla <- lm(Johnson_April_Per ~ asian, data = bla_dat)
est_model_asian_bla <- max(min(sum(coef(model_asian_bla)[1:2]),100),0)

model_asian_his <- lm(Johnson_April_Per ~ asian, data = his_dat)
est_model_asian_his <- max(min(sum(coef(model_asian_his)[1:2]),100),0)

asian_voters <- summary_df$asian_per * summary_df$votes
asian_est <- (est_model_asian_bla*asian_voters[1] + est_model_asian_mid*asian_voters[2] + est_model_asian_nor*asian_voters[3] + est_model_asian_his*asian_voters[4] + est_model_asian_wea*asian_voters[5]) / sum(asian_voters)


#white
model_white_nor <- lm(Johnson_April_Per ~ white, data = nor_dat)
est_model_white_nor <- max(min(sum(coef(model_white_nor)[1:2]),100),0)

model_white_wea <- lm(Johnson_April_Per ~ white, data = wea_dat)
est_model_white_wea <- max(min(sum(coef(model_white_wea)[1:2]),100),0)

model_white_mid <- lm(Johnson_April_Per ~ white, data = mid_dat)
est_model_white_mid <- max(min(sum(coef(model_white_mid)[1:2]),100),0)

model_white_bla <- lm(Johnson_April_Per ~ white, data = bla_dat)
est_model_white_bla <- max(min(sum(coef(model_white_bla)[1:2]),100),0)

model_white_his <- lm(Johnson_April_Per ~ white, data = his_dat)
est_model_white_his <- max(min(sum(coef(model_white_his)[1:2]),100),0)

white_voters <- summary_df$white_per * summary_df$votes
white_est <- (est_model_white_bla*white_voters[1] + est_model_white_mid*white_voters[2] + est_model_white_nor*white_voters[3] + est_model_white_his*white_voters[4] + est_model_white_wea*white_voters[5]) / sum(white_voters)

results_df2 <- data.frame(avg_sum_black = black_est, avg_sum_white = white_est, avg_sum_hisp = hisp_est, avg_sum_asian = asian_est)
results_df <- rbind(results_df,results_df2)


#MORE REGRESSION, WITH CONTROL

#hisp
model_hisp_nor <- lm(Johnson_April_Per ~ hisp + black + white + asian, data = nor_dat)
est_model_hisp_nor <- max(min(sum(coef(model_hisp_nor)[1:2]),100),0)

model_hisp_wea <- lm(Johnson_April_Per ~ hisp + black + white + asian, data = wea_dat)
est_model_hisp_wea <- max(min(sum(coef(model_hisp_wea)[1:2]),100),0)

model_hisp_mid <- lm(Johnson_April_Per ~ hisp + black + white + asian, data = mid_dat)
est_model_hisp_mid <- max(min(sum(coef(model_hisp_mid)[1:2]),100),0)

model_hisp_bla <- lm(Johnson_April_Per ~ hisp + black + white + asian, data = bla_dat)
est_model_hisp_bla <- max(min(sum(coef(model_hisp_bla)[1:2]),100),0)

model_hisp_his <- lm(Johnson_April_Per ~ hisp + black + white + asian, data = his_dat)
est_model_hisp_his <- max(min(sum(coef(model_hisp_his)[1:2]),100),0)

hisp_voters <- summary_df$hisp_per * summary_df$votes
hisp_est <- (est_model_hisp_bla*hisp_voters[1] + est_model_hisp_mid*hisp_voters[2] + est_model_hisp_nor*hisp_voters[3] + est_model_hisp_his*hisp_voters[4] + est_model_hisp_wea*hisp_voters[5]) / sum(hisp_voters)

#black
model_black_nor <- lm(Johnson_April_Per ~ black + white + hisp + asian, data = nor_dat)
est_model_black_nor <- max(min(sum(coef(model_black_nor)[1:2]),100),0)

model_black_wea <- lm(Johnson_April_Per ~ black + white + hisp + asian, data = wea_dat)
est_model_black_wea <- max(min(sum(coef(model_black_wea)[1:2]),100),0)

model_black_mid <- lm(Johnson_April_Per ~ black + white + hisp + asian, data = mid_dat)
est_model_black_mid <- max(min(sum(coef(model_black_mid)[1:2]),100),0)

model_black_bla <- lm(Johnson_April_Per ~ black + white + hisp + asian, data = bla_dat)
est_model_black_bla <- max(min(sum(coef(model_black_bla)[1:2]),100),0)

model_black_his <- lm(Johnson_April_Per ~ black + white + hisp + asian, data = his_dat)
est_model_black_his <- max(min(sum(coef(model_black_his)[1:2]),100),0)

black_voters <- summary_df$black_per * summary_df$votes
black_est <- (est_model_black_bla*black_voters[1] + est_model_black_mid*black_voters[2] + est_model_black_nor*black_voters[3] + est_model_black_his*black_voters[4] + est_model_black_wea*black_voters[5]) / sum(black_voters)

#asian
model_asian_nor <- lm(Johnson_April_Per ~ asian + white + hisp + black, data = nor_dat)
est_model_asian_nor <- max(min(sum(coef(model_asian_nor)[1:2]),100),0)

model_asian_wea <- lm(Johnson_April_Per ~ asian + white + hisp + black, data = wea_dat)
est_model_asian_wea <- max(min(sum(coef(model_asian_wea)[1:2]),100),0)

model_asian_mid <- lm(Johnson_April_Per ~ asian + white + hisp + black, data = mid_dat)
est_model_asian_mid <- max(min(sum(coef(model_asian_mid)[1:2]),100),0)

model_asian_bla <- lm(Johnson_April_Per ~ asian + white + hisp + black, data = bla_dat)
est_model_asian_bla <- max(min(sum(coef(model_asian_bla)[1:2]),100),0)

model_asian_his <- lm(Johnson_April_Per ~ asian + white + hisp + black, data = his_dat)
est_model_asian_his <- max(min(sum(coef(model_asian_his)[1:2]),100),0)

asian_voters <- summary_df$asian_per * summary_df$votes
asian_est <- (est_model_asian_bla*asian_voters[1] + est_model_asian_mid*asian_voters[2] + est_model_asian_nor*asian_voters[3] + est_model_asian_his*asian_voters[4] + est_model_asian_wea*asian_voters[5]) / sum(asian_voters)


#white
model_white_nor <- lm(Johnson_April_Per ~ white + black + hisp + asian, data = nor_dat)
est_model_white_nor <- max(min(sum(coef(model_white_nor)[1:2]),100),0)

model_white_wea <- lm(Johnson_April_Per ~ white + black + hisp + asian, data = wea_dat)
est_model_white_wea <- max(min(sum(coef(model_white_wea)[1:2]),100),0)

model_white_mid <- lm(Johnson_April_Per ~ white + black + hisp + asian, data = mid_dat)
est_model_white_mid <- max(min(sum(coef(model_white_mid)[1:2]),100),0)

model_white_bla <- lm(Johnson_April_Per ~ white + black + hisp + asian, data = bla_dat)
est_model_white_bla <- max(min(sum(coef(model_white_bla)[1:2]),100),0)

model_white_his <- lm(Johnson_April_Per ~ white + black + hisp + asian, data = his_dat)
est_model_white_his <- max(min(sum(coef(model_white_his)[1:2]),100),0)

white_voters <- summary_df$white_per * summary_df$votes
white_est <- (est_model_white_bla*white_voters[1] + est_model_white_mid*white_voters[2] + est_model_white_nor*white_voters[3] + est_model_white_his*white_voters[4] + est_model_white_wea*white_voters[5]) / sum(white_voters)

results_df2 <- data.frame(avg_sum_black = black_est, avg_sum_white = white_est, avg_sum_hisp = hisp_est, avg_sum_asian = asian_est)
results_df <- rbind(results_df,results_df2)


#MORE FIGURES
cor(comb_data_final$asian,comb_data_final$black)
cor(comb_data_final$asian,comb_data_final$hisp)
cor(comb_data_final$asian,comb_data_final$white)
cor(comb_data_final$white,comb_data_final$black)
cor(comb_data_final$white,comb_data_final$hisp)
cor(comb_data_final$hisp,comb_data_final$black)

correlations <- c(-0.4266278, 0.205219, -0.1001006, -0.2776113, -0.2188967, -0.8332217)
groups <- c("Asian-Black", "Asian-White", "Asian-Hispanic","Hispanic-Black","Hispanic-White","White-Black")
correlation_summary <- data.frame(Groups = groups, Correlation = correlations)

correlation_table <- gt(correlation_summary) %>%
  tab_header(
    title = "Voter Group Correlation (by Precinct)"
  ) %>%
  cols_label(
    Groups = "Pairs",
    Correlation = "Correlation"
  ) %>%
  fmt_number(
    columns = vars(Correlation),
    decimals = 2
  ) %>%
  tab_options(
    heading.background.color = "lightblue",
    column_labels.font.size = "larger",
    row_group.font.size = "larger"
  )

print(correlation_table)

results_df$implied_johnson <- white_share*results_df$avg_sum_white + asian_share*results_df$avg_sum_asian + black_share*results_df$avg_sum_black + hisp_share*results_df$avg_sum_hisp

results_df[7,] <- .1*results_df[2,] + .4*results_df[4,] + .5*results_df[6,]
results_df[8,] <- results_df[7,] + 51.4-47.5

#SUMMARY TABLE

library(gt)

data <- data.frame(
  Race = c("Black", "White", "Hispanic", "Asian"),
  Avg_Support_for_Johnson = c(.8708875, .3683825, .4604579, .41444772),
  Avg_Turnout = c(0.237, 0.408, 0.118, 0.296)
)

gt_table <- gt(data) %>%
  tab_header(
    title = "Chicago Mayor Runoff Support by Race",
    subtitle = "Analysis of Voter Support for Brandon Johnson"
  ) %>%
  cols_label(
    Race = "Race",
    Avg_Support_for_Johnson = "Johnson Support (%)",
    Avg_Turnout = "Turnout (as % of VAP)"
  ) %>%
  fmt_percent(
    columns = vars(Avg_Support_for_Johnson, Avg_Turnout),
    decimals = 1
  ) %>%
  tab_options(
    heading.background.color = "lightblue",
    column_labels.font.size = "small",
    row_group.background.color = "lightgray",
    data_row.padding = px(10)
  )

gt_table

results_df <- results_df[1:6,]
print(results_df)

data <- data.frame(
  Model = c(
    "Citywide (Single)", "Citywide (Multiple)", 
    "Neighborhood (Single)", "Neighborhood (Multiple)", 
    "Local (Single)", "Local (Multiple)"
  ),
  Black = c(87.84100, 81.54956, 88.21851, 83.39481, 87.62773, 82.75174),
  White = c(26.71351, 35.32687, 29.15724, 32.24457, 32.72302, 32.41546),
  Hispanic = c(34.67934, 27.74846, 45.67652, 45.01539, 45.71934, 42.12958),
  Asian = c(-8.133134, 29.855439, 16.897515, 34.573163, 31.478862, 40.859925),
  Implied_Johnson_Vote_Share = c(40.66131, 46.17754, 45.41285, 47.26347, 48.44528, 47.39870)
)

data$Black <- data$Black/100
data$White <- data$White/100
data$Hispanic <- data$Hispanic/100
data$Asian <- data$Asian/100
data <- data[,1:5]

gt_table <- gt(data) %>%
  tab_header(
    title = "Average Support for Brandon Johnson by Race Across Models"
  ) %>%
  cols_label(
    Model = "Model",
    Black = "Johnson% (Black)",
    White = "Johnson% (White)",
    Hispanic = "Johnson% (Hispanic)",
    Asian = "Johnson% (Asian)"
  ) %>%
  fmt_percent(
    columns = vars(Black, White, Hispanic, Asian),
    decimals = 1
  ) %>%
  tab_options(
    heading.background.color = "lightblue",
    column_labels.font.size = "small",
    row_group.background.color = "lightgray",
    data_row.padding = px(10)
  )

gt_table


# ##NOT USEFUL DATA PLAYGROUND
# model1 <- lm(VallasAprilVotes ~ 0 + White_2020_VAP + Asian_2020_VAP + Hispanic_2020_VAP + Black_2020_VAP, data = comb_data_final)
# summary(model1)
# comb_data_final$Vallas_April_ExpectedVotes <- predict(model1,comb_data_final)
# 
# model2 <- lm(JohnsonAprilVotes ~ 0 + White_2020_VAP + Asian_2020_VAP + Hispanic_2020_VAP + Black_2020_VAP, data = comb_data_final)
# summary(model2)
# comb_data_final$Johnson_April_ExpectedVotes <- predict(model2,comb_data_final)
# 
# comb_data_final$Johnson_April_ExpectedPer <- comb_data_final$Johnson_April_ExpectedVotes / (comb_data_final$Johnson_April_ExpectedVotes + comb_data_final$Vallas_April_ExpectedVotes)
# comb_data_final$Johnson_April_Per <- comb_data_final$JohnsonAprilVotes / (comb_data_final$JohnsonAprilVotes + comb_data_final$VallasAprilVotes)
# comb_data_final$Johnson_April_PercentVSExpectation <- comb_data_final$Johnson_April_ExpectedPer - comb_data_final$Johnson_April_Per
# 
# plot(comb_data_final$geometry,col=colorRampPalette(c('yellow','white','purple'))(100)[
#   cut(comb_data_final$Johnson_April_PercentVSExpectation,breaks=c(-.4,seq(-.39,.39,length=98),.4))])
# title(main="Vallas (Purple) vs. Johnson (Yellow) Overperformance Compared to Racial Modeling")


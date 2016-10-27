readMouseOx <- function(filename, timebins = FALSE){
    library(dplyr)
    library(ggplot2)
    
    ## Read MouseOx text output file
    mousedata <- read.table(filename, 
                         skip = 4, 
                         quote = "\"", 
                         sep = ",",
                         na.strings = "0",
                         col.names = c("Elapsed Time", "Custom ID", "Subject Number", "File Marker", "Error Code", "Arterial O2 Sat", "Heart Rate", "Breath Rate", "Pulse Distension", "Breath Distension", "Activity", "Core Temperature", "Computer Clock"), 
                         colClasses = c("numeric", "factor", "numeric", "numeric", "numeric", "numeric","numeric", "numeric","numeric", "numeric","numeric", "numeric","character"), 
                         stringsAsFactors = FALSE)

    ## Convert computer clock time to POSIXct
    mousedata$Computer.Clock<-as.POSIXct(strptime(mousedata$Computer.Clock, format = "%m/%d/%Y %I:%M:%S %p"))
    
    if(timebin){
      ## Assign to 5 min bins based on start time; useful for quick look at data but exact injection time preferred for analysis
      length <- round(length(mousedata$Elapsed.Time)/300 + 1)
      mousedata$timebin <- rep(seq(1,length), each = 300)[1:length(mousedata$Elapsed.Time)]
    }
    
    
    return(mousedata)
}

injection_time <- function(mousedata, inj_time){
  mousedata$injtime <- mousedata$Computer.Clock - as.POSIXct(strptime(inj_time, format = "%F %T"))
  mousedata$timebin <- round(mousedata$injtime/300) - 1
  
  return(mousedata)
}

individual_summary <- function(mousedata_raw){
    mousedata_individual_summary <- mousedata_raw %>%  filter(is.na(Error.Code)) %>%
                                        group_by(Custom.ID, timebin,drug) %>%
                                        summarise(mean_breath_rate = mean(Breath.Rate, na.rm = TRUE))
}
    
group_summary <- function(mousedata_individual_summary){
    mousedata_summary <- mousedata_individual_summary %>% filter(!is.nan(mean_breath_rate)) %>%
                                        group_by(drug, timebin) %>%
                                        summarize(overall_mean_breath_rate = mean(mean_breath_rate), overall_mean_breath_rate_sd = sd(mean_breath_rate))

}

plot_ind_summary <- function(mousedata_individual_summary){
  ggplot(mousedata_individual_summary, mapping = aes(timebin, mean_breath_rate, color = as.factor(Subject.Number))) +
    geom_point() +
    geom_line()
}

plot_group_summary <- function(mousedata_group_summary){
    ggplot(mousedata_group_summary, mapping = aes(timebin, overall_mean_breath_rate, color = drug)) +
                                        geom_point() +
                                        geom_line() +
                                        geom_errorbar(mapping = aes(ymax = overall_mean_breath_rate + overall_mean_breath_rate_sd/sqrt(5), ymin = overall_mean_breath_rate - overall_mean_breath_rate_sd/sqrt(5)))
}
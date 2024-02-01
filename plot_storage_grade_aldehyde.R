library(ggplot2)
library(readxl)

cbPalette = c("#AA4499", "#44AA99")

# Read in data
setwd("") 
storage = read_excel("Fluid\ Preservation\ Outcomes\ -\ Public.xlsx", sheet = "Aldehyde")

storage_years = vector()
for(i in 1:nrow(storage)){
  storage_years_vec = storage[i, "Storage duration for reported outcome"]
  split_vec = strsplit(as.character(storage_years_vec), " ")
  if(storage_years_vec == ""){
    storage_years[i] = NA
    next
  }
  storage_years_val = split_vec[[1]][1]
  storage_years[i] = storage_years_val
}

storage$storage_years = as.numeric(storage_years)

storage$Grade = as.numeric(storage$`Consensus grade`)

storage_plot = storage[!is.na(storage$Grade), ]

storage_plot$Grade = gsub("0", "None/Minimal", storage_plot$Grade)
storage_plot$Grade = gsub("1", "Partial", storage_plot$Grade)
storage_plot$Grade = gsub("2", "Severe/Total", storage_plot$Grade)
storage_plot$Grade = factor(storage_plot$Grade, levels =
  c("None/Minimal", "Partial", "Severe/Total"))

storage_plot = storage_plot[!is.na(storage_plot$Grade), ]

storage_plot$Feature = storage_plot$`Observation category predominant`

set.seed(123)
storage_plotted = ggplot(storage_plot, aes(x= storage_years, y = Grade, color = Feature)) +
  geom_point(alpha = 0.25, position = position_jitterdodge(jitter.width = 0.3, jitter.height = 0, dodge.width = 0.5), size = 3) + 
    scale_colour_manual(values = cbPalette, name = "Visualization Method") + 
    ylab("Storage Artifact Severity") + xlab("Storage Duration (Log Scale)") + theme_bw() + 
    scale_x_continuous(trans = "log", 
                       breaks = c(1/12, 0.25, 1, 3, 10, 30, 100), 
                       labels = c("1 month", "3 months", "1 year", "3 years", "10 years", "30 years", "100 years")) + 
    expand_limits(x = c(1/12, 100)) + 
    theme(axis.text=element_text(size=10), axis.title=element_text(size=11), legend.text=element_text(size=10)) + 
    guides(colour = guide_legend(override.aes = list(alpha = 0.6)))



# Functions
# Updated:    2023-11-17
# Auhtors:    Christian Vedel [christian-vs@sam.sdu.dk],
#
# Purpose:    Functions

# ==== Unique_misc_sum ====
# Print summary stats
Unique_misc_sum = function(x, top_n = 10){
  x = x %>% select(any_of(
    c(
      "EventYear",
      "EventMonth",
      "EventDay",
      "EventAge",
      "EventPlace",
      "EventParish",
      "EventMunicipality",
      "EventCounty",
      "EventState",
      "EventCountry"
    )
  ) 
  )
  
  res = lapply(x,
               function(x) {
                 unique(x)
               })
  
  for(i in seq(length(res))){
    name_i = names(res)[i]
    if(length(res[[i]])>top_n){
      cat(name_i,"\n")
      cat(paste(res[[i]][1:top_n], collapse = ", "))
      cat("\n\n")
    } else {
      cat(name_i,"\n")
      cat(paste(res[[i]], collapse = ", "))
      cat("\n\n")
    }
  }
}


# ==== ggsave0 ====
ggsave0 = function(p, name){
  fname1 = paste0("Plots/", name, ".png")
  fname2 = paste0("Slides/Figures/", name, ".png")
  
  ggsave(fname1, plot = p, width = plot_width, height = plot_height)
  ggsave(fname2, plot = p, width = plot_width, height = plot_height)
}




# TABLE FUNCTIONS

present_table <- function(dat) {
    kbl(dat) %>%
        kable_styling(bootstrap_options = c("striped", 
                                            "hover", 
                                            "condensed", 
                                            "responsive"))
}
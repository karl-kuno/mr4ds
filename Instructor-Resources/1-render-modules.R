rmds <- list.files("Student-Resources/rmarkdown/")
rmds <- rmds[1:3]

render_module <- function(rmd) {
  
 path <- paste0("Student-Resources/Handouts/", gsub(".Rmd", "", rmd))
 rmd <- paste0("Student-Resources/rmarkdown/", rmd)
 rmarkdown::render(rmd, output_format = "all", 
                   output_dir = path)
  
}

lapply(rmds, render_module)

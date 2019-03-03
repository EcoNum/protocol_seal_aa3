SciViews::R
render_report <- function(file1) {
  strsplit(file1,  "/") %>.%
    unlist(.) %>.%
    last(.) %>.%
    strsplit(., "[.]") %>.%
    unlist(.) %>.%
    first(.) -> ff
rmarkdown::render(input = "report/0_template.Rmd",
                    params = list(data = file1),
                    output_format = "html_document",
                    output_file = paste(ff,".html", sep=''),
                    output_dir = "report/")
}
render_report("../data/raw/190214B.TXT") # attention le chemin d'accès est par rapport au template et non la base du projet comme les scripts R habituellement.

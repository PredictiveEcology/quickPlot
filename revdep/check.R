#devtools::install_github("r-lib/revdepcheck")
library("revdepcheck")

options(repos = c(CRAN = 'https://cloud.r-project.org'))

revdep_reset()
revdep_check(num_workers = getOption("Ncpus", 2))
revdep_report_cran() ## update cran-comments with this output

### email maintainers of revdep packages (need to edit: `revdep/email.yml`)
#revdep_email(type = "broken") ## will send via gmail
#revdep_email(type = "failed") ## will send via gmail

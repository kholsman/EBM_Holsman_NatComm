# Figshare upload and update data and files
# https://cran.r-project.org/web/packages/rfigshare/README.html
# To download the data open R and enter the following:
# Install data files from a file in [figshare](http://figshare.com) using [rfigshare](https://github.com/ropensci/rfigshare):
# https://github.com/ropensci/rfigshare
require("rfigshare")
fs_author_search("Boettiger")
fs_author_search("Holsman")
fs_auth()
# create file
id <- fs_create("Test title2", "description of test")

data(mtcars)
write.csv(mtcars, "mtcars.csv")
fs_upload(id, "mtcars.csv")


#And we can add the category or categories we like,
fs_category_list()
fs_add_categories(id, c("Education", "Software Engineering"))

# publish privately:
fs_make_private(id)

fs_details(id)


mine <- fs_browse()
mine[1:2]
# download:

## Authenticate if needed
# fs_auth()

## Find article
load('art_id.Rdata')

## Find article info
details <- fs_details("10.6084/m9.figshare.11864505", mine = TRUE)
## Get download URLs
down_info <- fs_download(art_id)

info <- data.frame(url ="https://figshare.com/s/6dea7722df39e07d79f0", 
                   name = sapply(details$files, '[[', 'name'), stringsAsFactors = FALSE)
info
## Download rse file
dir.create('downloaded', showWarnings = FALSE)
download.file(info$url[info$name == 'rse.Rdata'],
              destfile = file.path('downloaded', 'rse.Rdata'))

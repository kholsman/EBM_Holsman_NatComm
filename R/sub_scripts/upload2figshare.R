# Figshare upload and update data and files
# https://cran.r-project.org/web/packages/rfigshare/README.html
# To download the data open R and enter the following:
# Install data files from a file in [figshare](http://figshare.com) using [rfigshare](https://github.com/ropensci/rfigshare):
# https://github.com/ropensci/rfigshare
require("rfigshare")
fs_author_search("Boettiger")

# create file
id <- fs_create("Test title", "description of test")

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


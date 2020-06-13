#plan2.R


plan2 <- drake_plan(
  raw_data = source("data/in/EBM_ceattlenew.Rdata"),
  data     = allDat,
  fig1     = GGplot_aclimTS(dat=allDat,h=2*1.3,w=4.75*1.3,
                            ylabb=expression(paste("Bottom temperature",'('^{o},"C)")),
                            ltyy=c("solid",rep("solid",6)),
                            subtitle_face="plain",
                            plotSet=list(c(1,rcp45_n),c(1,rcp85NoBio_n)),
                            coll=coll_use,tline=2,talpha=.5,
                            xlabb="",lgnpos= "right",plot_marginIN=c(-10,-1,-10,1)),
  fit = 10,
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)


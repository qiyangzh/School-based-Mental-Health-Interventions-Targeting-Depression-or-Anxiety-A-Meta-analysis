########################################################################################################
# PRISMA.R
########################################################################################################
# Create consort diagram for inclusion in manuscript

# use https://onlineconvertfree.com/convert-format/dot-to-png/ to convert file

########################################################################################################
# Initial Set-up
########################################################################################################
# Clear Variables
rm(list=ls(all=TRUE))
test<-require(DiagrammeR)   #grViz()
if (test == FALSE) {
  install.packages("DiagrammeR")
  require(DiagrammeR)
}
test<-require(DiagrammeRsvg)   
if (test == FALSE) {
  install.packages("DiagrammeRsvg")
  require(DiagrammeRsvg)
}
test<-require(rsvg)   
if (test == FALSE) {
  install.packages("rsvg")
  require(rsvg)
}
rm(test)

########################################################################################################
# Pull & Display Consort
########################################################################################################
prisma <- file("PRISMA.dot")
### For high resolution pdf
grViz(diagram = prisma) %>%
export_svg %>% charToRaw %>% rsvg_pdf("PRISMA.pdf")

grViz(diagram = prisma) %>%
export_svg %>% charToRaw %>% rsvg_png("PRISMA.png")
close(prisma)
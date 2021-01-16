disease <- read.csv(file = "diseaseworkbook1.csv")
diseasetb <- as_tibble(disease)
diseases <- diseasetb$Disease

chr <- diseasetb[[1, 2]]
gen <- 'hg19'
from <- diseasetb[[1, 3]]
to <- diseasetb[[1, 4]]

axTrack <- GenomeAxisTrack()
idxTrack <- IdeogramTrack(genome=gen, chromosome=chr, fontsize = 15, fontcolor = "black")

snpLocations <-  UcscTrack(genome = gen, chromosome = chr, 
                           track = "snp151Common", from = from, to = to,
                           trackType = "AnnotationTrack", 
                           start = "chromStart", end = "chromEnd", 
                           id = "name", feature = "func", 
                           strand = "strand", shape = "box", 
                           stacking = "dense", fill = "black",
                           name = "SNPs", background.title = "blue", fontsize = 20, background.panel = "#B9CAFF")

cpgIslands <- UcscTrack(genome = gen, chromosome = chr, 
                        track = "cpgIslandExt", from = from, to = to,
                        trackType = "AnnotationTrack", 
                        start = "chromStart", end = "chromEnd", 
                        id = "name", shape = "box", fill = "#006400", 
                        name = "CpG Islands", background.title = "red", fontsize = 20, background.panel = "#FFB9B9")

plotTracks(list(idxTrack, axTrack, snpLocations, cpgIslands), 
           from = from, to = to, showTitle = TRUE)
plot <- list(idxTrack, axTrack, snpLocations, cpgIslands)
plotTracks(plot,from = from, to = to, showTitle = TRUE )
plot
saveRDS(plot, file = "\\Plots")
?write_rds
for(i in 1:length(diseasetb[1])){
  chr <- diseasetb[[i, 2]]
  gen <- 'hg19'
  from <- diseasetb[[i, 3]]
  to <- diseasetb[[i, 4]]
  axTrack <- GenomeAxisTrack()
  idxTrack <- IdeogramTrack(genome=gen, chromosome=chr, fontsize = 15, fontcolor = "black")
  
  snpLocations <-  UcscTrack(genome = gen, chromosome = chr, 
                             track = "snp151Common", from = from, to = to,
                             trackType = "AnnotationTrack", 
                             start = "chromStart", end = "chromEnd", 
                             id = "name", feature = "func", 
                             strand = "strand", shape = "box", 
                             stacking = "dense", fill = "black",
                             name = "SNPs", background.title = "blue", fontsize = 20, background.panel = "#B9CAFF")
  
  cpgIslands <- UcscTrack(genome = gen, chromosome = chr, 
                          track = "cpgIslandExt", from = from, to = to,
                          trackType = "AnnotationTrack", 
                          start = "chromStart", end = "chromEnd", 
                          id = "name", shape = "box", fill = "#006400", 
                          name = "CpG Islands", background.title = "red", fontsize = 20, background.panel = "#FFB9B9")
  plot <- list(idxTrack, axTrack, snpLocations, cpgIslands)
}
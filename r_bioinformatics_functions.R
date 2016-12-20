# Functions for R Exercises for Bioinformatics | 9/2016

options(digits=10)

load_library <- function(x,dep=TRUE){
  x <- as.character(substitute(x))
  if (!suppressWarnings(require(x,character.only = TRUE))){
    suppressWarnings(install.packages(x,dep=dep))
    if (!suppressWarnings(require(x,character.only = TRUE))){
      source("https://bioconductor.org/biocLite.R")
      suppressWarnings(biocLite(x))
      if(!suppressWarnings(require(x,character.only = TRUE))) stop("Package not found")
    }
  }
}

problem_plot1 <- function(df){
  colnames(df) <- str_to_lower(colnames(df))
  
  ggplot(df,aes(x=disease,y=weight,colour=disease)) + facet_wrap(~sex) + 
    geom_violin(size=1.5,fill=NA,alpha=.6,colour='black') +
    geom_jitter() + 
    theme(legend.position='none')
}

problem_plot2 <- function(OUT){
  colnames(OUT$PLOT$DF) <- str_to_lower(colnames(OUT$PLOT$DF))
  
  ggplot(OUT$PLOT$DF,aes(x,y)) + 
    geom_point() + 
    geom_abline(intercept=OUT$PLOT$W[1],slope=OUT$PLOT$W[2],
                colour='red',size=1.25)
}

problem_plot3 <- function(M,MISSING){
  pc <- prcomp(M,center=TRUE,scale.=TRUE)
  df <- data.frame(pc1=pc$x[,1],pc2=pc$x[,2],sex=rep(c('male','female'),each=nrow(M)/2))
  
  ggplot(df,aes(x=pc1,y=pc2,colour=sex)) + geom_point(alpha=.8,size=2) + 
    geom_point(data=df[c(152,589),],aes(x=pc1,y=pc2),size=6,stroke=2,colour='black',shape=2) +
    geom_point(data=df[MISSING,],aes(x=pc1,y=pc2),size=2,stroke=2,colour='red',shape=3)
}

problem_plot4 <- function(position){
  df <- data.frame(Day=1:length(position),Position=position)
  
  ggplot(df,aes(Day,Position,colour=Position)) + 
    geom_hline(yintercept=0,colour='black') +
    geom_line(size=1)
}

problem_regression <- function(){
  suppressWarnings(rm(list=c('out'),pos='.GlobalEnv'))
  set.seed(1)
  
  N <- 50
  b <- 1.5
  x <- rnorm(N)
  y <- x*b + rnorm(N) 
  
  OUT <<- lm(y ~ x)
  
  cat('Added OUT to environment.')
}

problem_dataframe <- function(){
  suppressWarnings(rm(list=c('sex','weight','disease'),pos='.GlobalEnv'))
  set.seed(1)
  
  N <- 25
  SEX <<- rep(c('M','F'),each=N)
  WEIGHT <<- c(rnorm(8,250,35),rnorm(17,190,20),rnorm(25,125,12))
  DISEASE <<- c(rep(1:0,c(10,15)),rep(1:0,c(5,20)))
  
  cat('Added SEX, WEIGHT, and DISEASE to environment.')
}

problem_matrix <- function(){
  suppressWarnings(rm(list=c('M1','M2'),pos='.GlobalEnv'))
  set.seed(1)
  
  N <- 2000
  W <- c(rnorm(N/2,200,25),rnorm(N/2,135,15))
  H <- c(rnorm(N/2,173,9),rnorm(N/2,165,10))
  A <- floor(c(runif(N/2,18,74),runif(N/2,18,79)))
  S <- floor(c(rnorm(N/2,140,10),rnorm(N/2,130,8)))
  G <- c(rnorm(N/2,100,20),rnorm(N/2,90,15))
  
  W[152] <- NA
  G[589] <- NA
  
  M <- cbind(W,H,A,S,G)
  
  M1 <<- M[1:(N/2),]
  M2 <<- M[(N/2 + 1):N,]
  
  cat('Added M1 and M2 to environment.')
}

problem_forloop2 <- function(){
  suppressWarnings(rm(list=c('M'),pos='.GlobalEnv'))
  
  MM <- matrix(0,25,3)
  MM[1,] <- c(1,3,5)
  
  M <<- MM
  
  cat('Added M to environment.')
}

problem_forloop3 <- function(){
  suppressWarnings(rm(list=c('M'),pos='.GlobalEnv'))
  
  MM <- matrix(0,25,3)
  MM[1,1] <- 1

  M <<- MM  
  
  cat('Added M to environment.')
}

problem_ifelse1 <- function(){
  suppressWarnings(rm(list=c('L'),pos='.GlobalEnv'))
  set.seed(1)
  
  L <<- sample(LETTERS,100000,replace=TRUE)
  
  cat('Added L to environment.')
}

is_even <- function(n) n %% 2 == 0

is_raining <- function() {sample(0:1,1,prob=c(.6,.4)) == 1}

vector_of_animals <- c('Aardvark','Albatross','Alligator','Alpaca','Ant','Anteater','Antelope','Ape','Armadillo','Ass','Baboon','Badger','Barracuda','Bat','Bear','Beaver','Bee','Bison','Boar','Buffalo','Galago','Butterfly','Camel','Caribou','Cat','Caterpillar','Cattle','Chamois','Cheetah','Chicken','Chimpanzee','Chinchilla','Chough','Clam','Cobra','Cockroach','Cod','Cormorant','Coyote','Crab','Crane','Crocodile','Crow','Curlew','Deer','Dinosaur','Dog','Dogfish','Dolphin','Donkey','Dotterel','Dove','Dragonfly','Duck','Dugong','Dunlin','Eagle','Echidna','Eel','Eland','Elephant','Elephant seal','Elk','Emu','Falcon','Ferret','Finch','Fish','Flamingo','Fly','Fox','Frog','Gaur','Gazelle','Gerbil','Giant Panda','Giraffe','Gnat','Gnu','Goat','Goose','Goldfinch','Goldfish','Gorilla','Goshawk','Grasshopper','Grouse','Guanaco','Guinea fowl','Guinea pig','Gull','Hamster','Hare','Hawk','Hedgehog','Heron','Herring','Hippopotamus','Hornet','Horse','Human','Hummingbird','Hyena','Jackal','Jaguar','Jay','Jay, Blue','Jellyfish','Kangaroo','Koala','Komodo dragon','Kouprey','Kudu','Lapwing','Lark','Lemur','Leopard','Lion','Llama','Lobster','Locust','Loris','Louse','Lyrebird','Magpie','Mallard','Manatee','Marten','Meerkat','Mink','Mole','Monkey','Moose','Mouse','Mosquito','Mule','Narwhal','Newt','Nightingale','Octopus','Okapi','Opossum','Oryx','Ostrich','Otter','Owl','Ox','Oyster','Panther','Parrot','Partridge','Peafowl','Pelican','Penguin','Pheasant','Pig','Pigeon','Pony','Porcupine','Porpoise','Prairie Dog','Quail','Quelea','Rabbit','Raccoon','Rail','Ram','Rat','Raven','Red deer','Red panda','Reindeer','Rhinoceros','Rook','Ruff','Salamander','Salmon','Sand Dollar','Sandpiper','Sardine','Scorpion','Sea lion','Sea Urchin','Seahorse','Seal','Shark','Sheep','Shrew','Shrimp','Skunk','Snail','Snake','Spider','Squid','Squirrel','Starling','Stingray','Stinkbug','Stork','Swallow','Swan','Tapir','Tarsier','Termite','Tiger','Toad','Trout','Turkey','Turtle','Vicuña','Viper','Vulture','Wallaby','Walrus','Wasp','Water buffalo','Weasel','Whale','Wolf','Wolverine','Wombat','Woodcock','Woodpecker','Worm','Wren','Yak','Zebra')

sample_animal <- function() sample(vector_of_animals,1)

generate_dna_sequence_s <- function(){
  suppressWarnings(rm(list=c('s'),pos='.GlobalEnv'))
  
  s <<- 'CATGAGCTAGTCACCTCAGTGAAATGGGAACCAGATGGCAGATCGGAAACTATCGCCCTAGGCACTACGGATGTAGCAATGATCGTTGCGAATCTATGCCGGGTGGTAGGGGTCGAGTCTCTAAATTGATTATGGTACGCTGTGGCTGTCAATACGGTCAAGCACATCAGATTTCAGGGTCCCCCCGAGCGGGAATCGCAACTGTATGCGTGTCGTTATCGCATCGGCACTACCGCGGACTGCGACTAATATATTTTCTTACCGTTAAACTCAGAGCACCTATTTAGCCGGCTTGGTGTCCCTGTATCATACGTACCCCCGTTACCTTATTAGTTGTGGCTAAAATGACTCTTGTGGGGTCGAACTTTAAAGCTTTGGCGGACGGGCGTGCCGCAAAGGTCAATAAGGTAAATGAATGGATACTCTTTGCCTATTCCTTTGTGAACAGTCAGGATTTTATCTCGGGGAACTTTACCTGTTGGCACCGGGCAGAGACAAACGGACGATGATAAATACGTACTACGCTGCGTAAAGCAGGTACATTCCACGATGGTGTATCATGCGAGAGCGCGACTTCCTGGCTCAAACAATCAGGCGCGTGCCGTAAGCCTCTACCCAACGAACCGTGCTGACTGCTTTATCTAAGTCTTCTCTGGCTCACCACGCCCCGTGTCGTATAACTCCTGACAAATCCTGGACGATCAAGCATGAGGTTCAAAGTAAGATACTACCTATCTTGCTCGAATCCGGCCAATTAATGTTAGAAAACGCCCACTGAAACAGCCGGTCCAGTCCCCAACTAGGCGCCGCACACGTTCGCGGTCACAATCCATGGAACGCGTAGTACTGAGGCCGAAACAGACGCTGGGGGGGTCGCTGTACTAGGGAAAACGACTGAAGGATACACGGATGATCTAGCAACGATGTAAAA'

  cat('Added s to environment.')
}

generate_random_dna_sequence_s <- function(len=10,p=c(.4,.2,.1,.3),seed){
  suppressWarnings(rm(list=c('s'),pos='.GlobalEnv'))
  
  if (missing(seed)) seed <- sample(1:99999,1)
  set.seed(seed)
  
  s <<- paste0(sample(c('A','C','G','T'),len,replace=TRUE,prob=p),collapse='')
  
  cat('Added s to environment (seed=',seed,').',sep='')
}

generate_random_dna_reads_S <- function(len=10,p=c(.4,.2,.1,.3),N=10,seed){
  suppressWarnings(rm(list=c('S'),pos='.GlobalEnv'))
  
  if (missing(seed)) seed <- sample(1:99999,1)
  set.seed(seed)
  
  S <<- replicate(N,paste0(sample(c('A','C','G','T'),len,replace=TRUE,prob=p),collapse=''))
  
  cat('Added S to environment (seed=',seed,').',sep='')
}

generate_random_dna_gc_s <- function(len=10,seed){
  suppressWarnings(rm(list=c('s'),pos='.GlobalEnv'))
  
  if (missing(seed)) seed <- sample(1:99999,1)
  set.seed(seed)
  
  p1 <- runif(4,.2,.3)
  p1 <- p1/sum(p1)
  p2 <- runif(4,.15,.35)
  p2 <- p2/sum(p2)
  p3 <- runif(4,.2,.3) + c(0,runif(2,0,.3),0)
  p3 <- p3/sum(p3)
  p <- rbind(p1,p2,p3)
  
  s <<- paste0(sapply(seq_len(len), function(i) sample(c('A','C','G','T'),1,prob=p[((i-1) %% 3)+1,])),collapse='')
  
  cat('Added s to environment (seed=',seed,').',sep='')
}

generate_random_dna_skew_s <- function(len=100,w=1,seed){
  
  suppressWarnings(rm(list=c('s'),pos='.GlobalEnv'))
  
  if (missing(seed)) seed <- sample(1:99999,1)
  set.seed(seed)
  
  if (w < 0) p_c <- abs(w)*seq_len(len)/max(len)/10 + rnorm(len,0,.1) else p_c <- rnorm(len,0,.1)
  if (w > 0) p_g <- abs(w)*seq_len(len)/max(len)/10 + rnorm(len,0,.1) else p_g <- rnorm(len,0,.1)
  p_a <- rnorm(len,0,.1)
  p_t <- rnorm(len,0,.1)
  
  p_mat <- rbind(p_a,p_c,p_g,p_t)
  p_mat[p_mat <= 0] <- .001
  p_mat <- t(t(p_mat)/colSums(p_mat))
  
  s <<- paste0(sapply(seq_len(len),function(i) sample(c('A','C','G','T'),1,prob=p_mat[,i])),collapse='')
  
  cat('Added s to environment (seed=',seed,').',sep='')
}

plot_skew <- function(x){
  x <- as.vector(x)
  df <- data.frame(Window=1:length(x),Skew=x)
  
  ggplot(df, aes(x=Window,y=Skew)) + 
    geom_point(size=2) + geom_line(size=1) +
    stat_smooth(method='lm',color='red',se=FALSE) + 
    labs(y='GC Skew') 
}

problem_createsequencesets <- function(){
  suppressWarnings(rm(list=c('s1','s2','s3'),pos='.GlobalEnv'))
  
  s1 <<- 'GCTGTAGGAACAGCAGTCTTGGTGGTTAGCA'
  s2 <<- 'GGCATTGCAAAATTTATTACACCCCCAGATC'
  s3 <<- 'TATCCTTGCAATACTCTCCGAACGGGAGAGC'
  
  cat('Added s1, s2, and s3 to environment.')
}

problem_metadata <- function(fasta){
  suppressWarnings(rm(list=c('META'),pos='.GlobalEnv'))
  set.seed(1)
  
  META <<- data.frame(ID=names(fasta),
                     Organism=rep(c('Bacteria','Human'),c(80,20)),
                     Genus=c(sample(c('Escherichia','Entercoccus','Staphylococcus','Haemophilus'),80,replace=TRUE),rep('Homo',20)),
                     Center=c(sample(c('Houston','Dallas','Phoenix','Austin'),80,replace=TRUE),
                              sample(c('New York','Princeton','Philadelphia'),20,replace=TRUE)),
                     stringsAsFactors=FALSE)
  
  cat('Added META to environment.')
}

problem_hamming <- function(){
  suppressWarnings(rm(list=c('s1','s2'),pos='.GlobalEnv'))
  
  s1 <<- 'ACTGTACCAGAATCGCTATTAGCCCACCTTAGGCGAGTGAAATAACCAAATAAACAAGTGGTGAGGGGAATTGTCCCCACCGTTGCGTTTATGGAGGGGGTGGAAGTGGCCACGAACTGCCAGGTGTCGCCAAACGGAAGACTTCGGGCTTTAGATCCGACTTAACTAACATTTTTCCACCATGAAAGGAGCAATTCAAAGCAACGTAAGGTACTTGCCTGGCCAGGTTGATAAAAGATGCGGACGTCTGATGATGTACGATGATCTTGGCGAGTCAAACCCGGGGACCCCGAGCCGTGACCTAGAGATTGCAATACAGTAAGTAGCCAGGAAAGGAGGATACGATATAAATTAGGGTCACTGTACCCGTTCCGCCTTTCTGCGGCCAAAGACCCGCACGACACATGGACGCCACAGAGGCTATTTGGACCGATGACTCAGGATCATCAAGGGCGACGACGTTAGTCAGTTATATCTGACATTGGATATGTTATAAATAAAACTGGTAACCCACAACGATCCCGGTAGTGGGGACACTGGCCAGGCTTCTAAGCAGATGCGAGGCACAGACACAAACCGGCCGTATGTCAGAGGCAGTACTGAAGTCTAACTTTATCCACGGCAGACGCGTTACATGGCAATCTTGAGCGGGGCGAAGTTAGAGACGTTAAGCTATATGAAACACACTCGGCGTAGCCAATAGCCCATCTGCCTCATAAGGATGGCTGGTTCATTTGTAAAATACTGTATCAGGCGGGGGTAACCTCCCGCGCTCAGGTAATATAATGAGACTGGTACCCATAACACGTTTTCGTCAGTAATAAAAGCGCGATCATTCAAGGGGACGATAGCAGACCTTCAATGCGGAATGGTTTTGCGCCTCTAATAACTGAGAGCACTATAATAGAAGTGAGTGTATTGTTATGCCATCCT'
  s2 <<- 'GCTGGAGCGAACTGGACATCAACCCTACTAAGGGAAGAAAATTGGAATAATCATCAGGTACTGAGAACACACGACCCCACCGTTGAGGTTTCGACAGCTTGAATTCTTACAAGGGCTAGCCGTTGCTGGCTAGCCATTTGTCAGAGTGTCTAAGAGCAGACTAAACTACCCTCGTTCCTATATAAACGAAGCTACTACCAGCAAGGTCCGGAACGCAACTCTCAGGATTGATGGGATGTGCACATTTCGTTTGGAGTTGCCGGATAGACGCTCGCAATCCTCTTCGACCACACGGAATGACGCGCCGGTCGCTCCAAATATAGTAGCCCGGGGCGGAAGAGGCGAAACTATATCAGCTTCCGGAACAGATGTCGTTTCTATGGCGCCTTCAAAGAGTGCGACGCCATGCAATCGTACATGCGCCTAGAACTCCTGGATGAGGATCTTTAAGAACGACTGGGTAAGCTAGATACATTTCAGTCTGGTTATGGTCTAAGTAGAACAGGTAACCCATGTACATTCAAATGTATGAGTGCCGGTCCATGCTTATCGGTTAACATGACCAACATCGAAACTATGGTGCGAGTTTAAGTGCAATACCAAAGGCCAAATGAGCGCACTCCATAGGCTGACCCGCATAATAATGGTCTGGTCAAAGGATTAGAAGTAAGATGCTACGATAAGCTTCCCGCGGTCACGATGACCCTTTAGCCTCACCTTATTAAATGGCTAAATTTTTTATAACTGCTGCGGGCAGGGGAACCCAGCCGAGCTCCGTGGATTTACCGAGGCCGCTTACAGTCACATGTTTATGTCAACAACTTGTGCTCGAGTATGCGAGGGCCCTATTGAATCTCGGAAATGTGGCTTGGTTGTCGACCTCTCTAGTCCCGTAGTTCTCAACTCGGAATGGATGGCTGAAACATACGTCCT'

  cat('Added s1 and s2 to environment.')
}

problem_substrings <- function(){
  suppressWarnings(rm(list=c('s','S'),pos='.GlobalEnv'))
  
  S <<- 'GTTAGTACCACGGTCTATTAGGTCTATGGGTCTATAGGTCTATCACAGGTCTATTGGGGTCTATCAGGTCTATGGTCTATCTGGTCTATAGGTCTATAGGTCTATCGGGTCTATCGGTCTATTACGACACGTGCGGTCTATAGAGGTCTATACCAGGTCTATTAAGGTCTATGGCGGGAACACGGTCTATATCAAGGTCTATCCAAAAGGGTCTATTATAGGGTCTATGGTCTATGGTCTATTTTAGGTCTATTGGTCTATCGGTCTATAAGGTCTATGGTCTATCGGGTCTATATGAGAGATGTGTACAACCGGTACTTGAGGTCTATCGGTCTATAGGTCTATATGCGGTCTATGTCGGTCTATCAATCGGTCTATATAGTAACCACATGGTCTATTGGTCTATGGTCTATCTTGGTCTATCGGGTCTATGGTCTATAACAGGTCTATGTGCGGTCTATGGTCTATTGGTCTATAAGCGGGTCTATGGTCTATGGTCTATGGAAGTGGTCTATCTCAGCAGGTCTATGGTCTATGGTCTATGGTCTATACGGTCTATGAAGGGTCTATGCTAGGTCTATGGTCTATCAGGTCTATGGTCTATGGTCTATTGGTCTATAGGTCTATAGAGGTCTATGGTCTATGCGTGGTCTATGGTCTATCGTAGTTGGTCTATGGTCTATCCGGTCTATATCGGTCTATCAAGGTCTATCACTGGTCTATGGTCTATGGTCTATCCGGTCTATTTGGTCTATTAACACGCGAGGCGGTCTATGGTCTATTGGTCTATGTCGATGGTCTATGGTCTATGGTCTATGTCAGGTCTATAGGTCTATGGTCTATAGAGGGTCTATGGTCTATGGTCTATTTGGTCTATTCGTGCTGGTCTATTCGAAGGTCTATGGTCTATTAGGTCTATAGAGTGGTCTATCGATGCGTAATCGGTCTATCGCTAGGGGTCTATTATCTCTCG'
  s <<- 'GGTCTATGG'
  
  cat('Added s and S to environment.')
}

problem_ttratio <- function(){
  suppressWarnings(rm(list=c('s1','s2'),pos='.GlobalEnv'))
  
  s1 <<- 'GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGAAGTACGGGCATCAACCCAGTT'
  s2 <<- 'TTATCTGACAAAGAAAGCCGTCAACGGCTGGATAATTTCGCGATCGTGCTGGTTACTGGCGGTACGAGTGTTCCTTTGGGT'
  
  cat('Added s1 and s2 to environment.')
}

aa_table_translation <- function(){
  suppressWarnings(rm(list=c('CODONS'),pos='.GlobalEnv'))
  
  codons <- as.matrix(read.table('https://gist.githubusercontent.com/sw1/8870a124624f31585d5c15be72fcfc21/raw/e7a118690bf0dda64501e919268c990162f9590c/aa_table.dat'))
  codons <- rbind(codons[,1:2],codons[,3:4],codons[,5:6],codons[,7:8])
  codons <- as.data.frame(codons)
  colnames(codons) <- c('codon','aa')
  rownames(codons) <- codons$codon
  codons$codon <- as.character(codons$codon)
  codons$aa <- as.character(codons$aa)
  
  CODONS <<- codons
  
  cat('Added CODONS to environment.')
}

problem_translation <- function(){
  suppressWarnings(rm(list=c('s'),pos='.GlobalEnv'))
  
  s <<- scan('https://gist.githubusercontent.com/sw1/8870a124624f31585d5c15be72fcfc21/raw/fb882ce8f00048a1718aa8c8b70839f427995cbc/problems_translation.dat',what = 'char')
  
  cat('Added s to environment.')
}

cleanup_feat_table <- function(x){
  xx <- x$GBFeature_quals 
  xxx <- matrix(unlist(xx),ncol=2,byrow=TRUE)
  xxxx <- xxx[,2]
  names(xxxx) <- xxx[,1]
  xxxx
}

# Designer:
# https://github.com/broadinstitute/SatMut_VariantLibrary_Designer/releases/tag/v1.10 

##some common enzymes
bamh1<-'GGATCC-BamHI' 
mlu1<-'ACGCGT-MluI'
nhe1<-'GCTAGC-NheI' 
ecor1<-'GAATTC-EcoRI'
nde1<-'CATATG-NdeI' 
hind3<-'AAGCTT-HindIII' 
xba1<-'TCTAGA-XbaI'
spe1<-'ACTAGT-SpeI'
ecor5<-'GATATC-EcoRV' 

########## FUNCTION: 'installPackage' ##########
# This 'installPackage' function is for checking if a given package has
# been installed and, if not, installing it.

installPackage <- function(x){
    x<-as.character(x)
    if(x %in% as.vector(as.character(rownames(installed.packages())))==FALSE) {
      if(x %in% as.vector(as.character(rownames(available.packages())))==FALSE) {
      paste0(x," is not a valid package and cannot be installed - please check again...")
    } else {
      install.packages(x)           
      }
    
    } else {
      paste0(x," package already installed...")
    }
  }

########## The end of FUNCTION: 'installPackage' ##########

installPackage("tidyverse")
installPackage("stringdist")

library(tidyverse)
library(stringdist)

####################################################################
#                                                                  #
#      THIS DESIGNER NEEDS 'tidyverse' and 'stringdist' PACKAGES   #
#      ON YOUR MACHINE. MAKE SURE YOU HAVE MANUALLY INSTALLED      #
#      THEM OR THE ABOVE CODES CAN DO SO                           #
#                                                                  #
####################################################################


#################### set up the designer, then run it ################### 
## Below you need to populate 6 variables. Then ready to run the designer.

# 1. templateSeq
# 2. cloningSitesToEx
# 3. gene
# 4. codonRedunt
# 5. cdnPosToChange
# 6. folder_to_write
###################

# 1. templateSeq
# From the library expression clone configuration, in format of 
# Left_flank[start..ORF..stop]right_flank. Make sure the ORF 
# sequence is marked by the brackets [ ], and the 2 flanks are 
# at least 6 bases each.

templateSeq=as.character("TTCAGGTGTCGTGAGGCTAGCCATATGCCACC[ATGGCACAGTCAGTGCTGGTACCGCCAGGACCTGACAGCTTCCGCTTCTTTACCAGGGAATCCCTTGCTGCTATTGAACAACGCATTGCAGAAGAGAAAGCTAAGAGACCCAAACAGGAACGCAAGGATGAGGATGATGAAAATGGCCCAAAGCCAAACAGTGACTTGGAAGCAGGAAAATCTCTTCCATTTATTTATGGAGACATTCCTCCAGAGATGGTGTCAGTGCCCCTGGAGGATCTGGACCCCTACTATATCAATAAGAAAACGTTTATAGTATTGAATAAAGGGAAAGCAATCTCTCGATTCAGTGCCACCCCTGCCCTTTACATTTTAACTCCCTTCAACCCTATTAGAAAATTAGCTATTAAGATTTTGGTACATTCTTTATTCAATATGCTCATTATGTGCACGATTCTTACCAACTGTGTATTTATGACCATGAGTAACCCTCCAGACTGGACAAAGAATGTGGAGTATACCTTTACAGGAATTTATACTTTTGAATCACTTATTAAAATACTTGCAAGGGGCTTTTGTTTAGAAGATTTCACATTTTTACGGGATCCATGGAATTGGTTGGATTTCACAGTCATTACTTTTGCATATGTGACAGAGTTTGTGGACCTGGGCAATGTCTCAGCGTTGAGAACATTCAGAGTTCTCCGAGCATTGAAAACAATTTCAGTCATTCCAGGCCTGAAGACCATTGTGGGGGCCCTGATCCAGTCAGTGAAGAAGCTTTCTGATGTCATGATCTTGACTGTGTTCTGTCTAAGCGTGTTTGCGCTAATAGGATTGCAGTTGTTCATGGGCAACCTACGAAATAAATGTTTGCAATGGCCTCCAGATAATTCTTCCTTTGAAATAAATATCACTTCCTTCTTTAACAATTCATTGGATGGGAATGGTACTACTTTCAATAGGACAGTGAGCATATTTAACTGGGATGAATATATTGAGGATAAAAGTCACTTTTATTTTTTAGAGGGGCAAAATGATGCTCTGCTTTGTGGCAACAGCTCAGATGCAGGCCAGTGTCCTGAAGGATACATCTGTGTGAAGGCTGGTAGAAACCCCAACTATGGCTACACGAGCTTTGACACCTTTAGTTGGGCCTTTTTGTCCTTATTTCGTCTCATGACTCAAGACTTCTGGGAAAACCTTTATCAACTGACACTACGTGCTGCTGGGAAAACGTACATGATATTTTTTGTGCTGGTCATTTTCTTGGGCTCATTCTATCTAATAAATTTGATCTTGGCTGTGGTGGCCATGGCCTATGAGGAACAGAATCAGGCCACATTGGAAGAGGCTGAACAGAAGGAAGCTGAATTTCAGCAGATGCTCGAACAGTTGAAAAAGCAACAAGAAGAAGCTCAGGCGGCAGCTGCAGCCGCATCTGCTGAATCAAGAGACTTCAGTGGTGCTGGTGGGATAGGAGTTTTTTCAGAGAGTTCTTCAGTAGCATCTAAGTTGAGCTCCAAAAGTGAAAAAGAGCTGAAAAACAGAAGAAAGAAAAAGAAACAGAAAGAACAGTCTGGAGAAGAAGAGAAAAATGACAGAGTCCGAAAATCGGAATCTGAAGACAGCATAAGAAGAAAAGGTTTCCGTTTTTCCTTGGAAGGAAGTAGGCTGACATATGAAAAGAGATTTTCTTCTCCACACCAGTCCTTACTGAGCATCCGTGGCTCCCTTTTCTCTCCAAGACGCAACAGTAGGGCGAGCCTTTTCAGCTTCAGAGGTCGAGCAAAGGACATTGGCTCTGAGAATGACTTTGCTGATGATGAGCACAGCACCTTTGAGGACAATGACAGCCGAAGAGACTCTCTGTTCGTGCCGCACAGACATGGAGAACGGCGCCACAGCAATGTCAGCCAGGCCAGCCGTGCCTCCAGGGTGCTCCCCATCCTGCCCATGAATGGGAAGATGCATAGCGCTGTGGACTGCAATGGTGTGGTCTCCCTGGTCGGGGGCCCTTCTACCCTCACATCTGCTGGGCAGCTCCTACCAGAGGGCACAACTACTGAAACAGAAATAAGAAAGAGACGGTCCAGTTCTTATCATGTTTCCATGGATTTATTGGAAGATCCTACATCAAGGCAAAGAGCAATGAGTATAGCCAGTATTTTGACCAACACCATGGAAGAACTTGAAGAATCCAGACAGAAATGCCCACCATGCTGGTATAAATTTGCTAATATGTGTTTGATTTGGGACTGTTGTAAACCATGGTTAAAGGTGAAACACCTTGTCAACCTGGTTGTAATGGACCCATTTGTTGACCTGGCCATCACCATCTGCATTGTCTTAAATACACTCTTCATGGCTATGGAGCACTATCCCATGACGGAGCAGTTCAGCAGTGTACTGTCTGTTGGAAACCTGGTCTTCACAGGGATCTTCACAGCAGAAATGTTTCTCAAGATAATTGCCATGGATCCATATTATTACTTTCAAGAAGGCTGGAATATTTTTGATGGTTTTATTGTGAGCCTTAGTTTAATGGAACTTGGTTTGGCAAATGTGGAAGGATTGTCAGTTCTCCGATCATTCCGGCTGCTCCGAGTTTTCAAGTTGGCAAAATCTTGGCCAACTCTAAATATGCTAATTAAGATCATTGGCAATTCTGTGGGGGCTCTAGGAAACCTCACCTTGGTATTGGCCATCATCGTCTTCATTTTTGCTGTGGTCGGCATGCAGCTCTTTGGTAAGAGCTACAAAGAATGTGTCTGCAAGATTTCCAATGATTGTGAACTCCCACGCTGGCACATGCATGACTTTTTCCACTCCTTCCTGATCGTGTTCCGCGTGCTGTGTGGAGAGTGGATAGAGACCATGTGGGACTGTATGGAGGTCGCTGGCCAAACCATGTGCCTTACTGTCTTCATGATGGTCATGGTGATTGGAAATCTAGTGGTTCTGAACCTCTTCTTGGCCTTGCTTTTGAGTTCCTTCAGTTCTGACAATCTTGCTGCCACTGATGATGATAACGAAATGAATAATCTCCAGATTGCTGTGGGAAGGATGCAGAAAGGAATCGATTTTGTTAAAAGAAAAATACGTGAATTTATTCAGAAAGCCTTTGTTAGGAAGCAGAAAGCTTTAGATGAAATTAAACCGCTTGAAGATCTAAATAATAAAAAAGACAGCTGTATTTCCAACCATACCACCATAGAAATAGGCAAAGACCTCAATTATCTCAAAGACGGAAATGGAACTACTAGTGGCATAGGCAGCAGTGTAGAAAAATATGTCGTGGATGAAAGTGATTACATGTCATTTATAAACAACCCTAGCCTCACTGTGACAGTACCAATTGCTGTTGGAGAATCTGACTTTGAAAATTTAAATACTGAAGAATTCAGCAGCGAGTCAGATATGGAGGAAAGCAAAGAGAAGCTAAATGCAACTAGTTCATCTGAAGGCAGCACGGTTGATATTGGAGCTCCCGCCGAGGGAGAACAGCCTGAGGTTGAACCTGAGGAATCCCTTGAACCTGAAGCCTGCCTTAGTTACGAGACTGAGATCCTCACGGTCGAATACGGATTGCTGCCCATCGGGAAGATCGTTGAGAAAAGAATCGAATGTACGGTATACAGCGTAGACAACAATGGGAACATCTATACACAACCAGTAGCCCAGTGGCATGATCGAGGCGAACAGGAGGTATTCGAATACTGTCTGGAAGATGGATCTTTGATTCGGGCGACCAAGGATCATAAATTTATGACAGTGGACGGACAGATGCTGCCAATAGATGAAATATTTGAACGAGAACTGGACCTTATGCGGGTTGATAACCTTCCTAATTAA]GATATCGGATCCCGGGACTAGTACGCGTTAAGTCGACA")

# 2. cloningSitesToEx
# lowercase variable names, as.vector
# choose any number of them from bamh1,mlu1,nhe1,ecor1,nde1,hind3,xba1,spe1,ecor5
cloningSitesToEx<-c(nhe1,mlu1) 

# 3. gene
# Files are named with 'gene' being part of the name.
gene<-"SCN2A"

# 4. codonRedunt
# How many codons to choose, when available, for each amino acid mutant. 
codonRedunt<-1 # normally 1 codon for each intended amino acid mutation.

# 5. cdnPosToChange
#specify the positions of codons to be mutated, as.vector.
cdnPosToChange<-c(seq(820,1075,by=1)) #typically codon1 and codon-stop are not changed.

# 6. folder_to_write
# make and then specify a folder where the code will write out files.
folder_to_write <- "~/Desktop/YourGeneForSatMut/"

######### End of set up: select the entire code and run it! ###############


if(!dir.exists(path=folder_to_write)){
  print (paste0("New folder need to make:", folder_to_write))
}

folder <- paste0(folder_to_write,"lib_design/")
if(!dir.exists(path=folder)){
  print ("new folder need to make")
  dir.create(path=folder,mode="0777", showWarnings = TRUE)
  if(dir.exists(path=folder)){
      print ("folder now exists")
  }
} else {
  print ("folder exists")
}


########### FUNCTIONS and Constant values ############

#HumanCodonUsageFreq_201912.csv
codonTableFreq<-tibble(
  "CODON"=c('GCT','GCC','GCA','GCG','TGT','TGC','GAT','GAC','GAA','GAG','TTT','TTC','GGT','GGC','GGA','GGG','CAT','CAC','ATT','ATC','ATA','AAA','AAG','TTA','TTG','CTT','CTC','CTA','CTG','ATG','AAT','AAC','CCT','CCC','CCA','CCG','CAA','CAG','CGT','CGC','CGA','CGG','AGA','AGG','TCT','TCC','TCA','TCG','AGT','AGC','TAA','TAG','TGA','ACT','ACC','ACA','ACG','GTT','GTC','GTA','GTG','TGG','TAT','TAC'),
  "AA"=c('A','A','A','A','C','C','D','D','E','E','F','F','G','G','G','G','H','H','I','I','I','K','K','L','L','L','L','L','L','M','N','N','P','P','P','P','Q','Q','R','R','R','R','R','R','S','S','S','S','S','S','Z','Z','Z','T','T','T','T','V','V','V','V','W','Y','Y'),
  "AA3"=c('Ala','Ala','Ala','Ala','Cys','Cys','Asp','Asp','Glu','Glu','Phe','Phe','Gly','Gly','Gly','Gly','His','His','Ile','Ile','Ile','Lys','Lys','Leu','Leu','Leu','Leu','Leu','Leu','Met','Asn','Asn','Pro','Pro','Pro','Pro','Gln','Gln','Arg','Arg','Arg','Arg','Arg','Arg','Ser','Ser','Ser','Ser','Ser','Ser','stop','stop','stop','Thr','Thr','Thr','Thr','Val','Val','Val','Val','Trp','Tyr','Tyr'),
  "Aalong"=c('Alanine','Alanine','Alanine','Alanine','Cysteine','Cysteine','Asparticacid','Asparticacid','Glutamicacid','Glutamicacid','Phenylalanine','Phenylalanine','Glycine','Glycine','Glycine','Glycine','Histidine','Histidine','Isoleucine','Isoleucine','Isoleucine','Lysine','Lysine','Leucine','Leucine','Leucine','Leucine','Leucine','Leucine','Methionine','Asparagine','Asparagine','Proline','Proline','Proline','Proline','Glutamine','Glutamine','Arginine','Arginine','Arginine','Arginine','Arginine','Arginine','Serine','Serine','Serine','Serine','Serine','Serine','Stop_codon','Stop_codon','Stop_codon','Threonine','Threonine','Threonine','Threonine','Valine','Valine','Valine','Valine','Tryptophan','Tyrosine','Tyrosine'),
  "Freq"=c(0.26592161,0.39978112,0.22812126,0.106176,0.45615733,0.54384267,0.46454242,0.53545758,0.42245266,0.57754734,0.46413427,0.53586573,0.16308651,0.33710936,0.24992165,0.24988248,0.41851521,0.58148479,0.36107219,0.46986629,0.16906152,0.43404935,0.56595065,0.07656765,0.12905785,0.13171591,0.19557683,0.07138017,0.39570159,1,0.47036699,0.52963301,0.28673133,0.3234704,0.27660253,0.11319575,0.26501676,0.73498324,0.08010753,0.18377663,0.10881248,0.20155434,0.21465775,0.21109127,0.18758584,0.21795953,0.15051715,0.05439771,0.14960182,0.23993794,0.29701912,0.23673791,0.46624297,0.24676884,0.35523154,0.28418773,0.11381189,0.18177011,0.23830638,0.11657741,0.4633461,1,0.44333811,0.55666189),
  "Rank"=c(2,1,3,4,2,1,2,1,2,1,2,1,4,1,2,3,2,1,2,1,3,2,1,5,4,3,2,6,1,1,2,1,2,1,3,4,2,1,6,4,5,3,1,2,3,2,4,6,5,1,2,3,1,3,1,2,4,3,2,4,1,1,2,1)
)

str(codonTableFreq)

codonTable<-codonTableFreq[1:2]
codonTable_0<-codonTableFreq[1:4]

codonFreq <- codonTableFreq %>%
  select("Vtcodon" = CODON, AA, Freq, Rank)


###functions

fromCodonTo123nt<-function(codon){
  cdnIn<-codon
  cdn1nt<-c()
  cdn2nt<-c()
  cdn3nt<-c()
  nts<-c("A","C","G","T")
  codon64<-c()
  for(nt1 in nts){
    for(nt2 in nts){
      for(nt3 in nts){
        codon64<-c(codon64, paste(nt1,nt2,nt3,sep=''))
      }
    }
  }
  for (cdn in codon64){
    if(stringdist(cdn,cdnIn, method='hamming')==1){
      cdn1nt<-c(cdn1nt,cdn)
    }
    else if(stringdist(cdn,cdnIn,method='hamming')==2){
      cdn2nt<-c(cdn2nt,cdn)
    }
    else if(stringdist(cdn,cdnIn,method='hamming')==3){
      cdn3nt<-c(cdn3nt,cdn)
    }
  }
  print(cdnIn)
  print (cdn1nt)
  print ('=======')
  print (cdn2nt)
  print ('=======')
  print (cdn3nt)
  return(list("delta1nt"=cdn1nt,"delta2nt"=cdn2nt,"delta3nt"=cdn3nt))
}



rmEl<-function(array, element){
  array1<-as.vector(array)
  if(length(array1)==0){
    # print ("am NULL")
    return (array1)
  }
  for(m in length(array):1){
    if(array1[m]==element){
      array1<-array1[-m]
      return (array1)
    }
  }
  return (array1)
}

getBB<-function(wtAA,codonTable){
  return (codonTable[codonTable$AA==wtAA,]$CODON)
}

sortCodonByFreq <- function (cdnList, freqRankedCdnTable ) {
  cdns <- as.vector(cdnList)
  codonFreqTb <- data.frame(freqRankedCdnTable)
  df <- data.frame("cdn" = cdns)
  m_df <- merge(df, codonFreqTb, by.x='cdn', by.y='Vtcodon', all = FALSE)
  s_m_df <- m_df %>%
    arrange(AA, -Rank)
  cdns_ranked <- as.vector(s_m_df$cdn)
  freq_ranked <- as.vector(s_m_df$Freq)
  aa_ranked <- as.vector(s_m_df$AA)
  return(cdns_ranked)
}

sortCodonByFreq_weighted <- function (cdnList, freqRankedCdnTable ) {
  cdns <- as.vector(cdnList)
  codonFreqTb <- data.frame(freqRankedCdnTable)
  df <- data.frame("cdn" = cdns)
  m_df <- merge(df, codonFreqTb, by.x='cdn', by.y='Vtcodon', all = FALSE)
  cdns_ranked0<-sample(m_df$cdn, size = dim(m_df)[1], replace = FALSE, prob=m_df$Freq)
  cdns_ranked <- rev(cdns_ranked0)
  print(m_df)
  print(cdns_ranked)
  return(as.vector(cdns_ranked))
}
# 
############ end of functions ###############

##Cleanup input data
templateSeq<-gsub("\\t","",gsub("\\s","",gsub("\\n","",toupper(templateSeq))))
templateSeq<-str_replace_all(templateSeq," ","")
templateSeq<-str_replace_all(templateSeq,"\n","")
date_str<-format(Sys.time(), "%Y%m%d")

str(templateSeq)

leftFlank=strsplit(templateSeq,"\\[")[[1]][1]
rightFlank=strsplit(strsplit(templateSeq,"\\[")[[1]][2],"\\]")[[1]][2]
orfTemplate0=strsplit(strsplit(templateSeq,"\\[")[[1]][2],"\\]")[[1]][1]
lf6<-str_c(str_sub(leftFlank,nchar(leftFlank)-5,nchar(leftFlank)))
rf6<-str_sub(rightFlank,1,6)
orfTemplate<-str_c(lf6,orfTemplate0,rf6)
orfLen<-nchar(orfTemplate0)/3
nchar(orfTemplate0)/3

print (str_c("ORF length in codon:", nchar(orfTemplate0)/3))
print (str_c("ORF length in NT:", nchar(orfTemplate0)))

###check if the ORF template has sites of a few enzymes we are interested in
Template_fromStart2Stop <- orfTemplate0

str_c("Does template have ",nhe1,"? ",str_detect(Template_fromStart2Stop,str_sub(nhe1,1,6)),sep="")
str_c("Does template have ",bamh1,"? ",str_detect(Template_fromStart2Stop,str_sub(bamh1,1,6)),sep="")
str_c("Does template have ",spe1,"? ",str_detect(Template_fromStart2Stop,str_sub(spe1,1,6)),sep="")
str_c("Does template have ",mlu1,"? ",str_detect(Template_fromStart2Stop,str_sub(mlu1,1,6)),sep="")
str_c("Does template have ",ecor5,"? ",str_detect(Template_fromStart2Stop,str_sub(ecor5,1,6)),sep="")
str_c("Does template have ",nde1,"? ",str_detect(Template_fromStart2Stop,str_sub(nde1,1,6)),sep="")


sitesToEx_0=c(cloningSitesToEx,cloningSitesToEx,cloningSitesToEx,cloningSitesToEx,cloningSitesToEx)[1:5]

enzs<-'Sites_to_exclude'
for(g in 1:length(unique(sitesToEx_0))){
  enzs<-str_c(enzs,unique(str_sub(sitesToEx_0,8,15))[g],sep='_')
}



qu<-"N"

for(b in (1:length(sitesToEx_0))){
  
  siteSeq<-str_sub(sitesToEx_0[b],1,6)
  if(str_detect(orfTemplate0,siteSeq)){
    print ("sites existing in orf:")
    print (sitesToEx_0[b])
    qu<-"Y"
    stop("ORF seq violates restriction rule")
    stopifnot(!str_detect(orfTemplate0,siteSeq))
    
  }
  else{
    print ("No site:")
    print (sitesToEx_0[b])
  }
}

stopifnot(!identical(qu,"Y"))
"I passed stop"
if(identical(qu,"Y")){
  orfTemplate<-NULL
  returnValue()
  print ("set template to null to stop the code")
}


sitesToEx<-str_sub(sitesToEx_0,1,6)

x<-orfTemplate
templateIn3s<-plyr::laply(seq(1,nchar(x),3), function(i) substr(x, i, i+2))
templateIn3s
length(templateIn3s)

AA1<-c('M','W')
AA2<-c('C','D','E','F','H','K','N','Q','Y')
AA3<-c('I','Z')
AA4<-c('A','G','P','T','V')
AA6<-c('L','R','S')

codonTable<-codonTable_0

str(codonTable)
rownames(codonTable)<-NULL


vtAAs<-c()

if(codonRedunt==1){
  vtAAs<-c(AA1,AA2,AA3,AA4,AA6)
} else if(codonRedunt==2){
  vtAAs<-c(AA1,AA2,AA3,AA4,AA6,AA2,AA3,AA4,AA6)
} else if(codonRedunt==3){
  vtAAs<-c(AA1,AA2,AA3,AA4,AA6,AA2,AA3,AA4,AA6,AA3,AA4,AA6)
} else if(codonRedunt==4){
  vtAAs<-c(AA1,AA2,AA3,AA4,AA6,AA2,AA3,AA4,AA6,AA3,AA4,AA6,AA4,AA6)
} else if(codonRedunt>=5){
  vtAAs<-c(AA1,AA2,AA3,AA4,AA6,AA2,AA3,AA4,AA6,AA3,AA4,AA6,AA4,AA6,AA6,AA6)
} 

vtAAs<-sort(vtAAs,decreasing = FALSE)
vtAAs

DF<-data.frame(POS_Vtcodon_aa_nt_wtcdon_wtaa=character())

failed<-c()
failedRes<-c()
nt1<-c()
nt2<-c()
nt3<-c()
bb<-c()
# 
nativeZ<-templateIn3s[orfLen+2] ###end+4 is the last and stop codon



for (i in seq(1,orfLen,by=1)){
  vtaa1x<-c(AA1,AA2,AA3,AA4,AA6)
  bbCount<-0
  wt<-templateIn3s[i+2]
  wtAA<-codonTable[codonTable$CODON==wt,]$AA
   if(i %in% cdnPosToChange){
      hamming<-fromCodonTo123nt(wt)
      ones_0<-as.vector(hamming$delta1nt)
    threes_0<-c(as.vector(hamming$delta2nt),as.vector(hamming$delta3nt))
    ones<-sortCodonByFreq_weighted("cdnList"=ones_0, "freqRankedCdnTable"= codonFreq)
    threes<-sortCodonByFreq_weighted("cdnList"=threes_0, "freqRankedCdnTable"= codonFreq)
    
    sz1<-length(ones)
    sz3<-length(threes)
    print (str_c("codon = ",i, sep=''))
    print (str_c("TemplateCodon = ",wt, sep=''))
    print (str_c("sz1=",sz1,sep=''))
    print (ones)
    print (str_c("sz3=",sz3,sep=''))
    print (threes)
    vtcdns<-c()
    vtcdnsAA<-c()
    vtaa1x<-c(vtaa1x)
    lf<-str_c(templateIn3s[1:(i+1)],collapse = '')
    rf<-str_c(templateIn3s[(i+3):orfLen],collapse ='')
    
    wtAA<-codonTable[codonTable$CODON==wt,]$AA
    bbs<-getBB(wtAA,codonTable)

    for(s in 1:length(bbs)){
      if(stringdist(wt,bbs[s],method='hamming')==2 | stringdist(wt,bbs[s],method='hamming')==3){
        if(str_detect(str_c(lf,bbs[s],rf,sep=''),sitesToEx[1])|
         str_detect(str_c(lf,bbs[s],rf,sep=''),sitesToEx[2])|
         str_detect(str_c(lf,bbs[s],rf,sep=''),sitesToEx[3])|
         str_detect(str_c(lf,bbs[s],rf,sep=''),sitesToEx[4])|
         str_detect(str_c(lf,bbs[s],rf,sep=''),sitesToEx[5])){
          failedRes<-c(failedRes,str_c(i,bbs[s],wtAA,stringdist(wt,bbs[s],method='hamming'),sep="_"))
        next
        }
        vtcdns<-c(vtcdns,bbs[s])
        vtcdnsAA<-c(vtcdnsAA,str_c( bbs[s],"B",stringdist(wt,bbs[s],method='hamming'),sep="_"))
        threes<-rmEl(threes,bbs[s])
        bb<-c(bb,str_c( i,bbs[s],"B",stringdist(wt,bbs[s],method='hamming'),sep="_"))
        bbCount<-bbCount+1
        if(bbCount>=2){
        break ##if break is not commented out, only one B will be admitted/pos
        }
    }
  }
    vtaas<-vtAAs
    while (wtAA %in% vtaas) {
    vtaas<-rmEl(vtaas, wtAA)
  }
  vtaa1x<-rmEl(vtaa1x, wtAA)
  
  vtaas2<-vtaas  #list of aa for the given position
  vtUsedOnceOrMore<-c(wtAA)
  for (j in 1:length(vtaas)){
   aa<-vtaas[j]
   for(k in sz3:1){
     if(dim(codonTable[codonTable$CODON==threes[k],])[1]>0){
        if(is.element(aa, codonTable[codonTable$CODON==threes[k],]$AA)){
          if(!(aa %in% vtaas2)){
            threes<-rmEl(threes,threes[k])
            next
            }
          if(str_detect(str_c(lf,threes[k],rf,sep=''),sitesToEx[1])|
             str_detect(str_c(lf,threes[k],rf,sep=''),sitesToEx[2])|
             str_detect(str_c(lf,threes[k],rf,sep=''),sitesToEx[3])|
             str_detect(str_c(lf,threes[k],rf,sep=''),sitesToEx[4])|
             str_detect(str_c(lf,threes[k],rf,sep=''),sitesToEx[5])){
            print ("threes failed site restriction")
            print(str_c(lf,threes[k],sep=''))
            print (str_c(threes[k],rf,sep=''))
            print (threes[k])
            hm<-stringdist(threes[k],wt,method = 'hamming')
            failedRes<-c(failedRes,str_c(i,threes[k],aa,hm,sep="_"))
           
            next
          }
          hm<-stringdist(threes[k],wt,method = 'hamming')
          vtcdns<-c(vtcdns,threes[k])
          vtcdnsAA<-c(vtcdnsAA,str_c(threes[k],aa,hm,sep="_"))
          vtUsedOnceOrMore<-c(vtUsedOnceOrMore,aa)
          vtaa1x<-rmEl(vtaa1x, aa)
          hm<-stringdist(threes[k],wt,method = 'hamming')
          nt3<-c(nt3,str_c(i,threes[k],aa,hm,sep="_"))
          fq <- codonFreq[codonFreq$Vtcodon == threes[k],]$Freq
          fqaa <- codonFreq[codonFreq$Vtcodon == threes[k],]$AA
          print (str_c("Fulfilled by ",hm,"nt:",threes[k],"-", fqaa,":Freq=",fq,"==========>",i,sep=''))
          vtaas2<-rmEl(vtaas2,aa)
          threes<-rmEl(threes,threes[k])
          if(!(aa %in% vtaas2)){
            break;
          }
        }
       else{
         next;
         }
        }
   }
   if ((aa %in% vtaas2)){
     for(n in sz1:1){
       if(!(aa %in% vtUsedOnceOrMore)){
         if(dim(codonTable[codonTable$CODON==ones[n],])[1]>0){ 
           if(is.element(aa, codonTable[codonTable$CODON==ones[n],]$AA)){
             if(!(aa %in% vtaas2)){
               ones<-rmEl(ones,ones[n])
               next
             }
             if(str_detect(str_c(lf,ones[n],rf,sep=''),sitesToEx[1])|
                str_detect(str_c(lf,ones[n],rf,sep=''),sitesToEx[2])|
                str_detect(str_c(lf,ones[n],rf,sep=''),sitesToEx[3])|
                str_detect(str_c(lf,ones[n],rf,sep=''),sitesToEx[4])|
                str_detect(str_c(lf,ones[n],rf,sep=''),sitesToEx[5])){
                   print ("ones failed site restriction")
                   print(str_c(lf,ones[n],sep=''))
                   print (str_c(ones[n],rf,sep=''))
                   print (ones[n])
                   failedRes<-c(failedRes,str_c(i,ones[n],aa,"1",sep="_"))
                   ones<-rmEl(ones,ones[n])
                   next
                 }
             vtcdns<-c(vtcdns,ones[n])       
             vtcdnsAA<-c(vtcdnsAA,str_c(ones[n],aa,"1",sep="_"))
             vtUsedOnceOrMore<-c(vtUsedOnceOrMore,aa) 
             
             fq <- codonFreq[codonFreq$Vtcodon == ones[n],]$Freq
             fqaa <- codonFreq[codonFreq$Vtcodon == ones[n],]$AA
             print (str_c("Fulfilled by 1nt:",ones[n],"-", fqaa,":Freq=",fq,
                          "==========>",i,sep=''))
             
             vtaa1x<-rmEl(vtaa1x, aa)
             
             nt1<-c(nt1,str_c(i,ones[n],aa,"1",sep="_"))
             vtaas2<-rmEl(vtaas2,aa)
             ones<-rmEl(ones,ones[n])
             if(!(aa %in% vtaas2)){
               break;
             }
           }
         }  
        }
     } 
   }
  }
  
   # if(length(vtaas2)==0){
     print(i)
     print (wt)
     print (wtAA)
     if(length(vtaa1x)>0){
       failed<-c(failed,str_c(i,vtaa1x, sep=''))
     }
     str_c(i,vtaas2, sep='')
     if(length(vtaas2)>0){
        print (str_c("failed to have designs at pos ",i,":", vtaas2, sep=''))
     }else{
        print (str_c("failed to have designs at pos ",i,": NONE", sep=''))
     }
     print ("Fulfilled Designs:")
     print (str_c(i,vtcdnsAA,sep = '_'))
      df<-data.frame("POS_Vtcodon_aa_nt_wtcdon_wtaa"=as.character(str_c(i,vtcdnsAA,wt,wtAA,sep="_")))
     DF<-rbind(DF,df)
  } 
    df_wt<-data.frame("POS_Vtcodon_aa_nt_wtcdon_wtaa"=as.character(str_c(i,wt,wtAA,'0',wt,wtAA,sep="_")))
    DF<-rbind(DF,df_wt)

}



print ("restriction sites failed:")
failedRes
print ("still failed:")
failed

print ("In final designs:\nnt1:")
nt1
print ("nt2:")
nt2
print ("nt3:")
nt3
print ("BB:")
bb

names(DF)
DF0<-DF
str(DF0)
codon64<-codonTable[c(1,2)]
names(codon64)<-c('Vtcodon','VtaaNoB')
DF0[] <- lapply(DF0, as.character)
names(DF0)
DF1<-separate(DF0,col="POS_Vtcodon_aa_nt_wtcdon_wtaa",into=c("POS","Vtcodon","Vtaa","delta_nt","Wtcodon","Wtaa"), sep='_') 

names(DF1)
str(DF1)
unique(DF1$Vtaa)
##in .dat, 0 for wt, 1 for missense, 2 for B
DF1_forDat<-DF1 %>%
  mutate(count=ifelse(Vtaa == 'B',2,ifelse(delta_nt == 0, 0, 1)))
DF2_forDat<-merge(DF1_forDat,codon64,by='Vtcodon', all=TRUE)
DF1$count<-str_c(DF1$delta_nt,'nt',sep='')
DF2<-merge(DF1,codon64,by='Vtcodon', all=TRUE)

str(DF2)


tb_codon_aa<-DF2 %>%
  mutate(VtcodonAA=str_c(Vtcodon,VtaaNoB,sep = '_'))%>%
  select (-Vtcodon) %>%
  group_by(POS,Wtcodon,Wtaa)%>%
  select(-delta_nt, -Vtaa, -VtaaNoB) %>%
  spread(key=VtcodonAA, value=count)%>%
  arrange(as.numeric(POS))
###########make .dat
tb_dat<-DF2_forDat %>%
  group_by(POS,Wtcodon,Wtaa)%>%
  select(-delta_nt, -Vtaa, -VtaaNoB) %>%
  spread(key=Vtcodon, value=count)%>%
  arrange(as.numeric(POS))
tb_dat<-tb_dat %>%
  arrange(as.numeric(POS))

tb_dat[is.na(tb_dat)]<-0

###########finish making .dat

tb_codon_aa<-tb_codon_aa[order(as.numeric(tb_codon_aa$POS)),]

tb_aa_codon_review<-DF2 %>%
  # dplyr::filter(delta_nt != 0) %>%
  mutate(count = ifelse((Vtaa == 'B'),str_c("B", count, sep = "_"), count)) %>%
  mutate(count = ifelse(count == '0nt', 'wt', count))%>%
  mutate(VtcodonAA=str_c(VtaaNoB,Vtcodon,sep = '_'))%>%
  select (-Vtcodon) %>%
  group_by(POS,Wtcodon,Wtaa)%>%
  select(-delta_nt, -Vtaa, -VtaaNoB) %>%
  spread(key=VtcodonAA, value=count)%>%
  arrange(as.numeric(POS))
tb_aa_codon_review[is.na(tb_aa_codon_review)] <- ''

################Get Twist order per Twist order form
TwistCodonSort<-c('A_GCT',	'A_GCA',	'A_GCC',	'A_GCG',	'C_TGC',	'C_TGT',	'D_GAT',	'D_GAC',	'E_GAG',	'E_GAA',	'F_TTC',	'F_TTT',	'G_GGT',	'G_GGA',	'G_GGC',	'G_GGG',	'H_CAC',	'H_CAT',	'I_ATC',	'I_ATT',	'I_ATA',	'K_AAG',	'K_AAA',	'L_CTG',	'L_CTC',	'L_CTT',	'L_TTG',	'L_TTA',	'L_CTA',	'M_ATG',	'N_AAC',	'N_AAT',	'P_CCT',	'P_CCA',	'P_CCG',	'P_CCC',	'Q_CAG',	'Q_CAA',	'R_AGA',	'R_CGT',	'R_AGG',	'R_CGA',	'R_CGC',	'R_CGG',	'S_AGC',	'S_TCT',	'S_TCC',	'S_AGT',	'S_TCA',	'S_TCG',	'T_ACC',	'T_ACA',	'T_ACT',	'T_ACG',	'V_GTG',	'V_GTT',	'V_GTC',	'V_GTA',	'W_TGG',	'Y_TAC',	'Y_TAT',	'Z_TGA',	'Z_TAA',	'Z_TAG')



tb_aa_codon_TwistForm_0<-DF2 %>%
  #dplyr::filter(delta_nt != 0) %>%
  mutate(count = ifelse(delta_nt==0, '', Vtcodon)) %>%
  mutate(VtcodonAA=str_c(VtaaNoB,Vtcodon,sep = '_')) %>%
  select (-Vtcodon) %>%
  group_by(POS,Wtcodon,Wtaa)%>%
  select(-delta_nt, -Vtaa, -VtaaNoB) %>%
  spread(key=VtcodonAA, value=count)

colNMs<-names(tb_aa_codon_TwistForm_0)
newColNMs<-TwistCodonSort[!(TwistCodonSort  %in% colNMs)]
tb_aa_codon_TwistForm_0[,newColNMs] <- NA
tb_aa_codon_TwistForm <- tb_aa_codon_TwistForm_0 %>%
  select(c("POS", "Wtcodon","Wtaa",TwistCodonSort)) %>%
  arrange(as.numeric(POS))
tb_aa_codon_TwistForm[is.na(tb_aa_codon_TwistForm)] <- ''
tb_aa_codon_TwistForm_t <- t(tb_aa_codon_TwistForm)

#######this output file will be ready to paste into Twist Order form.


tb_aa_codon<-DF2 %>%
 # dplyr::filter(delta_nt != 0) %>%
  mutate(VtcodonAA=str_c(VtaaNoB,Vtcodon,sep = '_'))%>%
  select (-Vtcodon) %>%
  group_by(POS,Wtcodon,Wtaa)%>%
  select(-delta_nt, -Vtaa, -VtaaNoB) %>%
  spread(key=VtcodonAA, value=count)%>%
  arrange(as.numeric(POS))
tb_aa_codon<-tb_aa_codon[order(as.numeric(tb_aa_codon$POS)),]




tb_codon<-DF2 %>%
  group_by(POS,Wtcodon,Wtaa)%>%
  select(-delta_nt, -Vtaa, -VtaaNoB) %>%
  spread(key=Vtcodon, value=count)%>%
  arrange(as.numeric(POS))
tb_codon<-tb_codon[order(as.numeric(tb_codon$POS)),]


tb_codon[is.na(tb_codon)]<-''
tb_aa_codon[is.na(tb_aa_codon)]<-''
tb_codon_aa[is.na(tb_codon_aa)]<-''
rownames(tb_codon)<-NULL
rownames(tb_aa_codon)<-NULL
rownames(tb_codon_aa)<-NULL


write.csv(tb_aa_codon_TwistForm_t, file=paste(folder,"DesignBy_SatMut_1_9_TwistReady_",gene,"_",enzs,date_str,".csv", sep='') )
  
write.csv(tb_aa_codon_review, file=paste(folder,"DesignBy_SatMut_1_9_ToReview_",gene, date_str,".csv", sep=''))


write(failed, file=paste(folder,"DesignBy_SatMut_1_9","_",gene,"_failed_",enzs,date_str,".txt", sep=''))
write(nt1, file=paste(folder,"DesignBy_SatMut_1_9",gene,"_nt1Delta_",enzs,date_str,".txt", sep=''))

write(nt3, file=paste(folder,"DesignBy_SatMut_1_9",gene,"_nt2_nt3Delta_",enzs,date_str,".txt", sep=''))
write(bb, file=paste(folder,"DesignBy_SatMut_1_9",gene,"_silents_",enzs,date_str,".txt", sep=''))


#####################

intendedCdn<-tb_codon
codons64<-names(intendedCdn)[-c(1:3)]

library(reshape2)
m_tmp50 <-melt(intendedCdn, id=c("POS","Wtcodon","Wtaa"), variable.name="Vtcodon", value.name = "Ct") %>%
  dplyr::filter(Ct !='' & Ct !='0nt')

tmp50<-m_tmp50 %>%
  dplyr::filter(Ct!='') %>%
  mutate(delta_nt=stringdist(Wtcodon,Vtcodon, method='hamming')) %>%
  arrange(as.numeric(POS))

    write.csv(tmp50, file=paste(folder,"DesignBy_SatMut_1_9","_",gene,"_HammingInfo_",enzs, ".csv", sep=''))

unique(tmp50$delta_nt)
str(tmp50)

fc<-as.vector(tmp50$delta_nt)
"Wt variants:"
length(bb)
"total variants:"
sm1<-sum(str_count(fc,"3"))+sum(str_count(fc,"2"))+sum(str_count(fc,"1"))
sm2<-sum(str_count(fc,"3"))
sm3<-sum(str_count(fc,"2"))
sm4<-sum(str_count(fc,"1"))
sm5<-sum(str_count(fc,"1"))/(sum(str_count(fc,"3"))+sum(str_count(fc,"2"))+sum(str_count(fc,"1")))
sm1
sm2
sm3
sm4
sm5
"Restriction fails:"
failed

###logFile
flname<-paste(folder,"DesignBy_SatMut_1_9_",gene,"_LogFile_",enzs,date_str,".txt", sep='')
cat("\n=== LOG FILE ===", file=flname, sep="\n", append=TRUE)
cat("Variants to miss out due to restriction sites fails:", file=flname, sep="\n",append=TRUE)
cat(failed, file=flname, sep="\n", append=TRUE)
cat("Wt silent variants:", file=flname, sep="\n",append=TRUE)
cat(length(bb), file=flname, sep="\n", append=TRUE)
cat("total variants:", file=flname, sep="\n", append=TRUE)
cat(as.character(sm1), file=flname, sep=" \n", append=TRUE)
cat("total codon positions mutated:", file=flname, sep="\n", append=TRUE)
cat(length(cdnPosToChange), file=flname, sep="\n", append=TRUE)
cat("3nt-delta:", file=flname, sep="\n", append=TRUE)
cat(as.character(sm2), file=flname, sep="\n", append=TRUE)
cat("2nt-delta:", file=flname, sep="\n", append=TRUE)
cat(as.character(sm3), file=flname, sep="\n", append=TRUE)
cat("1nt-delta:", file=flname, sep="\n", append=TRUE)
cat(as.character(sm4), file=flname, sep="\n", append=TRUE)
cat("1nt-delta fraction:", file=flname, sep="\n", append=TRUE)
cat(as.character(sm5), file=flname, sep="\n", append=TRUE)

## the end

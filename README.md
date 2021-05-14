# SatMut_VariantLibrary_Designer



Below you need to populate 6 variables. Then ready to run the designer.

1. templateSeq
2. cloningSitesToEx
3. gene
4. codonRedunt
5. cdnPosToChange
6. folder 

```
#format Left_flank[orf]right_flank, make sure the ORF sequence is marked by the brackets [ ]

templateSeq=as.character("TTCAGGTGTCGTGAGGCTAGCCATATGCCACC[ATGGCACAGTCAGTGCTGGTACCGCCAGGACCTGACAGCTTCCGCTTCTTTACCAGGGAATCCCTTGCTGCTATTGAACAACGCATTGCAGAAGAGAAAGCTAAGAGACCCAAACAGGAACGCAAGGATGAGGATGATGAAAATGGCCCAAAGCCAAACAGTGACTTGGAAGCAGGAAAATCTCTTCCATTTATTTATGGAGACATTCCTCCAGAGATGGTGTCAGTGCCCCTGGAGGATCTGGACCCCTACTATATCAATAAGAAAACGTTTATAGTATTGAATAAAGGGAAAGCAATCTCTCGATTCAGTGCCACCCCTGCCCTTTACATTTTAACTCCCTTCAACCCTATTAGAAAATTAGCTATTAAGATTTTGGTACATTCTTTATTCAATATGCTCATTATGTGCACGATTCTTACCAACTGTGTATTTATGACCATGAGTAACCCTCCAGACTGGACAAAGAATGTGGAGTATACCTTTACAGGAATTTATACTTTTGAATCACTTATTAAAATACTTGCAAGGGGCTTTTGTTTAGAAGATTTCACATTTTTACGGGATCCATGGAATTGGTTGGATTTCACAGTCATTACTTTTGCATATGTGACAGAGTTTGTGGACCTGGGCAATGTCTCAGCGTTGAGAACATTCAGAGTTCTCCGAGCATTGAAAACAATTTCAGTCATTCCAGGCCTGAAGACCATTGTGGGGGCCCTGATCCAGTCAGTGAAGAAGCTTTCTGATGTCATGATCTTGACTGTGTTCTGTCTAAGCGTGTTTGCGCTAATAGGATTGCAGTTGTTCATGGGCAACCTACGAAATAAATGTTTGCAATGGCCTCCAGATAATTCTTCCTTTGAAATAAATATCACTTCCTTCTTTAACAATTCATTGGATGGGAATGGTACTACTTTCAATAGGACAGTGAGCATATTTAACTGGGATGAATATATTGAGGATAAAAGTCACTTTTATTTTTTAGAGGGGCAAAATGATGCTCTGCTTTGTGGCAACAGCTCAGATGCAGGCCAGTGTCCTGAAGGATACATCTGTGTGAAGGCTGGTAGAAACCCCAACTATGGCTACACGAGCTTTGACACCTTTAGTTGGGCCTTTTTGTCCTTATTTCGTCTCATGACTCAAGACTTCTGGGAAAACCTTTATCAACTGACACTACGTGCTGCTGGGAAAACGTACATGATATTTTTTGTGCTGGTCATTTTCTTGGGCTCATTCTATCTAATAAATTTGATCTTGGCTGTGGTGGCCATGGCCTATGAGGAACAGAATCAGGCCACATTGGAAGAGGCTGAACAGAAGGAAGCTGAATTTCAGCAGATGCTCGAACAGTTGAAAAAGCAACAAGAAGAAGCTCAGGCGGCAGCTGCAGCCGCATCTGCTGAATCAAGAGACTTCAGTGGTGCTGGTGGGATAGGAGTTTTTTCAGAGAGTTCTTCAGTAGCATCTAAGTTGAGCTCCAAAAGTGAAAAAGAGCTGAAAAACAGAAGAAAGAAAAAGAAACAGAAAGAACAGTCTGGAGAAGAAGAGAAAAATGACAGAGTCCGAAAATCGGAATCTGAAGACAGCATAAGAAGAAAAGGTTTCCGTTTTTCCTTGGAAGGAAGTAGGCTGACATATGAAAAGAGATTTTCTTCTCCACACCAGTCCTTACTGAGCATCCGTGGCTCCCTTTTCTCTCCAAGACGCAACAGTAGGGCGAGCCTTTTCAGCTTCAGAGGTCGAGCAAAGGACATTGGCTCTGAGAATGACTTTGCTGATGATGAGCACAGCACCTTTGAGGACAATGACAGCCGAAGAGACTCTCTGTTCGTGCCGCACAGACATGGAGAACGGCGCCACAGCAATGTCAGCCAGGCCAGCCGTGCCTCCAGGGTGCTCCCCATCCTGCCCATGAATGGGAAGATGCATAGCGCTGTGGACTGCAATGGTGTGGTCTCCCTGGTCGGGGGCCCTTCTACCCTCACATCTGCTGGGCAGCTCCTACCAGAGGGCACAACTACTGAAACAGAAATAAGAAAGAGACGGTCCAGTTCTTATCATGTTTCCATGGATTTATTGGAAGATCCTACATCAAGGCAAAGAGCAATGAGTATAGCCAGTATTTTGACCAACACCATGGAAGAACTTGAAGAATCCAGACAGAAATGCCCACCATGCTGGTATAAATTTGCTAATATGTGTTTGATTTGGGACTGTTGTAAACCATGGTTAAAGGTGAAACACCTTGTCAACCTGGTTGTAATGGACCCATTTGTTGACCTGGCCATCACCATCTGCATTGTCTTAAATACACTCTTCATGGCTATGGAGCACTATCCCATGACGGAGCAGTTCAGCAGTGTACTGTCTGTTGGAAACCTGGTCTTCACAGGGATCTTCACAGCAGAAATGTTTCTCAAGATAATTGCCATGGATCCATATTATTACTTTCAAGAAGGCTGGAATATTTTTGATGGTTTTATTGTGAGCCTTAGTTTAATGGAACTTGGTTTGGCAAATGTGGAAGGATTGTCAGTTCTCCGATCATTCCGGCTGCTCCGAGTTTTCAAGTTGGCAAAATCTTGGCCAACTCTAAATATGCTAATTAAGATCATTGGCAATTCTGTGGGGGCTCTAGGAAACCTCACCTTGGTATTGGCCATCATCGTCTTCATTTTTGCTGTGGTCGGCATGCAGCTCTTTGGTAAGAGCTACAAAGAATGTGTCTGCAAGATTTCCAATGATTGTGAACTCCCACGCTGGCACATGCATGACTTTTTCCACTCCTTCCTGATCGTGTTCCGCGTGCTGTGTGGAGAGTGGATAGAGACCATGTGGGACTGTATGGAGGTCGCTGGCCAAACCATGTGCCTTACTGTCTTCATGATGGTCATGGTGATTGGAAATCTAGTGGTTCTGAACCTCTTCTTGGCCTTGCTTTTGAGTTCCTTCAGTTCTGACAATCTTGCTGCCACTGATGATGATAACGAAATGAATAATCTCCAGATTGCTGTGGGAAGGATGCAGAAAGGAATCGATTTTGTTAAAAGAAAAATACGTGAATTTATTCAGAAAGCCTTTGTTAGGAAGCAGAAAGCTTTAGATGAAATTAAACCGCTTGAAGATCTAAATAATAAAAAAGACAGCTGTATTTCCAACCATACCACCATAGAAATAGGCAAAGACCTCAATTATCTCAAAGACGGAAATGGAACTACTAGTGGCATAGGCAGCAGTGTAGAAAAATATGTCGTGGATGAAAGTGATTACATGTCATTTATAAACAACCCTAGCCTCACTGTGACAGTACCAATTGCTGTTGGAGAATCTGACTTTGAAAATTTAAATACTGAAGAATTCAGCAGCGAGTCAGATATGGAGGAAAGCAAAGAGAAGCTAAATGCAACTAGTTCATCTGAAGGCAGCACGGTTGATATTGGAGCTCCCGCCGAGGGAGAACAGCCTGAGGTTGAACCTGAGGAATCCCTTGAACCTGAAGCCTGCCTTAGTTACGAGACTGAGATCCTCACGGTCGAATACGGATTGCTGCCCATCGGGAAGATCGTTGAGAAAAGAATCGAATGTACGGTATACAGCGTAGACAACAATGGGAACATCTATACACAACCAGTAGCCCAGTGGCATGATCGAGGCGAACAGGAGGTATTCGAATACTGTCTGGAAGATGGATCTTTGATTCGGGCGACCAAGGATCATAAATTTATGACAGTGGACGGACAGATGCTGCCAATAGATGAAATATTTGAACGAGAACTGGACCTTATGCGGGTTGATAACCTTCCTAATTAA]GATATCGGATCCCGGGACTAGTACGCGTTAAGTCGACA")

#lowercases
cloningSitesToEx<-c(nhe1,mlu1) 

gene<-"SCN2A"

codonRedunt<-1 # normally 1 codon for each amino acid. If you need 2 codons/aa, set it to 2

cdnPosToChange<-c(seq(820,1075,by=1)) #typically codon1 and codon-stop are not changed.

# the fiiles generated by the designer will be saved in this folder.
folder <- "~/Documents/SatMut-SCN2A/HalfProtein_SatMut/lib_design/"
```

Then RUN the entire .r code. The data files and plots will be written into the folder you specified.
****************

Good luck and direct your questions to xyang@broadinstitute.org

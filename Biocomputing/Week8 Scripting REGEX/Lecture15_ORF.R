#Regex ORF Challenge in R
#10/11/17, MMD

#Load package
library('stringr')

#Read in fasta file, each line becomes an item in a vector
InFile=scan("R.mendax.1.fasta",what=character(),sep="\n")

#Loop over vector of lines
for (Line in 1:length(InFile)){
  #Operate only on lines that do not include >, skips header lines
  #Note that str_detect returns a logical T/F
  if (!str_detect(InFile[Line],">")){
    #Find the ORF in each sequence line
    match = str_extract(InFile[Line],"ATG([ATCG]{3})+(TAA|TAG|TGA)")
    #Print the ORF to standard out
    print(match)
  }
}
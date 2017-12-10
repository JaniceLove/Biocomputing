#Regex ORF Challenge in Python
#10/11/17, MMD

#Load module
import re

#Open fasta file to read
InFile=open("R.mendax.1.fasta","r")

#Loop through open file
for Line in InFile:
    #Remove end of line character
    Line = Line.strip()
    #Operate only on lines that do not include >, skips header lines
    if ">" not in Line:
        #Find the ORF in each sequence line
        match = re.search(r"ATG([ATCG]{3})+(TAA|TAG|TGA)",Line)
        #Print the ORF to standard out, note that group(0) is the full match
        print(match.group(0))

InFile.close()


# answers for Lecture 11 extra practice
import pandas

# 0 - calculate sum of female and male wages in wages.csv
wages=pandas.read_csv("wages.csv",header=0,sep=",")

femaleSum=0
maleSum=0

for i in range(0,len(wages),1):
    if wages.gender[i]=="female":
        femaleSum=femaleSum+wages.wage[i]
    else:
        maleSum=maleSum+wages.wage[i]

femaleSum
maleSum

sum(wages.gender=="female")
sum(wages.gender=="male")


# 1
# open fasta file
InFile=open("Lecture11.fasta","r")

#create lists for storing information about sequences
sequenceID=[]
sequenceLength=[]
percentGC=[]
meltingTemp=[]

#loop through each line of fasta file to process sequences
for Line in InFile:
    # remove newline character from file line
    Line=Line.strip()
    # if a sequence record
    if '>' in Line:
        # add the sequence ID (except the ">" character) to the sequenceID list
        sequenceID.append(Line[1:])
    # if a sequence line
    else:
        # get the number of characters in the sequence and convert to a float to avoid integer division
        seqLen=float(len(Line))
        # count the number of G's and C's
        nG=Line.count("G")
        nC=Line.count("C")
        
        # if the sequence is 14 or fewer bases calculate melting temperature
        if seqLen<=14:
            Tm=2*(nG+nC)+2*seqLen
        else:
            Tm=-9999
        
        # append values to the lists
        sequenceLength.append(seqLen)
        percentGC.append((nG+nC)/seqLen*100)
        meltingTemp.append(Tm)

# combine lists into dataframe
seqDF = pandas.DataFrame(list(zip(sequenceID,sequenceLength,percentGC,meltingTemp)),columns=['sequenceID','sequenceLength','percentGC','meltingTemp'])

# close file
InFile.close()


# 2
# load file
findRuns=pandas.read_csv("findRuns.txt",header=None,sep="\t")

# create a variable out that is currently undefined
out=pandas.DataFrame(columns=['startIndex','runLength'])
# I will use this variable cur to hold onto the previous number in the vector;
# this is analagous to using findRuns[i-1]
cur=findRuns.iloc[0,0]
# this is a counter that I use to keep track of how long a run of repeated values is;
# if there are not repeated values than this count equals 1
count=1

# loop through each entry of our vector (except the 1st one, which we set to cur above)
for i in range(1,50,1):
  # test if the ith value in the vector findRuns equals the previous (stored in cur)
  if findRuns.iloc[i,0]==cur:
    # test whether count is 1 (we aren't in the middle of a run) or >1 (in the middle of a run)
    if count==1:
      # if the ith value in the vector equals the previous (stored in cur) and count is 1, we
      # are at the beginning of a run and we want to store this value (we temporarily store it in 'start')
      start=(i-1)
    
    # we add one to count because the run continued based on the ith value of findRuns being equal to
    # the previous (stored in cur)
    count=count+1
    # if the ith value in findRuns is not the same as the previous (stored in cur) we either are not in a run
    # or we are ending a run
  else:
    # if count is greater than 1 it means we were in a run and must be exiting one
    if count>1:
      # add a row to 'out' that will hold the starting positions in the first column and the length
      # of runs in the second column; this appends rows to out after finding and counting each run
      out.loc[len(out)]=[start,count]
      # reset count to 1 because we just exited a run
      count=1
  # remember cur holds the previous element in findRuns, so we need to update this after each time
  # we go through the for loop
  cur=findRuns.iloc[i,0]



README:

The code in this directory will replicate the tables and figures, as well as the calculations and additional analyses referenced in the paper and online appendix of “The Political Effects of Opioid Addiction Frames.”
 
Requirements: 
 
The code should be run using R version 3.6.1 or earlier. The scripts were tested on the denoted versions of the packages. 
· ggplot2 3.2.1
· gridExtra 2.3
· glm.predict 3.1-0 MASS_7.3-51.4
· gtools 3.8.1
· ggpubr 0.4.0
· ivpack 1.2
 
File Naming Conventions:
 
Each script is named for the figure or table it creates. The naming convention is [Table/Figure] [number].R. For calculations and additional analyses referenced in text, the naming convention is [Paper/Appendix][in text calcs].R
 
Instructions:
 
Download the replication archive and make a note of the path. Open an R session. 

The code “[download location path]/Replication/code/0_Run_All.R” will call all the necessary files to reproduce all figures and tables in the paper and appendix. Create a subfolder called "output" and place in "[download location path]/Replication/code." Figures and tables will be saved to the subfolder "[download location path]/Replication/code/output.”  

The code in the subfolder: "[download location path]/Replication/code/03_Intext_Calculations” contains code files for calculations and additional analyses referenced in the paper and appendix. These should be run separately and the results can be viewed directly in the R terminal.
 
Datasets:
 
The replication archive contains three datasets: “dynatadata.csv” contains data from Dynata pre-test, “tessdata.csv” contains the data from main experiment conducted on NORC Amerispeak Panel, and “statemortality.csv” contains the CDC data for the state-level opioid mortality analysis. Data is saved in “[download location path]/Replication/code.”
 
Description of Files and Subfolders:

[download location path]/Replication” – Folder containing all files
· “Codebook.pdf” – PDF lists and describes all variables used in analyses 

"[download location path]/Replication/code” – Subfolder containing code and data files
· “dynatadata.csv” – Data from pre-test (Dynata panel)
· “tessdata.csv” – Data from main experiment (NORC Amerispeak Panel)
· “statemortality.csv” – Data for the state-level opioid mortality analysis (CDC) 
· “0_Run_All.R” – Master code file that calls all necessary files to reproduce figures and tables
· “opioid_frames_log.txt” – Log file that will report and describe figures and tables during 	replication process

"[download location path]/Replication/code/01_Main_Paper” – Subfolder containing replication code files for paper
· “Figure1.R” – Code to reproduce Figure 1 (paper)
· “Figure2.R” – Code to reproduce Figure 2 (paper)
· “Figure3.R” – Code to reproduce Figure 3 (paper)
· “Figure4.R” – Code to reproduce Figure 4 (paper)

"[download location path]/Replication/code/02_Appendix” – Subfolder containing code files for appendix[1]
· “Figures A2 - A3.R” – Code to reproduce Figures A2 and A3 (appendix)
· “Figure A4.R” – Code to reproduce Figures A4 (appendix)
· “Figure A5.R” – Code to reproduce Figures A5 (appendix)
· “Figure A6.R” – Code to reproduce Figures A6 (appendix)
· “Figure A7.R” – Code to reproduce Figures A7 (appendix)
· “Figure A8.R” – Code to reproduce Figures A8 (appendix)
· “Tables A1 - A2.R” – Code to reproduce Tables A1 and A2 (appendix)
· “Tables A4 – A10.R” – Code to reproduce Tables A4 and A10 (appendix)
· “Tables A11 – A20.R” – Code to reproduce Tables A11 and A20 (appendix)

"[download location path]/Replication/code/03_Intext_Calculations” – Subfolder containing code files for calculations and additional analyses that appear in the text of the paper and appendix
· “Paper in text calcs.R” – Code to reproduce in-text calculations (paper)
· “Appendix in text calcs.R” – Code to reproduce in-text calculations (appendix)
 

"[download location path]/Replication/code/output” – Subfolder where figures and tables will be printed


[1] Figures A1 and Table A3 do not draw on empirical data

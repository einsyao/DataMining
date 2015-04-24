It is provided as four R files. Each file corresponds one task. It is essential to load the packages in advance. Besides, set this folder as default folder. keep the treetagger file exist. The task 1 may be run 5 hours.

In task 3, when users want to change the dataset, it can be finished manually. 
	line 16, 17. there are two different dataframe. 
	line 45,46 and line 136,137: both are loading ' two types of data frame'.
	line 74 to 77 and line 142 to 145: four different classifers. make sure only the purpose classifer don't have the comment mark.

For example, 
	ds8<-ds81
	#ds8<-ds82
if you want to use the ds82,delete the comment mark and comment the other line.
	#ds8<-ds81
	ds8<-ds82

The default version in task 3 is to run the combination data version. If you want to try the LDA version, the following lines should be changed, line 16, line 40, line 46, line 137. If you want to change it back to the combination data, you should comment the above line and run line 21, 41, 47,138.


line 75,146 run SVM as default
Line 76,147 run Naive Bayes
line 78,149 run RandomForest 


Finally, I also provide some data in the data back up document.
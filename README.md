This program is written to take an absorbance readout of a 96 well plate, divide it into four plates, and graph 4 parameter non linear regression on the experiments. The output is 4 pdf graphs for each plate.

The program requires the R gdata library and the installXLSsupport function to read the data from the excel file. 
To do the regression, the drc library is required.

To run the program, the items at the top of the code need to be changed, especially the directory in which your data files are located and where you want the output pdf files.

In the data folder are test data (though the files are copies of each other). If the program works correctly, you should have 12 pdfs in the folder specified pdfSaveDirectory, each graphing a quarter of the plate. Each quarter's graph will be labeled with the xls file name and A-D. An example graph would look like this: 


![](http://i.imgur.com/Sqskq.png)

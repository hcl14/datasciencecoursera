# This is my programming assignment for the Practical Machine Learning course on Coursera

To view the html file you can use rawgit link:


[https://cdn.rawgit.com/hcl14/datasciencecoursera/master/randomforesmodel_out.html](https://cdn.rawgit.com/hcl14/datasciencecoursera/master/randomforesmodel_out.html)

Here I have obtained an interesting result - although my model looks like an overfit, it clearly predicts on the test data which may point to either strong corellation or something strange about the data origin.

You can easily view html files from Github by generating links on [https://rawgit.com](https://rawgit.com) (but sometimes they are not updated, so check it always).

.Rmd file contains R markup to generate this html, and the .R file contatins my code.

**Important!**
If you would like to run my code stored in the randomforesmodel.R, you should do it *twice*, I have no idea why. I have set the random seed but on the first run the program somehow produces corrupted model and reports all predictions as NA's. But just running it second time gives exactly the results provided in a html report. My opinion is that something may be caused by R's lazy loading, so some data isn't fully loaded when it's needed.

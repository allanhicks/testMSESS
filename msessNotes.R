1) Sometimes it is nice to have input files and report.sso for each assessment in case we didn't save some info we needed.  
I added code to readLines the SS input files and Report file and save as a separate R object called ssFiles???.Rdat object.

2) It would be nice to get rid of error opening ss3.dat. This only shows up in the command window (because cerr is used)
--can do this by either call ss3 -ind THEDATAFILENAME or place an empty ss3.dat file in the directory
--I added code to create an empty ss3.dat file, which is deleted later

3) I added an argument for printstats, which eliminates a lot of r4ss output from SS_output when printstats=F

4) The object convCheck wasn't numeric in the while statement (~line 757), thus the statement always evaluated to TRUE
   until MAX.ITERATIONS was exceeded
--added as.numeric(convCheck[3])

5) It is important to have the correct version of r4ss. When I updated to the 2013 model, I was getting NA's. Updating r4ss fixed it.
   So, I put in an argument called r4ssDir. If NULL (default) it runs update_r4ss_files() to get most recent version.
   If you supply a directory path, it sources from that path (faster than updating from web, and you can use a specific version).
   If you put in NA, it doesn't do the updating at all. Just loads the package.
   
6) I start with both the assessment_control.ss and forecastAssessment.ss files in the scenario directory. It doesn't matter where there
   are at the start since everything from the operating model is copied over.  However, I do not delete it at the end of the run.
   
   
Fix ageing error to occur after multinomial    

Rterm.exe is not found on my path by default

Readme needs to include setting up of Scenarios folder. Why not have it create the folder if the directory name is missing?

Example process scripts do not line up with commented line in scenarios. Why is scen=1 when scenario 1 is commented out.
I guess that it is the number of the row. It would be nice to have a scen column and then you don't have to comment out any lines.
You can simply have a scenario column and call that scenario number in the function call.  Also, the checking of directories is cumbersome
due to failures. Maybe having it create hte directory if the name does not exist.

why is readme a .md file?

Instructions for running merge() function are vague. I imagine you have to set the working directory.

The modification of the control and par files are very specific to hake and may have to be recoded for every assessment, 
and possibly each year of hake.  They are fine for updating to the 2013 model since there were minimal changes.

When running in R, it pauses just before doing an assessment. (says Browse[1]>). This is the broswer() command in runSingleAssessment.

It may be useful to have ability for non-annual assessments.

should put assessment_control in OperatingModels, as with forecastassessment.sso, or vice versa

Should have update_r4ss, or call a specific version. It changes, and some differences may occur

posterior_mse and posterior_selex are the same file. I had renamed them in previous versions of MSE code.  
I changed .MSE_POST_FILE and .SELEX_POST_FILE to reference the same files.

Is it doing mceval between each scenario? Does it need to? It is time consuming.




MODIFICATIONS TO SS3.24S
--added in test for existence of mcmc .sso files so that error is not present when they do not exist (lines 14230-14240)

--added in output in files called posterior_natage.sso and posterior_selex.sso
---multiple locations
---file creation, headers, and output

--output an additional column in posteriors.sso for Q (lines 14268 and 14419)
----this is specific to using an analytical q, and I'm not sure how it will respond when not.

--when running mcmc, does "First forecast loop with stochastic recruitment" have to be 1?  NO



PROCESS OF SETTING UP A SIMULATION
-- run mcmc on model with forecast years set to number of years into the future that you want stochastic recruitment



















If I change the operating model, it is missing ss3.par in an assessment run. What am I missing?  You may have to run at least 1000 iterations of the mcmc. i was doing testing with smaller runs to reduce time. This is an ADMB thing to write out posterior stuff, it has to go beyond the iterations where it is scaling.

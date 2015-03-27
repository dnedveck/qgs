### 3/26

The weekly report should take in the gleeo data, and generate a report of the time spent that week 

possibly I could take this and have a script that just generates reports, reports for weeks, and then a report for a month. This could work where the script checks an output dir for the presence of reports of a certain week / month, and if they are not there, but the script has data from gleeo for it, than it generates it. 

The question now is that I need to take the processGleeoExport.r and turn it into a script that I could source from the report generator script, so it could work something like this:

1. makeReport.r is given the gleeoData
2. sources the processGleeo.r
3. uses a function present in processGleeo.r 

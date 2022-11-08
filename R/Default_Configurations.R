User_Configuration <- base::list(
  'Maximum_Level_For_Group_By' = 20,#If the levels of a column be more than this then application would ignore it. Note that too many levels makes the plots unuseful
  'Open_In_Chrome' = FALSE, #Do you want to open the application in Chrome?
  'Debuge_Mode' = TRUE,# Do want enter the developer mode?
  'Maximum_Number_of_Columns' = 500,#Number of columns in a dataset must not be more than this, otherwise application may not work properly
  'Replace_Reserved_Letters' = FALSE,#if there be invalid letter in the name of columns, should it be replaced by hyphen?
  'Ignore_Reserved_Letters' = TRUE, #should the names of columns be validated?
  'Path_For_Saving_Results' = 'C:/Users/Alihdr/Desktop/Results' # All the output figures will be saved here
)


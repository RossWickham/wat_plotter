@echo off
SETLOCAL ENABLEEXTENSIONS
title FRA to SQL
rem Finding the most current version of R installed and running script
rem executable path should be something like this: "C:\Program Files\R\R-3.3.2\bin\x64\R.exe"

rem The assumed base R directory and script name
set scriptname=scripts/fra_to_sql.R

rem retrieving R, Java, and local server url configuration
call r_java_config.bat


echo Executing the following script: %scriptname%
echo Executing R from the following executable: %rExecutablePath%

@echo on
"%rExecutablePath%" "%scriptname%" %defaultHost% %defaultPort% %baseJavaDir%

@echo off
echo Finished Execution of script.

pause
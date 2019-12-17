@echo off
SETLOCAL ENABLEEXTENSIONS
title WAT Plotter
rem This runs the R scripts that launch the WAT Plotter

rem retrieving R and Java configuration
call r_java_config.bat

rem for execution of shiny app
set rScript=webapp\run.R

rem for testing and debugging
rem set rScript=webapp\test.R

echo --- Launching WAT Plotter ------


set defaultURL=http://%defaultHost%:%defaultPort%/

echo Host IP: %defaultHost%
echo Port:    %defaultPort%
echo url:     %defaultURL%
echo R Executable Path: %rExecutablePath%
echo Java Path: %baseJavaDir%
echo webapp location:   %rScript%
echo ---------------------------------
echo.
echo.
echo.
echo Navigate browser to the following url:  %defaultURL%
echo   (Use Chrome or Firefox)
echo.
echo.



rem passing arguments to R script for host and port
%rExecutablePath% "%rScript%" %defaultHost% %defaultPort% %baseJavaDir%


pause
@echo off

rem Set the path to the Rscript executable
set RSCRIPT="portable-r-4.6.0-win-x64\bin\Rscript.exe"

rem Set the path to the R script to execute
set RSCRIPT_FILE="app.R"

rem Execute the R script
%RSCRIPT% %RSCRIPT_FILE%

REM This will open the URL in whatever browser the user has set as their default.
rem START http://127.0.0.1:7158/

rem Pause so the user can see the output
exit
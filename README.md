# PRZM5lib
This is a version of PRZM5 with the input and output routines replaced so it can be compiled as a dll called directly as a library function. It takes the standard PRZM5 input (.inp) file and an array holding weather data as input and returns an array with the results.

The error checking done by PRZM5 on the input has also been removed, but since the input format is unchanged, the input string can be written to a file and run through PRZM5.exe

/*
resizes an image by four, using massive pattern matching.

General form:
XXX    YYYY
XXX -> YYYY
XXX    YYYY
       YYYY

*/
void oren4x( unsigned char * pIn, unsigned char * pOut, int Xres, int Yres, int srcBpL, int BpL);

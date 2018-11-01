program
   searchnews;
uses
   crt, dos, sysutils, STRUTILS, ARCHIVOS, RESTO;
{****************************************************************************}
{programa Principal}
VAR
   X: BYTE;
BEGIN
   IF LOWERCASE(RIGHTSTR(PARAMSTR(0), 3)) = 'exe' THEN
      CHDIR(MIDSTR(PARAMSTR(0), 1, LENGTH(PARAMSTR(0))-14))
   ELSE
      CHDIR(MIDSTR(PARAMSTR(0), 1, LENGTH(PARAMSTR(0))-10));
   CLRSCR;
   FOR X:= 0 TO PARAMCOUNT DO
      WRITE(PARAMSTR(X), CHR(32));
   WRITELN;
   If paramcount=0 Then
      Error
   Else
      Hay_parametros
{****************************************************************************}
END.


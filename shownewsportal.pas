PROGRAM
   shownewsportal;

USES
   CRT, DOS, SYSUTILS, STRUTILS, DATEUTILS, ARCHIVOS, RESTO;
{****************************************************************************}
{PROGRAMA PRINCIPAL}
VAR
   VECTOR: T_VECTOR;
   MAX, ERROR, X: BYTE;
BEGIN
   ERROR:= 0;
   MAX:= 0;

   DATESEPARATOR:= '/';
   SHORTDATEFORMAT:= 'dd/MM/yyyy';

   IF LOWERCASE(RIGHTSTR(PARAMSTR(0), 3)) = 'exe' THEN
      CHDIR(MIDSTR(PARAMSTR(0), 1, LENGTH(PARAMSTR(0))-18))
   ELSE
      CHDIR(MIDSTR(PARAMSTR(0), 1, LENGTH(PARAMSTR(0))-14));

   CLRSCR;
   FOR X:= 0 TO PARAMCOUNT DO
      WRITE(PARAMSTR(X), CHR(32));
   WRITELN;
   WRITELN;

   ULTIMAS(VECTOR, MAX);

   CASE PARAMCOUNT OF
      0:
         PORTAL(VECTOR, MAX, '');
      1:
         IF VALEXT(PARAMSTR(1), 'html') THEN
            PORTAL(VECTOR, MAX, '..\out\' + PARAMSTR(1))
         ELSE
            SINPARAMETROS('shownewsportal', ERROR)
      ELSE
         SINPARAMETROS('shownewsportal', ERROR)
   END
END.

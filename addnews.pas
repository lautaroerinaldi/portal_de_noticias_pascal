PROGRAM
   addnews;

USES
   CRT, DOS, SYSUTILS, DATEUTILS, STRUTILS, ARCHIVOS, RESTO;
{***************************************************************************}
{PROGRAMA PRINCIPAL}
PROCEDURE PRINCIPAL;
VAR
   INFORMACION: T_INFORMACION;
   X, ERROR: BYTE;
   ESCAPE: BOOLEAN;
BEGIN
   DATESEPARATOR:= '/';
   SHORTDATEFORMAT:= 'dd/MM/yyyy';

   ERROR:= 0;

   IF LOWERCASE(RIGHTSTR(PARAMSTR(0), 3)) = 'exe' THEN
      CHDIR(MIDSTR(PARAMSTR(0), 1, LENGTH(PARAMSTR(0))-11))
   ELSE
      CHDIR(MIDSTR(PARAMSTR(0), 1, LENGTH(PARAMSTR(0))-7));

   CLRSCR;
   FOR X:= 0 TO PARAMCOUNT DO
      WRITE(PARAMSTR(X), CHR(32));
   WRITELN;
   WRITELN;

   IF PARAMCOUNT <> 1 THEN
      SINPARAMETROS('addnews', ERROR)
   ELSE
   BEGIN
      WRITE('Verificando la existencia del archivo ' + PARAMSTR(1) + ': ');
      IF FILEEXISTS(PARAMSTR(1)) THEN
      BEGIN
         WRITELN('OK');
         WRITELN;
         WRITE('Verificando que el archivo sea de texto: ');
         IF VALEXT(PARAMSTR(1), 'TXT') THEN
         BEGIN
            WRITELN('OK');
            WRITELN;
            IF NOT NOTICIAENBLANCO(PARAMSTR(1), ERROR) THEN
            BEGIN
               CARGARINFORMACION(INFORMACION, ESCAPE);
               IF ESCAPE = FALSE THEN
               BEGIN
                  INFORMACION.REFERENCIA:= PARAMSTR(1);
                  NOTICIASDAT(INFORMACION, ERROR);
                  IF ERROR = 0 THEN
                  BEGIN
                     COPIARARCHIVO(INFORMACION.NUMNOTICIA, INFORMACION.REFERENCIA, ERROR);
                     IF ERROR = 0 THEN
                     BEGIN
                        WRITELN('La noticia dada de alta tiene el n£mero:');
                        WRITELN(INFORMACION.NUMNOTICIA);
                        WRITELN;
                        WRITELN('Indexando noticia...');
                        INDEXAR(INFORMACION.NUMNOTICIA, ERROR);
                        IF ERROR = 0 THEN
                           WRITELN('OK. Noticia Agregada Satisfactoriamente.')
                        ELSE
                           WRITELN('La noticia no se pudo indexar.')
                     END
                     ELSE
                        WRITELN('La noticia no se ha agregado.')
                  END
               END
            END
            ELSE
               WRITELN('La noticia ', PARAMSTR(1), ' se encuentra en BLANCO.')
         END
         ELSE
         BEGIN
            WRITELN('FAILED');
            WRITELN;
            WRITELN('El archivo ', PARAMSTR(1), ' NO es de texto.')
         END
      END
      ELSE
      BEGIN
         WRITELN('FAILED');
         WRITELN;
         WRITELN('El archivo ', PARAMSTR(1), ' NO existe.')
      END
   END;
   IF ESCAPE = TRUE THEN
      CLRSCR;
   IF ERROR <> 0 THEN
      WRITELN('La aplicaci¢n termin¢ de forma inesperada.' + #10#13 + 'C¢digo de ERROR: ', ERROR)
END;
{***************************************************************************}
BEGIN
   PRINCIPAL
END.

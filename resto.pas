unit
   resto;

interface
{***************************************************************************}
{***************************************************************************}
uses
   CRT, DOS, SYSUTILS, STRUTILS, DATEUTILS;

Const
     A = '..\Indices\'; {Direccion de la carpeta de archivos indexados}
     B = '.idx'; {Extension de los archivos indexados}
     Q = '..\bd\'; {carpeta en la que se guardan los resultados de la busqueda}

type
   T_CONJBYTE = SET OF BYTE;
   T_CONJCHAR = SET OF CHAR;
   T_INFORMACION = RECORD
      TITULO: STRING[80];
      FECHA: TDATETIME;
      RESUMEN: STRING[240];
      AUTOR: STRING[50];
      NUMNOTICIA: WORD;
      REFERENCIA: STRING
   END;
   T_VECTOR = ARRAY[0..5] OF T_INFORMACION;

   PROCEDURE VERIFCHR(MAL, BIEN: STRING; VAR CONJMAL: T_CONJBYTE; VAR MINIMO: BYTE; VAR ERROR: BYTE);
   FUNCTION CADENANOVACIA(CADENA: ANSISTRING; NOBLANCOS: T_CONJCHAR): BOOLEAN;
   FUNCTION VALEXT(NOMBREARCHIVO: STRING; EXTENSION: STRING): BOOLEAN;
   PROCEDURE FILTRO(VAR TEXTO1: ANSISTRING; MAL, BIEN: STRING; CONJMAL: T_CONJBYTE; MINIMO: BYTE);
   PROCEDURE CARGARINFORMACION(VAR INFORMACION: T_INFORMACION; VAR ESCAPE: BOOLEAN);
   PROCEDURE INTERCAMBIO(VAR A: T_INFORMACION; VAR B: T_INFORMACION);
   FUNCTION CADENANOVALIDA(CADENA: ANSISTRING; VALIDOS: T_CONJCHAR): BOOLEAN;
   Procedure Error;
   Procedure no_hay_noticia;
   Procedure encabezado_html;
   Procedure Cuerpo_html (reg: T_INFORMACION);
   Procedure cerrar_html;

implementation
{***************************************************************************}
{***************************************************************************}
PROCEDURE VERIFCHR(MAL, BIEN: STRING; VAR CONJMAL: T_CONJBYTE; VAR MINIMO: BYTE; VAR ERROR: BYTE);
{Realiza una peque¤a comprobaci¢n del archivo auxiliar CAMBIOS.ROL;
El mismo, contiene los caracteres que han de ser reemplazados antes de la
indexaci¢n.
El mero objetivo es que el programa no aborte durante su ejecuci¢n debido a
una modificaci¢n no intencional del archivo auxiliar.}
VAR
   X: BYTE;
BEGIN
   IF (LENGTH(MAL) = LENGTH(BIEN)) AND (LENGTH(MAL) > 0) THEN
   BEGIN
      CONJMAL:= [];
      MINIMO:= ORD(MAL[1]);
      FOR X:= 1 TO LENGTH(MAL) DO
      BEGIN
         INCLUDE(CONJMAL, ORD(MAL[X]));
         IF MINIMO > ORD(MAL[X]) THEN
            MINIMO:= ORD(MAL[X])
      END;
      MINIMO:= MINIMO - 1;
      FOR X:= 1 TO LENGTH(BIEN) DO
         IF ORD(BIEN[X]) IN CONJMAL THEN
            ERROR:= 3
   END
   ELSE
      ERROR:= 3
END;
{***************************************************************************}
FUNCTION CADENANOVACIA(CADENA: ANSISTRING; NOBLANCOS: T_CONJCHAR): BOOLEAN;
{Comprueba si una cadena de caracteres contiene solamente caracteres
insignificantes para el programa (se tratan como si fueran nulos),
dici‚ndole previamente cu les son los que si interesan.}
VAR
   X: WORD;
BEGIN
   CADENANOVACIA:= FALSE;
   X:= 0;
   WHILE (X < LENGTH(CADENA)) AND (NOT CADENANOVACIA) DO
   BEGIN
      INC(X);
      IF (CADENA[X] IN NOBLANCOS) THEN
         CADENANOVACIA:= TRUE
   END
END;
{***************************************************************************}
FUNCTION VALEXT(NOMBREARCHIVO: STRING; EXTENSION: STRING): BOOLEAN;
{Dado un nombre de archivo (con su ruta o sin ella), valida si la extensi¢n
es la que uno quiere. Se utiliza para saber si un archivo es de texto o no.}
BEGIN
   IF UPCASE(RIGHTSTR(NOMBREARCHIVO, LENGTH(EXTENSION) + 1)) = ('.' + UPCASE(EXTENSION)) THEN
      VALEXT:= TRUE
   ELSE
      VALEXT:= FALSE
END;
{***************************************************************************}
PROCEDURE FILTRO(VAR TEXTO1: ANSISTRING; MAL, BIEN: STRING; CONJMAL: T_CONJBYTE; MINIMO: BYTE);
{Dada una cadena de caracteres, se encarga de eliminar todos aquellos que no
se deben indexar, y de reemplazar los que se indican en el archivo auxiliar
CAMBIOS.ROL. Deja £nicamente una concatenaci¢n de palabras v lidas.}
VAR
   TEXTO2: ANSISTRING;
   X, Y: WORD;
   CARACTER: BYTE;
BEGIN
   TEXTO2:= '';
   TEXTO1:= LOWERCASE(TEXTO1);
   FOR X:= 1 TO LENGTH(TEXTO1) DO
   BEGIN
      CARACTER:= ORD(ANSIMIDSTR(TEXTO1, X, 1)[1]);
      IF (CARACTER > 96) AND (CARACTER < 123) THEN
         TEXTO2:= TEXTO2 + CHR(CARACTER)
      ELSE
         IF CARACTER IN CONJMAL THEN
         BEGIN
            Y:= 0;
            WHILE (Y < LENGTH(MAL)) AND (CARACTER > MINIMO) DO
            BEGIN
               INC(Y);
               IF CARACTER = ORD(MAL[Y]) THEN
                  CARACTER:= ORD(BIEN[Y])
            END;
            TEXTO2:= TEXTO2 + CHR(CARACTER)
         END
         ELSE
            TEXTO2:= TEXTO2 + CHR(32)
   END;
   TEXTO2:= DELSPACE1(TEXTO2);
   TEXTO1:= TRIMSET(TEXTO2, [CHR(32)]) + CHR(32);
END;
{***************************************************************************}
PROCEDURE ESC(VAR ESCAPE: BOOLEAN);
{En caso que se presione la tecla ESC, este realiza una visualizaci¢n en
pantalla del pedido de confirmaci¢n, y la lectura de la nueva entrada.}
VAR
   TECLA: CHAR;
BEGIN
   GOTOXY(9, 24);
   TEXTCOLOR(10);
   WRITE('°°°°°±±±±±²²²²² ');
   TEXTCOLOR(14);
   WRITE('CONFIRMA QUE DESEA SALIR (S/N)');
   TEXTCOLOR(10);
   WRITE(' ²²²²²±±±±±°°°°°');
   TECLA:= READKEY;
   CASE UPCASE(TECLA) OF
      'S':
         ESCAPE:= TRUE;
      ELSE
         BEGIN
            IF ORD(TECLA) = 0 THEN
               READKEY;
            GOTOXY(1, 24);
            CLREOL;
            TEXTCOLOR(7)
         END
   END
END;
{***************************************************************************}
PROCEDURE CARGARFECHA(TEXTO: STRING; VAR FECHA: TDATETIME; COL, FILA: BYTE; VAR ESCAPE: BOOLEAN);
{Permite que se ingresen solamente n£meros, mostrando con antelaci¢n las
barras. Valida que la fecha ingresada sea correcta, sino, pide que se
reingrese la misma. El ingreso es obligatorio.}
VAR
   CONT: BYTE; {CUENTA EL NUMERO DE CARACTERES INGRESADOS HASTA EL MOMENTO}
   CARACTER: CHAR; {GUARDA CADA TECLA PRESIONADA (INCLUYENDO LAS NO VALIDAS)}
   DIA, MES, ANO, CADENA, POSCURSOR: STRING;
   {CADENA TIENE CONCATENADOS LOS CARACTERES VALIDOS INGRESADOS}
   {POSCURSOR HACE QUE EL CURSOR TITILE DONDE VA A INGRESARSE UNA TECLA}
BEGIN
   IF ESCAPE = FALSE THEN
   BEGIN
      WRITELN(TEXTO);
      CONT:= 0;
      POSCURSOR:= '0001030406070809';
      CADENA:= '';
      INC(FILA)
   END;
   WHILE  (CONT < 9) AND (ESCAPE = FALSE) DO
   BEGIN
      IF CONT = 0 THEN
      BEGIN
         GOTOXY(COL, FILA);
         WRITE('  /  /    ')
      END;
      IF CONT < 8 THEN
         GOTOXY(COL + STRTOINT(MIDSTR(POSCURSOR, CONT * 2 + 1, 2)), FILA);
      CARACTER:= READKEY;
      CASE ORD(CARACTER) OF
         0:
            READKEY;
         48..57:
            IF CONT < 8 THEN
            BEGIN
               CADENA:= CADENA + CARACTER;
               INC(CONT);
               WRITE(CARACTER)
            END;
         8:
            IF CONT > 0 THEN
            BEGIN
               DEC(CONT);
               CADENA:= LEFTSTR(CADENA, CONT);
               GOTOXY(COL + STRTOINT(MIDSTR(POSCURSOR, CONT * 2 + 1, 2)), FILA);
               WRITE(CHR(0));
            END;
         27:
            ESC(ESCAPE);
         13:
            IF CONT = 8 THEN
            BEGIN
               DIA:= MIDSTR(CADENA, 1, 2);
               MES:= MIDSTR(CADENA, 3, 2);
               ANO:= MIDSTR(CADENA, 5, 4);
               IF ISVALIDDATE(STRTOINT(ANO), STRTOINT(MES), STRTOINT(DIA)) THEN
               BEGIN
                  INC(CONT);
                  WRITELN;
                  FECHA:= STRTODATE(DIA + DATESEPARATOR + MES + DATESEPARATOR + ANO)
               END
               ELSE
               BEGIN
                  CONT:= 0;
                  CADENA:= '';
                  GOTOXY(COL, FILA + 2);
                  TEXTCOLOR(15);
                  WRITE('La fecha ingresada NO ES VALIDA' + #10#13 + 'Presione cualquier tecla e ingrese nuevamente la FECHA.');
                  TEXTCOLOR(7);
                  READKEY;
                  GOTOXY(COL, FILA + 2);
                  DELLINE;
                  DELLINE
               END
            END
      END
   END;
   WRITELN
END;
{***************************************************************************}
PROCEDURE CARGARSTRING(TEXTO: STRING; VAR CADENA: STRING; MAXCARACTERES, COLUMNA, FILA: BYTE; VAR ESCAPE: BOOLEAN);
{Realiza una carga obligatoria de una variable tipo STRING, verificando que
los caracteres de la misma no sean solamente nulos, limitando la cantidad
m xima de ellos. Trabaja en multil¡nea.}
VAR
   CONT, NL: BYTE;  {CUENTA EL NUMERO DE CARACTERES INGRESADOS HASTA EL MOMENTO + 1}
   CARACTER: CHAR; {GUARDA CADA TECLA PRESIONADA (INCLUYENDO LAS NO VALIDAS)}
BEGIN
   IF ESCAPE = FALSE THEN
   BEGIN
      WRITELN(TEXTO);
      CONT:= 1;
      NL:= 0;
      CADENA:= '';
      INC(FILA)
   END;
   WHILE (CONT < (MAXCARACTERES + 2)) AND (ESCAPE = FALSE) DO
   BEGIN
      GOTOXY(COLUMNA + CONT - (NL * 80) - 1, FILA + NL);
      CARACTER:= READKEY;
      CASE ORD(CARACTER) OF
         0:
            READKEY;
         32..254:
            IF (CONT - 1) < MAXCARACTERES THEN
            BEGIN
               CADENA:= CADENA + CARACTER;
               WRITE(CARACTER);
               INC(CONT);
               IF ((CONT MOD 80) = 1) AND (CONT <> 1) THEN
                  INC(NL)
            END;
         8:
            IF (CONT - 1) > 0 THEN
            BEGIN
               IF (CONT MOD 80) = 1 THEN
                  DEC(NL);
               DEC(CONT);
               CADENA:= LEFTSTR(CADENA, CONT - 1);
               GOTOXY(COLUMNA + CONT - (NL * 80) - 1, FILA + NL);
               WRITE(CHR(0))
            END;
         27:
            ESC(ESCAPE);
         13:
            IF ((CONT - 1) > 0) AND (CADENANOVACIA(CADENA, [CHR(33)..CHR(254)])) THEN
            BEGIN
               CONT:= MAXCARACTERES + 2;
               WRITELN
            END
      END
   END;
   WRITELN
END;
{***************************************************************************}
PROCEDURE CARGARINFORMACION(VAR INFORMACION: T_INFORMACION; VAR ESCAPE: BOOLEAN);
{Hace una lectura secuencial de todos los campos de entrada obligatoria, que
luego se agregar n al archivo NOTICIAS.DAT.}
BEGIN
   ESCAPE:= FALSE;
   CARGARSTRING('Ingrese el TITULO de la Noticia (MAX 80 caracteres).', INFORMACION.TITULO, 80, WHEREX, WHEREY, ESCAPE);
   CARGARFECHA('Ingrese la FECHA (dd/MM/aaaa) y presione ENTER para continuar.', INFORMACION.FECHA, WHEREX, WHEREY, ESCAPE);
   CARGARSTRING('Ingrese el RESUMEN de la Noticia (MAX 240 caracteres).', INFORMACION.RESUMEN, 240, WHEREX, WHEREY, ESCAPE);
   CARGARSTRING('Ingrese el AUTOR de la Noticia (MAX 50 caracteres).', INFORMACION.AUTOR, 50, WHEREX, WHEREY, ESCAPE);
END;
{***************************************************************************}
PROCEDURE INTERCAMBIO(VAR A: T_INFORMACION; VAR B: T_INFORMACION);
{Dados dos registros de tipo T_INFORMACION, se encarga de intercambiar sus
contenidos, mediante copias binarias (no son copias campo a campo).}
VAR
   AUX: T_INFORMACION;
BEGIN
   AUX:= A;
   A:= B;
   B:= AUX
END;
{****************************************************************************}
FUNCTION CADENANOVALIDA(CADENA: ANSISTRING; VALIDOS: T_CONJCHAR): BOOLEAN;
{Esta funci¢n considera que una cadena de caracteres (STRING) es no v lida,
si contiene al menos un car cter que no se encuentra en el conjunto de los
v lidos. Se utiliza en el programa shownews, para saber si al ingresar el
n£mero de noticia, el usuario no intercala alg£n otro car cter que no sea
num‚rico, generando en el programa un error de conversi¢n de tipos.}
VAR
   X: WORD;
BEGIN
   CADENANOVALIDA:= FALSE;
   X:= 0;
   WHILE (X < LENGTH(CADENA)) AND (NOT CADENANOVALIDA) DO
   BEGIN
      INC(X);
      IF NOT (CADENA[X] IN VALIDOS) THEN
         CADENANOVALIDA:= TRUE
   END
END;
{***************************************************************************}
Procedure Error;
Begin
     Writeln;
     Writeln('Modo de uso');
     Writeln;
     Writeln('C:\XX\YY\Bin>searchnews.exe {palabra 1} {palabra 2} {AAAA.html}');
     Writeln('{palabra 1} Palabra que se desea buscar en las noticias.');
     Writeln('{palabra 2} Palabra que se desea buscar en las noticias (Opcional).');
     Writeln('{AAAA.html} Archivo .html donde se guarda el resultado de la busqueda (Opcional)');
     Writeln('Se puede buscar un maximo de 2 palabras simultaneamente.')
End;
{***************************************************************************}
Procedure no_hay_noticia;
Begin
   writeln;
   writeln ('<html>');
   writeln (' <head>');
   writeln ('  <title>','Resultado busqueda','</title>');
   writeln (' </head>');
   writeln (' <body>');
   Writeln ('  <p><b>El resultado de la b£squeda es...</b></p>');
   Writeln ('  <br>');
   writeln ('  <p>No se encontraron noticias que cumplan ese criterio</p>');
   writeln (' </body>');
   writeln ('</html>')
End;
{****************************************************************************}
Procedure encabezado_html;
Begin
   writeln;
   writeln ('<html>');
   writeln (' <head>');
   writeln ('  <title>','El Resultado de la busqueda es','</title>');
   writeln (' </head>');
   writeln (' <body>')
End;
{****************************************************************************}
Procedure Cuerpo_html (reg: T_INFORMACION);
Begin
   WRITELN('  <p><A HREF=/shownews.exe?-noticia?',inttostr(reg.NUMNOTICIA), '>', reg.titulo, '</A></p>')
End;
{****************************************************************************}
Procedure cerrar_html;
Begin
   writeln (' </body>');
   writeln ('</html>')
End;
{****************************************************************************}
end.


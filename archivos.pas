unit
   archivos;

interface
{***************************************************************************}
{***************************************************************************}
uses
   CRT, DOS, SYSUTILS, STRUTILS, DATEUTILS, RESTO;

   PROCEDURE COPIARARCHIVO(NUMNOTICIA: WORD; RUTA: STRING; VAR ERROR: BYTE);
   PROCEDURE NOTICIASDAT(VAR INFORMACION: T_INFORMACION; VAR ERROR: BYTE);
   FUNCTION NOTICIAENBLANCO(RUTA: STRING; VAR ERROR: BYTE): BOOLEAN;
   PROCEDURE INDEXAR(NUMNOTICIA: WORD; VAR ERROR: BYTE);
   PROCEDURE SINPARAMETROS(COMANDO: STRING; VAR ERROR: BYTE);
   PROCEDURE PORTAL(VECTOR: T_VECTOR; MAX: BYTE; RUTA: STRING);
   PROCEDURE ULTIMAS(VAR VECTOR: T_VECTOR; VAR MAX: BYTE);
   Function archivo_existe(var ar:file):boolean;
   Function texto_existe(var ar:text):boolean;
   Procedure htmlnews(reg:T_INFORMACION);
   Procedure htmlnews2file(reg:T_INFORMACION; out:string);
   Procedure Crear_salida (D, E, F, G, H: byte; reg: T_INFORMACION);
   Function Existe (var c: file of Word): boolean;
   Procedure una_palabra (Out: boolean);
   Procedure dos_palabras(out:boolean);
   Procedure Verificar;
   Procedure Hay_parametros;

implementation
{***************************************************************************}
{***************************************************************************}
PROCEDURE ROLCHR(VAR MAL, BIEN: STRING; VAR CONJMAL: T_CONJBYTE; VAR MINIMO: BYTE; VAR ERROR: BYTE);
{Hace una lectura desde un archivo de los caracteres que han de ser
reemplazados antes de indexar.}
VAR
   ARCHIVO: TEXT;
BEGIN
   IF FILEEXISTS('..\auxi\cambios.rol') THEN
   BEGIN
      ASSIGN(ARCHIVO, '..\auxi\cambios.rol');
      {$I-}
      RESET(ARCHIVO);
      {$I+}
      IF IORESULT = 0 THEN
      BEGIN
         READLN(ARCHIVO, MAL);
         READLN(ARCHIVO, BIEN);
         CLOSE(ARCHIVO);
         VERIFCHR(MAL, BIEN, CONJMAL, MINIMO, ERROR)
      END
      ELSE
         ERROR:= 2
   END
   ELSE
      ERROR:= 1
END;
{***************************************************************************}
FUNCTION NOTICIAENBLANCO(RUTA: STRING; VAR ERROR: BYTE): BOOLEAN;
{Verifica que una noticia no contenga solamente caracteres NULOS (en ese caso
no tendr¡a sentido alguno agregarla).}
VAR
   ARCHIVO: TEXT;
   RENGLON: ANSISTRING;
BEGIN
   NOTICIAENBLANCO:= TRUE;
   ASSIGN(ARCHIVO, RUTA);
   {$I-}
   RESET(ARCHIVO);
   {$I+}
   IF IORESULT = 0 THEN
   BEGIN
      WHILE (NOT EOF(ARCHIVO)) AND (NOTICIAENBLANCO) DO
      BEGIN
         READLN(ARCHIVO, RENGLON);
         IF CADENANOVACIA(RENGLON, [CHR(33)..CHR(254)]) THEN
            NOTICIAENBLANCO:= FALSE
      END;
      CLOSE(ARCHIVO)
   END
   ELSE
      ERROR:= 2
END;
{***************************************************************************}
PROCEDURE NOTICIASDAT(VAR INFORMACION: T_INFORMACION; VAR ERROR: BYTE);
{Encuentra qu‚ n£mero de noticia le corresponde a la que se est  cargando,
y graba en el archivo NOTICIAS.DAT todos los campos de entrada obligatoria
por el usuario (junto con la referencia al archivo original).}
VAR
   ARCHIVO: FILE OF T_INFORMACION;
   ULTREG: T_INFORMACION;
BEGIN
   ASSIGN(ARCHIVO, '..\bd\noticias.dat');
   IF NOT FILEEXISTS('..\bd\noticias.dat') THEN
   BEGIN
      REWRITE(ARCHIVO);
      INFORMACION.NUMNOTICIA:= 1;
      RESET(ARCHIVO)
   END
   ELSE
   BEGIN
      {$I-}
      RESET(ARCHIVO);
      {$I+}
      IF IORESULT = 0 THEN
      BEGIN
         SEEK(ARCHIVO, FILESIZE(ARCHIVO) - 1);
         READ(ARCHIVO, ULTREG);
         INFORMACION.NUMNOTICIA:= ULTREG.NUMNOTICIA + 1
      END
      ELSE
         ERROR:= 2
   END;
   WRITE(ARCHIVO, INFORMACION);
   CLOSE(ARCHIVO)
END;
{***************************************************************************}
PROCEDURE COPIARARCHIVO(NUMNOTICIA: WORD; RUTA: STRING; VAR ERROR: BYTE);
{Copia la noticia desde su directorio original a la carpeta NOTICIAS,
cambiando su nombre por el n£mero de noticia.}
VAR
   ARCHIVO1, ARCHIVO2: TEXT;
   RENGLON: ANSISTRING;
BEGIN
   IF FILEEXISTS(RUTA) THEN
   BEGIN
      ASSIGN(ARCHIVO1, RUTA);
      {$I-}
      RESET(ARCHIVO1);
      {$I+}
      IF IORESULT = 0 THEN
      BEGIN
         ASSIGN(ARCHIVO2, '..\noticias\' + INTTOSTR(NUMNOTICIA) + '.txt');
         REWRITE(ARCHIVO2);
         APPEND(ARCHIVO2);
         WHILE NOT EOF(ARCHIVO1) DO
         BEGIN
            READLN(ARCHIVO1, RENGLON);
            WRITELN(ARCHIVO2, RENGLON)
         END;
         CLOSE(ARCHIVO1);
         CLOSE(ARCHIVO2)
      END
      ELSE
         ERROR:= 4
   END
   ELSE
      ERROR:= 5
END;
{***************************************************************************}
PROCEDURE INDEXA(PALABRA: STRING; NUMNOTICIA: WORD);
{Dada una palabra que debe ser indexada, verifica la existencia del archivo
que tiene por nombre a la palabra y la extensi¢n IDX, si no existe lo crea,
luego lo abre y agrega un registro que contiene el n£mero de noticia que
conten¡a a dicha palabra.}
VAR
   ARCHIVO: FILE OF WORD;
   AUX: WORD;
BEGIN
   {$I-}
   ASSIGN(ARCHIVO, '..\indices\' + PALABRA + '.idx');
   IF NOT FILEEXISTS('..\indices\' + PALABRA + '.idx') THEN
   BEGIN
      REWRITE(ARCHIVO);
      IF IORESULT = 0 THEN
      BEGIN
         RESET(ARCHIVO);
         SEEK(ARCHIVO, 0);
         WRITE(ARCHIVO, NUMNOTICIA);
         CLOSE(ARCHIVO)
      END
   END
   ELSE
   BEGIN
      RESET(ARCHIVO);
      IF IORESULT = 0 THEN
      BEGIN
         SEEK(ARCHIVO, FILESIZE(ARCHIVO) - 1);
         IF IORESULT = 0 THEN
         BEGIN
            READ(ARCHIVO, AUX);
            IF AUX <> NUMNOTICIA THEN
               WRITE(ARCHIVO, NUMNOTICIA)
         END;
         CLOSE(ARCHIVO)
      END
   END
   {$I+}
END;
{***************************************************************************}
PROCEDURE PORPALABRAS(RENGLON: ANSISTRING; NUMNOTICIA: WORD);
{Dada una cadena de caracteres (que puede contener muchas palabras para
indexar, pero solo tiene caracteres v lidos), la va fraccionando por palabras
(verificando que no sea nula y que tenga longitud suficiente para que sea
indexada) para que el procedimiento INDEXA, pueda indexar la palabra en
cuesti¢n.}
VAR
   X: WORD;
   PALABRA: ANSISTRING;
BEGIN
   PALABRA:= '';
   IF CADENANOVACIA(RENGLON, ['a'..'z']) THEN
      FOR X:= 1 TO LENGTH(RENGLON) DO
      BEGIN
         IF ORD(ANSIMIDSTR(RENGLON, X, 1)[1]) = 32 THEN
         BEGIN
            IF (LENGTH(PALABRA) > 2) AND (LENGTH(PALABRA) < 252) THEN
               INDEXA(PALABRA, NUMNOTICIA);
            PALABRA:= ''
         END
         ELSE
            PALABRA:= PALABRA + ANSIMIDSTR(RENGLON, X, 1)[1]
      END
END;
{***************************************************************************}
PROCEDURE INDEXAR(NUMNOTICIA: WORD; VAR ERROR: BYTE);
{Realiza una lectura secuencial del archivo que contiene a la noticia en
texto plano, y la devuelve rengl¢n por rengl¢n a los distintos procedimientos
para que puedan indexarla (con el fin de no cargar toda la memoria pasando
todo el archivo a ella).}
VAR
   ARCHIVO: TEXT;
   RENGLON: ANSISTRING;
   MAL, BIEN: STRING;
   CONJMAL: T_CONJBYTE;
   MINIMO: BYTE;
BEGIN
   ROLCHR(MAL, BIEN, CONJMAL, MINIMO, ERROR);
   IF ERROR = 0 THEN
   BEGIN
      ASSIGN(ARCHIVO, '..\noticias\' + INTTOSTR(NUMNOTICIA) + '.txt');
      RESET(ARCHIVO);
      WHILE NOT EOF(ARCHIVO) DO
      BEGIN
         READLN(ARCHIVO, RENGLON);
         FILTRO(RENGLON, MAL, BIEN, CONJMAL, MINIMO);
         PORPALABRAS(RENGLON, NUMNOTICIA)
      END;
      CLOSE(ARCHIVO)
   END
END;
{***************************************************************************}
PROCEDURE VIEWTEXTFILE(RUTA: STRING; VAR ERROR: BYTE);
{Muestra un archivo de texto en pantalla.}
VAR
   ARCHIVO: TEXT;
   RENGLON: ANSISTRING;
BEGIN
   ASSIGN(ARCHIVO, RUTA);
   {$I-}
   RESET(ARCHIVO);
   {$I+}
   IF IORESULT = 0 THEN
   BEGIN
      WHILE NOT EOF(ARCHIVO) DO
      BEGIN
         READLN(ARCHIVO, RENGLON);
         WRITELN(RENGLON)
      END;
      CLOSE(ARCHIVO)
   END
   ELSE
      ERROR:= 2
END;
{***************************************************************************}
PROCEDURE SINPARAMETROS(COMANDO: STRING; VAR ERROR: BYTE);
{En este procedimiento se especifica lo que el programa debe hacer en caso
de que ADDNEWS.EXE se corra sin par metros.}
VAR
   RUTA: STRING;
BEGIN
   RUTA:= '..\auxi\' + COMANDO + '.f1';
   IF FILEEXISTS(RUTA) THEN
      VIEWTEXTFILE(RUTA, ERROR)
   ELSE
      ERROR:= 1
END;
{***************************************************************************}
PROCEDURE PORTAL(VECTOR: T_VECTOR; MAX: BYTE; RUTA: STRING);
{Dadas las £ltimas noticias que se agregaron, este procedimiento se encarga
de mostrar su t¡tulo y su resumen ya sea por pantalla en la l¡nea de comandos,
o bien, en un archivo HTML, seg£n se indique. Genera el portal de noticias.}
VAR
   ARCHIVO: TEXT;
   X: BYTE;
BEGIN
   ASSIGN (ARCHIVO, RUTA);
   REWRITE (ARCHIVO);
   WRITELN (ARCHIVO, '<HTML>');
   WRITELN (ARCHIVO, ' <HEAD>');
   WRITELN (ARCHIVO, '  <TITLE>PORTAL DE NOTICIAS</TITLE>');
   WRITELN (ARCHIVO, ' </HEAD>');
   WRITELN (ARCHIVO, ' <BODY>');
   WRITELN (ARCHIVO, '  <P><H2><CENTER><BR>PORTAL DE NOTICIAS</BR></CENTER></H2></P>');
   IF MAX <> 0 THEN
   BEGIN
      FOR X:= 1 TO MAX DO
      BEGIN
         WRITELN (ARCHIVO, '  <H3><B>', VECTOR[X].TITULO, '</B></H3>');
         WRITELN (ARCHIVO, '  <P>', VECTOR[X].RESUMEN, '</P><BR>')
      END
   END
   ELSE
      WRITELN(ARCHIVO, '  <H3><B>NO EXISTE NINGUNA NOTICIA</B></H3>');

   WRITELN (ARCHIVO,' </BODY>');
   WRITELN (ARCHIVO,'</HTML>');
   CLOSE (ARCHIVO);
   IF RUTA <> '' THEN
      WRITELN ('GENERADO EL PORTAL EN EL ARCHIVO: ', RUTA)
END;
{****************************************************************************}
PROCEDURE ULTIMAS(VAR VECTOR: T_VECTOR; VAR MAX: BYTE);
{Se encarga de filtrar, de todo el conjunto de noticias, las £ltimas cinco,
utilizando un c¢digo similar al del m‚todo de ordenamiento de vectores de
inserci¢n.}
VAR
   REG, AUX: T_INFORMACION;
   ARCHIVO: FILE OF T_INFORMACION;
   X, Y: BYTE;
BEGIN
   IF LOWERCASE(RIGHTSTR(PARAMSTR(0), 3)) = 'exe' THEN
      CHDIR(MIDSTR(PARAMSTR(0), 1, LENGTH(PARAMSTR(0))-18))
   ELSE
      CHDIR(MIDSTR(PARAMSTR(0), 1, LENGTH(PARAMSTR(0))-14));
   ASSIGN(ARCHIVO, '..\bd\noticias.dat');
   IF FILEEXISTS('..\bd\noticias.dat') THEN
   BEGIN
      {$I-}
      RESET(ARCHIVO);
      {$I+}
      IF IORESULT = 0 THEN
      BEGIN
         IF FILESIZE(ARCHIVO) >= 5 THEN
            MAX:= 5
         ELSE
            MAX:= FILESIZE(ARCHIVO);
         FOR X:= 1 TO MAX DO
         BEGIN
            READ(ARCHIVO, REG);
            VECTOR[X]:= REG
         END;
         FOR X:= 1 TO (MAX - 1) DO
            FOR Y:= 1 TO (MAX - X) DO
               IF (COMPAREDATE(VECTOR[Y].FECHA, VECTOR[Y + 1].FECHA) <= 0) THEN
                  INTERCAMBIO(VECTOR[Y], VECTOR[Y + 1]);
         IF FILESIZE(ARCHIVO) <> MAX THEN
            WHILE NOT EOF(ARCHIVO) DO
            BEGIN
               READ(ARCHIVO, REG);
               X:= 5;
               WHILE (COMPAREDATE(REG.FECHA, VECTOR[X].FECHA) >= 0) AND (X > 0) DO
               BEGIN
                  IF X = 5 THEN
                     VECTOR[X]:= REG
                  ELSE
                     INTERCAMBIO(VECTOR[X], VECTOR[X + 1]);
                  DEC(X)
               END
            END;
         CLOSE(ARCHIVO)
      END
   END
END;
{****************************************************************************}
Function archivo_existe(var ar:file):boolean;

Var
	existe:byte;
	ar_ex:boolean;

Begin
	{$I-}
	reset(ar);
	existe := IOResult;
	{$I+}

	if (existe=0) then
		ar_ex := true
	else
		ar_ex := false;
	archivo_existe := ar_ex
End;

{******************************************************************}
{******************************************************************}

Function texto_existe(var ar:text):boolean;

Var
	existe:byte;
	ar_ex:boolean;

Begin

	{$I-}
	reset(ar);
	existe := IOResult;
	{$I+}

	if (existe=0) then
		ar_ex := true
	else
		ar_ex := false;
	texto_existe := ar_ex
End;

{******************************************************************}
{******************************************************************}

Procedure htmlnews(reg:T_INFORMACION);

Var
	textfile:text;
	cordel:string;

Begin
writeln ('<html>');
writeln (' <head>');
writeln ('  <title>',reg.titulo,'</title>');
writeln (' </head>');
writeln (' <body>');
writeln ('  <h3>',reg.titulo,'</h3>');
writeln ('  <p>',reg.resumen,'</p>');
if (reg.autor<>'') then
writeln ('  <p>Por ',reg.autor,'</p>');

assign(textfile,'..\noticias\' + inttostr(reg.numnoticia) + '.txt');

if (texto_existe(textfile)) and (inttostr(reg.numnoticia)<>'') then
	begin
	reset(textfile);
   writeln('  <p>');
	while (not(eof(textfile))) do
	begin
	readln(textfile,cordel);
	writeln (cordel, '<br>');
	end;
   writeln('</p>');
	close (textfile);
	end;

writeln (' </body>');
writeln ('</html>')

End;

{******************************************************************}
{******************************************************************}

Procedure htmlnews2file(reg:T_INFORMACION; out:string);

Var
	textfile,htmlfile:text;
	cordel:ansistring;

Begin
out := '..\out\' + out;
assign(htmlfile,out);
rewrite(htmlfile);

writeln (htmlfile,'<html>');
writeln (htmlfile,' <head>');
writeln (htmlfile,'  <title>',reg.titulo,'</title>');
writeln (htmlfile,' </head>');
writeln (htmlfile,' <body>');
writeln (htmlfile,'  <h3>',reg.titulo,'</h3>');
writeln (htmlfile,'  <p>',reg.resumen,'</p>');
if (reg.autor<>'') then
writeln (htmlfile,'  <p>Por ',reg.autor,'</p>');

assign(textfile,'..\noticias\' + inttostr(reg.NUMNOTICIA) + '.txt');

if (texto_existe(textfile)) and (reg.REFERENCIA<>'') then
	begin
	reset(textfile);
   writeln(htmlfile, '  <p>');
	while (not(eof(textfile))) do
	Begin
	 readln(textfile,cordel);
	 writeln (htmlfile,cordel, '<br>')
	End;
   writeln(htmlfile, '</p>');
	close (textfile);
	end;

writeln (htmlfile,' </body>');
writeln (htmlfile,'</html>');
close (htmlfile);

writeln ('Generado el archivo ',out,' con la noticia en la carpeta \out\.')

End;
{******************************************************************}
Procedure Crear_salida (D, E, F, G, H: byte; reg: T_INFORMACION);
{D crea\abre, E no hay noticia, F encabezado, G cuerpo, H cierra}
Var
   I: longint;
   C, J: string;
   ars: text;
Begin
   If D = 0 then  {crea\abre el archivo de salida}
   Begin
      For I:= 1 to paramcount do
      Begin
         C:= extractfileext(paramstr(I));
         C:= lowercase(C);
         If C = '.html' Then
         Begin
            J:= '..\out\'+ lowercase(paramstr(I));
            Assign (ars, J)
         End
      End;
      If FileExists(J) Then
         Append (ars)
      Else
         Rewrite (ars)
   End;
   If E = 0 Then {escribe no hay noticia}
   Begin
      writeln;
      writeln (ars, '<html>');
      writeln (ars, ' <head>');
      writeln (ars, '  <title>','Resultado busqueda','</title>');
      writeln (ars, ' </head>');
      writeln (ars, ' <body>');
      Writeln (ars, '  <p> <b>El resultado de la busqueda es...</b></p>');
      writeln (ars, '  <p>No se encontraron noticias que cumplan ese criterio</p>');
      writeln (ars, ' </body>');
      writeln (ars, '</html>')
   End;
   If F = 0 Then   {escribe el encabezado de la busqueda}
   Begin
      writeln;
      writeln (ars, '<html>');
      writeln (ars, ' <head>');
      writeln (ars, '  <title>','El Resultado de la busqueda es','</title>');
      writeln (ars, ' </head>');
      writeln (ars, ' <body>');
      writeln (ars, '  <p><b>','El Resultado de la busqueda es...','</b></p>')
   End;
   If G = 0 Then
      WRITELN(ars, '  <p><A HREF=/shownews.exe?-noticia?', inttostr(reg.NUMNOTICIA), '>', reg.titulo, '</A></p>'); {Escribe el resultado de la busqueda}
   If H = 0 Then  {cierra la busqueda}
   Begin
      writeln (ars, ' </body>');
      writeln (ars, '</html>')
   End;
   Close(ars)
End;
{****************************************************************************}
Function Existe (var c: file of Word): boolean;
Var
   d: boolean;
Begin
   {$I-}
   Reset(c);
   {$I+}
   If (IOResult <> 0) Then
      d:= False
   else
   Begin
      d:= True;
      Close(c)
   End;
   Existe:= d
End;
{****************************************************************************}
Procedure una_palabra (Out: boolean);
Var
   C: string;
   D, n_noticia: Word;
   palabra: file of Word;
   archivo_dat: file of T_INFORMACION;
   noticia: T_INFORMACION;
   sw: boolean;
Begin
   sw:= false;
   C:= lowercase(A + paramstr(1) + B);
   Assign (palabra, C);
   If not Existe (palabra) then
      If Out Then
      Begin
         Crear_Salida (0, 0, 1, 1, 1 ,noticia);
         Write ('Archivo Generado')
      End
      Else
         no_hay_noticia
   Else
   Begin
      Reset (palabra);
      Assign (archivo_dat, '..\bd\noticias.dat');
      Reset (archivo_dat);
      For D:= 0 to (filesize (palabra) - 1) Do
      Begin
         seek (palabra, D);
         read (palabra, n_noticia);
         seek (archivo_dat, (n_noticia - 1));
         read (archivo_dat, noticia);
         if Out then
         Begin
            if sw = false then
            begin
               Crear_Salida (0, 1, 0, 0, 1, noticia);
               Write ('Archivo Generado');
               sw:= true
            end
            else
               crear_salida (0, 1, 1, 0, 1, noticia)
         End
         else
         Begin
            if sw = false then
            begin
               Encabezado_html;
               Cuerpo_html (noticia);
               sw:= true
            end
            else
               Cuerpo_html (noticia)
         End
      End;
      if (out) and (sw) then
         crear_salida (0, 1, 1, 1, 0, noticia);
      if (not out) and (sw) then
         Cerrar_html;
      Close (palabra);
      Close (archivo_dat)
   End
End;
{****************************************************************************}
Procedure dos_palabras(out:boolean);
Var
   D, G: string;
   archivo: File of T_INFORMACION;
   noticia: T_INFORMACION;
   J, E, H: Word;
   n_noticia1, n_noticia2: word;
   palabra, palabra2: file of Word;
   Busqueda, Cond1, Cond2, Cond3, Cond4, Cond5: boolean;
Begin
   Busqueda:= True;
   For J:= 1 to 2 Do
   Begin
      D:= lowercase(A + paramstr(J) + B);
      Assign (palabra, D);
      If Not Existe(palabra) Then
      Begin
         if busqueda = true then
            If out Then
            Begin
               Crear_Salida(0, 0, 1, 1, 1, noticia);
               Write('Archivo Generado')
            End
            Else
               No_Hay_Noticia;
         Busqueda:= False
      End
   End;
   If Busqueda Then
   Begin
      D:= lowercase (A + paramstr(1) + B);
      G:= lowercase (A + paramstr(2) + B);
      Assign (Palabra, D);
      Assign (palabra2, G);
      Reset (Palabra);
      Reset (Palabra2);
      Cond2:= false;
      Cond5:= true;
      Cond4:= true;
      For E:= 0 To (FileSize(Palabra)-1) Do
      Begin
         Seek (palabra, E);
         Read (palabra, n_noticia1);
         Cond1:= false;
         Cond3:= False;
         While (Not(Cond1)) and (Not(Cond3)) Do
            For H:= 0 To (FileSize(Palabra2) - 1) Do
            Begin
               Seek(Palabra2, H);
               Read(Palabra2, n_noticia2);
               If (n_noticia1 = n_noticia2) Then
                  Cond1:= True;
               If (FileSize(Palabra2) - 1)= H Then
                  Cond3:= True
            End;
         If Cond1 Then
         Begin
            Assign (archivo, Q + 'noticias.dat');
            Reset (archivo);
            seek (archivo, (n_noticia1 - 1));
            read (archivo, noticia);
            Close (archivo);
            if Cond5 then
            Begin
               If Out Then
                  Crear_Salida(0, 1, 0, 1, 1, noticia)
               Else
                  encabezado_html;
               Cond5:= false
            end;
            If Out Then
               Crear_Salida(0, 1, 1, 0, 1 , noticia)
            Else
               cuerpo_html(noticia);
            Cond2:= true
         End
      End;
      Close (Palabra);
      Close (Palabra2);
      If (Not Cond2) and (Cond4) Then
      Begin
         If Out Then
         Begin
            Crear_Salida (0, 0, 1, 1, 1, noticia);
            Write ('Archivo Generado')
         End
         Else
            no_hay_noticia;
         Cond4:= False;
      End
      Else
         If out Then
         Begin
            Crear_Salida (0, 1, 1, 1, 0, noticia);
            Write('Archivo Generado')
         End
         Else
            cerrar_html
   End
END;
{****************************************************************************}
Procedure Verificar;
Var
   Arg1, Arg2, Arg3: boolean;
   {Arg1 dos palabras con salida a archivo}
   {Arg2 dos palabras sin salida a archivo}
   {Arg3 una palabra con salida a archivo}
   C: string;
Begin
   Arg1:= False;
   Arg2:= False;
   Arg3:= False;
   If not(extractfileext(paramstr(1))='') then
      Error
   Else
      If(extractfileext(paramstr(2))='') Then
         If not(extractfileext(paramstr(3))='') then
            Arg1:=True
         Else
            If paramstr(3) = '' then
               Arg2:=True
            Else
               Error
         Else
            if paramstr(3) = '' then
               Arg3:= True
            Else
               Error;
   If Arg1 Then
   Begin
      C:= extractfileext(paramstr(3));
      C:= Lowercase(C);
      If not (C = '.html') then
         Error
      Else
         dos_palabras (Arg1)
   End;
   If Arg3 Then
   Begin
      C:= extractfileext(paramstr(2));
      C:= Lowercase(C);
      If not (C = '.html') then
         Error
      Else
         una_palabra (Arg3)
   End;
   If Arg2 Then
      dos_palabras (Arg1)
End;
{****************************************************************************}
Procedure Hay_parametros;
Var
   C: string;
   D: boolean;
Begin
   D:=false;
   If(paramcount = 1) Then
   Begin
      C:= ExtractFileExt(paramstr(1));
      If (C = '') Then
         Una_palabra(D)
      Else
         Error
   End;
   If (paramcount > 1) and (paramcount < 4) Then
      Verificar
   Else
      if (paramcount>3) then
         error
End;
{****************************************************************************}
end.


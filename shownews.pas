Program Shownews;

Uses
	dos, crt, sysutils, strutils, archivos, resto;
{******************************************************************}
{PROGRAMA PRINCIPAL}
Var
	num_noticia, outfile: string;
	reg: t_INFORMACION;
	bd: file of t_INFORMACION;
	num: integer;
   X: BYTE;
Begin

   IF LOWERCASE(RIGHTSTR(PARAMSTR(0), 3)) = 'exe' THEN
      CHDIR(MIDSTR(PARAMSTR(0), 1, LENGTH(PARAMSTR(0))-12))
   ELSE
      CHDIR(MIDSTR(PARAMSTR(0), 1, LENGTH(PARAMSTR(0))-8));

   CLRSCR;
   FOR X:= 0 TO PARAMCOUNT DO
      WRITE(PARAMSTR(X), CHR(32));
   WRITELN;
   WRITELN;

outfile := '';
num_noticia := '';

	 reg.titulo := 'No existe la noticia';
	 reg.resumen := '';
	 reg.autor := '';
    reg.REFERENCIA := '';

num_noticia := getcmdlinearg('noticia',StdSwitchChars);
outfile := LOWERCASE(getcmdlinearg('html',StdSwitchChars));

if (CADENANOVALIDA(num_noticia, ['0'..'9'])) or (num_noticia='') or ((outfile <> '') and (lowercase(rightstr(outfile,4)) <> 'html')) then
	writeln ('Modo de uso:' + #10#13 + 'shownews -noticia [numero de noticia]' + #10#13 +  '(opcional: -html [nombre de archivo de salida con extension html])')
else
	Begin
	assign (bd,'..\bd\noticias.dat');
	if (archivo_existe(bd)) then
	 Begin
	 num := strtoint(num_noticia);
	 reset(bd);
	 if (filesize(bd) >= num) and (num > 0) then
		Begin
		seek(bd,num-1);
		read(bd,reg)
		End;
	 close(bd)
	 End;
	if (outfile='') then
	  htmlnews(reg)
	 else
	  htmlnews2file(reg,outfile)
	End

End.

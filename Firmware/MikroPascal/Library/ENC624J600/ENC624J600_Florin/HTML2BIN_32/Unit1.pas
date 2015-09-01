unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Memo1: TMemo;
    Edit2: TEdit;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    procedure SearchFiles(St: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  my_names_cnt : integer;
  my_names : array[0..31] of string;
  my_new_names : array[0..31] of string;
  my_names_len : array[0..31] of integer;
  my_names_file_size : array[0..31] of cardinal;
  f : file of byte;
  file_j : TextFile;
  my_data : array[0..1999999] of byte;
  my_data_cnt : cardinal;
  my_boot : array[0..2047] of byte;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  St: string;
begin
  Memo1.Clear;
  St:='d:\';
  if SelectDirectory(St, [], 0) then
  begin
    Edit1.Text:=St;
    SearchFiles(St);
    Edit2.Text := IntToStr(my_names_cnt);
  end;
end;

procedure TForm1.SearchFiles(St: string);
var
  MySearch: TSearchRec;
  FindResult,i,k,t: Integer;
  txt,extension,gz_,html_,plain_,svg_,jpg_,gif_,png_,text3,css_,js_,ico_ : string;
  crlf_ : string;
  j,data,gzip_detected : byte;
  start,stop,buf,len : cardinal;
begin
  js_ := 'HTTP/1.1 200 OK'+chr(13)+chr(10)+'Content-type: text/javascript'+chr(13)+chr(10);
  css_ := 'HTTP/1.1 200 OK'+chr(13)+chr(10)+'Content-type: text/css'+chr(13)+chr(10);
  html_ := 'HTTP/1.1 200 OK'+chr(13)+chr(10)+'Content-type: text/html'+chr(13)+chr(10);
  svg_  := 'HTTP/1.1 200 OK'+chr(13)+chr(10)+'Content-type: image/svg+xml'+chr(13)+chr(10);
  jpg_  := 'HTTP/1.1 200 OK'+chr(10)+'Content-type: image/jpg'+chr(13)+chr(10);
  ico_  := 'HTTP/1.1 200 OK'+chr(13)+chr(10)+'Content-type: image/ico'+chr(13)+chr(10);
  gif_  := 'HTTP/1.1 200 OK'+chr(13)+chr(10)+'Content-type: image/gif'+chr(13)+chr(10);
  png_  := 'HTTP/1.1 200 OK'+chr(13)+chr(10)+'Content-type: image/png'+chr(13)+chr(10);
  plain_ := 'HTTP/1.1 200 OK'+chr(13)+chr(10)+'Content-type: text/plain'+chr(13)+chr(10);
  gz_ := 'Content-Encoding: gzip'+chr(13)+chr(10);
  crlf_ := ''+chr(13)+chr(10);
  
  my_names_cnt := 0;

  FindResult:=FindFirst(St+'\*.*', faAnyFile, MySearch);
  if (MySearch.Name<>'.')and(MySearch.Name<>'..') then
    begin
        my_names[my_names_cnt] := MySearch.Name;
        i := Length(my_names[my_names_cnt]);
        if i >= 3 then inc(my_names_cnt);
    end;
  while FindNext(MySearch)=0 do
  begin
    if (MySearch.Attr<>faDirectory)and(MySearch.Name<>'.')and(MySearch.Name<>'..') then
      begin
        my_names[my_names_cnt] := MySearch.Name;
        i := Length(my_names[my_names_cnt]);
        if i >= 3 then inc(my_names_cnt);
        if my_names_cnt > 31 then break;
      end;
  end;

  my_data_cnt := 0;

  i := 0;
  while i < my_names_cnt do
    begin

      gzip_detected := 0;

      my_new_names[i] := '';

      extension := '';

      k := 1;
      while k <= Length(my_names[i]) do
        begin
          j := Ord(my_names[i][k]);
          my_new_names[i] := my_new_names[i] + Chr(j);
          if my_names[i][k] = '.' then break;
          inc(k);
        end;
      inc(k);
      if k < Length(my_names[i]) then
        begin
          while k <= Length(my_names[i]) do
            begin
              if my_names[i][k] = '.' then break;
              j := Ord(my_names[i][k]);
              my_new_names[i] := my_new_names[i] + Chr(j);
              if (j >= 65) and (j <= 90) then j := j + 32;
              extension := extension + Chr(j);
              inc(k);
            end;
        end;
      inc(k);
      if k < Length(my_names[i]) then
        begin
          if (my_names[i][k] = 'g') or (my_names[i][k] = 'G') then
          if (my_names[i][k+1] = 'z') or (my_names[i][k+1] = 'Z') then gzip_detected := 1;
        end;

      if (extension = 'html') or (extension = 'htm') then txt := html_
      else if (extension = 'css') then txt := css_
      else if (extension = 'js') or (extension = 'json') then txt := js_
      else if (extension = 'svg') then txt := svg_
      else if (extension = 'jpg') then txt := jpg_
      else if (extension = 'gif') then txt := gif_
      else if (extension = 'png') then txt := png_
      else if (extension = 'ico') then txt := ico_
      else txt := plain_;

      if CheckBox1.Checked = false then
        begin
          txt := ''; // NO header
          crlf_ := ''; // NO crlf
          gz_ := ''; // NO gzip
        end;

      AssignFile(f, my_names[i]);
      my_names_file_size[i] := 0;
      Reset(f);

      k := 1;
      while k <= Length(txt) do
        begin
          my_data[my_data_cnt] := Ord(txt[k]);
          inc(my_data_cnt);
          inc(my_names_file_size[i]);
          inc(k);
        end;

      if gzip_detected <> 0 then txt := gz_
      else txt := '';

      k := 1;
      while k <= Length(txt) do
        begin
          my_data[my_data_cnt] := Ord(txt[k]);
          inc(my_data_cnt);
          inc(my_names_file_size[i]);
          inc(k);
        end;

      txt := crlf_;

      k := 1;
      while k <= Length(txt) do
        begin
          my_data[my_data_cnt] := Ord(txt[k]);
          inc(my_data_cnt);
          inc(my_names_file_size[i]);
          inc(k);
        end;

      while not EOF(f) do
        begin
          Read(f,j);
          my_data[my_data_cnt] := j;
          inc(my_data_cnt);
          inc(my_names_file_size[i]);
        end;
      CloseFile(f);
      txt := my_new_names[i];
      k := Length(txt);
      while k < 55 do
        begin
          txt := txt + ' ';
          inc(k);
        end;
      txt := txt + IntToStr(my_names_file_size[i]);
      k := Length(txt);
      while k < 70 do
        begin
          txt := txt + ' ';
          inc(k);
        end;
      if gzip_detected <> 0 then txt := txt + 'gzip';
      Memo1.Lines.Add(txt);
      inc(i);
    end;

  txt := ' ';
  Memo1.Lines.Add(txt);
  txt := 'Total Bytes';
  k := Length(txt);
  while k < 55 do
    begin
      txt := txt + ' ';
      inc(k);
    end;
  txt := txt + IntToStr(my_data_cnt);
  Memo1.Lines.Add(txt);
  txt := ' ';
  Memo1.Lines.Add(txt);
  txt := 'webpage.bin size';
  k := Length(txt);
  while k < 55 do
    begin
      txt := txt + ' ';
      inc(k);
    end;
  txt := txt + IntToStr(my_data_cnt+2048);
  Memo1.Lines.Add(txt);

  i := 0;
  while i < 2048 do
    begin
      my_boot[i] := 0;
      inc(i);
    end;

  stop := 2048;
  i := 0;
  while i < my_names_cnt do
    begin
      k := Length(my_new_names[i]);
      if k > 55 then k := 55; // max 55 char in name
      t := 0;
      while t < k do
        begin
          my_boot[(i*64)+t] := Ord(my_new_names[i][t+1]);
          inc(t);
        end;
      start := stop;
      stop := start + my_names_file_size[i];

      buf := start div 65536;
      my_boot[(i*64)+56] := Lo(start);
      my_boot[(i*64)+57] := Hi(start);
      my_boot[(i*64)+58] := Lo(buf);
      my_boot[(i*64)+59] := Hi(buf);
      buf := stop div 65536;
      my_boot[(i*64)+60] := Lo(stop);
      my_boot[(i*64)+61] := Hi(stop);
      my_boot[(i*64)+62] := Lo(buf);
      my_boot[(i*64)+63] := Hi(buf);

      inc(i);
    end;

  AssignFile(f, 'webpage.bin');
  Rewrite(f);
  i := 0;
  while i < 2048 do
    begin
      Write(f,my_boot[i]);
      inc(i);
    end;
  buf := 0;
  while buf < my_data_cnt do
    begin
      Write(f,my_data[buf]);
      inc(buf);
    end;
   CloseFile(f);
{
  assignfile(file_j,'webpage.mpas');
  rewrite(file_j);

  text3 := 'unit webpage; ';
  writeln(file_j,text3);

  text3 := '  ';
  writeln(file_j,text3);


      len := my_data_cnt + 2048;

      text3 := 'const webpage_len = '+ IntTostr(len) +'; ';
      writeln(file_j,text3);

      text3 := '  ';
      writeln(file_j,text3);

      text3 := 'const webpage : array['+ IntTostr(len) +'] of byte = (';
      writeln(file_j,text3);

  AssignFile(f, 'webpage.bin');
  Reset(f);

      text3 := '      ';  // put data
      t := 0;
      while len > 0 do
        begin
          Read(f,data);
          if len = 1 then text3 := text3 + '$'+ IntToHex(data,2)+' );'
          else text3 := text3 + '$'+ IntToHex(data,2)+', ';
          inc(t);
          if t = 16 then
            begin
              writeln(file_j,text3);
              t := 0;
              text3 := '      ';
            end;
          dec(len);
        end;
      if t <> 0 then
        begin
          writeln(file_j,text3);
        end;


  text3 := '  ';
  writeln(file_j,text3);
  text3 := 'implementation';
  writeln(file_j,text3);
  text3 := '  ';
  writeln(file_j,text3);
  text3 := 'end.';
  writeln(file_j,text3);
  closefile(file_j);
  
  closefile(f);   }
end;

end.

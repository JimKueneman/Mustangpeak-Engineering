(*        GREATIS DELPHI PAGES         *)
(* Copyright (C) 2001 Greatis Software *)
(*  web: http://www.greatisdelphi.com  *)
(*     e-mail: delphi@greatis.com      *)

program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'HTML2BIN';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

unit form_notes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TFormNotes }

  TFormNotes = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Panel1: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormNotes: TFormNotes;

implementation

{$R *.lfm}

end.


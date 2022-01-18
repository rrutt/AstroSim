unit AstroSimMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TAstroSimMainForm }

  TAstroSimMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  end;

  var
    MainForm: TAstroSimMainForm;

implementation

{$R *.lfm}

  //var
    //ResourceDirectory: UTF8String {$IFNDEF MACOSX} = '../res/' {$ENDIF};

  procedure TAstroSimMainForm.FormCreate(Sender: TObject);
  begin
  end;
end.


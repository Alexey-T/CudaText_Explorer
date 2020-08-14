unit formmenubox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls;

type

  { TfmMenuBox }

  TfmMenuBox = class(TForm)
    ButtonPanel1: TButtonPanel;
    ListBox: TListBox;
  private

  public

  end;

var
  fmMenuBox: TfmMenuBox;

implementation

{$R *.lfm}

end.


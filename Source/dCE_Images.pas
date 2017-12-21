//******************************************************************************
//  CubicExplorer                                                                             
//  Version: 0.90                                                                             
//                                                                                            
//  The Original Code is dCE_Images.pas.
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit dCE_Images;

interface

uses
  // PNG Controls
  PngImageList,
  // System Units
  SysUtils, Classes, ImgList, Controls, System.ImageList;

type
  TCE_Images = class(TDataModule)
    MediumIcons: TPngImageList;
    SmallIcons: TPngImageList;
    BookmarkImages: TPngImageList;
    MiscImages: TPngImageList;
    QuickViewImages: TPngImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CE_Images: TCE_Images;

implementation

{$R *.dfm}

end.

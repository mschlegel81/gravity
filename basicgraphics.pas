UNIT basicGraphics;

{$mode objfpc}{$H+}

INTERFACE
USES ExtCtrls;
TYPE
  T_rgbColor=array[0..2] of byte;
  P_rgbPicture=^T_rgbPicture;

  { T_rgbPicture }

  T_rgbPicture=object
    private
      followedBy:P_rgbPicture;
      width,height:longint;
      Pixels:^T_rgbColor;
      PROCEDURE setPixel(CONST x,y:longint; CONST value:T_rgbColor);
      FUNCTION getPixel(CONST x,y:longint):T_rgbColor;
    public
      CONSTRUCTOR create(CONST width_,height_:longint);
      DESTRUCTOR destroy;

      PROCEDURE copyToImage(VAR destImage:TImage);
      PROPERTY pixel[x,y:longint]:T_rgbColor read getPixel write setPixel;
      FUNCTION toString:string;
  end;

  P_animation=^T_animation;
  T_animation=object
    private
      section:TRTLCriticalSection;
      firstFrame,lastFrame:P_rgbPicture;
      frameCount:longint;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION render(VAR destImage:TImage):boolean;
      FUNCTION dropFrame:longint;
      FUNCTION addFrame(CONST frame:P_rgbPicture):longint;
      PROPERTY getFrameCount:longint read frameCount;
  end;
IMPLEMENTATION
USES sysutils,Classes,  Graphics, IntfGraphics, GraphType;
{ T_animation }

CONSTRUCTOR T_animation.create;
  begin
    initCriticalSection(section);
    frameCount:=0;
    firstFrame:=nil;
    lastFrame:=nil;
  end;

DESTRUCTOR T_animation.destroy;
  begin
    enterCriticalSection(section);
    while frameCount>0 do dropFrame;
    leaveCriticalSection(section);
    doneCriticalSection(section);
  end;

FUNCTION T_animation.render(VAR destImage: TImage): boolean;
  begin
    if (frameCount>0) and (firstFrame<>nil) then begin
      firstFrame^.copyToImage(destImage);
      result:=true;
    end else result:=false;
  end;

FUNCTION T_animation.dropFrame: longint;
  VAR dropped:P_rgbPicture=nil;
  begin
    enterCriticalSection(section);
    if (frameCount>0) and (firstFrame<>nil) then begin
      dropped:=firstFrame;
      firstFrame:=firstFrame^.followedBy;
      dec(frameCount);
    end;
    result:=frameCount;
    leaveCriticalSection(section);
    if dropped<>nil then dispose(dropped,destroy);
  end;

FUNCTION T_animation.addFrame(CONST frame: P_rgbPicture): longint;
  begin
    enterCriticalSection(section);
    if firstFrame=nil then begin
      firstFrame:=frame;
      frameCount:=0;
    end else
      lastFrame^.followedBy:=frame;
    lastFrame:=frame;
    inc(frameCount);
    result:=frameCount;
    leaveCriticalSection(section);
  end;

{ T_rgbPicture }

PROCEDURE T_rgbPicture.setPixel(CONST x, y: longint; CONST value: T_rgbColor);
  begin
    Pixels[x+y*width]:=value;
  end;

FUNCTION T_rgbPicture.getPixel(CONST x, y: longint): T_rgbColor;
  begin
    result:=Pixels[x+y*width];
  end;

CONSTRUCTOR T_rgbPicture.create(CONST width_, height_: longint);
  begin
    followedBy:=nil;
    width:=width_; height:=height_;
    getMem(Pixels,sizeOf(T_rgbColor)*width*height);
  end;

DESTRUCTOR T_rgbPicture.destroy;
  begin
    freeMem(Pixels,sizeOf(T_rgbColor)*width*height);
  end;

PROCEDURE T_rgbPicture.copyToImage(VAR destImage: TImage);
  VAR ScanLineImage,                 //image with representation as in T_24BitImage
      tempIntfImage: TLazIntfImage;  //image with representation as in TBitmap
      ImgFormatDescription: TRawImageDescription;
      y:longint;
      pix:PByte;
      src:^T_rgbColor;
  begin
    ScanLineImage:=TLazIntfImage.create(width,height);
    try
      ImgFormatDescription.Init_BPP24_B8G8R8_BIO_TTB(width,height);
      ImgFormatDescription.ByteOrder:=riboMSBFirst;
      ScanLineImage.DataDescription:=ImgFormatDescription;
      for y:=0 to height-1 do begin
        pix:=ScanLineImage.GetDataLineStart(y);
        src:=Pixels+y*width;
        move(src^,pix^,sizeOf(T_rgbColor)*width);
      end;
      destImage.picture.Bitmap.setSize(width,height);
      tempIntfImage:=destImage.picture.Bitmap.CreateIntfImage;
      tempIntfImage.CopyPixels(ScanLineImage);
      destImage.picture.Bitmap.LoadFromIntfImage(tempIntfImage);
      tempIntfImage.free;
    finally
      ScanLineImage.free;
    end;

  end;

FUNCTION T_rgbPicture.toString: string;
  VAR i:longint;
    c: T_rgbColor;
  begin
    result:='['+intToStr(width)+',[';
    for i:=0 to width*height-1 do begin
      c:=Pixels[i];
      if i>0 then result+=',';
      result+='['+intToStr(c[0])+','+intToStr(c[1])+','+intToStr(c[2])+']';
    end;
    result+=']]';
  end;

end.


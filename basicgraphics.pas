UNIT basicGraphics;

{$mode objfpc}{$H+}

INTERFACE
USES ExtCtrls,Classes,myGenerics;
CONST UPPER_BLACK_LEVEL=0.0000029263403116397158;
      UPPER_C1_LEVEL   =0.00002633706280475744;
      UPPER_C2_LEVEL   =0.00007315850778885903;
      UPPER_C3_LEVEL   =0.0001433906752697249 ;
      UPPER_C4_LEVEL   =0.00023703356523894044;
      UPPER_C5_LEVEL   =0.00035408717770519913;
      UPPER_C6_LEVEL   =0.0004945515126605124 ;
      UPPER_C7_LEVEL   =0.0006584265701174937 ;
      UPPER_C8_LEVEL   =0.00084571235005176826;
      UPPER_C9_LEVEL   =0.0010564088525123138 ;
      UPPER_C10_LEVEL  =0.0012905160774322384 ;
      UPPER_C11_LEVEL  =0.0015480340248639987 ;
      UPPER_C12_LEVEL  =0.0018289626947692759 ;
      UPPER_C13_LEVEL  =0.0021333020871741586 ;
TYPE
  T_rgbColor=array[0..2] of byte;
  P_rgbPicture=^T_rgbPicture;

  T_rgbPicture=object
    private
      followedBy:P_rgbPicture;
      Pixels:PSmallInt;
    public
      mass:double;
      PROCEDURE setPixel(CONST x,y:longint; CONST m:double);

      CONSTRUCTOR create;
      DESTRUCTOR destroy;

      PROCEDURE copyToImage(VAR destImage:TImage; CONST extension,aspectRatio:double);

      FUNCTION load(fileStream:TFileStream; CONST previous:P_rgbPicture):boolean;
      PROCEDURE write(fileStream:TFileStream; CONST previous:P_rgbPicture);
      FUNCTION loadCompressed(fileStream:TFileStream; CONST previous:P_rgbPicture):boolean;
      PROCEDURE writeCompressed(fileStream:TFileStream; CONST previous:P_rgbPicture);
      FUNCTION equals(CONST other:P_rgbPicture):boolean;
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
      FUNCTION render(VAR destImage:TImage; CONST extension,aspectRatio:double):boolean;
      FUNCTION dropFrame:longint;
      FUNCTION addFrame(CONST frame:P_rgbPicture):longint;
      PROPERTY getFrameCount:longint read frameCount;
  end;
IMPLEMENTATION
USES sysutils,Graphics, IntfGraphics, GraphType, math;

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

FUNCTION T_animation.render(VAR destImage: TImage; CONST extension,aspectRatio:double): boolean;
  begin
    if (frameCount>0) and (firstFrame<>nil) then begin
      firstFrame^.copyToImage(destImage,extension,aspectRatio);
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

PROCEDURE T_rgbPicture.setPixel(CONST x,y:longint; CONST m:double);
  begin
    if not(isNan(m)) then mass+=m;
    if isNan(m) or (m>12.25)
    then             Pixels[x+y*SYS_SIZE]:=1023
    else if m<0 then Pixels[x+y*SYS_SIZE]:=   0
    else begin
      Pixels[x+y*SYS_SIZE]:=round(sqrt(m)*292.28571428571428);
    end;
  end;

CONSTRUCTOR T_rgbPicture.create;
  begin
    followedBy:=nil;
    mass:=0;
    getMem(Pixels,sizeOf(SmallInt)*SYS_SIZE*SYS_SIZE);
  end;

DESTRUCTOR T_rgbPicture.destroy;
  begin
    freeMem(Pixels,sizeOf(SmallInt)*SYS_SIZE*SYS_SIZE);
  end;

PROCEDURE T_rgbPicture.copyToImage(VAR destImage: TImage; CONST extension,aspectRatio:double);
  CONST COLOR_TABLE: array[0..1023] of T_rgbColor=(
  (  0,  0,  0),(  0,  0,  0),(  0,  0,  1),(  0,  0,  2),(  0,  0,  3),(  0,  0,  4),(  0,  0,  5),(  0,  0,  6),(  0,  0,  6),(  0,  0,  7),(  0,  0,  8),(  0,  0,  9),(  0,  0, 10),(  0,  0, 11),(  0,  0, 12),(  0,  0, 13),
  (  0,  0, 13),(  0,  0, 14),(  0,  0, 15),(  0,  0, 16),(  0,  0, 17),(  0,  0, 18),(  0,  0, 19),(  0,  0, 20),(  0,  0, 20),(  0,  0, 21),(  0,  0, 22),(  0,  0, 23),(  0,  0, 24),(  0,  0, 25),(  0,  0, 26),(  0,  0, 27),
  (  0,  0, 27),(  0,  0, 28),(  0,  0, 29),(  0,  0, 30),(  0,  0, 31),(  0,  0, 32),(  0,  0, 33),(  0,  0, 34),(  0,  0, 34),(  0,  0, 35),(  0,  0, 36),(  0,  0, 37),(  0,  0, 38),(  0,  0, 39),(  0,  0, 40),(  0,  0, 41),
  (  0,  0, 41),(  0,  0, 42),(  0,  0, 43),(  0,  0, 44),(  0,  0, 45),(  0,  0, 46),(  0,  0, 47),(  0,  0, 47),(  0,  0, 48),(  0,  0, 49),(  0,  0, 50),(  0,  0, 51),(  0,  0, 52),(  0,  0, 53),(  0,  0, 54),(  0,  0, 54),
  (  0,  0, 55),(  0,  0, 56),(  0,  0, 57),(  0,  0, 58),(  0,  0, 59),(  0,  0, 60),(  0,  0, 61),(  0,  0, 61),(  0,  0, 62),(  0,  0, 63),(  0,  0, 64),(  0,  0, 65),(  0,  0, 66),(  0,  0, 67),(  0,  0, 68),(  0,  0, 68),
  (  0,  0, 69),(  0,  0, 70),(  0,  0, 71),(  0,  0, 72),(  0,  0, 73),(  0,  0, 74),(  0,  0, 75),(  0,  0, 75),(  0,  0, 76),(  0,  0, 77),(  0,  0, 78),(  0,  0, 79),(  0,  0, 80),(  0,  0, 81),(  0,  0, 82),(  0,  0, 82),
  (  0,  0, 83),(  0,  0, 84),(  0,  0, 85),(  0,  0, 86),(  0,  0, 87),(  0,  0, 88),(  0,  0, 88),(  0,  0, 89),(  0,  0, 90),(  0,  0, 91),(  0,  0, 92),(  0,  0, 93),(  0,  0, 94),(  0,  0, 95),(  0,  0, 95),(  0,  0, 96),
  (  0,  0, 97),(  0,  0, 98),(  0,  0, 99),(  0,  0,100),(  0,  0,101),(  0,  0,102),(  0,  0,102),(  0,  0,103),(  0,  0,104),(  0,  0,105),(  0,  0,106),(  0,  0,107),(  0,  0,108),(  0,  0,109),(  0,  0,109),(  0,  0,110),
  (  0,  0,111),(  0,  0,112),(  0,  0,113),(  0,  0,114),(  0,  0,115),(  0,  0,116),(  0,  0,116),(  0,  0,117),(  0,  0,118),(  0,  0,119),(  0,  0,120),(  0,  0,121),(  0,  0,122),(  0,  0,123),(  0,  0,123),(  0,  0,124),
  (  0,  0,125),(  0,  0,126),(  0,  0,127),(  0,  0,126),(  1,  0,125),(  2,  0,125),(  3,  0,124),(  4,  0,123),(  5,  0,122),(  5,  0,121),(  6,  0,120),(  7,  0,119),(  8,  0,118),(  9,  0,118),( 10,  0,117),( 11,  0,116),
  ( 12,  0,115),( 12,  0,114),( 13,  0,113),( 14,  0,112),( 15,  0,111),( 16,  0,111),( 17,  0,110),( 18,  0,109),( 19,  0,108),( 19,  0,107),( 20,  0,106),( 21,  0,105),( 22,  0,104),( 23,  0,104),( 24,  0,103),( 25,  0,102),
  ( 26,  0,101),( 26,  0,100),( 27,  0, 99),( 28,  0, 98),( 29,  0, 97),( 30,  0, 97),( 31,  0, 96),( 32,  0, 95),( 33,  0, 94),( 33,  0, 93),( 34,  0, 92),( 35,  0, 91),( 36,  0, 90),( 37,  0, 90),( 38,  0, 89),( 39,  0, 88),
  ( 40,  0, 87),( 40,  0, 86),( 41,  0, 85),( 42,  0, 84),( 43,  0, 84),( 44,  0, 83),( 45,  0, 82),( 46,  0, 81),( 46,  0, 80),( 47,  0, 79),( 48,  0, 78),( 49,  0, 77),( 50,  0, 77),( 51,  0, 76),( 52,  0, 75),( 53,  0, 74),
  ( 53,  0, 73),( 54,  0, 72),( 55,  0, 71),( 56,  0, 70),( 57,  0, 70),( 58,  0, 69),( 59,  0, 68),( 60,  0, 67),( 60,  0, 66),( 61,  0, 65),( 62,  0, 64),( 63,  0, 63),( 64,  0, 63),( 65,  0, 62),( 66,  0, 61),( 67,  0, 60),
  ( 67,  0, 59),( 68,  0, 58),( 69,  0, 57),( 70,  0, 56),( 71,  0, 56),( 72,  0, 55),( 73,  0, 54),( 74,  0, 53),( 74,  0, 52),( 75,  0, 51),( 76,  0, 50),( 77,  0, 49),( 78,  0, 49),( 79,  0, 48),( 80,  0, 47),( 81,  0, 46),
  ( 81,  0, 45),( 82,  0, 44),( 83,  0, 43),( 84,  0, 42),( 85,  0, 42),( 86,  0, 41),( 87,  0, 40),( 87,  0, 39),( 88,  0, 38),( 89,  0, 37),( 90,  0, 36),( 91,  0, 36),( 92,  0, 35),( 93,  0, 34),( 94,  0, 33),( 94,  0, 32),
  ( 95,  0, 31),( 96,  0, 30),( 97,  0, 29),( 98,  0, 29),( 99,  0, 28),(100,  0, 27),(101,  0, 26),(101,  0, 25),(102,  0, 24),(103,  0, 23),(104,  0, 22),(105,  0, 22),(106,  0, 21),(107,  0, 20),(108,  0, 19),(108,  0, 18),
  (109,  0, 17),(110,  0, 16),(111,  0, 15),(112,  0, 15),(113,  0, 14),(114,  0, 13),(115,  0, 12),(115,  0, 11),(116,  0, 10),(117,  0,  9),(118,  0,  8),(119,  0,  8),(120,  0,  7),(121,  0,  6),(122,  0,  5),(122,  0,  4),
  (123,  0,  3),(124,  0,  2),(125,  0,  1),(126,  0,  1),(127,  0,  0),(128,  0,  0),(128,  0,  0),(129,  0,  0),(130,  0,  0),(131,  0,  0),(132,  0,  0),(133,  0,  0),(134,  0,  0),(135,  0,  0),(135,  0,  0),(136,  0,  0),
  (137,  0,  0),(138,  0,  0),(139,  0,  0),(140,  0,  0),(141,  0,  0),(142,  0,  0),(142,  0,  0),(143,  0,  0),(144,  0,  0),(145,  0,  0),(146,  0,  0),(147,  0,  0),(148,  0,  0),(149,  0,  0),(149,  0,  0),(150,  0,  0),
  (151,  0,  0),(152,  0,  0),(153,  0,  0),(154,  0,  0),(155,  0,  0),(156,  0,  0),(156,  0,  0),(157,  0,  0),(158,  0,  0),(159,  0,  0),(160,  0,  0),(161,  0,  0),(162,  0,  0),(163,  0,  0),(163,  0,  0),(164,  0,  0),
  (165,  0,  0),(166,  0,  0),(167,  0,  0),(168,  0,  0),(169,  0,  0),(169,  0,  0),(170,  0,  0),(171,  0,  0),(172,  0,  0),(173,  0,  0),(174,  0,  0),(175,  0,  0),(176,  0,  0),(176,  0,  0),(177,  0,  0),(178,  0,  0),
  (179,  0,  0),(180,  0,  0),(181,  0,  0),(182,  0,  0),(183,  0,  0),(183,  0,  0),(184,  0,  0),(185,  0,  0),(186,  0,  0),(187,  0,  0),(188,  0,  0),(189,  0,  0),(190,  0,  0),(190,  0,  0),(191,  0,  0),(192,  0,  0),
  (193,  0,  0),(194,  0,  0),(195,  0,  0),(196,  0,  0),(197,  0,  0),(197,  0,  0),(198,  0,  0),(199,  0,  0),(200,  0,  0),(201,  0,  0),(202,  0,  0),(203,  0,  0),(204,  0,  0),(204,  0,  0),(205,  0,  0),(206,  0,  0),
  (207,  0,  0),(208,  0,  0),(209,  0,  0),(210,  0,  0),(211,  0,  0),(211,  0,  0),(212,  0,  0),(213,  0,  0),(214,  0,  0),(215,  0,  0),(216,  0,  0),(217,  0,  0),(217,  0,  0),(218,  0,  0),(219,  0,  0),(220,  0,  0),
  (221,  0,  0),(222,  0,  0),(223,  0,  0),(224,  0,  0),(224,  0,  0),(225,  0,  0),(226,  0,  0),(227,  0,  0),(228,  0,  0),(229,  0,  0),(230,  0,  0),(231,  0,  0),(231,  0,  0),(232,  0,  0),(233,  0,  0),(234,  0,  0),
  (235,  0,  0),(236,  0,  0),(237,  0,  0),(238,  0,  0),(238,  0,  0),(239,  0,  0),(240,  0,  0),(241,  0,  0),(242,  0,  0),(243,  0,  0),(244,  0,  0),(245,  0,  0),(245,  0,  0),(246,  0,  0),(247,  0,  0),(248,  0,  0),
  (249,  0,  0),(250,  0,  0),(251,  0,  0),(252,  0,  0),(252,  0,  0),(253,  0,  0),(254,  0,  0),(255,  0,  0),(255,  1,  0),(255,  2,  0),(255,  3,  0),(255,  3,  0),(255,  4,  0),(255,  5,  0),(255,  6,  0),(255,  7,  0),
  (255,  8,  0),(255,  9,  0),(255, 10,  0),(255, 10,  0),(255, 11,  0),(255, 12,  0),(255, 13,  0),(255, 14,  0),(255, 15,  0),(255, 16,  0),(255, 17,  0),(255, 17,  0),(255, 18,  0),(255, 19,  0),(255, 20,  0),(255, 21,  0),
  (255, 22,  0),(255, 23,  0),(255, 24,  0),(255, 24,  0),(255, 25,  0),(255, 26,  0),(255, 27,  0),(255, 28,  0),(255, 29,  0),(255, 30,  0),(255, 31,  0),(255, 31,  0),(255, 32,  0),(255, 33,  0),(255, 34,  0),(255, 35,  0),
  (255, 36,  0),(255, 37,  0),(255, 38,  0),(255, 38,  0),(255, 39,  0),(255, 40,  0),(255, 41,  0),(255, 42,  0),(255, 43,  0),(255, 44,  0),(255, 44,  0),(255, 45,  0),(255, 46,  0),(255, 47,  0),(255, 48,  0),(255, 49,  0),
  (255, 50,  0),(255, 51,  0),(255, 51,  0),(255, 52,  0),(255, 53,  0),(255, 54,  0),(255, 55,  0),(255, 56,  0),(255, 57,  0),(255, 58,  0),(255, 58,  0),(255, 59,  0),(255, 60,  0),(255, 61,  0),(255, 62,  0),(255, 63,  0),
  (255, 64,  0),(255, 65,  0),(255, 65,  0),(255, 66,  0),(255, 67,  0),(255, 68,  0),(255, 69,  0),(255, 70,  0),(255, 71,  0),(255, 72,  0),(255, 72,  0),(255, 73,  0),(255, 74,  0),(255, 75,  0),(255, 76,  0),(255, 77,  0),
  (255, 78,  0),(255, 79,  0),(255, 79,  0),(255, 80,  0),(255, 81,  0),(255, 82,  0),(255, 83,  0),(255, 84,  0),(255, 85,  0),(255, 85,  0),(255, 86,  0),(255, 87,  0),(255, 88,  0),(255, 89,  0),(255, 90,  0),(255, 91,  0),
  (255, 92,  0),(255, 92,  0),(255, 93,  0),(255, 94,  0),(255, 95,  0),(255, 96,  0),(255, 97,  0),(255, 98,  0),(255, 99,  0),(255, 99,  0),(255,100,  0),(255,101,  0),(255,102,  0),(255,103,  0),(255,104,  0),(255,105,  0),
  (255,106,  0),(255,106,  0),(255,107,  0),(255,108,  0),(255,109,  0),(255,110,  0),(255,111,  0),(255,112,  0),(255,113,  0),(255,113,  0),(255,114,  0),(255,115,  0),(255,116,  0),(255,117,  0),(255,118,  0),(255,119,  0),
  (255,120,  0),(255,120,  0),(255,121,  0),(255,122,  0),(255,123,  0),(255,124,  0),(255,125,  0),(255,126,  0),(255,127,  0),(255,127,  0),(255,128,  0),(255,129,  0),(255,130,  0),(255,131,  0),(255,132,  0),(255,133,  0),
  (255,133,  0),(255,134,  0),(255,135,  0),(255,136,  0),(255,137,  0),(255,138,  0),(255,139,  0),(255,140,  0),(255,140,  0),(255,141,  0),(255,142,  0),(255,143,  0),(255,144,  0),(255,145,  0),(255,146,  0),(255,147,  0),
  (255,147,  0),(255,148,  0),(255,149,  0),(255,150,  0),(255,151,  0),(255,152,  0),(255,153,  0),(255,154,  0),(255,154,  0),(255,155,  0),(255,156,  0),(255,157,  0),(255,158,  0),(255,159,  0),(255,160,  0),(255,161,  0),
  (255,161,  0),(255,162,  0),(255,163,  0),(255,164,  0),(255,165,  0),(255,166,  0),(255,167,  0),(255,168,  0),(255,168,  0),(255,169,  0),(255,170,  0),(255,171,  0),(255,172,  0),(255,173,  0),(255,174,  0),(255,174,  0),
  (255,175,  0),(255,176,  0),(255,177,  0),(255,178,  0),(255,179,  0),(255,180,  0),(255,181,  0),(255,181,  0),(255,182,  0),(255,183,  0),(255,184,  0),(255,185,  0),(255,186,  0),(255,187,  0),(255,188,  0),(255,188,  0),
  (255,189,  0),(255,190,  0),(255,191,  0),(255,192,  0),(255,193,  0),(255,194,  0),(255,195,  0),(255,195,  0),(255,196,  0),(255,197,  0),(255,198,  0),(255,199,  0),(255,200,  0),(255,201,  0),(255,202,  0),(255,202,  0),
  (255,203,  0),(255,204,  0),(255,205,  0),(255,206,  0),(255,207,  0),(255,208,  0),(255,209,  0),(255,209,  0),(255,210,  0),(255,211,  0),(255,212,  0),(255,213,  0),(255,214,  0),(255,215,  0),(255,215,  0),(255,216,  0),
  (255,217,  0),(255,218,  0),(255,219,  0),(255,220,  0),(255,221,  0),(255,222,  0),(255,222,  0),(255,223,  0),(255,224,  0),(255,225,  0),(255,226,  0),(255,227,  0),(255,228,  0),(255,229,  0),(255,229,  0),(255,230,  0),
  (255,231,  0),(255,232,  0),(255,233,  0),(255,234,  0),(255,235,  0),(255,236,  0),(255,236,  0),(255,237,  0),(255,238,  0),(255,239,  0),(255,240,  0),(255,241,  0),(255,242,  0),(255,243,  0),(255,243,  0),(255,244,  0),
  (255,245,  0),(255,246,  0),(255,247,  0),(255,248,  0),(255,249,  0),(255,250,  0),(255,250,  0),(255,251,  0),(255,252,  0),(255,253,  0),(255,254,  0),(255,255,  0),(255,255,  1),(255,255,  1),(255,255,  2),(255,255,  3),
  (255,255,  4),(255,255,  5),(255,255,  6),(255,255,  7),(255,255,  8),(255,255,  8),(255,255,  9),(255,255, 10),(255,255, 11),(255,255, 12),(255,255, 13),(255,255, 14),(255,255, 15),(255,255, 15),(255,255, 16),(255,255, 17),
  (255,255, 18),(255,255, 19),(255,255, 20),(255,255, 21),(255,255, 22),(255,255, 22),(255,255, 23),(255,255, 24),(255,255, 25),(255,255, 26),(255,255, 27),(255,255, 28),(255,255, 29),(255,255, 29),(255,255, 30),(255,255, 31),
  (255,255, 32),(255,255, 33),(255,255, 34),(255,255, 35),(255,255, 36),(255,255, 36),(255,255, 37),(255,255, 38),(255,255, 39),(255,255, 40),(255,255, 41),(255,255, 42),(255,255, 42),(255,255, 43),(255,255, 44),(255,255, 45),
  (255,255, 46),(255,255, 47),(255,255, 48),(255,255, 49),(255,255, 49),(255,255, 50),(255,255, 51),(255,255, 52),(255,255, 53),(255,255, 54),(255,255, 55),(255,255, 56),(255,255, 56),(255,255, 57),(255,255, 58),(255,255, 59),
  (255,255, 60),(255,255, 61),(255,255, 62),(255,255, 63),(255,255, 63),(255,255, 64),(255,255, 65),(255,255, 66),(255,255, 67),(255,255, 68),(255,255, 69),(255,255, 70),(255,255, 70),(255,255, 71),(255,255, 72),(255,255, 73),
  (255,255, 74),(255,255, 75),(255,255, 76),(255,255, 77),(255,255, 77),(255,255, 78),(255,255, 79),(255,255, 80),(255,255, 81),(255,255, 82),(255,255, 83),(255,255, 84),(255,255, 84),(255,255, 85),(255,255, 86),(255,255, 87),
  (255,255, 88),(255,255, 89),(255,255, 90),(255,255, 90),(255,255, 91),(255,255, 92),(255,255, 93),(255,255, 94),(255,255, 95),(255,255, 96),(255,255, 97),(255,255, 97),(255,255, 98),(255,255, 99),(255,255,100),(255,255,101),
  (255,255,102),(255,255,103),(255,255,104),(255,255,104),(255,255,105),(255,255,106),(255,255,107),(255,255,108),(255,255,109),(255,255,110),(255,255,111),(255,255,111),(255,255,112),(255,255,113),(255,255,114),(255,255,115),
  (255,255,116),(255,255,117),(255,255,118),(255,255,118),(255,255,119),(255,255,120),(255,255,121),(255,255,122),(255,255,123),(255,255,124),(255,255,125),(255,255,125),(255,255,126),(255,255,127),(255,255,128),(255,255,129),
  (255,255,130),(255,255,131),(255,255,131),(255,255,132),(255,255,133),(255,255,134),(255,255,135),(255,255,136),(255,255,137),(255,255,138),(255,255,138),(255,255,139),(255,255,140),(255,255,141),(255,255,142),(255,255,143),
  (255,255,144),(255,255,145),(255,255,145),(255,255,146),(255,255,147),(255,255,148),(255,255,149),(255,255,150),(255,255,151),(255,255,152),(255,255,152),(255,255,153),(255,255,154),(255,255,155),(255,255,156),(255,255,157),
  (255,255,158),(255,255,159),(255,255,159),(255,255,160),(255,255,161),(255,255,162),(255,255,163),(255,255,164),(255,255,165),(255,255,166),(255,255,166),(255,255,167),(255,255,168),(255,255,169),(255,255,170),(255,255,171),
  (255,255,172),(255,255,172),(255,255,173),(255,255,174),(255,255,175),(255,255,176),(255,255,177),(255,255,178),(255,255,179),(255,255,179),(255,255,180),(255,255,181),(255,255,182),(255,255,183),(255,255,184),(255,255,185),
  (255,255,186),(255,255,186),(255,255,187),(255,255,188),(255,255,189),(255,255,190),(255,255,191),(255,255,192),(255,255,193),(255,255,193),(255,255,194),(255,255,195),(255,255,196),(255,255,197),(255,255,198),(255,255,199),
  (255,255,200),(255,255,200),(255,255,201),(255,255,202),(255,255,203),(255,255,204),(255,255,205),(255,255,206),(255,255,207),(255,255,207),(255,255,208),(255,255,209),(255,255,210),(255,255,211),(255,255,212),(255,255,213),
  (255,255,213),(255,255,214),(255,255,215),(255,255,216),(255,255,217),(255,255,218),(255,255,219),(255,255,220),(255,255,220),(255,255,221),(255,255,222),(255,255,223),(255,255,224),(255,255,225),(255,255,226),(255,255,227),
  (255,255,227),(255,255,228),(255,255,229),(255,255,230),(255,255,231),(255,255,232),(255,255,233),(255,255,234),(255,255,234),(255,255,235),(255,255,236),(255,255,237),(255,255,238),(255,255,239),(255,255,240),(255,255,241),
  (255,255,241),(255,255,242),(255,255,243),(255,255,244),(255,255,245),(255,255,246),(255,255,247),(255,255,248),(255,255,248),(255,255,249),(255,255,250),(255,255,251),(255,255,252),(255,255,253),(255,255,254),(255,255,255));

  CONST ssm1=SYS_SIZE-1;
  VAR ScanLineImage,                 //image with representation as in T_24BitImage
      tempIntfImage: TLazIntfImage;  //image with representation as in TBitmap
      ImgFormatDescription: TRawImageDescription;
      y,x:longint;
      pix:PByte;
      src:PSmallInt;

      colLine:^T_rgbColor;
      extendedWidth,extendedHeight:longint;
  begin
    if aspectRatio>1 then begin
      extendedWidth:=round(SYS_SIZE*extension);
      extendedHeight:=round(extendedWidth*aspectRatio);
    end else begin
      extendedHeight:=round(SYS_SIZE*extension);
      extendedWidth:=round(extendedHeight/aspectRatio);
    end;
    getMem(colLine,sizeOf(T_rgbColor)*extendedWidth);

    ScanLineImage:=TLazIntfImage.create(extendedWidth,extendedHeight);
    try
      ImgFormatDescription.Init_BPP24_B8G8R8_BIO_TTB(extendedWidth,extendedHeight);
      ImgFormatDescription.ByteOrder:=riboMSBFirst;
      ScanLineImage.DataDescription:=ImgFormatDescription;
      for y:=0 to extendedHeight-1 do begin
        src:=Pixels+(y and ssm1)*SYS_SIZE;
        for x:=0 to extendedWidth-1 do colLine[x]:=COLOR_TABLE[src[x and ssm1]];
        pix:=ScanLineImage.GetDataLineStart(y);
        move(colLine^,pix^,sizeOf(T_rgbColor)*extendedWidth);
      end;
      destImage.picture.Bitmap.setSize(extendedWidth,extendedHeight);
      tempIntfImage:=destImage.picture.Bitmap.CreateIntfImage;
      tempIntfImage.CopyPixels(ScanLineImage);
      destImage.picture.Bitmap.LoadFromIntfImage(tempIntfImage);
      tempIntfImage.free;
    finally
      ScanLineImage.free;
      freeMem(colLine,sizeOf(T_rgbColor)*extendedWidth);
    end;
  end;

FUNCTION T_rgbPicture.load(fileStream: TFileStream; CONST previous:P_rgbPicture): boolean;
  VAR expected,read,p:longint;
      i:longint;
  begin
    p:=fileStream.position;
    expected:=sizeOf(SmallInt)*SYS_SIZE*SYS_SIZE;
    read:=fileStream.read(Pixels^,expected);
    if previous<>nil then for i:=0 to SYS_SIZE*SYS_SIZE-1 do Pixels[i]+=previous^.Pixels[i];
    if expected=read then begin p+=read; fileStream.Seek(p,soBeginning); end else exit(false);

    expected:=sizeOf(mass);
    read:=fileStream.read(mass,sizeOf(mass));
    if expected=read then begin p+=read; fileStream.Seek(p,soBeginning); result:=true; end else exit(false);
  end;

PROCEDURE T_rgbPicture.write(fileStream: TFileStream; CONST previous:P_rgbPicture);
  var i: longint;
      dataToWrite:array[0..SYS_SIZE*SYS_SIZE-1] of SmallInt;
  begin
    if previous=nil then fileStream.write(Pixels^,sizeOf(SmallInt)*SYS_SIZE*SYS_SIZE)
    else begin
      for i:=0 to SYS_SIZE*SYS_SIZE-1 do dataToWrite[i]:=pixels[i]-previous^.Pixels[i];
      fileStream.write(dataToWrite,sizeOf(SmallInt)*SYS_SIZE*SYS_SIZE)
    end;
    fileStream.write(mass,sizeOf(mass));
  end;

TYPE T_nodeKind=(intermediary,leaf);
     P_node=^T_node;
     T_node=record
       kind  : T_nodeKind;
       symbol: smallint;
       child : array[false..true] of P_node;
     end;
     P_builderNode=^T_builderNode;
     T_builderNode=record
       kind  : T_nodeKind;
       symbol: smallint;
       child : array[false..true] of P_node;
       count : int64;
     end;
     T_arrayOfBoolean=array of boolean;
     T_bitArray=object
       private
         datFill:longint;
         data:array of byte;
         trashBits:byte;
         cursorIndex:longint;
         FUNCTION getBit(CONST index:longint):boolean;
       public
         CONSTRUCTOR create;
         DESTRUCTOR destroy;
         PROCEDURE append(CONST nextBit:boolean); inline;
         PROCEDURE append(CONST arr:T_bitArray; CONST finalAppend:boolean=false);
         PROCEDURE parseString(CONST stringOfZeroesAndOnes:ansistring);
         FUNCTION size:longint;
         PROPERTY bit[CONST index:longint]:boolean read getBit; default;
         FUNCTION getRawDataAsString:ansistring;
         FUNCTION getBitString:ansistring;
         FUNCTION bits:T_arrayOfBoolean;
     end;
     T_encodeTable=array[0..1023] of T_bitArray;

 FUNCTION T_bitArray.getBit(CONST index: longint): boolean;
    VAR byteIndex:longint;
        bitMask:byte;
    begin
      if index<size then begin
        byteIndex:=index shr 3;
        bitMask:=1 shl (7-(index and 7));
        result:=data[byteIndex] and bitMask=bitMask;
      end else result:=false;
    end;

CONSTRUCTOR T_bitArray.create;
  begin
    setLength(data,0);
    datFill:=0;
    trashBits:=0;
    cursorIndex:=0;
  end;

DESTRUCTOR T_bitArray.destroy;
  begin
    setLength(data,0);
    trashBits:=0;
  end;

PROCEDURE T_bitArray.append(CONST nextBit: boolean);
  begin
    if trashBits=0 then begin
      if datFill>=length(data) then setLength(data,1+round(1.1*datFill));
      inc(datFill);
      data[datFill-1]:=0;
      trashBits:=7;
      if nextBit then data[datFill-1]:=data[datFill-1] or (1 shl trashBits);
    end else begin
      dec(trashBits);
      if nextBit then data[datFill-1]:=data[datFill-1] or (1 shl trashBits);
    end;
  end;

PROCEDURE T_bitArray.append(CONST arr: T_bitArray; CONST finalAppend: boolean);
  VAR i:longint;
      b:boolean;
  begin
    if finalAppend then begin
      i:=0;
      while (trashBits<>0) and (i<arr.size) do begin
        append(arr[i]);
        inc(i);
      end;
    end else for b in arr.bits do append(b);
  end;

PROCEDURE T_bitArray.parseString(CONST stringOfZeroesAndOnes: ansistring);
  VAR i:longint;
  begin
    setLength(data,0);
    trashBits:=0;
    for i:=1 to length(stringOfZeroesAndOnes) do append(stringOfZeroesAndOnes[i]='1');
  end;

FUNCTION T_bitArray.size: longint;
  begin
    result:=datFill shl 3-trashBits;
  end;

FUNCTION T_bitArray.getRawDataAsString: ansistring;
  VAR i:longint;
  begin
    result:='';
    for i:=0 to datFill-1 do result:=result+chr(data[i]);
  end;

FUNCTION T_bitArray.getBitString: ansistring;
  VAR i:longint;
  begin
    result:='';
    for i:=0 to size-1 do if bit[i]
    then result:=result+'1'
    else result:=result+'0';
  end;

FUNCTION T_bitArray.bits:T_arrayOfBoolean;
  CONST M:array[0..7] of byte=(1, 2, 4,  8,
                              16,32,64,128);
  VAR i:longint;
      k:longint=0;
  begin
    initialize(result);
    setLength(result,datFill shl 3);
    for i:=0 to datFill-1 do begin
      result[k]:=(data[i] and M[7])>0; inc(k);
      result[k]:=(data[i] and M[6])>0; inc(k);
      result[k]:=(data[i] and M[5])>0; inc(k);
      result[k]:=(data[i] and M[4])>0; inc(k);
      result[k]:=(data[i] and M[3])>0; inc(k);
      result[k]:=(data[i] and M[2])>0; inc(k);
      result[k]:=(data[i] and M[1])>0; inc(k);
      result[k]:=(data[i] and M[0])>0; inc(k);
    end;
    setLength(result,size);
  end;

PROCEDURE buildCode(OUT decodeRoot:P_node; OUT encodeTable:T_encodeTable);
  VAR table:array [0..1023] of P_builderNode;

  FUNCTION initNode(CONST count:int64; CONST symbol:smallint; CONST kind:T_nodeKind):P_builderNode;
    begin
      getMem(result,sizeOf(T_builderNode));
      result^.count:=count;
      result^.kind:=kind;
      result^.symbol:=symbol;
      result^.child[false]:=nil;
      result^.child[true ]:=nil;
    end;

  PROCEDURE mergeTrees(i,j:longint; CONST toDrop:longint);
    VAR tmp:longint;
        newRoot:P_builderNode;

        child:P_node;
    begin
      if i>j then begin tmp:=i; i:=j; j:=tmp; end;
      getMem(newRoot,sizeOf(T_builderNode));

      newRoot^.count :=table[i]^.count+table[j]^.count;
      newRoot^.kind  :=intermediary;
      newRoot^.symbol:=0;

      getMem(child,sizeOf(T_node));
      child^.symbol:=table[i]^.symbol;
      child^.child :=table[i]^.child;
      child^.kind  :=table[i]^.kind;
      newRoot^.child[false]:=child;

      getMem(child,sizeOf(T_node));
      child^.symbol:=table[j]^.symbol;
      child^.child :=table[j]^.child;
      child^.kind  :=table[j]^.kind;
      newRoot^.child[true]:=child;

      freeMem(table[i],sizeOf(T_builderNode));
      freeMem(table[j],sizeOf(T_builderNode));
      table[i]:=newRoot;
      table[j]:=nil;
      table[j]:=table[toDrop];
    end;

  FUNCTION makeRoot:P_node;
    begin
      getMem(result,sizeOf(T_node));
      result^.symbol:=table[0]^.symbol;
      result^.child :=table[0]^.child;
      result^.kind  :=table[0]^.kind;
      freeMem(table[0],sizeOf(T_builderNode));
    end;

  VAR smallestIdx,
      secondSmallestIdx:longint;
  PROCEDURE swap;
    VAR tmp:longint;
    begin
      if table[smallestIdx]^.count>table[secondSmallestIdx]^.count then begin
        tmp:=smallestIdx;
        smallestIdx:=secondSmallestIdx;
        secondSmallestIdx:=tmp;
      end;
    end;

  PROCEDURE traverseTree(CONST prefix:ansistring; VAR root:P_node);
    begin
      case root^.kind of
        intermediary: begin
          traverseTree(prefix+'0',root^.child[false]);
          traverseTree(prefix+'1',root^.child[true]);
        end;
        leaf: begin
          encodeTable[root^.symbol].create;
          encodeTable[root^.symbol].parseString(prefix);
        end;
      end;
    end;

  VAR tabFill:longint;
      k: longint;
  begin
    table[   0]:=initNode(1197542013 ,   0 ,leaf);
    table[   1]:=initNode( 153734976 ,   1 ,leaf);
    table[   2]:=initNode(  54583037 ,   2 ,leaf);
    table[   3]:=initNode(  31678545 ,   3 ,leaf);
    table[   4]:=initNode(  22303018 ,   4 ,leaf);
    table[   5]:=initNode(  16855606 ,   5 ,leaf);
    table[   6]:=initNode(  13130946 ,   6 ,leaf);
    table[   7]:=initNode(  10559292 ,   7 ,leaf);
    table[   8]:=initNode(   8727466 ,   8 ,leaf);
    table[   9]:=initNode(   7287137 ,   9 ,leaf);
    table[  10]:=initNode(   6217675 ,  10 ,leaf);
    table[  11]:=initNode(   5399273 ,  11 ,leaf);
    table[  12]:=initNode(   4706959 ,  12 ,leaf);
    table[  13]:=initNode(   4181119 ,  13 ,leaf);
    table[  14]:=initNode(   3744976 ,  14 ,leaf);
    table[  15]:=initNode(   3380148 ,  15 ,leaf);
    table[  16]:=initNode(   3022119 ,  16 ,leaf);
    table[  17]:=initNode(   2762935 ,  17 ,leaf);
    table[  18]:=initNode(   2560716 ,  18 ,leaf);
    table[  19]:=initNode(   2319667 ,  19 ,leaf);
    table[  20]:=initNode(   2172896 ,  20 ,leaf);
    table[  21]:=initNode(   1998502 ,  21 ,leaf);
    table[  22]:=initNode(   1841037 ,  22 ,leaf);
    table[  23]:=initNode(   1713260 ,  23 ,leaf);
    table[  24]:=initNode(   1595738 ,  24 ,leaf);
    table[  25]:=initNode(   1491319 ,  25 ,leaf);
    table[  26]:=initNode(   1395407 ,  26 ,leaf);
    table[  27]:=initNode(   1309701 ,  27 ,leaf);
    table[  28]:=initNode(   1233043 ,  28 ,leaf);
    table[  29]:=initNode(   1158023 ,  29 ,leaf);
    table[  30]:=initNode(   1096018 ,  30 ,leaf);
    table[  31]:=initNode(   1037489 ,  31 ,leaf);
    table[  32]:=initNode(    979985 ,  32 ,leaf);
    table[  33]:=initNode(    931516 ,  33 ,leaf);
    table[  34]:=initNode(    882897 ,  34 ,leaf);
    table[  35]:=initNode(    842427 ,  35 ,leaf);
    table[  36]:=initNode(    799564 ,  36 ,leaf);
    table[  37]:=initNode(    761228 ,  37 ,leaf);
    table[  38]:=initNode(    724435 ,  38 ,leaf);
    table[  39]:=initNode(    692506 ,  39 ,leaf);
    table[  40]:=initNode(    658722 ,  40 ,leaf);
    table[  41]:=initNode(    632271 ,  41 ,leaf);
    table[  42]:=initNode(    604191 ,  42 ,leaf);
    table[  43]:=initNode(    578811 ,  43 ,leaf);
    table[  44]:=initNode(    556206 ,  44 ,leaf);
    table[  45]:=initNode(    532915 ,  45 ,leaf);
    table[  46]:=initNode(    513106 ,  46 ,leaf);
    table[  47]:=initNode(    493537 ,  47 ,leaf);
    table[  48]:=initNode(    476070 ,  48 ,leaf);
    table[  49]:=initNode(    456665 ,  49 ,leaf);
    table[  50]:=initNode(    441112 ,  50 ,leaf);
    table[  51]:=initNode(    426143 ,  51 ,leaf);
    table[  52]:=initNode(    409446 ,  52 ,leaf);
    table[  53]:=initNode(    396887 ,  53 ,leaf);
    table[  54]:=initNode(    383142 ,  54 ,leaf);
    table[  55]:=initNode(    369374 ,  55 ,leaf);
    table[  56]:=initNode(    358019 ,  56 ,leaf);
    table[  57]:=initNode(    344307 ,  57 ,leaf);
    table[  58]:=initNode(    333542 ,  58 ,leaf);
    table[  59]:=initNode(    322274 ,  59 ,leaf);
    table[  60]:=initNode(    311588 ,  60 ,leaf);
    table[  61]:=initNode(    302251 ,  61 ,leaf);
    table[  62]:=initNode(    292452 ,  62 ,leaf);
    table[  63]:=initNode(    282864 ,  63 ,leaf);
    table[  64]:=initNode(    274390 ,  64 ,leaf);
    table[  65]:=initNode(    266107 ,  65 ,leaf);
    table[  66]:=initNode(    259063 ,  66 ,leaf);
    table[  67]:=initNode(    250501 ,  67 ,leaf);
    table[  68]:=initNode(    243736 ,  68 ,leaf);
    table[  69]:=initNode(    236728 ,  69 ,leaf);
    table[  70]:=initNode(    228057 ,  70 ,leaf);
    table[  71]:=initNode(    222406 ,  71 ,leaf);
    table[  72]:=initNode(    216466 ,  72 ,leaf);
    table[  73]:=initNode(    210240 ,  73 ,leaf);
    table[  74]:=initNode(    205082 ,  74 ,leaf);
    table[  75]:=initNode(    199221 ,  75 ,leaf);
    table[  76]:=initNode(    194467 ,  76 ,leaf);
    table[  77]:=initNode(    189008 ,  77 ,leaf);
    table[  78]:=initNode(    183417 ,  78 ,leaf);
    table[  79]:=initNode(    178973 ,  79 ,leaf);
    table[  80]:=initNode(    174344 ,  80 ,leaf);
    table[  81]:=initNode(    169234 ,  81 ,leaf);
    table[  82]:=initNode(    165437 ,  82 ,leaf);
    table[  83]:=initNode(    160901 ,  83 ,leaf);
    table[  84]:=initNode(    157016 ,  84 ,leaf);
    table[  85]:=initNode(    152580 ,  85 ,leaf);
    table[  86]:=initNode(    148616 ,  86 ,leaf);
    table[  87]:=initNode(    146070 ,  87 ,leaf);
    table[  88]:=initNode(    141202 ,  88 ,leaf);
    table[  89]:=initNode(    138399 ,  89 ,leaf);
    table[  90]:=initNode(    135099 ,  90 ,leaf);
    table[  91]:=initNode(    131997 ,  91 ,leaf);
    table[  92]:=initNode(    155207 ,  92 ,leaf);
    table[  93]:=initNode(    269812 ,  93 ,leaf);
    table[  94]:=initNode(    122694 ,  94 ,leaf);
    table[  95]:=initNode(    119460 ,  95 ,leaf);
    table[  96]:=initNode(    117309 ,  96 ,leaf);
    table[  97]:=initNode(    114622 ,  97 ,leaf);
    table[  98]:=initNode(    111508 ,  98 ,leaf);
    table[  99]:=initNode(    108972 ,  99 ,leaf);
    table[ 100]:=initNode(    106726 , 100 ,leaf);
    table[ 101]:=initNode(    105009 , 101 ,leaf);
    table[ 102]:=initNode(    102131 , 102 ,leaf);
    table[ 103]:=initNode(     99671 , 103 ,leaf);
    table[ 104]:=initNode(     98130 , 104 ,leaf);
    table[ 105]:=initNode(     95551 , 105 ,leaf);
    table[ 106]:=initNode(     93636 , 106 ,leaf);
    table[ 107]:=initNode(     92181 , 107 ,leaf);
    table[ 108]:=initNode(     89466 , 108 ,leaf);
    table[ 109]:=initNode(     88260 , 109 ,leaf);
    table[ 110]:=initNode(     86776 , 110 ,leaf);
    table[ 111]:=initNode(     84888 , 111 ,leaf);
    table[ 112]:=initNode(     82890 , 112 ,leaf);
    table[ 113]:=initNode(     81270 , 113 ,leaf);
    table[ 114]:=initNode(     79691 , 114 ,leaf);
    table[ 115]:=initNode(     78376 , 115 ,leaf);
    table[ 116]:=initNode(     76258 , 116 ,leaf);
    table[ 117]:=initNode(     75138 , 117 ,leaf);
    table[ 118]:=initNode(     73744 , 118 ,leaf);
    table[ 119]:=initNode(     73048 , 119 ,leaf);
    table[ 120]:=initNode(     70419 , 120 ,leaf);
    table[ 121]:=initNode(     69271 , 121 ,leaf);
    table[ 122]:=initNode(     68136 , 122 ,leaf);
    table[ 123]:=initNode(     68173 , 123 ,leaf);
    table[ 124]:=initNode(     66416 , 124 ,leaf);
    table[ 125]:=initNode(     64775 , 125 ,leaf);
    table[ 126]:=initNode(     63738 , 126 ,leaf);
    table[ 127]:=initNode(     62484 , 127 ,leaf);
    table[ 128]:=initNode(     61460 , 128 ,leaf);
    table[ 129]:=initNode(     60485 , 129 ,leaf);
    table[ 130]:=initNode(     59128 , 130 ,leaf);
    table[ 131]:=initNode(     58207 , 131 ,leaf);
    table[ 132]:=initNode(     57347 , 132 ,leaf);
    table[ 133]:=initNode(     56442 , 133 ,leaf);
    table[ 134]:=initNode(     55148 , 134 ,leaf);
    table[ 135]:=initNode(     54419 , 135 ,leaf);
    table[ 136]:=initNode(     53105 , 136 ,leaf);
    table[ 137]:=initNode(     52643 , 137 ,leaf);
    table[ 138]:=initNode(     52131 , 138 ,leaf);
    table[ 139]:=initNode(     50829 , 139 ,leaf);
    table[ 140]:=initNode(     50107 , 140 ,leaf);
    table[ 141]:=initNode(     49495 , 141 ,leaf);
    table[ 142]:=initNode(     48025 , 142 ,leaf);
    table[ 143]:=initNode(     47655 , 143 ,leaf);
    table[ 144]:=initNode(     47164 , 144 ,leaf);
    table[ 145]:=initNode(     46029 , 145 ,leaf);
    table[ 146]:=initNode(     45419 , 146 ,leaf);
    table[ 147]:=initNode(     44408 , 147 ,leaf);
    table[ 148]:=initNode(     44114 , 148 ,leaf);
    table[ 149]:=initNode(     43420 , 149 ,leaf);
    table[ 150]:=initNode(     42700 , 150 ,leaf);
    table[ 151]:=initNode(     41658 , 151 ,leaf);
    table[ 152]:=initNode(     41340 , 152 ,leaf);
    table[ 153]:=initNode(     40031 , 153 ,leaf);
    table[ 154]:=initNode(     40061 , 154 ,leaf);
    table[ 155]:=initNode(     38984 , 155 ,leaf);
    table[ 156]:=initNode(     38352 , 156 ,leaf);
    table[ 157]:=initNode(     37852 , 157 ,leaf);
    table[ 158]:=initNode(     37746 , 158 ,leaf);
    table[ 159]:=initNode(     36918 , 159 ,leaf);
    table[ 160]:=initNode(     36015 , 160 ,leaf);
    table[ 161]:=initNode(     35542 , 161 ,leaf);
    table[ 162]:=initNode(     34957 , 162 ,leaf);
    table[ 163]:=initNode(     34744 , 163 ,leaf);
    table[ 164]:=initNode(     34139 , 164 ,leaf);
    table[ 165]:=initNode(     33675 , 165 ,leaf);
    table[ 166]:=initNode(     33099 , 166 ,leaf);
    table[ 167]:=initNode(     32663 , 167 ,leaf);
    table[ 168]:=initNode(     32325 , 168 ,leaf);
    table[ 169]:=initNode(     31664 , 169 ,leaf);
    table[ 170]:=initNode(     31378 , 170 ,leaf);
    table[ 171]:=initNode(     30509 , 171 ,leaf);
    table[ 172]:=initNode(     30151 , 172 ,leaf);
    table[ 173]:=initNode(     30179 , 173 ,leaf);
    table[ 174]:=initNode(     29394 , 174 ,leaf);
    table[ 175]:=initNode(     29154 , 175 ,leaf);
    table[ 176]:=initNode(     28477 , 176 ,leaf);
    table[ 177]:=initNode(     28012 , 177 ,leaf);
    table[ 178]:=initNode(     27905 , 178 ,leaf);
    table[ 179]:=initNode(     27612 , 179 ,leaf);
    table[ 180]:=initNode(     27259 , 180 ,leaf);
    table[ 181]:=initNode(     26664 , 181 ,leaf);
    table[ 182]:=initNode(     26481 , 182 ,leaf);
    table[ 183]:=initNode(     25796 , 183 ,leaf);
    table[ 184]:=initNode(     25932 , 184 ,leaf);
    table[ 185]:=initNode(     25496 , 185 ,leaf);
    table[ 186]:=initNode(     24875 , 186 ,leaf);
    table[ 187]:=initNode(     24790 , 187 ,leaf);
    table[ 188]:=initNode(     24485 , 188 ,leaf);
    table[ 189]:=initNode(     24313 , 189 ,leaf);
    table[ 190]:=initNode(     23841 , 190 ,leaf);
    table[ 191]:=initNode(     23648 , 191 ,leaf);
    table[ 192]:=initNode(     23225 , 192 ,leaf);
    table[ 193]:=initNode(     22997 , 193 ,leaf);
    table[ 194]:=initNode(     22381 , 194 ,leaf);
    table[ 195]:=initNode(     22497 , 195 ,leaf);
    table[ 196]:=initNode(     22164 , 196 ,leaf);
    table[ 197]:=initNode(     21819 , 197 ,leaf);
    table[ 198]:=initNode(     21637 , 198 ,leaf);
    table[ 199]:=initNode(     21235 , 199 ,leaf);
    table[ 200]:=initNode(     21024 , 200 ,leaf);
    table[ 201]:=initNode(     20997 , 201 ,leaf);
    table[ 202]:=initNode(     20373 , 202 ,leaf);
    table[ 203]:=initNode(     20386 , 203 ,leaf);
    table[ 204]:=initNode(     20209 , 204 ,leaf);
    table[ 205]:=initNode(     19756 , 205 ,leaf);
    table[ 206]:=initNode(     19430 , 206 ,leaf);
    table[ 207]:=initNode(     32673 , 207 ,leaf);
    table[ 208]:=initNode(     19315 , 208 ,leaf);
    table[ 209]:=initNode(     18763 , 209 ,leaf);
    table[ 210]:=initNode(     18721 , 210 ,leaf);
    table[ 211]:=initNode(     18284 , 211 ,leaf);
    table[ 212]:=initNode(     18109 , 212 ,leaf);
    table[ 213]:=initNode(     18034 , 213 ,leaf);
    table[ 214]:=initNode(     17763 , 214 ,leaf);
    table[ 215]:=initNode(     17778 , 215 ,leaf);
    table[ 216]:=initNode(     17588 , 216 ,leaf);
    table[ 217]:=initNode(     17203 , 217 ,leaf);
    table[ 218]:=initNode(     17034 , 218 ,leaf);
    table[ 219]:=initNode(     16834 , 219 ,leaf);
    table[ 220]:=initNode(     16707 , 220 ,leaf);
    table[ 221]:=initNode(     16389 , 221 ,leaf);
    table[ 222]:=initNode(     16153 , 222 ,leaf);
    table[ 223]:=initNode(     15880 , 223 ,leaf);
    table[ 224]:=initNode(     16009 , 224 ,leaf);
    table[ 225]:=initNode(     15757 , 225 ,leaf);
    table[ 226]:=initNode(     15588 , 226 ,leaf);
    table[ 227]:=initNode(     15535 , 227 ,leaf);
    table[ 228]:=initNode(     15373 , 228 ,leaf);
    table[ 229]:=initNode(     15040 , 229 ,leaf);
    table[ 230]:=initNode(     14931 , 230 ,leaf);
    table[ 231]:=initNode(     14715 , 231 ,leaf);
    table[ 232]:=initNode(     14511 , 232 ,leaf);
    table[ 233]:=initNode(     14462 , 233 ,leaf);
    table[ 234]:=initNode(     14423 , 234 ,leaf);
    table[ 235]:=initNode(     14232 , 235 ,leaf);
    table[ 236]:=initNode(     14159 , 236 ,leaf);
    table[ 237]:=initNode(     13993 , 237 ,leaf);
    table[ 238]:=initNode(     13550 , 238 ,leaf);
    table[ 239]:=initNode(     13625 , 239 ,leaf);
    table[ 240]:=initNode(     13318 , 240 ,leaf);
    table[ 241]:=initNode(     13153 , 241 ,leaf);
    table[ 242]:=initNode(     13205 , 242 ,leaf);
    table[ 243]:=initNode(     13167 , 243 ,leaf);
    table[ 244]:=initNode(     12834 , 244 ,leaf);
    table[ 245]:=initNode(     12812 , 245 ,leaf);
    table[ 246]:=initNode(     12477 , 246 ,leaf);
    table[ 247]:=initNode(     12686 , 247 ,leaf);
    table[ 248]:=initNode(     12329 , 248 ,leaf);
    table[ 249]:=initNode(     12279 , 249 ,leaf);
    table[ 250]:=initNode(     12176 , 250 ,leaf);
    table[ 251]:=initNode(     12065 , 251 ,leaf);
    table[ 252]:=initNode(     11908 , 252 ,leaf);
    table[ 253]:=initNode(     11949 , 253 ,leaf);
    table[ 254]:=initNode(     11644 , 254 ,leaf);
    table[ 255]:=initNode(     11547 , 255 ,leaf);
    table[ 256]:=initNode(     11355 , 256 ,leaf);
    table[ 257]:=initNode(     11340 , 257 ,leaf);
    table[ 258]:=initNode(     11409 , 258 ,leaf);
    table[ 259]:=initNode(     11164 , 259 ,leaf);
    table[ 260]:=initNode(     11057 , 260 ,leaf);
    table[ 261]:=initNode(     10988 , 261 ,leaf);
    table[ 262]:=initNode(     10750 , 262 ,leaf);
    table[ 263]:=initNode(     10732 , 263 ,leaf);
    table[ 264]:=initNode(     10677 , 264 ,leaf);
    table[ 265]:=initNode(     10585 , 265 ,leaf);
    table[ 266]:=initNode(     10522 , 266 ,leaf);
    table[ 267]:=initNode(     10466 , 267 ,leaf);
    table[ 268]:=initNode(     10319 , 268 ,leaf);
    table[ 269]:=initNode(     10250 , 269 ,leaf);
    table[ 270]:=initNode(     10153 , 270 ,leaf);
    table[ 271]:=initNode(     10010 , 271 ,leaf);
    table[ 272]:=initNode(      9969 , 272 ,leaf);
    table[ 273]:=initNode(      9752 , 273 ,leaf);
    table[ 274]:=initNode(      9798 , 274 ,leaf);
    table[ 275]:=initNode(      9753 , 275 ,leaf);
    table[ 276]:=initNode(      9536 , 276 ,leaf);
    table[ 277]:=initNode(      9479 , 277 ,leaf);
    table[ 278]:=initNode(      9512 , 278 ,leaf);
    table[ 279]:=initNode(      9518 , 279 ,leaf);
    table[ 280]:=initNode(      9412 , 280 ,leaf);
    table[ 281]:=initNode(      9312 , 281 ,leaf);
    table[ 282]:=initNode(      9242 , 282 ,leaf);
    table[ 283]:=initNode(      9246 , 283 ,leaf);
    table[ 284]:=initNode(      9042 , 284 ,leaf);
    table[ 285]:=initNode(      8925 , 285 ,leaf);
    table[ 286]:=initNode(      8858 , 286 ,leaf);
    table[ 287]:=initNode(      8830 , 287 ,leaf);
    table[ 288]:=initNode(      8597 , 288 ,leaf);
    table[ 289]:=initNode(      8773 , 289 ,leaf);
    table[ 290]:=initNode(      8473 , 290 ,leaf);
    table[ 291]:=initNode(      8572 , 291 ,leaf);
    table[ 292]:=initNode(    196904 , 292 ,leaf);
    table[ 293]:=initNode(      8351 , 293 ,leaf);
    table[ 294]:=initNode(      8448 , 294 ,leaf);
    table[ 295]:=initNode(      8114 , 295 ,leaf);
    table[ 296]:=initNode(      8262 , 296 ,leaf);
    table[ 297]:=initNode(      8206 , 297 ,leaf);
    table[ 298]:=initNode(      8013 , 298 ,leaf);
    table[ 299]:=initNode(      8048 , 299 ,leaf);
    table[ 300]:=initNode(      8080 , 300 ,leaf);
    table[ 301]:=initNode(      7822 , 301 ,leaf);
    table[ 302]:=initNode(      7880 , 302 ,leaf);
    table[ 303]:=initNode(      7932 , 303 ,leaf);
    table[ 304]:=initNode(      7794 , 304 ,leaf);
    table[ 305]:=initNode(      7666 , 305 ,leaf);
    table[ 306]:=initNode(      7725 , 306 ,leaf);
    table[ 307]:=initNode(      7773 , 307 ,leaf);
    table[ 308]:=initNode(      7466 , 308 ,leaf);
    table[ 309]:=initNode(      7480 , 309 ,leaf);
    table[ 310]:=initNode(      7323 , 310 ,leaf);
    table[ 311]:=initNode(      7331 , 311 ,leaf);
    table[ 312]:=initNode(      7262 , 312 ,leaf);
    table[ 313]:=initNode(      7222 , 313 ,leaf);
    table[ 314]:=initNode(      7192 , 314 ,leaf);
    table[ 315]:=initNode(      7080 , 315 ,leaf);
    table[ 316]:=initNode(      7126 , 316 ,leaf);
    table[ 317]:=initNode(      7054 , 317 ,leaf);
    table[ 318]:=initNode(      7104 , 318 ,leaf);
    table[ 319]:=initNode(      7111 , 319 ,leaf);
    table[ 320]:=initNode(      6993 , 320 ,leaf);
    table[ 321]:=initNode(      6959 , 321 ,leaf);
    table[ 322]:=initNode(      6815 , 322 ,leaf);
    table[ 323]:=initNode(      6793 , 323 ,leaf);
    table[ 324]:=initNode(      6733 , 324 ,leaf);
    table[ 325]:=initNode(      6774 , 325 ,leaf);
    table[ 326]:=initNode(      6741 , 326 ,leaf);
    table[ 327]:=initNode(      6708 , 327 ,leaf);
    table[ 328]:=initNode(      6629 , 328 ,leaf);
    table[ 329]:=initNode(      6497 , 329 ,leaf);
    table[ 330]:=initNode(      6486 , 330 ,leaf);
    table[ 331]:=initNode(      6450 , 331 ,leaf);
    table[ 332]:=initNode(      6256 , 332 ,leaf);
    table[ 333]:=initNode(      6240 , 333 ,leaf);
    table[ 334]:=initNode(      6352 , 334 ,leaf);
    table[ 335]:=initNode(      6301 , 335 ,leaf);
    table[ 336]:=initNode(      6222 , 336 ,leaf);
    table[ 337]:=initNode(      6235 , 337 ,leaf);
    table[ 338]:=initNode(      6084 , 338 ,leaf);
    table[ 339]:=initNode(      6028 , 339 ,leaf);
    table[ 340]:=initNode(      5969 , 340 ,leaf);
    table[ 341]:=initNode(      6184 , 341 ,leaf);
    table[ 342]:=initNode(      5889 , 342 ,leaf);
    table[ 343]:=initNode(      5828 , 343 ,leaf);
    table[ 344]:=initNode(      5958 , 344 ,leaf);
    table[ 345]:=initNode(      5749 , 345 ,leaf);
    table[ 346]:=initNode(      5735 , 346 ,leaf);
    table[ 347]:=initNode(      5828 , 347 ,leaf);
    table[ 348]:=initNode(      5876 , 348 ,leaf);
    table[ 349]:=initNode(      5727 , 349 ,leaf);
    table[ 350]:=initNode(      5536 , 350 ,leaf);
    table[ 351]:=initNode(      5659 , 351 ,leaf);
    table[ 352]:=initNode(      5646 , 352 ,leaf);
    table[ 353]:=initNode(      5614 , 353 ,leaf);
    table[ 354]:=initNode(      5407 , 354 ,leaf);
    table[ 355]:=initNode(      5450 , 355 ,leaf);
    table[ 356]:=initNode(      5410 , 356 ,leaf);
    table[ 357]:=initNode(      5374 , 357 ,leaf);
    table[ 358]:=initNode(      5387 , 358 ,leaf);
    table[ 359]:=initNode(      5365 , 359 ,leaf);
    table[ 360]:=initNode(      5263 , 360 ,leaf);
    table[ 361]:=initNode(      5253 , 361 ,leaf);
    table[ 362]:=initNode(      5211 , 362 ,leaf);
    table[ 363]:=initNode(      5266 , 363 ,leaf);
    table[ 364]:=initNode(      5211 , 364 ,leaf);
    table[ 365]:=initNode(      5116 , 365 ,leaf);
    table[ 366]:=initNode(      5063 , 366 ,leaf);
    table[ 367]:=initNode(      5208 , 367 ,leaf);
    table[ 368]:=initNode(      5077 , 368 ,leaf);
    table[ 369]:=initNode(      5047 , 369 ,leaf);
    table[ 370]:=initNode(      5122 , 370 ,leaf);
    table[ 371]:=initNode(      5024 , 371 ,leaf);
    table[ 372]:=initNode(      5014 , 372 ,leaf);
    table[ 373]:=initNode(      4923 , 373 ,leaf);
    table[ 374]:=initNode(      5016 , 374 ,leaf);
    table[ 375]:=initNode(      4813 , 375 ,leaf);
    table[ 376]:=initNode(      4803 , 376 ,leaf);
    table[ 377]:=initNode(      4883 , 377 ,leaf);
    table[ 378]:=initNode(      4777 , 378 ,leaf);
    table[ 379]:=initNode(      4857 , 379 ,leaf);
    table[ 380]:=initNode(      4683 , 380 ,leaf);
    table[ 381]:=initNode(      4627 , 381 ,leaf);
    table[ 382]:=initNode(      4718 , 382 ,leaf);
    table[ 383]:=initNode(      4688 , 383 ,leaf);
    table[ 384]:=initNode(      4654 , 384 ,leaf);
    table[ 385]:=initNode(      4749 , 385 ,leaf);
    table[ 386]:=initNode(      4610 , 386 ,leaf);
    table[ 387]:=initNode(      4526 , 387 ,leaf);
    table[ 388]:=initNode(      4555 , 388 ,leaf);
    table[ 389]:=initNode(      4444 , 389 ,leaf);
    table[ 390]:=initNode(      4580 , 390 ,leaf);
    table[ 391]:=initNode(      4465 , 391 ,leaf);
    table[ 392]:=initNode(      4561 , 392 ,leaf);
    table[ 393]:=initNode(      4333 , 393 ,leaf);
    table[ 394]:=initNode(      4522 , 394 ,leaf);
    table[ 395]:=initNode(      4309 , 395 ,leaf);
    table[ 396]:=initNode(      4372 , 396 ,leaf);
    table[ 397]:=initNode(      4439 , 397 ,leaf);
    table[ 398]:=initNode(      4339 , 398 ,leaf);
    table[ 399]:=initNode(      4387 , 399 ,leaf);
    table[ 400]:=initNode(      4344 , 400 ,leaf);
    table[ 401]:=initNode(      4136 , 401 ,leaf);
    table[ 402]:=initNode(      4261 , 402 ,leaf);
    table[ 403]:=initNode(      4150 , 403 ,leaf);
    table[ 404]:=initNode(      4158 , 404 ,leaf);
    table[ 405]:=initNode(      4118 , 405 ,leaf);
    table[ 406]:=initNode(      4128 , 406 ,leaf);
    table[ 407]:=initNode(      4141 , 407 ,leaf);
    table[ 408]:=initNode(      4135 , 408 ,leaf);
    table[ 409]:=initNode(      3977 , 409 ,leaf);
    table[ 410]:=initNode(      4252 , 410 ,leaf);
    table[ 411]:=initNode(      4059 , 411 ,leaf);
    table[ 412]:=initNode(      4018 , 412 ,leaf);
    table[ 413]:=initNode(     17266 , 413 ,leaf);
    table[ 414]:=initNode(      3879 , 414 ,leaf);
    table[ 415]:=initNode(      4020 , 415 ,leaf);
    table[ 416]:=initNode(      3938 , 416 ,leaf);
    table[ 417]:=initNode(      3948 , 417 ,leaf);
    table[ 418]:=initNode(      4027 , 418 ,leaf);
    table[ 419]:=initNode(      3947 , 419 ,leaf);
    table[ 420]:=initNode(      3856 , 420 ,leaf);
    table[ 421]:=initNode(      3815 , 421 ,leaf);
    table[ 422]:=initNode(      3927 , 422 ,leaf);
    table[ 423]:=initNode(      3925 , 423 ,leaf);
    table[ 424]:=initNode(      3905 , 424 ,leaf);
    table[ 425]:=initNode(      3729 , 425 ,leaf);
    table[ 426]:=initNode(      3791 , 426 ,leaf);
    table[ 427]:=initNode(      3828 , 427 ,leaf);
    table[ 428]:=initNode(      3851 , 428 ,leaf);
    table[ 429]:=initNode(      3806 , 429 ,leaf);
    table[ 430]:=initNode(      3860 , 430 ,leaf);
    table[ 431]:=initNode(      3698 , 431 ,leaf);
    table[ 432]:=initNode(      3681 , 432 ,leaf);
    table[ 433]:=initNode(      3615 , 433 ,leaf);
    table[ 434]:=initNode(      3720 , 434 ,leaf);
    table[ 435]:=initNode(      3659 , 435 ,leaf);
    table[ 436]:=initNode(      3716 , 436 ,leaf);
    table[ 437]:=initNode(      3682 , 437 ,leaf);
    table[ 438]:=initNode(      3627 , 438 ,leaf);
    table[ 439]:=initNode(      3648 , 439 ,leaf);
    table[ 440]:=initNode(      3569 , 440 ,leaf);
    table[ 441]:=initNode(      3538 , 441 ,leaf);
    table[ 442]:=initNode(      3585 , 442 ,leaf);
    table[ 443]:=initNode(      3615 , 443 ,leaf);
    table[ 444]:=initNode(      3606 , 444 ,leaf);
    table[ 445]:=initNode(      3556 , 445 ,leaf);
    table[ 446]:=initNode(      3596 , 446 ,leaf);
    table[ 447]:=initNode(      3489 , 447 ,leaf);
    table[ 448]:=initNode(      3615 , 448 ,leaf);
    table[ 449]:=initNode(      3625 , 449 ,leaf);
    table[ 450]:=initNode(      3510 , 450 ,leaf);
    table[ 451]:=initNode(      3430 , 451 ,leaf);
    table[ 452]:=initNode(      3572 , 452 ,leaf);
    table[ 453]:=initNode(      3386 , 453 ,leaf);
    table[ 454]:=initNode(      3414 , 454 ,leaf);
    table[ 455]:=initNode(      3505 , 455 ,leaf);
    table[ 456]:=initNode(      3471 , 456 ,leaf);
    table[ 457]:=initNode(      3440 , 457 ,leaf);
    table[ 458]:=initNode(      3477 , 458 ,leaf);
    table[ 459]:=initNode(      3484 , 459 ,leaf);
    table[ 460]:=initNode(      3354 , 460 ,leaf);
    table[ 461]:=initNode(      3296 , 461 ,leaf);
    table[ 462]:=initNode(      3453 , 462 ,leaf);
    table[ 463]:=initNode(      3410 , 463 ,leaf);
    table[ 464]:=initNode(      3310 , 464 ,leaf);
    table[ 465]:=initNode(      3367 , 465 ,leaf);
    table[ 466]:=initNode(      3305 , 466 ,leaf);
    table[ 467]:=initNode(      3318 , 467 ,leaf);
    table[ 468]:=initNode(      3353 , 468 ,leaf);
    table[ 469]:=initNode(      3238 , 469 ,leaf);
    table[ 470]:=initNode(      3408 , 470 ,leaf);
    table[ 471]:=initNode(      3347 , 471 ,leaf);
    table[ 472]:=initNode(      3332 , 472 ,leaf);
    table[ 473]:=initNode(      3316 , 473 ,leaf);
    table[ 474]:=initNode(      3245 , 474 ,leaf);
    table[ 475]:=initNode(      3229 , 475 ,leaf);
    table[ 476]:=initNode(      3261 , 476 ,leaf);
    table[ 477]:=initNode(      3293 , 477 ,leaf);
    table[ 478]:=initNode(      3266 , 478 ,leaf);
    table[ 479]:=initNode(      3386 , 479 ,leaf);
    table[ 480]:=initNode(      3214 , 480 ,leaf);
    table[ 481]:=initNode(      3307 , 481 ,leaf);
    table[ 482]:=initNode(      3391 , 482 ,leaf);
    table[ 483]:=initNode(      3199 , 483 ,leaf);
    table[ 484]:=initNode(      3244 , 484 ,leaf);
    table[ 485]:=initNode(      3267 , 485 ,leaf);
    table[ 486]:=initNode(      3283 , 486 ,leaf);
    table[ 487]:=initNode(      3261 , 487 ,leaf);
    table[ 488]:=initNode(      3347 , 488 ,leaf);
    table[ 489]:=initNode(      3224 , 489 ,leaf);
    table[ 490]:=initNode(      3155 , 490 ,leaf);
    table[ 491]:=initNode(      3192 , 491 ,leaf);
    table[ 492]:=initNode(      3181 , 492 ,leaf);
    table[ 493]:=initNode(      3245 , 493 ,leaf);
    table[ 494]:=initNode(      3126 , 494 ,leaf);
    table[ 495]:=initNode(      3130 , 495 ,leaf);
    table[ 496]:=initNode(      3150 , 496 ,leaf);
    table[ 497]:=initNode(      3096 , 497 ,leaf);
    table[ 498]:=initNode(      3083 , 498 ,leaf);
    table[ 499]:=initNode(      3168 , 499 ,leaf);
    table[ 500]:=initNode(      3238 , 500 ,leaf);
    table[ 501]:=initNode(      3205 , 501 ,leaf);
    table[ 502]:=initNode(      3096 , 502 ,leaf);
    table[ 503]:=initNode(      3145 , 503 ,leaf);
    table[ 504]:=initNode(      3107 , 504 ,leaf);
    table[ 505]:=initNode(      3099 , 505 ,leaf);
    table[ 506]:=initNode(      3214 , 506 ,leaf);
    table[ 507]:=initNode(      3071 , 507 ,leaf);
    table[ 508]:=initNode(      3189 , 508 ,leaf);
    table[ 509]:=initNode(      3168 , 509 ,leaf);
    table[ 510]:=initNode(      3282 , 510 ,leaf);
    table[ 511]:=initNode(      3153 , 511 ,leaf);
    table[ 512]:=initNode(      3151 , 512 ,leaf);
    table[ 513]:=initNode(      3135 , 513 ,leaf);
    table[ 514]:=initNode(      3093 , 514 ,leaf);
    table[ 515]:=initNode(      3189 , 515 ,leaf);
    table[ 516]:=initNode(      3106 , 516 ,leaf);
    table[ 517]:=initNode(      3175 , 517 ,leaf);
    table[ 518]:=initNode(      3092 , 518 ,leaf);
    table[ 519]:=initNode(      3177 , 519 ,leaf);
    table[ 520]:=initNode(      3134 , 520 ,leaf);
    table[ 521]:=initNode(      3140 , 521 ,leaf);
    table[ 522]:=initNode(      3077 , 522 ,leaf);
    table[ 523]:=initNode(      3166 , 523 ,leaf);
    table[ 524]:=initNode(      3081 , 524 ,leaf);
    table[ 525]:=initNode(      3059 , 525 ,leaf);
    table[ 526]:=initNode(      3112 , 526 ,leaf);
    table[ 527]:=initNode(      3128 , 527 ,leaf);
    table[ 528]:=initNode(      3103 , 528 ,leaf);
    table[ 529]:=initNode(      3118 , 529 ,leaf);
    table[ 530]:=initNode(      3109 , 530 ,leaf);
    table[ 531]:=initNode(      3142 , 531 ,leaf);
    table[ 532]:=initNode(      3127 , 532 ,leaf);
    table[ 533]:=initNode(      3193 , 533 ,leaf);
    table[ 534]:=initNode(      3120 , 534 ,leaf);
    table[ 535]:=initNode(      3089 , 535 ,leaf);
    table[ 536]:=initNode(      3136 , 536 ,leaf);
    table[ 537]:=initNode(      3055 , 537 ,leaf);
    table[ 538]:=initNode(      3085 , 538 ,leaf);
    table[ 539]:=initNode(      3064 , 539 ,leaf);
    table[ 540]:=initNode(      3035 , 540 ,leaf);
    table[ 541]:=initNode(      3225 , 541 ,leaf);
    table[ 542]:=initNode(      3068 , 542 ,leaf);
    table[ 543]:=initNode(      3089 , 543 ,leaf);
    table[ 544]:=initNode(      3071 , 544 ,leaf);
    table[ 545]:=initNode(      3092 , 545 ,leaf);
    table[ 546]:=initNode(      3132 , 546 ,leaf);
    table[ 547]:=initNode(      3193 , 547 ,leaf);
    table[ 548]:=initNode(      3100 , 548 ,leaf);
    table[ 549]:=initNode(      3186 , 549 ,leaf);
    table[ 550]:=initNode(      3230 , 550 ,leaf);
    table[ 551]:=initNode(      3089 , 551 ,leaf);
    table[ 552]:=initNode(      3132 , 552 ,leaf);
    table[ 553]:=initNode(      3184 , 553 ,leaf);
    table[ 554]:=initNode(      3206 , 554 ,leaf);
    table[ 555]:=initNode(      3071 , 555 ,leaf);
    table[ 556]:=initNode(      3054 , 556 ,leaf);
    table[ 557]:=initNode(      3162 , 557 ,leaf);
    table[ 558]:=initNode(      3097 , 558 ,leaf);
    table[ 559]:=initNode(      3138 , 559 ,leaf);
    table[ 560]:=initNode(      3089 , 560 ,leaf);
    table[ 561]:=initNode(      3180 , 561 ,leaf);
    table[ 562]:=initNode(      3228 , 562 ,leaf);
    table[ 563]:=initNode(      3089 , 563 ,leaf);
    table[ 564]:=initNode(      3097 , 564 ,leaf);
    table[ 565]:=initNode(      3102 , 565 ,leaf);
    table[ 566]:=initNode(      3189 , 566 ,leaf);
    table[ 567]:=initNode(      3140 , 567 ,leaf);
    table[ 568]:=initNode(      3267 , 568 ,leaf);
    table[ 569]:=initNode(      3151 , 569 ,leaf);
    table[ 570]:=initNode(      3218 , 570 ,leaf);
    table[ 571]:=initNode(      3123 , 571 ,leaf);
    table[ 572]:=initNode(      3246 , 572 ,leaf);
    table[ 573]:=initNode(      3250 , 573 ,leaf);
    table[ 574]:=initNode(      3150 , 574 ,leaf);
    table[ 575]:=initNode(      3116 , 575 ,leaf);
    table[ 576]:=initNode(      3255 , 576 ,leaf);
    table[ 577]:=initNode(      3259 , 577 ,leaf);
    table[ 578]:=initNode(      3216 , 578 ,leaf);
    table[ 579]:=initNode(      3078 , 579 ,leaf);
    table[ 580]:=initNode(      3158 , 580 ,leaf);
    table[ 581]:=initNode(      3188 , 581 ,leaf);
    table[ 582]:=initNode(      3262 , 582 ,leaf);
    table[ 583]:=initNode(      3260 , 583 ,leaf);
    table[ 584]:=initNode(      3398 , 584 ,leaf);
    table[ 585]:=initNode(      3268 , 585 ,leaf);
    table[ 586]:=initNode(      3270 , 586 ,leaf);
    table[ 587]:=initNode(      3204 , 587 ,leaf);
    table[ 588]:=initNode(      3364 , 588 ,leaf);
    table[ 589]:=initNode(      3351 , 589 ,leaf);
    table[ 590]:=initNode(      3298 , 590 ,leaf);
    table[ 591]:=initNode(      3358 , 591 ,leaf);
    table[ 592]:=initNode(      3321 , 592 ,leaf);
    table[ 593]:=initNode(      3339 , 593 ,leaf);
    table[ 594]:=initNode(      3417 , 594 ,leaf);
    table[ 595]:=initNode(      3449 , 595 ,leaf);
    table[ 596]:=initNode(      3415 , 596 ,leaf);
    table[ 597]:=initNode(      3300 , 597 ,leaf);
    table[ 598]:=initNode(      3408 , 598 ,leaf);
    table[ 599]:=initNode(      3462 , 599 ,leaf);
    table[ 600]:=initNode(      3470 , 600 ,leaf);
    table[ 601]:=initNode(      3438 , 601 ,leaf);
    table[ 602]:=initNode(      3572 , 602 ,leaf);
    table[ 603]:=initNode(      3530 , 603 ,leaf);
    table[ 604]:=initNode(      3464 , 604 ,leaf);
    table[ 605]:=initNode(      3446 , 605 ,leaf);
    table[ 606]:=initNode(      3357 , 606 ,leaf);
    table[ 607]:=initNode(      3547 , 607 ,leaf);
    table[ 608]:=initNode(      3546 , 608 ,leaf);
    table[ 609]:=initNode(      3506 , 609 ,leaf);
    table[ 610]:=initNode(      3552 , 610 ,leaf);
    table[ 611]:=initNode(      3566 , 611 ,leaf);
    table[ 612]:=initNode(      3592 , 612 ,leaf);
    table[ 613]:=initNode(      3561 , 613 ,leaf);
    table[ 614]:=initNode(      3634 , 614 ,leaf);
    table[ 615]:=initNode(      3617 , 615 ,leaf);
    table[ 616]:=initNode(      3579 , 616 ,leaf);
    table[ 617]:=initNode(      3674 , 617 ,leaf);
    table[ 618]:=initNode(      3615 , 618 ,leaf);
    table[ 619]:=initNode(      3746 , 619 ,leaf);
    table[ 620]:=initNode(      3733 , 620 ,leaf);
    table[ 621]:=initNode(      3745 , 621 ,leaf);
    table[ 622]:=initNode(      3825 , 622 ,leaf);
    table[ 623]:=initNode(      3819 , 623 ,leaf);
    table[ 624]:=initNode(      3777 , 624 ,leaf);
    table[ 625]:=initNode(      3755 , 625 ,leaf);
    table[ 626]:=initNode(      3853 , 626 ,leaf);
    table[ 627]:=initNode(      3846 , 627 ,leaf);
    table[ 628]:=initNode(      3884 , 628 ,leaf);
    table[ 629]:=initNode(      3859 , 629 ,leaf);
    table[ 630]:=initNode(      3812 , 630 ,leaf);
    table[ 631]:=initNode(      3796 , 631 ,leaf);
    table[ 632]:=initNode(      3782 , 632 ,leaf);
    table[ 633]:=initNode(      3850 , 633 ,leaf);
    table[ 634]:=initNode(      3954 , 634 ,leaf);
    table[ 635]:=initNode(      3894 , 635 ,leaf);
    table[ 636]:=initNode(      4009 , 636 ,leaf);
    table[ 637]:=initNode(      3953 , 637 ,leaf);
    table[ 638]:=initNode(      4066 , 638 ,leaf);
    table[ 639]:=initNode(      4076 , 639 ,leaf);
    table[ 640]:=initNode(      4063 , 640 ,leaf);
    table[ 641]:=initNode(      4162 , 641 ,leaf);
    table[ 642]:=initNode(      4051 , 642 ,leaf);
    table[ 643]:=initNode(      4122 , 643 ,leaf);
    table[ 644]:=initNode(      4197 , 644 ,leaf);
    table[ 645]:=initNode(      4067 , 645 ,leaf);
    table[ 646]:=initNode(      4173 , 646 ,leaf);
    table[ 647]:=initNode(      4289 , 647 ,leaf);
    table[ 648]:=initNode(      4245 , 648 ,leaf);
    table[ 649]:=initNode(      4155 , 649 ,leaf);
    table[ 650]:=initNode(      4305 , 650 ,leaf);
    table[ 651]:=initNode(      4282 , 651 ,leaf);
    table[ 652]:=initNode(      4329 , 652 ,leaf);
    table[ 653]:=initNode(      4281 , 653 ,leaf);
    table[ 654]:=initNode(      4342 , 654 ,leaf);
    table[ 655]:=initNode(      4442 , 655 ,leaf);
    table[ 656]:=initNode(      4444 , 656 ,leaf);
    table[ 657]:=initNode(      4505 , 657 ,leaf);
    table[ 658]:=initNode(      4388 , 658 ,leaf);
    table[ 659]:=initNode(      4427 , 659 ,leaf);
    table[ 660]:=initNode(      4529 , 660 ,leaf);
    table[ 661]:=initNode(      4528 , 661 ,leaf);
    table[ 662]:=initNode(      4567 , 662 ,leaf);
    table[ 663]:=initNode(      4584 , 663 ,leaf);
    table[ 664]:=initNode(      4567 , 664 ,leaf);
    table[ 665]:=initNode(      4689 , 665 ,leaf);
    table[ 666]:=initNode(      4640 , 666 ,leaf);
    table[ 667]:=initNode(      4619 , 667 ,leaf);
    table[ 668]:=initNode(      4625 , 668 ,leaf);
    table[ 669]:=initNode(      4772 , 669 ,leaf);
    table[ 670]:=initNode(      4721 , 670 ,leaf);
    table[ 671]:=initNode(      4835 , 671 ,leaf);
    table[ 672]:=initNode(      4677 , 672 ,leaf);
    table[ 673]:=initNode(      4784 , 673 ,leaf);
    table[ 674]:=initNode(      4796 , 674 ,leaf);
    table[ 675]:=initNode(      4944 , 675 ,leaf);
    table[ 676]:=initNode(      5019 , 676 ,leaf);
    table[ 677]:=initNode(      4924 , 677 ,leaf);
    table[ 678]:=initNode(      4963 , 678 ,leaf);
    table[ 679]:=initNode(      5040 , 679 ,leaf);
    table[ 680]:=initNode(      5056 , 680 ,leaf);
    table[ 681]:=initNode(      5047 , 681 ,leaf);
    table[ 682]:=initNode(      5126 , 682 ,leaf);
    table[ 683]:=initNode(      5070 , 683 ,leaf);
    table[ 684]:=initNode(      5167 , 684 ,leaf);
    table[ 685]:=initNode(      5240 , 685 ,leaf);
    table[ 686]:=initNode(      5198 , 686 ,leaf);
    table[ 687]:=initNode(      5192 , 687 ,leaf);
    table[ 688]:=initNode(      5322 , 688 ,leaf);
    table[ 689]:=initNode(      5355 , 689 ,leaf);
    table[ 690]:=initNode(      5321 , 690 ,leaf);
    table[ 691]:=initNode(      5504 , 691 ,leaf);
    table[ 692]:=initNode(      5547 , 692 ,leaf);
    table[ 693]:=initNode(      5678 , 693 ,leaf);
    table[ 694]:=initNode(      5449 , 694 ,leaf);
    table[ 695]:=initNode(      5716 , 695 ,leaf);
    table[ 696]:=initNode(      5789 , 696 ,leaf);
    table[ 697]:=initNode(      5505 , 697 ,leaf);
    table[ 698]:=initNode(      5721 , 698 ,leaf);
    table[ 699]:=initNode(      5701 , 699 ,leaf);
    table[ 700]:=initNode(      5744 , 700 ,leaf);
    table[ 701]:=initNode(      5797 , 701 ,leaf);
    table[ 702]:=initNode(      5975 , 702 ,leaf);
    table[ 703]:=initNode(      5995 , 703 ,leaf);
    table[ 704]:=initNode(      6018 , 704 ,leaf);
    table[ 705]:=initNode(      6053 , 705 ,leaf);
    table[ 706]:=initNode(      5925 , 706 ,leaf);
    table[ 707]:=initNode(      6094 , 707 ,leaf);
    table[ 708]:=initNode(      6106 , 708 ,leaf);
    table[ 709]:=initNode(      6211 , 709 ,leaf);
    table[ 710]:=initNode(      6115 , 710 ,leaf);
    table[ 711]:=initNode(      6216 , 711 ,leaf);
    table[ 712]:=initNode(      6342 , 712 ,leaf);
    table[ 713]:=initNode(      6203 , 713 ,leaf);
    table[ 714]:=initNode(      6319 , 714 ,leaf);
    table[ 715]:=initNode(      6488 , 715 ,leaf);
    table[ 716]:=initNode(      6451 , 716 ,leaf);
    table[ 717]:=initNode(      6489 , 717 ,leaf);
    table[ 718]:=initNode(      6643 , 718 ,leaf);
    table[ 719]:=initNode(      6485 , 719 ,leaf);
    table[ 720]:=initNode(      6749 , 720 ,leaf);
    table[ 721]:=initNode(      6675 , 721 ,leaf);
    table[ 722]:=initNode(      6819 , 722 ,leaf);
    table[ 723]:=initNode(      6790 , 723 ,leaf);
    table[ 724]:=initNode(      6876 , 724 ,leaf);
    table[ 725]:=initNode(      6847 , 725 ,leaf);
    table[ 726]:=initNode(      6999 , 726 ,leaf);
    table[ 727]:=initNode(      7028 , 727 ,leaf);
    table[ 728]:=initNode(      6989 , 728 ,leaf);
    table[ 729]:=initNode(      7122 , 729 ,leaf);
    table[ 730]:=initNode(      7180 , 730 ,leaf);
    table[ 731]:=initNode(      7054 , 731 ,leaf);
    table[ 732]:=initNode(      7203 , 732 ,leaf);
    table[ 733]:=initNode(      7307 , 733 ,leaf);
    table[ 734]:=initNode(      7350 , 734 ,leaf);
    table[ 735]:=initNode(      7542 , 735 ,leaf);
    table[ 736]:=initNode(      7319 , 736 ,leaf);
    table[ 737]:=initNode(      7642 , 737 ,leaf);
    table[ 738]:=initNode(      7620 , 738 ,leaf);
    table[ 739]:=initNode(      7827 , 739 ,leaf);
    table[ 740]:=initNode(      7705 , 740 ,leaf);
    table[ 741]:=initNode(      7906 , 741 ,leaf);
    table[ 742]:=initNode(      7874 , 742 ,leaf);
    table[ 743]:=initNode(      7901 , 743 ,leaf);
    table[ 744]:=initNode(      8144 , 744 ,leaf);
    table[ 745]:=initNode(      7976 , 745 ,leaf);
    table[ 746]:=initNode(      8281 , 746 ,leaf);
    table[ 747]:=initNode(      8341 , 747 ,leaf);
    table[ 748]:=initNode(      8298 , 748 ,leaf);
    table[ 749]:=initNode(      8474 , 749 ,leaf);
    table[ 750]:=initNode(      8484 , 750 ,leaf);
    table[ 751]:=initNode(      8609 , 751 ,leaf);
    table[ 752]:=initNode(      8686 , 752 ,leaf);
    table[ 753]:=initNode(      8722 , 753 ,leaf);
    table[ 754]:=initNode(      8551 , 754 ,leaf);
    table[ 755]:=initNode(      8864 , 755 ,leaf);
    table[ 756]:=initNode(      8788 , 756 ,leaf);
    table[ 757]:=initNode(      8780 , 757 ,leaf);
    table[ 758]:=initNode(      9172 , 758 ,leaf);
    table[ 759]:=initNode(      9319 , 759 ,leaf);
    table[ 760]:=initNode(      9177 , 760 ,leaf);
    table[ 761]:=initNode(      9444 , 761 ,leaf);
    table[ 762]:=initNode(      9615 , 762 ,leaf);
    table[ 763]:=initNode(      9482 , 763 ,leaf);
    table[ 764]:=initNode(      9665 , 764 ,leaf);
    table[ 765]:=initNode(      9679 , 765 ,leaf);
    table[ 766]:=initNode(      9722 , 766 ,leaf);
    table[ 767]:=initNode(      9648 , 767 ,leaf);
    table[ 768]:=initNode(      9942 , 768 ,leaf);
    table[ 769]:=initNode(      9943 , 769 ,leaf);
    table[ 770]:=initNode(     10259 , 770 ,leaf);
    table[ 771]:=initNode(     10308 , 771 ,leaf);
    table[ 772]:=initNode(     10370 , 772 ,leaf);
    table[ 773]:=initNode(     10307 , 773 ,leaf);
    table[ 774]:=initNode(     10552 , 774 ,leaf);
    table[ 775]:=initNode(     10556 , 775 ,leaf);
    table[ 776]:=initNode(     10667 , 776 ,leaf);
    table[ 777]:=initNode(     10809 , 777 ,leaf);
    table[ 778]:=initNode(     10802 , 778 ,leaf);
    table[ 779]:=initNode(     10938 , 779 ,leaf);
    table[ 780]:=initNode(     11059 , 780 ,leaf);
    table[ 781]:=initNode(     11137 , 781 ,leaf);
    table[ 782]:=initNode(     11302 , 782 ,leaf);
    table[ 783]:=initNode(     11265 , 783 ,leaf);
    table[ 784]:=initNode(     11446 , 784 ,leaf);
    table[ 785]:=initNode(     11684 , 785 ,leaf);
    table[ 786]:=initNode(     11751 , 786 ,leaf);
    table[ 787]:=initNode(     11810 , 787 ,leaf);
    table[ 788]:=initNode(     12016 , 788 ,leaf);
    table[ 789]:=initNode(     12133 , 789 ,leaf);
    table[ 790]:=initNode(     12156 , 790 ,leaf);
    table[ 791]:=initNode(     12493 , 791 ,leaf);
    table[ 792]:=initNode(     12518 , 792 ,leaf);
    table[ 793]:=initNode(     12637 , 793 ,leaf);
    table[ 794]:=initNode(     12645 , 794 ,leaf);
    table[ 795]:=initNode(     12918 , 795 ,leaf);
    table[ 796]:=initNode(     12922 , 796 ,leaf);
    table[ 797]:=initNode(     13160 , 797 ,leaf);
    table[ 798]:=initNode(     13209 , 798 ,leaf);
    table[ 799]:=initNode(     13244 , 799 ,leaf);
    table[ 800]:=initNode(     13628 , 800 ,leaf);
    table[ 801]:=initNode(     13835 , 801 ,leaf);
    table[ 802]:=initNode(     13785 , 802 ,leaf);
    table[ 803]:=initNode(     14024 , 803 ,leaf);
    table[ 804]:=initNode(     14185 , 804 ,leaf);
    table[ 805]:=initNode(     14274 , 805 ,leaf);
    table[ 806]:=initNode(     14276 , 806 ,leaf);
    table[ 807]:=initNode(     14554 , 807 ,leaf);
    table[ 808]:=initNode(     14753 , 808 ,leaf);
    table[ 809]:=initNode(     14832 , 809 ,leaf);
    table[ 810]:=initNode(     14877 , 810 ,leaf);
    table[ 811]:=initNode(     15032 , 811 ,leaf);
    table[ 812]:=initNode(     15373 , 812 ,leaf);
    table[ 813]:=initNode(     15355 , 813 ,leaf);
    table[ 814]:=initNode(     15657 , 814 ,leaf);
    table[ 815]:=initNode(     15762 , 815 ,leaf);
    table[ 816]:=initNode(     16208 , 816 ,leaf);
    table[ 817]:=initNode(     16275 , 817 ,leaf);
    table[ 818]:=initNode(     16462 , 818 ,leaf);
    table[ 819]:=initNode(     16636 , 819 ,leaf);
    table[ 820]:=initNode(     16757 , 820 ,leaf);
    table[ 821]:=initNode(     16948 , 821 ,leaf);
    table[ 822]:=initNode(     17202 , 822 ,leaf);
    table[ 823]:=initNode(     17315 , 823 ,leaf);
    table[ 824]:=initNode(     17683 , 824 ,leaf);
    table[ 825]:=initNode(     17767 , 825 ,leaf);
    table[ 826]:=initNode(     17811 , 826 ,leaf);
    table[ 827]:=initNode(     18014 , 827 ,leaf);
    table[ 828]:=initNode(     18215 , 828 ,leaf);
    table[ 829]:=initNode(     18569 , 829 ,leaf);
    table[ 830]:=initNode(     18893 , 830 ,leaf);
    table[ 831]:=initNode(     19191 , 831 ,leaf);
    table[ 832]:=initNode(     19131 , 832 ,leaf);
    table[ 833]:=initNode(     19194 , 833 ,leaf);
    table[ 834]:=initNode(     19563 , 834 ,leaf);
    table[ 835]:=initNode(     19985 , 835 ,leaf);
    table[ 836]:=initNode(     20049 , 836 ,leaf);
    table[ 837]:=initNode(     20450 , 837 ,leaf);
    table[ 838]:=initNode(     20621 , 838 ,leaf);
    table[ 839]:=initNode(     20793 , 839 ,leaf);
    table[ 840]:=initNode(     21146 , 840 ,leaf);
    table[ 841]:=initNode(     21019 , 841 ,leaf);
    table[ 842]:=initNode(     21375 , 842 ,leaf);
    table[ 843]:=initNode(     21883 , 843 ,leaf);
    table[ 844]:=initNode(     21975 , 844 ,leaf);
    table[ 845]:=initNode(     22509 , 845 ,leaf);
    table[ 846]:=initNode(     22664 , 846 ,leaf);
    table[ 847]:=initNode(     23228 , 847 ,leaf);
    table[ 848]:=initNode(     23371 , 848 ,leaf);
    table[ 849]:=initNode(     23412 , 849 ,leaf);
    table[ 850]:=initNode(     23932 , 850 ,leaf);
    table[ 851]:=initNode(     24287 , 851 ,leaf);
    table[ 852]:=initNode(     24764 , 852 ,leaf);
    table[ 853]:=initNode(     24910 , 853 ,leaf);
    table[ 854]:=initNode(     25362 , 854 ,leaf);
    table[ 855]:=initNode(     25790 , 855 ,leaf);
    table[ 856]:=initNode(     25994 , 856 ,leaf);
    table[ 857]:=initNode(     26018 , 857 ,leaf);
    table[ 858]:=initNode(     26819 , 858 ,leaf);
    table[ 859]:=initNode(     27090 , 859 ,leaf);
    table[ 860]:=initNode(     27511 , 860 ,leaf);
    table[ 861]:=initNode(     28238 , 861 ,leaf);
    table[ 862]:=initNode(     28311 , 862 ,leaf);
    table[ 863]:=initNode(     28561 , 863 ,leaf);
    table[ 864]:=initNode(     29166 , 864 ,leaf);
    table[ 865]:=initNode(     29176 , 865 ,leaf);
    table[ 866]:=initNode(     30061 , 866 ,leaf);
    table[ 867]:=initNode(     30110 , 867 ,leaf);
    table[ 868]:=initNode(     30985 , 868 ,leaf);
    table[ 869]:=initNode(     30927 , 869 ,leaf);
    table[ 870]:=initNode(     31925 , 870 ,leaf);
    table[ 871]:=initNode(     32214 , 871 ,leaf);
    table[ 872]:=initNode(     32605 , 872 ,leaf);
    table[ 873]:=initNode(     33168 , 873 ,leaf);
    table[ 874]:=initNode(     33941 , 874 ,leaf);
    table[ 875]:=initNode(     34196 , 875 ,leaf);
    table[ 876]:=initNode(     34890 , 876 ,leaf);
    table[ 877]:=initNode(     35530 , 877 ,leaf);
    table[ 878]:=initNode(     36031 , 878 ,leaf);
    table[ 879]:=initNode(     36653 , 879 ,leaf);
    table[ 880]:=initNode(     37510 , 880 ,leaf);
    table[ 881]:=initNode(     38027 , 881 ,leaf);
    table[ 882]:=initNode(     38500 , 882 ,leaf);
    table[ 883]:=initNode(     39409 , 883 ,leaf);
    table[ 884]:=initNode(     39716 , 884 ,leaf);
    table[ 885]:=initNode(     40787 , 885 ,leaf);
    table[ 886]:=initNode(     40900 , 886 ,leaf);
    table[ 887]:=initNode(     41660 , 887 ,leaf);
    table[ 888]:=initNode(     42931 , 888 ,leaf);
    table[ 889]:=initNode(     43365 , 889 ,leaf);
    table[ 890]:=initNode(     44147 , 890 ,leaf);
    table[ 891]:=initNode(     45068 , 891 ,leaf);
    table[ 892]:=initNode(     45600 , 892 ,leaf);
    table[ 893]:=initNode(     46658 , 893 ,leaf);
    table[ 894]:=initNode(     47420 , 894 ,leaf);
    table[ 895]:=initNode(     48193 , 895 ,leaf);
    table[ 896]:=initNode(     49186 , 896 ,leaf);
    table[ 897]:=initNode(     50242 , 897 ,leaf);
    table[ 898]:=initNode(     50907 , 898 ,leaf);
    table[ 899]:=initNode(     52013 , 899 ,leaf);
    table[ 900]:=initNode(     52946 , 900 ,leaf);
    table[ 901]:=initNode(     54087 , 901 ,leaf);
    table[ 902]:=initNode(     55195 , 902 ,leaf);
    table[ 903]:=initNode(     56451 , 903 ,leaf);
    table[ 904]:=initNode(     57037 , 904 ,leaf);
    table[ 905]:=initNode(     58320 , 905 ,leaf);
    table[ 906]:=initNode(     59248 , 906 ,leaf);
    table[ 907]:=initNode(     60649 , 907 ,leaf);
    table[ 908]:=initNode(     61859 , 908 ,leaf);
    table[ 909]:=initNode(     63588 , 909 ,leaf);
    table[ 910]:=initNode(     64475 , 910 ,leaf);
    table[ 911]:=initNode(     65954 , 911 ,leaf);
    table[ 912]:=initNode(     67673 , 912 ,leaf);
    table[ 913]:=initNode(     68990 , 913 ,leaf);
    table[ 914]:=initNode(     70379 , 914 ,leaf);
    table[ 915]:=initNode(     71826 , 915 ,leaf);
    table[ 916]:=initNode(     73687 , 916 ,leaf);
    table[ 917]:=initNode(     75208 , 917 ,leaf);
    table[ 918]:=initNode(     76959 , 918 ,leaf);
    table[ 919]:=initNode(     79100 , 919 ,leaf);
    table[ 920]:=initNode(     81033 , 920 ,leaf);
    table[ 921]:=initNode(     82757 , 921 ,leaf);
    table[ 922]:=initNode(     84456 , 922 ,leaf);
    table[ 923]:=initNode(     86182 , 923 ,leaf);
    table[ 924]:=initNode(    118413 , 924 ,leaf);
    table[ 925]:=initNode(     91710 , 925 ,leaf);
    table[ 926]:=initNode(     93117 , 926 ,leaf);
    table[ 927]:=initNode(     95645 , 927 ,leaf);
    table[ 928]:=initNode(     97849 , 928 ,leaf);
    table[ 929]:=initNode(    100793 , 929 ,leaf);
    table[ 930]:=initNode(    103645 , 930 ,leaf);
    table[ 931]:=initNode(    105467 , 931 ,leaf);
    table[ 932]:=initNode(    108107 , 932 ,leaf);
    table[ 933]:=initNode(    112006 , 933 ,leaf);
    table[ 934]:=initNode(    114728 , 934 ,leaf);
    table[ 935]:=initNode(    117301 , 935 ,leaf);
    table[ 936]:=initNode(    120637 , 936 ,leaf);
    table[ 937]:=initNode(    123829 , 937 ,leaf);
    table[ 938]:=initNode(    127640 , 938 ,leaf);
    table[ 939]:=initNode(    131918 , 939 ,leaf);
    table[ 940]:=initNode(    134977 , 940 ,leaf);
    table[ 941]:=initNode(    138916 , 941 ,leaf);
    table[ 942]:=initNode(    143051 , 942 ,leaf);
    table[ 943]:=initNode(    147239 , 943 ,leaf);
    table[ 944]:=initNode(    152358 , 944 ,leaf);
    table[ 945]:=initNode(    156555 , 945 ,leaf);
    table[ 946]:=initNode(    160798 , 946 ,leaf);
    table[ 947]:=initNode(    166227 , 947 ,leaf);
    table[ 948]:=initNode(    172035 , 948 ,leaf);
    table[ 949]:=initNode(    176576 , 949 ,leaf);
    table[ 950]:=initNode(    183454 , 950 ,leaf);
    table[ 951]:=initNode(    188594 , 951 ,leaf);
    table[ 952]:=initNode(    195967 , 952 ,leaf);
    table[ 953]:=initNode(    201135 , 953 ,leaf);
    table[ 954]:=initNode(    207933 , 954 ,leaf);
    table[ 955]:=initNode(    215260 , 955 ,leaf);
    table[ 956]:=initNode(    222739 , 956 ,leaf);
    table[ 957]:=initNode(    229236 , 957 ,leaf);
    table[ 958]:=initNode(    238255 , 958 ,leaf);
    table[ 959]:=initNode(    246624 , 959 ,leaf);
    table[ 960]:=initNode(    255548 , 960 ,leaf);
    table[ 961]:=initNode(    264683 , 961 ,leaf);
    table[ 962]:=initNode(    274311 , 962 ,leaf);
    table[ 963]:=initNode(    283930 , 963 ,leaf);
    table[ 964]:=initNode(    294857 , 964 ,leaf);
    table[ 965]:=initNode(    306291 , 965 ,leaf);
    table[ 966]:=initNode(    319345 , 966 ,leaf);
    table[ 967]:=initNode(    330652 , 967 ,leaf);
    table[ 968]:=initNode(    343013 , 968 ,leaf);
    table[ 969]:=initNode(    357019 , 969 ,leaf);
    table[ 970]:=initNode(    369590 , 970 ,leaf);
    table[ 971]:=initNode(    386648 , 971 ,leaf);
    table[ 972]:=initNode(    401838 , 972 ,leaf);
    table[ 973]:=initNode(    418789 , 973 ,leaf);
    table[ 974]:=initNode(    438584 , 974 ,leaf);
    table[ 975]:=initNode(    457128 , 975 ,leaf);
    table[ 976]:=initNode(    477055 , 976 ,leaf);
    table[ 977]:=initNode(    498832 , 977 ,leaf);
    table[ 978]:=initNode(    522752 , 978 ,leaf);
    table[ 979]:=initNode(    547649 , 979 ,leaf);
    table[ 980]:=initNode(    574624 , 980 ,leaf);
    table[ 981]:=initNode(    603247 , 981 ,leaf);
    table[ 982]:=initNode(    633171 , 982 ,leaf);
    table[ 983]:=initNode(    666883 , 983 ,leaf);
    table[ 984]:=initNode(    701757 , 984 ,leaf);
    table[ 985]:=initNode(    741557 , 985 ,leaf);
    table[ 986]:=initNode(    782571 , 986 ,leaf);
    table[ 987]:=initNode(    827568 , 987 ,leaf);
    table[ 988]:=initNode(    877268 , 988 ,leaf);
    table[ 989]:=initNode(    926268 , 989 ,leaf);
    table[ 990]:=initNode(    979262 , 990 ,leaf);
    table[ 991]:=initNode(   1039993 , 991 ,leaf);
    table[ 992]:=initNode(   1104262 , 992 ,leaf);
    table[ 993]:=initNode(   1171340 , 993 ,leaf);
    table[ 994]:=initNode(   1249526 , 994 ,leaf);
    table[ 995]:=initNode(   1333259 , 995 ,leaf);
    table[ 996]:=initNode(   1424857 , 996 ,leaf);
    table[ 997]:=initNode(   1526236 , 997 ,leaf);
    table[ 998]:=initNode(   1638890 , 998 ,leaf);
    table[ 999]:=initNode(   1756410 , 999 ,leaf);
    table[1000]:=initNode(   1893025 ,1000 ,leaf);
    table[1001]:=initNode(   2043558 ,1001 ,leaf);
    table[1002]:=initNode(   2213696 ,1002 ,leaf);
    table[1003]:=initNode(   2402721 ,1003 ,leaf);
    table[1004]:=initNode(   2616530 ,1004 ,leaf);
    table[1005]:=initNode(   2852019 ,1005 ,leaf);
    table[1006]:=initNode(   3120742 ,1006 ,leaf);
    table[1007]:=initNode(   3426028 ,1007 ,leaf);
    table[1008]:=initNode(   3777613 ,1008 ,leaf);
    table[1009]:=initNode(   4184002 ,1009 ,leaf);
    table[1010]:=initNode(   4670103 ,1010 ,leaf);
    table[1011]:=initNode(   5244490 ,1011 ,leaf);
    table[1012]:=initNode(   5922240 ,1012 ,leaf);
    table[1013]:=initNode(   6749027 ,1013 ,leaf);
    table[1014]:=initNode(   7786475 ,1014 ,leaf);
    table[1015]:=initNode(   9082329 ,1015 ,leaf);
    table[1016]:=initNode(  10773472 ,1016 ,leaf);
    table[1017]:=initNode(  13042181 ,1017 ,leaf);
    table[1018]:=initNode(  16192533 ,1018 ,leaf);
    table[1019]:=initNode(  20779621 ,1019 ,leaf);
    table[1020]:=initNode(  27891152 ,1020 ,leaf);
    table[1021]:=initNode(  40020139 ,1021 ,leaf);
    table[1022]:=initNode(  65216219 ,1022 ,leaf);
    table[1023]:=initNode( 173612209 ,1023 ,leaf);
    for tabFill:=1023 downto 1 do begin
      smallestIdx:=0; secondSmallestIdx:=1; swap;
      for k:=2 to tabFill do if table[k]^.count<table[secondSmallestIdx]^.count then begin
        secondSmallestIdx:=k; swap;
      end;
      mergeTrees(smallestIdx,secondSmallestIdx,tabFill);
    end;
    decodeRoot:=makeRoot;
    traverseTree('',decodeRoot);
  end;

VAR decodeRoot:P_node;
    encodeTable:T_encodeTable;

FUNCTION T_rgbPicture.loadCompressed(fileStream:TFileStream; CONST previous:P_rgbPicture):boolean;
  VAR p:longint;
      i:longint;
      entriesRead:longint=0;

      bufferStartInStream:longint;
      buffer:bitpacked array [0..8191] of boolean;
      bufferFill:longint=0;
      bufferCursor:longint=0;

  FUNCTION refillBuffer:boolean;
    begin
      bufferStartInStream:=fileStream.Position;
      bufferFill:=8*fileStream.read(buffer,1024);
      bufferCursor:=0;
      result:=bufferFill>0;
    end;

  FUNCTION decode:smallint;
    VAR node:P_node;
    begin
      node:=decodeRoot;
      while node^.kind<>leaf do begin
        if (bufferCursor>=bufferFill) and not(refillBuffer) then exit(0);
        node:=node^.child[buffer[bufferCursor]];
        inc(bufferCursor);
      end;
      inc(entriesRead);
      result:=node^.symbol;
    end;

  begin
    p:=fileStream.position;
    if previous=nil
    then for i:=0 to SYS_SIZE*SYS_SIZE-1 do pixels[i]:=decode
    else for i:=0 to SYS_SIZE*SYS_SIZE-1 do pixels[i]:=(previous^.Pixels[i]+decode) and 1023;
    result:=entriesRead=SYS_SIZE*SYS_SIZE;
    if result then begin
      mass:=0;
      p:=bufferCursor shr 3;
      if p shl 3<bufferCursor then inc(p); //Round up to full byte
      fileStream.Seek(bufferStartInStream+p,soBeginning);
    end else fileStream.Seek(p,soBeginning);
  end;

PROCEDURE T_rgbPicture.writeCompressed(fileStream:TFileStream; CONST previous:P_rgbPicture);
  VAR buffer:bitpacked array [0..8191] of boolean;
      bufferFill:longint=0;

  PROCEDURE flushBuffer;
    VAR toFlush:longint;
    begin
      toFlush:=bufferFill shr 3;
      if toFlush shl 3<bufferFill then inc(toFlush); //Round up to full byte
      if toFlush>0 then fileStream.Write(buffer,toFlush);
      bufferFill:=0;
    end;

  PROCEDURE encode(CONST symbol:SmallInt);
    VAR en:T_bitArray;
        k:longint;
    begin
      en:=encodeTable[symbol];
      for k:=0 to en.size-1 do begin
        buffer[bufferFill]:=en.bit[k];
        inc(bufferFill);
        if bufferFill>=8192 then flushBuffer;
      end;
    end;

  var i: longint;
  begin
    if previous=nil
    then for i:=0 to SYS_SIZE*SYS_SIZE-1 do encode(pixels[i])
    else for i:=0 to SYS_SIZE*SYS_SIZE-1 do encode((pixels[i]-previous^.Pixels[i]) and 1023);
    flushBuffer;
  end;

FUNCTION T_rgbPicture.equals(CONST other:P_rgbPicture):boolean;
  VAR i:longint;
      a,b:smallint;
  begin
    for i:=0 to SYS_SIZE*SYS_SIZE-1 do begin
      a:=       Pixels[i];
      b:=other^.Pixels[i];
      if a<>b then exit(false);
    end;
    result:=true;
  end;

initialization
  buildCode(decodeRoot,encodeTable);
end.


UNIT vectors;

{$mode objfpc}{$H+}

INTERFACE
TYPE
  T_2dVector=array[0..1] of double;
  T_complex =record re,im:double; end;
  T_systemState=array[0..SYS_SIZE-1,0..SYS_SIZE-1] of record mass:double; p,dp,a,da:T_2dVector; end;
  T_vectorField=array[0..SYS_SIZE-1,0..SYS_SIZE-1] of T_2dVector;

  T_arrayOfComplex=record
    fill:longint;
    dat:array[0..SYS_SIZE-1] of T_complex;
  end;
  T_scalarFieldFFT=array[0..SYS_SIZE-1,0..SYS_SIZE-1     ] of T_complex;
  T_vectorFieldFFT=array[0..1,0..SYS_SIZE-1,0..SYS_SIZE-1] of T_complex;
CONST
  epsilon=1E-10;
  zeroVec:T_2dVector=(0,0);

OPERATOR *(CONST x:T_2dVector; CONST y:double):T_2dVector;  inline;
OPERATOR +(CONST x,y:T_2dVector):T_2dVector;                  inline;
OPERATOR -(CONST x,y:T_2dVector):T_2dVector;                  inline;
FUNCTION massFFT(CONST s:T_systemState):T_scalarFieldFFT;
FUNCTION accelFFT(CONST a:T_vectorField):T_vectorFieldFFT;
OPERATOR *(CONST x:T_scalarFieldFFT; CONST y:T_vectorFieldFFT):T_vectorField;
IMPLEMENTATION
OPERATOR *(CONST x:T_2dVector; CONST y:double):T_2dVector;
  begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
  end;

OPERATOR +(CONST x,y:T_2dVector):T_2dVector;
  begin
    result[0]:=x[0]+y[0];
    result[1]:=x[1]+y[1];
  end;

OPERATOR -(CONST x,y:T_2dVector):T_2dVector;
  begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
  end;

OPERATOR +(CONST x,y:T_complex):T_complex; inline;
  begin
    result.re:=x.re+y.re;
    result.im:=x.im+y.im;
  end;

OPERATOR -(CONST x,y:T_complex):T_complex; inline;
  begin
    result.re:=x.re-y.re;
    result.im:=x.im-y.im;
  end;

OPERATOR *(CONST x,y:T_complex):T_complex; inline;
  begin
    result.re:=x.re*y.re-x.im*y.im;
    result.im:=x.re*y.im+y.re*x.im;
  end;

OPERATOR *(CONST x:T_complex; CONST y:double):T_complex; inline;
  begin
    result.re:=x.re*y;
    result.im:=x.im*y;
  end;

CONST ROOT_OF_UNITY:array[0..255] of T_complex =
  ((re: 1.0                 ; im: 0                   ),
   (re: 0.9996988186962041  ; im: 0.024541228522912288),
   (re: 0.9987954562051724  ; im: 0.049067674327418015),
   (re: 0.9972904566786902  ; im: 0.07356456359966742 ),
   (re: 0.9951847266721968  ; im: 0.0980171403295606  ),
   (re: 0.99247953459871008 ; im: 0.1224106751992162  ),
   (re: 0.989176509964781   ; im: 0.14673047445536175 ),
   (re: 0.9852776423889412  ; im: 0.17096188876030122 ),
   (re: 0.9807852804032304  ; im: 0.19509032201612825 ),
   (re: 0.97570213003852857 ; im: 0.2191012401568698  ),
   (re: 0.97003125319454409 ; im: 0.24298017990326387 ),
   (re: 0.9637760657954398  ; im: 0.26671275747489837 ),
   (re: 0.9569403357322088  ; im: 0.29028467725446233 ),
   (re: 0.94952818059303667 ; im: 0.3136817403988915  ),
   (re: 0.9415440651830208  ; im: 0.33688985339222005 ),
   (re: 0.9329927988347388  ; im: 0.3598950365349881  ),
   (re: 0.9238795325112867  ; im: 0.38268343236508978 ),
   (re: 0.91420975570353069 ; im: 0.40524131400498986 ),
   (re: 0.9039892931234433  ; im: 0.42755509343028208 ),
   (re: 0.8932243011955153  ; im: 0.44961132965460654 ),
   (re: 0.881921264348355   ; im: 0.47139673682599764 ),
   (re: 0.8700869911087113  ; im: 0.49289819222978404 ),
   (re: 0.8577286100002721  ; im: 0.51410274419322166 ),
   (re: 0.8448535652497071  ; im: 0.5349976198870971  ),
   (re: 0.8314696123025453  ; im: 0.55557023301960218 ),
   (re: 0.8175848131515837  ; im: 0.5758081914178453  ),
   (re: 0.8032075314806449  ; im: 0.59569930449243336 ),
   (re: 0.7883464276266063  ; im: 0.6152315905806268  ),
   (re: 0.773010453362737   ; im: 0.63439328416364549 ),
   (re: 0.75720884650648457 ; im: 0.65317284295377676 ),
   (re: 0.7409511253549591  ; im: 0.6715589548470183  ),
   (re: 0.724247082951467   ; im: 0.6895405447370668  ),
   (re: 0.70710678118654757 ; im: 0.70710678118654746 ),
   (re: 0.6895405447370669  ; im: 0.72424708295146678 ),
   (re: 0.6715589548470184  ; im: 0.7409511253549591  ),
   (re: 0.65317284295377687 ; im: 0.75720884650648446 ),
   (re: 0.6343932841636456  ; im: 0.77301045336273688 ),
   (re: 0.6152315905806269  ; im: 0.7883464276266062  ),
   (re: 0.59569930449243347 ; im: 0.8032075314806448  ),
   (re: 0.5758081914178454  ; im: 0.8175848131515836  ),
   (re: 0.55557023301960229 ; im: 0.8314696123025452  ),
   (re: 0.53499761988709738 ; im: 0.844853565249707   ),
   (re: 0.51410274419322177 ; im: 0.8577286100002721  ),
   (re: 0.49289819222978415 ; im: 0.8700869911087113  ),
   (re: 0.4713967368259978  ; im: 0.8819212643483549  ),
   (re: 0.4496113296546066  ; im: 0.8932243011955153  ),
   (re: 0.4275550934302822  ; im: 0.9039892931234433  ),
   (re: 0.4052413140049898  ; im: 0.91420975570353069 ),
   (re: 0.38268343236508984 ; im: 0.9238795325112867  ),
   (re: 0.35989503653498828 ; im: 0.9329927988347387  ),
   (re: 0.33688985339222005 ; im: 0.9415440651830208  ),
   (re: 0.3136817403988915  ; im: 0.94952818059303667 ),
   (re: 0.29028467725446228 ; im: 0.9569403357322089  ),
   (re: 0.2667127574748984  ; im: 0.9637760657954398  ),
   (re: 0.24298017990326398 ; im: 0.970031253194544   ),
   (re: 0.21910124015686977 ; im: 0.97570213003852857 ),
   (re: 0.1950903220161283  ; im: 0.9807852804032304  ),
   (re: 0.17096188876030136 ; im: 0.9852776423889412  ),
   (re: 0.14673047445536175 ; im: 0.989176509964781   ),
   (re: 0.12241067519921628 ; im: 0.99247953459871008 ),
   (re: 0.09801714032956077 ; im: 0.9951847266721968  ),
   (re: 0.07356456359966745 ; im: 0.9972904566786902  ),
   (re: 0.04906767432741813 ; im: 0.9987954562051724  ),
   (re: 0.024541228522912267; im: 0.9996988186962041  ),
   (re: 0                   ; im: 1.0                 ),
   (re:-0.024541228522912142; im: 0.9996988186962041  ),
   (re:-0.049067674327418008; im: 0.9987954562051724  ),
   (re:-0.07356456359966734 ; im: 0.9972904566786902  ),
   (re:-0.098017140329560659; im: 0.9951847266721968  ),
   (re:-0.12241067519921617 ; im: 0.99247953459871008 ),
   (re:-0.14673047445536164 ; im: 0.989176509964781   ),
   (re:-0.17096188876030124 ; im: 0.9852776423889412  ),
   (re:-0.19509032201612819 ; im: 0.9807852804032304  ),
   (re:-0.21910124015686966 ; im: 0.97570213003852857 ),
   (re:-0.24298017990326387 ; im: 0.97003125319454409 ),
   (re:-0.2667127574748983  ; im: 0.9637760657954398  ),
   (re:-0.2902846772544622  ; im: 0.9569403357322089  ),
   (re:-0.31368174039889146 ; im: 0.94952818059303667 ),
   (re:-0.33688985339221994 ; im: 0.9415440651830209  ),
   (re:-0.35989503653498817 ; im: 0.9329927988347388  ),
   (re:-0.3826834323650897  ; im: 0.9238795325112867  ),
   (re:-0.40524131400498975 ; im: 0.9142097557035308  ),
   (re:-0.42755509343028186 ; im: 0.9039892931234434  ),
   (re:-0.4496113296546067  ; im: 0.8932243011955153  ),
   (re:-0.4713967368259977  ; im: 0.8819212643483549  ),
   (re:-0.4928981922297839  ; im: 0.87008699110871146 ),
   (re:-0.5141027441932215  ; im: 0.8577286100002722  ),
   (re:-0.5349976198870969  ; im: 0.8448535652497072  ),
   (re:-0.555570233019602   ; im: 0.8314696123025453  ),
   (re:-0.5758081914178453  ; im: 0.8175848131515837  ),
   (re:-0.5956993044924332  ; im: 0.8032075314806449  ),
   (re:-0.6152315905806267  ; im: 0.7883464276266063  ),
   (re:-0.63439328416364527 ; im: 0.7730104533627371  ),
   (re:-0.6531728429537765  ; im: 0.75720884650648479 ),
   (re:-0.6715589548470184  ; im: 0.7409511253549591  ),
   (re:-0.6895405447370668  ; im: 0.724247082951467   ),
   (re:-0.70710678118654746 ; im: 0.70710678118654757 ),
   (re:-0.72424708295146678 ; im: 0.689540544737067   ),
   (re:-0.74095112535495888 ; im: 0.6715589548470185  ),
   (re:-0.75720884650648468 ; im: 0.6531728429537766  ),
   (re:-0.773010453362737   ; im: 0.63439328416364549 ),
   (re:-0.7883464276266062  ; im: 0.6152315905806269  ),
   (re:-0.8032075314806448  ; im: 0.59569930449243347 ),
   (re:-0.81758481315158349 ; im: 0.5758081914178454  ),
   (re:-0.8314696123025453  ; im: 0.55557023301960218 ),
   (re:-0.8448535652497071  ; im: 0.5349976198870971  ),
   (re:-0.8577286100002721  ; im: 0.51410274419322177 ),
   (re:-0.8700869911087113  ; im: 0.49289819222978415 ),
   (re:-0.8819212643483549  ; im: 0.4713967368259978  ),
   (re:-0.8932243011955152  ; im: 0.44961132965460687 ),
   (re:-0.9039892931234434  ; im: 0.42755509343028203 ),
   (re:-0.91420975570353069 ; im: 0.40524131400498986 ),
   (re:-0.9238795325112867  ; im: 0.38268343236508984 ),
   (re:-0.9329927988347387  ; im: 0.35989503653498833 ),
   (re:-0.9415440651830207  ; im: 0.33688985339222027 ),
   (re:-0.94952818059303667 ; im: 0.3136817403988914  ),
   (re:-0.9569403357322088  ; im: 0.29028467725446233 ),
   (re:-0.9637760657954398  ; im: 0.26671275747489848 ),
   (re:-0.970031253194544   ; im: 0.24298017990326404 ),
   (re:-0.97570213003852846 ; im: 0.21910124015687002 ),
   (re:-0.9807852804032303  ; im: 0.19509032201612858 ),
   (re:-0.9852776423889412  ; im: 0.17096188876030119 ),
   (re:-0.989176509964781   ; im: 0.1467304744553618  ),
   (re:-0.99247953459871008 ; im: 0.12241067519921635 ),
   (re:-0.9951847266721968  ; im: 0.09801714032956084 ),
   (re:-0.9972904566786902  ; im: 0.07356456359966774 ),
   (re:-0.9987954562051724  ; im: 0.04906767432741797 ),
   (re:-0.9996988186962041  ; im: 0.024541228522912326),
   (re:-1.0                 ; im: 0                   ),
   (re:-0.9996988186962041  ; im:-0.024541228522912083),
   (re:-0.9987954562051724  ; im:-0.049067674327417724),
   (re:-0.9972904566786902  ; im:-0.0735645635996675  ),
   (re:-0.9951847266721968  ; im:-0.09801714032956059 ),
   (re:-0.99247953459871008 ; im:-0.1224106751992161  ),
   (re:-0.989176509964781   ; im:-0.14673047445536158 ),
   (re:-0.9852776423889412  ; im:-0.17096188876030097 ),
   (re:-0.9807852804032304  ; im:-0.19509032201612836 ),
   (re:-0.97570213003852857 ; im:-0.21910124015686983 ),
   (re:-0.97003125319454409 ; im:-0.24298017990326382 ),
   (re:-0.96377606579544    ; im:-0.26671275747489825 ),
   (re:-0.9569403357322089  ; im:-0.29028467725446216 ),
   (re:-0.94952818059303679 ; im:-0.31368174039889118 ),
   (re:-0.9415440651830208  ; im:-0.3368898533922201  ),
   (re:-0.9329927988347388  ; im:-0.3598950365349881  ),
   (re:-0.9238795325112868  ; im:-0.38268343236508967 ),
   (re:-0.9142097557035308  ; im:-0.40524131400498969 ),
   (re:-0.90398929312344356 ; im:-0.4275550934302818  ),
   (re:-0.8932243011955153  ; im:-0.44961132965460665 ),
   (re:-0.881921264348355   ; im:-0.47139673682599764 ),
   (re:-0.87008699110871146 ; im:-0.4928981922297839  ),
   (re:-0.8577286100002722  ; im:-0.5141027441932215  ),
   (re:-0.8448535652497072  ; im:-0.5349976198870969  ),
   (re:-0.8314696123025453  ; im:-0.555570233019602   ),
   (re:-0.8175848131515837  ; im:-0.5758081914178453  ),
   (re:-0.8032075314806449  ; im:-0.5956993044924332  ),
   (re:-0.7883464276266063  ; im:-0.6152315905806267  ),
   (re:-0.7730104533627371  ; im:-0.63439328416364527 ),
   (re:-0.75720884650648479 ; im:-0.6531728429537765  ),
   (re:-0.7409511253549591  ; im:-0.6715589548470184  ),
   (re:-0.724247082951467   ; im:-0.6895405447370668  ),
   (re:-0.70710678118654768 ; im:-0.70710678118654746 ),
   (re:-0.68954054473706716 ; im:-0.72424708295146667 ),
   (re:-0.67155895484701866 ; im:-0.74095112535495888 ),
   (re:-0.65317284295377709 ; im:-0.7572088465064842  ),
   (re:-0.6343932841636459  ; im:-0.77301045336273666 ),
   (re:-0.61523159058062737 ; im:-0.78834642762660589 ),
   (re:-0.5956993044924332  ; im:-0.803207531480645   ),
   (re:-0.5758081914178452  ; im:-0.8175848131515837  ),
   (re:-0.55557023301960218 ; im:-0.8314696123025453  ),
   (re:-0.53499761988709726 ; im:-0.8448535652497071  ),
   (re:-0.51410274419322188 ; im:-0.857728610000272   ),
   (re:-0.49289819222978426 ; im:-0.8700869911087113  ),
   (re:-0.47139673682599786 ; im:-0.8819212643483548  ),
   (re:-0.44961132965460693 ; im:-0.8932243011955152  ),
   (re:-0.42755509343028247 ; im:-0.9039892931234432  ),
   (re:-0.40524131400499036 ; im:-0.91420975570353047 ),
   (re:-0.38268343236509034 ; im:-0.9238795325112865  ),
   (re:-0.35989503653498794 ; im:-0.932992798834739   ),
   (re:-0.33688985339221994 ; im:-0.9415440651830209  ),
   (re:-0.31368174039889146 ; im:-0.94952818059303667 ),
   (re:-0.29028467725446239 ; im:-0.9569403357322088  ),
   (re:-0.26671275747489853 ; im:-0.9637760657954398  ),
   (re:-0.24298017990326412 ; im:-0.970031253194544   ),
   (re:-0.2191012401568701  ; im:-0.97570213003852846 ),
   (re:-0.19509032201612866 ; im:-0.9807852804032303  ),
   (re:-0.17096188876030172 ; im:-0.9852776423889411  ),
   (re:-0.14673047445536233 ; im:-0.9891765099647809  ),
   (re:-0.12241067519921596 ; im:-0.99247953459871008 ),
   (re:-0.09801714032956045 ; im:-0.9951847266721968  ),
   (re:-0.07356456359966735 ; im:-0.9972904566786902  ),
   (re:-0.049067674327418029; im:-0.9987954562051724  ),
   (re:-0.024541228522912389; im:-0.9996988186962041  ),
   (re: 0                   ; im:-1.0                 ),
   (re: 0.02454122852291202 ; im:-0.9996988186962041  ),
   (re: 0.049067674327417668; im:-0.9987954562051724  ),
   (re: 0.073564563599667   ; im:-0.9972904566786903  ),
   (re: 0.09801714032956009 ; im:-0.9951847266721969  ),
   (re: 0.1224106751992156  ; im:-0.99247953459871008 ),
   (re: 0.14673047445536194 ; im:-0.9891765099647809  ),
   (re: 0.17096188876030133 ; im:-0.9852776423889412  ),
   (re: 0.19509032201612828 ; im:-0.9807852804032304  ),
   (re: 0.21910124015686974 ; im:-0.97570213003852857 ),
   (re: 0.24298017990326376 ; im:-0.97003125319454409 ),
   (re: 0.2667127574748982  ; im:-0.96377606579544    ),
   (re: 0.2902846772544621  ; im:-0.9569403357322089  ),
   (re: 0.31368174039889113 ; im:-0.94952818059303679 ),
   (re: 0.3368898533922196  ; im:-0.941544065183021   ),
   (re: 0.35989503653498767 ; im:-0.93299279883473907 ),
   (re: 0.38268343236509    ; im:-0.9238795325112866  ),
   (re: 0.40524131400499    ; im:-0.91420975570353058 ),
   (re: 0.4275550934302822  ; im:-0.9039892931234433  ),
   (re: 0.4496113296546066  ; im:-0.8932243011955153  ),
   (re: 0.47139673682599759 ; im:-0.881921264348355   ),
   (re: 0.49289819222978387 ; im:-0.87008699110871157 ),
   (re: 0.5141027441932214  ; im:-0.8577286100002722  ),
   (re: 0.5349976198870969  ; im:-0.8448535652497073  ),
   (re: 0.5555702330196018  ; im:-0.83146961230254546 ),
   (re: 0.57580819141784489 ; im:-0.8175848131515839  ),
   (re: 0.5956993044924328  ; im:-0.80320753148064528 ),
   (re: 0.6152315905806269  ; im:-0.7883464276266061  ),
   (re: 0.6343932841636456  ; im:-0.77301045336273688 ),
   (re: 0.65317284295377676 ; im:-0.75720884650648457 ),
   (re: 0.6715589548470183  ; im:-0.7409511253549591  ),
   (re: 0.6895405447370668  ; im:-0.724247082951467   ),
   (re: 0.70710678118654746 ; im:-0.70710678118654768 ),
   (re: 0.72424708295146667 ; im:-0.68954054473706716 ),
   (re: 0.74095112535495888 ; im:-0.67155895484701866 ),
   (re: 0.7572088465064842  ; im:-0.65317284295377709 ),
   (re: 0.77301045336273666 ; im:-0.6343932841636459  ),
   (re: 0.78834642762660589 ; im:-0.61523159058062737 ),
   (re: 0.803207531480645   ; im:-0.5956993044924332  ),
   (re: 0.8175848131515837  ; im:-0.5758081914178452  ),
   (re: 0.8314696123025453  ; im:-0.55557023301960218 ),
   (re: 0.8448535652497071  ; im:-0.53499761988709726 ),
   (re: 0.857728610000272   ; im:-0.51410274419322188 ),
   (re: 0.8700869911087113  ; im:-0.49289819222978426 ),
   (re: 0.8819212643483548  ; im:-0.4713967368259979  ),
   (re: 0.8932243011955152  ; im:-0.449611329654607   ),
   (re: 0.9039892931234431  ; im:-0.42755509343028253 ),
   (re: 0.91420975570353047 ; im:-0.4052413140049904  ),
   (re: 0.9238795325112865  ; im:-0.38268343236509039 ),
   (re: 0.932992798834739   ; im:-0.359895036534988   ),
   (re: 0.9415440651830208  ; im:-0.33688985339222    ),
   (re: 0.94952818059303667 ; im:-0.3136817403988915  ),
   (re: 0.9569403357322088  ; im:-0.29028467725446244 ),
   (re: 0.9637760657954398  ; im:-0.26671275747489859 ),
   (re: 0.970031253194544   ; im:-0.24298017990326418 ),
   (re: 0.97570213003852846 ; im:-0.21910124015687016 ),
   (re: 0.9807852804032303  ; im:-0.19509032201612872 ),
   (re: 0.9852776423889411  ; im:-0.17096188876030177 ),
   (re: 0.9891765099647809  ; im:-0.14673047445536239 ),
   (re: 0.99247953459871008 ; im:-0.12241067519921603 ),
   (re: 0.9951847266721968  ; im:-0.09801714032956052 ),
   (re: 0.9972904566786902  ; im:-0.07356456359966742 ),
   (re: 0.9987954562051724  ; im:-0.04906767432741809 ),
   (re: 0.9996988186962041  ; im:-0.02454122852291245 ));

FUNCTION FastFourierTransform(CONST X:T_arrayOfComplex; CONST inverse:boolean):T_arrayOfComplex;
  CONST N1:longint=2;
  VAR N2:longint;
      commonFactor:double;
      innerX ,
      innerFT:array [0..1] of T_arrayOfComplex;
      i,j,k:longint;
      lut_factor:longint;
      r:T_complex;
  begin
    if X.fill=1 then exit(X);
    N2:=x.fill shr 1;
    for i:=0 to N1-1 do innerX[i].fill:=N2;
    j:=0;
    k:=0;
    for i:=0 to X.fill-1 do begin
      innerX[j].dat[k]:=X.dat[i];
      inc(j);
      if j>=N1 then begin j:=0; inc(k); end;
    end;
    for i:=0 to 1 do innerFT[i]:=FastFourierTransform(innerX[i],inverse);
    lut_factor:=256 div x.fill;
    if inverse
    then for i:=0 to N2-1 do innerFT[1].dat[i]*=ROOT_OF_UNITY[i*lut_factor]
    else for i:=0 to N2-1 do begin
      r:=ROOT_OF_UNITY[i*lut_factor];
      r.im:=-r.im;
      innerFT[1].dat[i]*=r;
    end;

    result.fill:=X.fill;
    if inverse
    then commonFactor:=1/2
    else commonFactor:=1;
    for i:=0 to N2-1 do result.dat[   i]:=(innerFT[0].dat[i]+innerFT[1].dat[i])*commonFactor;
    for i:=0 to N2-1 do result.dat[N2+i]:=(innerFT[0].dat[i]-innerFT[1].dat[i])*commonFactor;
  end;


FUNCTION massFFT(CONST s:T_systemState):T_scalarFieldFFT;
  VAR i,j:longint;
      temp:T_arrayOfComplex;
      rowWise:array[0..SYS_SIZE-1] of T_arrayOfComplex;
  begin
    for i:=0 to SYS_SIZE-1 do begin
      temp.fill:=SYS_SIZE;
      for j:=0 to SYS_SIZE-1 do with temp.dat[j] do begin
        re:=s[i,j].mass;
        im:=0
      end;
      rowWise[i]:=FastFourierTransform(temp,false);
    end;
    for j:=0 to SYS_SIZE-1 do begin
      temp.fill:=SYS_SIZE;
      for i:=0 to SYS_SIZE-1 do temp.dat[i]:=rowWise[i].dat[j];
      result[j]:=FastFourierTransform(temp,false).dat;
    end;
  end;

FUNCTION accelFFT(CONST a:T_vectorField):T_vectorFieldFFT;
  VAR i,j,k:longint;
      temp:T_arrayOfComplex;
      rowWise:array of T_arrayOfComplex;
  begin
    setLength(rowWise,SYS_SIZE);
    for k:=0 to 1 do begin
      for i:=0 to SYS_SIZE-1 do begin
        temp.fill:=SYS_SIZE;
        for j:=0 to SYS_SIZE-1 do with temp.dat[j] do begin
          re:=a[i,j,k];
          im:=0
        end;
        rowWise[i]:=FastFourierTransform(temp,false);
      end;
      for j:=0 to SYS_SIZE-1 do begin
        temp.fill:=SYS_SIZE;
        for i:=0 to SYS_SIZE-1 do temp.dat[i]:=rowWise[i].dat[j];
        result[k,j]:=FastFourierTransform(temp,false).dat;
      end;
    end;
    setLength(rowWise,0);
  end;

OPERATOR *(CONST x:T_scalarFieldFFT; CONST y:T_vectorFieldFFT):T_vectorField;
  VAR i,j,k:longint;
      temp:T_arrayOfComplex;
      rowWise:array[0..SYS_SIZE-1] of T_arrayOfComplex;
  begin
    for k:=0 to 1 do begin
      for i:=0 to SYS_SIZE-1 do begin
        temp.fill:=SYS_SIZE;
        for j:=0 to SYS_SIZE-1 do temp.dat[j]:=x[i,j]*y[k,i,j];
        rowWise[i]:=FastFourierTransform(temp,true);
      end;
      for j:=0 to SYS_SIZE-1 do begin
        temp.fill:=SYS_SIZE;
        for i:=0 to SYS_SIZE-1 do temp.dat[i]:=rowWise[i].dat[j];
        temp:=FastFourierTransform(temp,true);
        for i:=0 to SYS_SIZE-1 do result[j,i,k]:=temp.dat[i].re;
      end;
    end;
  end;


end.


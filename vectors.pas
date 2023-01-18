UNIT vectors;

{$mode objfpc}{$H+}

INTERFACE
TYPE
  T_2dVector=array[0..1] of double;
  T_systemState=array[0..SYS_SIZE-1,0..SYS_SIZE-1] of record mass:double; p,a:T_2dVector; end;
  T_vectorField=array[0..SYS_SIZE-1,0..SYS_SIZE-1] of T_2dVector;
CONST
  epsilon=1E-10;
  zeroVec:T_2dVector=(0,0);

OPERATOR *(CONST x:T_2dVector; CONST y:double):T_2dVector;  inline;
OPERATOR +(CONST x,y:T_2dVector):T_2dVector;                  inline;
OPERATOR -(CONST x,y:T_2dVector):T_2dVector;                  inline;
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

end.


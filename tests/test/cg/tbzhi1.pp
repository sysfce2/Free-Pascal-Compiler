{ %CPU=i386,x86_64 }
{ %OPT=-O3 -CpCOREAVX2 -OpCOREAVX2 }
{$R-}

program tbzhi1;

uses
  cpu;

function MaskOut(Input: LongInt; Index: Byte): LongInt; noinline;
begin
  MaskOut := Input and ((1 shl Index) - 1);
end;

const
  Inputs:  array[0..3] of LongInt = (0, LongInt($FFFFFFFF), $12345678, LongInt($87654321));
  Expected: array[0..3] of array[0..31] of LongInt =
    (
      ($00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000),
      ($00000000, $00000001, $00000003, $00000007, $0000000F, $0000001F, $0000003F, $0000007F, $000000FF, $000001FF, $000003FF, $000007FF, $00000FFF, $00001FFF, $00003FFF, $00007FFF, $0000FFFF, $0001FFFF, $0003FFFF, $0007FFFF, $000FFFFF, $001FFFFF, $003FFFFF, $007FFFFF, $00FFFFFF, $01FFFFFF, $03FFFFFF, $07FFFFFF, $0FFFFFFF, $1FFFFFFF, $3FFFFFFF, $7FFFFFFF),
      ($00000000, $00000000, $00000000, $00000000, $00000008, $00000018, $00000038, $00000078, $00000078, $00000078, $00000278, $00000678, $00000678, $00001678, $00001678, $00005678, $00005678, $00005678, $00005678, $00045678, $00045678, $00145678, $00345678, $00345678, $00345678, $00345678, $02345678, $02345678, $02345678, $12345678, $12345678, $12345678),
      ($00000000, $00000001, $00000001, $00000001, $00000001, $00000001, $00000021, $00000021, $00000021, $00000121, $00000321, $00000321, $00000321, $00000321, $00000321, $00004321, $00004321, $00014321, $00014321, $00054321, $00054321, $00054321, $00254321, $00654321, $00654321, $01654321, $03654321, $07654321, $07654321, $07654321, $07654321, $07654321)
    );

var
  X: Byte;
  Y: Integer;
  Output: LongInt;
begin
  if avx2support then
    begin
      for Y := Low(Inputs) to High(Inputs) do
	for X := 0 to 31 do
	  begin
	    Output := MaskOut(Inputs[Y], X);
	    if Output <> Expected[Y][X] then
	      begin
		WriteLn('FAIL: $', HexStr(Inputs[Y], 8), ' and ((1 shl ', X, ') - 1) returned $', HexStr(Output, 8), '; expected $', HexStr(Expected[Y][X], 8));
		Halt(1);
	      end;
	  end;

      WriteLn('ok');
    end
  else
    writeln('CPU does not support AVX2 extension');
end.

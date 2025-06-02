unit USIZSizes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MANHEIGHTS: array [0..5] of String =
    ('', '146-152', '158-164', '170-176', '182-188', '194-200');
  CLOTHES: array [0..7] of String =
    ('', '40-42', '44-46', '48-50', '52-54', '56-58', '60-62', '64-66');
  SHOES: array [0..18] of String =
    ('',   '33', '34', '35', '36', '37', '38', '39', '40', '41', '42', '43',
     '44', '45', '46', '47', '48', '49', '50');
  HEADDRESS: array [0..21] of String =
    ('',   '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '60',
     '61', '62', '63', '64', '65', '66', '67', '68', '69', '70');
  MITTENS: array [0..7] of String =
    ('', '6', '7', '8', '9', '10', '11', '12');
  GASMASK: array [0..5] of String =
    ('', '0', '1', '2', '3', '4');
  RESPIRATOR: array [0..3] of String =
    ('', '1', '2', '3');

type

  TSIZStaffSizeIndexes = record
    Clothes,
    Height,
    Shoes,
    HeadDress,
    Mittens,
    Gloves,
    Gasmask,
    Respirator: Integer;
  end;
  procedure SIZStaffSizeIndexesClear(var AStaffSizes: TSIZStaffSizeIndexes);
  procedure SIZStaffSizesForSizeType(const AStaffSizes: TSIZStaffSizeIndexes;
                                     const ASizeType: Integer;
                                     out ASizeID, AHeightID: Integer);
type

  TSIZStaffSizes = record
    Clothes,
    Height,
    Shoes,
    HeadDress,
    Mittens,
    Gloves,
    Gasmask,
    Respirator: String;
  end;
  procedure SIZStaffSizesClear(var AStaffSizes: TSIZStaffSizes);

implementation

procedure SIZStaffSizeIndexesClear(var AStaffSizes: TSIZStaffSizeIndexes);
begin
  AStaffSizes.Clothes:= 0;
  AStaffSizes.Height:= 0;
  AStaffSizes.Shoes:= 0;
  AStaffSizes.HeadDress:= 0;
  AStaffSizes.Mittens:= 0;
  AStaffSizes.Gloves:= 0;
  AStaffSizes.Gasmask:= 0;
  AStaffSizes.Respirator:= 0;
end;

procedure SIZStaffSizesForSizeType(const AStaffSizes: TSIZStaffSizeIndexes;
                                   const ASizeType: Integer;
                                   out ASizeID, AHeightID: Integer);
begin
  ASizeID:= 0;
  AHeightID:= 0;
  if ASizeType=0 then Exit;
  case ASizeType of
  1: begin
       ASizeID:= AStaffSizes.Clothes;
       AHeightID:= AStaffSizes.Height;
     end;
  2: ASizeID:= AStaffSizes.Shoes;
  3: ASizeID:= AStaffSizes.HeadDress;
  4: ASizeID:= AStaffSizes.Mittens;
  5: ASizeID:= AStaffSizes.Gloves;
  6: ASizeID:= AStaffSizes.Gasmask;
  7: ASizeID:= AStaffSizes.Respirator;
  end;
end;

procedure SIZStaffSizesClear(var AStaffSizes: TSIZStaffSizes);
begin
  AStaffSizes.Clothes:= EmptyStr;
  AStaffSizes.Height:= EmptyStr;
  AStaffSizes.Shoes:= EmptyStr;
  AStaffSizes.HeadDress:= EmptyStr;
  AStaffSizes.Mittens:= EmptyStr;
  AStaffSizes.Gloves:= EmptyStr;
  AStaffSizes.Gasmask:= EmptyStr;
  AStaffSizes.Respirator:= EmptyStr;
end;

end.


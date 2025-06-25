unit USIZSizes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  PERSONHEIGHTS: array of String =
    ('', '146-152', '158-164', '170-176', '182-188', '194-200');
  CLOTHES: array of String =
    ('', '40-42', '44-46', '48-50', '52-54', '56-58', '60-62', '64-66');
  SHOES: array of String =
    ('',   '33', '34', '35', '36', '37', '38', '39', '40', '41', '42', '43',
     '44', '45', '46', '47', '48', '49', '50');
  HEADDRESS: array of String =
    ('',   '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '60',
     '61', '62', '63', '64', '65', '66', '67', '68', '69', '70');
  HANDS: array of String =
    ('', '6', '7', '8', '9', '10', '11', '12');
  GASMASKS: array of String =
    ('', '0', '1', '2', '3', '4');
  RESPIRATORS: array of String =
    ('', '1', '2', '3');

type

  TSIZStaffSizeIndexes = record
    Clothes,
    Height,
    Shoes,
    Head,
    Hand,
    Gasmask,
    Respirator: Integer;
  end;
  procedure SIZStaffSizeIndexesClear(var AStaffSizes: TSIZStaffSizeIndexes);
  procedure SIZStaffSizeIndexesSet(var AStaffSizes: TSIZStaffSizeIndexes;
                                   const AClothes, AHeight, AShoes, AHead,
                                         AHand, AGasmask, ARespirator: Integer);
  procedure SIZStaffSizesForSizeType(const AStaffSizes: TSIZStaffSizeIndexes;
                                     const ASizeType: Integer;
                                     out ASizeID, AHeightID: Integer);


implementation

procedure SIZStaffSizeIndexesClear(var AStaffSizes: TSIZStaffSizeIndexes);
begin
  AStaffSizes.Clothes:= 0;
  AStaffSizes.Height:= 0;
  AStaffSizes.Shoes:= 0;
  AStaffSizes.Head:= 0;
  AStaffSizes.Hand:= 0;
  AStaffSizes.Gasmask:= 0;
  AStaffSizes.Respirator:= 0;
end;

procedure SIZStaffSizeIndexesSet(var AStaffSizes: TSIZStaffSizeIndexes;
                                   const AClothes, AHeight, AShoes, AHead,
                                         AHand, AGasmask, ARespirator: Integer);
begin
  AStaffSizes.Height:= AHeight;
  AStaffSizes.Clothes:= AClothes;
  AStaffSizes.Shoes:= AShoes;
  AStaffSizes.Head:= AHead;
  AStaffSizes.Hand:= AHand;
  AStaffSizes.Gasmask:= AGasmask;
  AStaffSizes.Respirator:= ARespirator;
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
  3: ASizeID:= AStaffSizes.Head;
  4: ASizeID:= AStaffSizes.Hand;
  5: ASizeID:= AStaffSizes.Gasmask;
  6: ASizeID:= AStaffSizes.Respirator;
  end;
end;

end.


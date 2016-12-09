(* Wolfram Language Package *)
(* :Name: Netmath299`Utilities` *)
(* :Author: Leytzher Muro, 2016 *)
(* :Summary: This package provides basic utilities to estimate polygon sides and internal angles *)

BeginPackage["Netmath299`Utilities`"]
(* Exported symbols added here with SymbolName::usage *)

GetCoordinates::usage = "Convert a complex number z = x + Iy to a pair of coordinates {x,y}"

PolygonCoordinates::usage = "Create a closed polygon from a set of points ordered in counterclockwise manner"

PlotPolygon::usage = "Plot polygon from list of coordinates"

CalculateAngle::usage = "Calculate angle in radians given 3 vertices A,B,C. The angle calculated is between vector AB and BC"

CalculateVectorMagnitude::usage = "Calculate length of the vector defined by 2 vertices"

PolygonSides::usage = "Given a list of polygon vertices returns a list of the length of the polygon sides"

CalculateCrossProduct::usage = "Calculate the cross product of 2 vectors defined by the vertices a,b,c"

PolygonAngles::usage = "Calculate interior angles of a polygon. It needs a list of vertices"

(* Function definition *)
(***********************)


Begin["`Private`"] (* Begin Private Context *)

GetCoordinates[z_] = {Re[z], Im[z]};

PolygonCoordinates[z_List] :=
    Module[ {dupgone, polygonCoordinates},
(* Eliminate duplicates from the list *)
        dupgone = DeleteDuplicates[z];


        (* Get coordinates in x,y format *)
        polygonCoordinates = GetCoordinates[#]&/@dupgone
    ];

PlotPolygon[polygonCoord_List] :=
    Module[ {closedp},
    (* close polygon *)
        closedp = Append[polygonCoord,First[polygonCoord]];
        ListLinePlot[closedp]
    ];


CalculateAngle[{vertex1_,vertex2_,vertex3_}] :=
    Module[ {vector1,vector2,magnitudeVector1,magnitudeVector2, angle},
        vector1 = vertex2-vertex1;
        vector2 = vertex3-vertex2;
        magnitudeVector1 = Sqrt[vector1.vector1];
        magnitudeVector2 = Sqrt[vector2.vector2];
        angle = ArcCos[Dot[vector1,vector2]/(magnitudeVector1 magnitudeVector2)]
    ];

CalculateVectorMagnitude[{vertex1_, vertex2_}] :=
    Sqrt[(vertex2 - vertex1).(vertex2 - vertex1)];

CalculateCrossProduct[{vertex1_, vertex2_, vertex3_}] := 
 Module[{vector1, vector2, crossProduct},
  vector1 = vertex2 - vertex1;
  vector2 = vertex3 - vertex2;
  crossProduct = Cross[vector1, vector2]];
 
PolygonSides[polygon_] := Module[{sideList},
   sideList = {};
   Do[
    AppendTo[sideList, 
     CalculateVectorMagnitude[Take[RotateLeft[polygon, n], 2]]], {n, 
     0, Length[polygon] - 1}];
   sideList];

PolygonAngles[polygon_] := 
  Module[{anglesFromCos, vertices3D, crossProducts, signs, 
    signedAnglesFromCos, sumOfAngles, correctedAngles},
   anglesFromCos = {};
   crossProducts = {};
   
   Do[
    AppendTo[anglesFromCos, 
     CalculateAngle[Take[RotateLeft[polygon, n], 3]]], {n, 0, 
     Length[polygon] - 1}];
   (* convert vertices to 3D points *)
   vertices3D = Append[#, 0] & /@ polygon;
   (* now try cross products *)
   Do[
    AppendTo[crossProducts, 
     CalculateCrossProduct[Take[RotateLeft[vertices3D, n], 3]]], {n, 
     0, Length[vertices3D] - 1}];
   (* get signs of the z-index *)
   signs = Sign@Last[#] & /@ crossProducts;
   (* now we use the signs to modify the values of the angles \
obtained from Cosine formula (dot product) *)
   signedAnglesFromCos = anglesFromCos* signs;
   (* calculate sum of angles *)
   sumOfAngles = Total[signedAnglesFromCos];
   correctedAngles = 
    If[sumOfAngles == 2 Pi, Pi - signedAnglesFromCos, 
     If[sumOfAngles == -2 Pi, Pi + signedAnglesFromCos, 
      Print["Error"]]];
   (* rotateRight one time to set proper angle order *)
   RotateRight[correctedAngles]
   
   ];



End[] (* End Private Context *)

EndPackage[]

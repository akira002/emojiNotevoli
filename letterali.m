(* ::Package:: *)

(* :Title: letterali *)


(* :Context: ProgrammingInMathematica`letterali` *)
(* :Author: Alessandro Cocilova, Luca Sciullo *)
(* :Summary: il nucleo del nostro progetto di MC sui letterali ed i polinomi *)
(* :Copyright:  *)
(* :Package Version *)
(* :Mathematica Version *)
(* :History:  *)
(* :Keywords: letterali, polinomi *)
(* :Sources: *)
(* :Discussion:  *)

Commutativa::usage = "Per ripassare la propriet\[AGrave] commutativa nei letterali"

Begin["Proprieta`"]
Clear[Commutativa]
Clear[x,y, w, z, a,b]
SetDirectory["/Users/alessandro/Developer/ProgettoMC"];

emoji = Range[1, 189];
Do[emoji[[i]] = Import[StringJoin["emoji/Emoji Smiley/Emoji Smiley-0",ToString[i],".png"], ImageSize->40],{i,9}]
Do[emoji[[i]] = Import[StringJoin["emoji/Emoji Smiley/Emoji Smiley-",ToString[i],".png"], ImageSize->40],{i,10, 189}]

getEmoji[a_,b_]:= Module[{num1,num2,tmp},
num1= Do[If[Equal[a,prova1[[i]] ], Return[i]],{i, 189}];
num2= Do[If[Equal[b,prova1[[i]] ], Return[i]],{i, 189}];
tmp = emoji[[num1+num2]];
Return[tmp]];

getResult[a_,b_]:=
	If[Not[IntegerQ[a]]&&Not[IntegerQ[b]],Return[getEmoji[a,b]],Return[Dynamic[a+b]]];

Commutativa[]:=DynamicModule[{x=Import["emoji/Emoji Smiley/Emoji Smiley-41.png"], y= Import["emoji/Emoji Smiley/Emoji Smiley-03.png"],
					q= Import["emoji/Emoji Smiley/Emoji Smiley-44.png", ImageSize->40], sm1 = "emoji/Emoji Smiley/Emoji Smiley-01.png", sm2 = "emoji/Emoji Smiley/Emoji Smiley-02.png",
					sm3 = "emoji/Emoji Smiley/Emoji Smiley-03.png", sm4 = "emoji/Emoji Smiley/Emoji Smiley-04.png", sm5 = "emoji/Emoji Smiley/Emoji Smiley-05.png",
					sm6 = "emoji/Emoji Smiley/Emoji Smiley-06.png", sm7 = "emoji/Emoji Smiley/Emoji Smiley-07.png",sm8 ="emoji/Emoji Smiley/Emoji Smiley-08.png", 
					sm9 = "emoji/Emoji Smiley/Emoji Smiley-09.png", sm10 = "emoji/Emoji Smiley/Emoji Smiley-10.png", k},
Panel[Column[{
	Style
	[Panel[Grid[
	{  {InputField[Dynamic[x]]+ InputField[Dynamic[y]], 
		Text["="], 
		Defer[Dynamic[x]+ Dynamic[y]],
		Text["="],
		TextCell[Dynamic[q]]}},
		 Alignment -> Right], ImageMargins -> 10], 
   DefaultOptions -> {InputField -> {ContinuousAction -> True, 
       FieldSize -> {{5, 30}, {1, Infinity}}}}
	],
	Panel[Row[{
	Panel[Row[{
		Text["seleziona primo operando: "],
		PopupMenu[Dynamic[z],{Import[sm1], Import[sm2],Import[sm3],Import[sm4],Import[sm5]}]
	}]],
	Panel[Row[{
		Text["seleziona secondo operando: "],
		PopupMenu[Dynamic[w],{Import[sm6], Import[sm7],Import[sm8],Import[sm9],Import[sm10]}]
	}]]
	}]],
	Style
	[Panel[Grid[
	{  {Defer[InputField[Dynamic[z]]+ InputField[Dynamic[w]]], 
		Text["="], 
		Dynamic[z]+ Dynamic[w],
		Text["="],
		Dynamic[k = getResult[z,w]; k]}},
		 Alignment -> Right], ImageMargins -> 10], 
   DefaultOptions -> {InputField -> {ContinuousAction -> True, 
       FieldSize -> {{5, 30}, {1, Infinity}}}}
	]
},
Alignment->Center]]]

Commutativa[]

Commutativa[]:=DynamicModule[{x,y, l},x=Blank[];
y=3;
l = {};
Deploy[Style[Panel[Grid[{{InputField[Dynamic[x]]+InputField[Dynamic[y]],Text["="],Defer[Dynamic[y]+Dynamic[x]],Text["="],(*Dynamic[MyPrint[x,Text[" + "],y,Text[" = "],y,Text[" + "],x,Text[" = "],x+y];x+y]*)Dynamic[x+y],
Dynamic@Refresh[AppendTo[l, x+y];"",TrackedSymbols:>{x,y}],
Dynamic@Panel[Column[l,Background->LightBlue,Spacings->{1,1},ItemSize->{0,0},Alignment->{Center,Center}],Background->LightBlue]
}},Alignment->Right],ImageMargins->10, ImageSize->{800,800}],DefaultOptions->{InputField->{ContinuousAction->True,FieldSize->{{5,30},{1,Infinity}}}}]]]





End[]


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)

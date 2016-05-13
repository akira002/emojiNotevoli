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
SetDirectory[NotebookDirectory[]];

emoji = Range[1, 189];
Do[emoji[[i]] = Import[StringJoin["emoji/Emoji Smiley/Emoji Smiley-0",ToString[i],".png"], ImageSize->40],{i,9}]
Do[emoji[[i]] = Import[StringJoin["emoji/Emoji Smiley/Emoji Smiley-",ToString[i],".png"], ImageSize->40],{i,10, 189}]

getEmoji[a_,b_]:= Module[{num1,num2,tmp},
num1= Do[If[Equal[a,emoji[[i]] ], Return[i]],{i, 189}];
num2= Do[If[Equal[b,emoji[[i]] ], Return[i]],{i, 189}];
tmp = emoji[[num1+num2]];
Return[tmp]];

getResult[a_,b_]:=
	If[ImageQ[a]&&ImageQ[b],Return[getEmoji[a,b]],Return[Dynamic[a+b]]];

Commutativa[]:=DynamicModule[{x = emoji[[1]], y = emoji[[2]], q= emoji[[3]], k, mylist},
mylist = {};
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
		PopupMenu[Dynamic[z],{emoji[[1]], emoji[[2]], emoji[[3]], emoji[[4]], emoji[[5]], emoji[[6]], emoji[[7]], emoji[[8]], emoji[[9]], emoji[[10]] }]
	}]],
	Panel[Row[{
		Text["seleziona secondo operando: "],
		PopupMenu[Dynamic[w],{emoji[[11]], emoji[[12]], emoji[[13]], emoji[[14]], emoji[[15]], emoji[[16]], emoji[[17]], emoji[[18]], emoji[[19]], emoji[[20]] }]
	}]]
	}]],
	Style
	[Panel[Grid[
	{  {Defer[InputField[Dynamic[z]]+ InputField[Dynamic[w]]],
		Text["="],
		Dynamic[z]+ Dynamic[w],
		Text["="],
		Dynamic[k = getResult[z,w]; k], 
Dynamic@Refresh[AppendTo[mylist, w+z];"",TrackedSymbols:>{w,z}],
Dynamic@Panel[Column[mylist,Background->LightBlue,Spacings->{1,1},ItemSize->{0,0},Alignment->{Center,Center}],Background->LightBlue]}},
		 Alignment -> Right], ImageMargins -> 10],
   DefaultOptions -> {InputField -> {ContinuousAction -> True,
       FieldSize -> {{5, 30}, {1, Infinity}}
}}
	]
},
Alignment->Center]]]

Commutativa[]

CommutativaL[]:=DynamicModule[{x,y, l},x=Blank[];
y=3;
l = {};
Deploy[
	Style[Panel[Grid[{{InputField[Dynamic[x]]+InputField[Dynamic[y]],Text["="],Defer[Dynamic[y]+Dynamic[x]],Text["="],Dynamic[x+y],
Dynamic@Refresh[AppendTo[l, x+y];"",TrackedSymbols:>{x,y}],
Dynamic@Panel[Column[l,Background->LightBlue,Spacings->{1,1},ItemSize->{0,0},Alignment->{Center,Center}],Background->LightBlue]
}},Alignment->Right],ImageMargins->10, ImageSize->{800,800}],DefaultOptions->{InputField->{ContinuousAction->True,FieldSize->{{5,30},{1,Infinity}}}}]
	]
]





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

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
neutroSomm = Import["emoji/Emoji Nature/Emoji Natur-11.png"];
neutroProd = Import["emoji/Emoji Nature/Emoji Natur-20.png"];


getEmoji[a_]:= Module[ {num1},
If[Equal[neutroProd, a], Return[1]];
If[Equal[neutroSomm, a], Return[0]];
num1= Do[If[Equal[a,emoji[[i]] ], Return[i]],{i, 189}];
Return[num1]];

getResult[commsomm, a_,b_]:=
	If[ImageQ[a]&&ImageQ[b],Return[emoji[[getEmoji[a]+getEmoji[b] ]]],Return[Dynamic[a+b]]];
getResult[commprod, a_,b_]:=
	If[ImageQ[a]&&ImageQ[b],Return[emoji[[ Mod[getEmoji[a]*getEmoji[b], 189] ]]],Return[Dynamic[a*b]]];
getResult[distrprod, a_,b_,c_]:=
	If[ImageQ[a]&&ImageQ[b]&&ImageQ[c],Return[emoji[[ Mod[getEmoji[a]*(getEmoji[b]+getEmoji[c]), 189] ]]],Return[Dynamic[a*(b+c)]]];

CommutativaSomma[]:=DynamicModule[{x, y, k, mylist},
mylist = {};
x = 1;
y = 2;
panelSizex = 900;
panelSizey = 600;
Panel[

Style[
Grid[{  
{
Grid[{ 
{Row[{Defer[InputField[Dynamic[x]]+ InputField[Dynamic[y]]],
		Text[" = "],
		Dynamic[y],
		Text[" + "],
		Dynamic[x],
		Text[" = "],
		Dynamic[k = getResult[commsomm, x,y]; k], 
Dynamic@Refresh[AppendTo[mylist, Row[{x, Text[" + "], y, Text[" = "], y, Text[" + "], x, Text[" = "], k}]];" ",TrackedSymbols:>{x,y}]
}]},
{
	Row[{
		(*Text["seleziona primo operando: "],*)
		PopupMenu[Dynamic[x],{neutroSomm, neutroProd, emoji[[2]], emoji[[3]], emoji[[4]], emoji[[5]], emoji[[6]], emoji[[7]], emoji[[8]], emoji[[9]], emoji[[10]] }],
		 " ",
	
		(*Text["seleziona secondo operando: "],*)
		PopupMenu[Dynamic[y],{emoji[[11]], emoji[[12]], emoji[[13]], emoji[[14]], emoji[[15]], emoji[[16]], emoji[[17]], emoji[[18]], emoji[[19]], emoji[[20]] }]
	}]
	
}
}, ItemSize->{60}],
Dynamic@Panel[Column[mylist,Background->LightBlue,Spacings->{1,1},ItemSize->{0,0}, Alignment->{Center,Center} ],Background->LightBlue]} 


}],
   DefaultOptions -> {InputField -> {ContinuousAction -> True,
       FieldSize -> {{5, 30}, {1, Infinity}}
}}
	], ImageSize ->{panelSizex,panelSizex}
]]


CommutativaProdotto[]:=DynamicModule[{x, y, k, mylist},
mylist = {};
x = 1;
y = 2;
panelSizex = 900;
panelSizey = 600;
Panel[

Style[
Grid[{  
{
Grid[{ 
{Row[{Defer[InputField[Dynamic[x]]* InputField[Dynamic[y]]],
		Text[" = "],
		Dynamic[y],
		Text[" * "],
		Dynamic[x],
		Text[" = "],
		Dynamic[k = getResult[commprod, x,y]; k], 
Dynamic@Refresh[AppendTo[mylist, Row[{x, Text[" * "], y, Text[" = "], y, Text[" * "], x, Text[" = "], k}]];" ",TrackedSymbols:>{x,y}]
}]},
{
	Row[{
		(*Text["seleziona primo operando: "],*)
		PopupMenu[Dynamic[x],{neutroSomm, neutroMolt, emoji[[2]], emoji[[3]], emoji[[4]], emoji[[5]], emoji[[6]], emoji[[7]], emoji[[8]], emoji[[9]], emoji[[10]] }],
		 " ",
	
		(*Text["seleziona secondo operando: "],*)
		PopupMenu[Dynamic[y],{emoji[[11]], emoji[[12]], emoji[[13]], emoji[[14]], emoji[[15]], emoji[[16]], emoji[[17]], emoji[[18]], emoji[[19]], emoji[[20]] }]
	}]
	
}
}, ItemSize->{60}],
Dynamic@Panel[Column[mylist,Background->LightBlue,Spacings->{1,1},ItemSize->{0,0}, Alignment->{Center,Center} ],Background->LightBlue]} 


}],
   DefaultOptions -> {InputField -> {ContinuousAction -> True,
       FieldSize -> {{5, 30}, {1, Infinity}}
}}
	], ImageSize ->{panelSizex,panelSizex}
]]

DistributivaProdotto[]:=DynamicModule[{x, y, z, k, mylist},
mylist = {};
x = 1;
y = 2;
z = 3;
panelSizex = 1200;
panelSizey = 600;
Panel[

Style[
Grid[{  
{
Grid[{ 
{Row[{InputField[Dynamic[x]] , Text["* ("], InputField[Dynamic[y]]+InputField[Dynamic[z]], Text[")"] ,
		Text[" = "],
		"(",  Dynamic[x], " * ", Dynamic[y], ")", " + ", "(",  Dynamic[x], " * ", Dynamic[z], ")"
		Text[" = "],
		Dynamic[k = getResult[distrprod, x, y, z]; k], 
Dynamic@Refresh[AppendTo[mylist, Row[{ x, Text[" * ("], y, Text[" + "], z ,Text[") = ("], x, Text[" * "], y, Text[") + ("], x, Text[" * "], z, ,Text[") = "], k }]];" ",TrackedSymbols:>{x,y,z}]
}]},
{
	Row[{
		(*Text["seleziona primo operando: "],*)
		PopupMenu[Dynamic[x],{neutroSomm, neutroProd, emoji[[2]], emoji[[3]], emoji[[4]], emoji[[5]], emoji[[6]], emoji[[7]], emoji[[8]], emoji[[9]], emoji[[10]] }],
		 " ",
	
		(*Text["seleziona secondo operando: "],*)
		PopupMenu[Dynamic[y],{emoji[[11]], emoji[[12]], emoji[[13]], emoji[[14]], emoji[[15]], emoji[[16]], emoji[[17]], emoji[[18]], emoji[[19]], emoji[[20]] }],
		" ", 
		PopupMenu[Dynamic[z],{emoji[[21]], emoji[[22]], emoji[[23]], emoji[[24]], emoji[[25]], emoji[[26]], emoji[[27]], emoji[[28]], emoji[[29]], emoji[[3
0]] }]
	}]
	
}
}, ItemSize->{60}],
Dynamic@Panel[Column[mylist,Background->LightBlue,Spacings->{1,1},ItemSize->{0,0}, Alignment->{Center,Center} ],Background->LightBlue]} 


}],
   DefaultOptions -> {InputField -> {ContinuousAction -> True,
       FieldSize -> {{5, 30}, {1, Infinity}}
}}
	], ImageSize ->{panelSizex,panelSizex}
]]


DistributivaProdotto[]




(* ::InheritFromParent:: *)
(**)


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

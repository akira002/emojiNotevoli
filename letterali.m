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


Begin["Proprieta`"];
panelSizex = 1200;
panelSizey = 600;

SetDirectory[NotebookDirectory[]];

emoji = Range[0, 189];
Do[emoji[[i]] = Import[StringJoin["emoji/Emoji Smiley/Emoji Smiley-0",ToString[i-1],".png"], ImageSize->40],{i,10}]
Do[emoji[[i]] = Import[StringJoin["emoji/Emoji Smiley/Emoji Smiley-",ToString[i-1],".png"], ImageSize->40],{i,11, 190}]


getEmoji[a_]:= Module[ {num1},
num1= Do[If[Equal[a,emoji[[i]] ], Return[i]],{i, 189}];
(*Perch\[EAcute] gli smiley sono memorizzati nelle celle 1-190 del vettore, ma corrispondono ai numeri 0-189*)
Return[num1-1]];
(*proteggere variabili AAAAAAAAAAAA*)
(*per le stesse ragioni del -1 sopra, qui faccio +1*)
getResult[commsomm, a_,b_]:=
	If[ImageQ[a]&&ImageQ[b],Return[emoji[[ (getEmoji[a]+getEmoji[b])+1 ]]],Return[Dynamic[a+b]]];
getResult[commprod, a_,b_]:=
	If[ImageQ[a]&&ImageQ[b],Return[emoji[[ Mod[(getEmoji[a]*getEmoji[b])+1, 190] ]]],Return[Dynamic[a*b]]];
getResult[distrprod, a_,b_,c_]:=
	If[ImageQ[a]&&ImageQ[b]&&ImageQ[c],Return[emoji[[ Mod[(getEmoji[a]*(getEmoji[b]+getEmoji[c]))+1, 190] ]]],Return[Dynamic[a*(b+c)]]];
getResult[assocprod, a_,b_,c_]:=
	If[ImageQ[a]&&ImageQ[b]&&ImageQ[c],Return[emoji[[ Mod[(getEmoji[a]*getEmoji[b]*getEmoji[c])+1, 190] ]]],Return[Dynamic[a*b*c]]];
getResult[assocsomm, a_,b_,c_]:=
	If[ImageQ[a]&&ImageQ[b]&&ImageQ[c],Return[emoji[[ Mod[(getEmoji[a]+getEmoji[b]+getEmoji[c])+1, 190] ]]],Return[Dynamic[a+b+c]]];
getResult[prodpotb, a_,b_,c_]:=
	If[ImageQ[a],Return[emoji[[ Mod[((getEmoji[a]^b)*(getEmoji[a]^c))+1, 190] ]]],Return[Dynamic[(a^b)*(a^c)]]];
getResult[divpotb, a_,b_,c_]:=
	If[ImageQ[a],
			Return[emoji[[ Mod[((getEmoji[a]^b)/(getEmoji[a]^c))+1, 190] ]]],
			Return[Dynamic[(a^b)/(a^c)]]
	];
getResult[potpot, a_,b_,c_]:=
	If[ImageQ[a],Return[emoji[[ Mod[((getEmoji[a]^(b*c)))+1, 190] ]] ],Return[Dynamic[a^(b*c)]]];
getResult[prodpotesp, a_,b_,c_]:=
	If[ImageQ[a]&&ImageQ[b],Return[ emoji[[ Mod[((getEmoji[a]*getEmoji[b])^c)+1, 190] ]] ],Return[Dynamic[(a*b)^c]] ];
getResult[divpotesp, a_,b_,c_]:=
	If[ImageQ[a]&&ImageQ[b],Return[ emoji[[ Mod[((getEmoji[a]/getEmoji[b])^c)+1, 190] ]] ],Return[Dynamic[(a/b)^c]] ];

(*All next functions use Dynamic Module and Dynamic Mathematica's functions. The idea is to dynamically update a field for future computation, 
always showing the same math property. For istance, if we want to show how commutative property works for addition, we use just a field for 
input that can accept different kind of inputs, each time computing the property ad each time refreshing the result.
 DynamicModule[{x,y,\[Ellipsis]},expr] (https://reference.wolfram.com/language/ref/DynamicModule.html?q=DynamicModule) represents an object which maintains 
the same local instance of the symbols x, y, \[Ellipsis] in the course of all evaluations of Dynamic objects in expr. 
Symbols specified in a DynamicModule will by default have their values maintained even across Wolfram System sessions.
Dynamic[expr] (https://reference.wolfram.com/language/ref/Dynamic.html?q=Dynamic) represents an object that displays as the dynamically updated current value of expr. 
If the displayed form of Dynamic[expr] is interactively changed or edited, an assignment is done to give expr the new value val that corresponds to the displayed form. 
*)


(*CommutativaSomma is more or less a template for all the other functions in this package. The idea is to dynamically take an input, i.e., a
 integer value, an emoji or a letteral value. Once we have the input, we show and compute the commutative property for addition. 
We store the entire string showing the all computation inside a list, that is printed inside a blue panel on the left of our input fields.
Dynamically the list is updated and so modified in our panel, thanking Mathematica's Refresh function (https://reference.wolfram.com/language/ref/Refresh.html).
Refresh[expr,opts] represents an object whose value in a Dynamic should be refreshed at times specified by the options opts.

*)
CommutativaSomma[]:=DynamicModule[{x, y, k, mylist},
mylist = {};
x = 1;
y = 2;

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
		PopupMenu[Dynamic[x],{ emoji[[1]], emoji[[2]], emoji[[3]], emoji[[4]], emoji[[5]], emoji[[6]], emoji[[7]], emoji[[8]], emoji[[9]], emoji[[10]], emoji[[11]] }],
		 " ",
		PopupMenu[Dynamic[y],{emoji[[12]], emoji[[13]], emoji[[14]], emoji[[15]], emoji[[16]], emoji[[17]], emoji[[18]], emoji[[19]], emoji[[20]], emoji[[21]] }]
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

Panel[

Style[
Grid[{  
{
Grid[{ 
{Row[{InputField[Dynamic[x]],Text["*"], InputField[Dynamic[y]],
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
		PopupMenu[Dynamic[x],{emoji[[1]],emoji[[2]],emoji[[3]],emoji[[4]],emoji[[5]],emoji[[6]],emoji[[7]],emoji[[8]],emoji[[9]],emoji[[10]],emoji[[11]]} ],
		 " ",
		PopupMenu[Dynamic[y],{emoji[[12]], emoji[[13]], emoji[[14]], emoji[[15]], emoji[[16]], emoji[[17]], emoji[[18]], emoji[[19]], emoji[[20]], emoji[[21]] }],
		
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
		PopupMenu[Dynamic[x],{emoji[[1]],emoji[[2]],emoji[[3]],emoji[[4]],emoji[[5]],emoji[[6]],emoji[[7]],emoji[[8]],emoji[[9]],emoji[[10]],emoji[[11]]} ],
		 " ",
		PopupMenu[Dynamic[y],{emoji[[12]], emoji[[13]], emoji[[14]], emoji[[15]], emoji[[16]], emoji[[17]], emoji[[18]], emoji[[19]], emoji[[20]], emoji[[21]] }],
		" ", 
		PopupMenu[Dynamic[z],{emoji[[22]], emoji[[23]], emoji[[24]], emoji[[25]], emoji[[26]], emoji[[27]], emoji[[28]], emoji[[29]], emoji[[30]], emoji[[31]] }]
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

AssociativaProdotto[]:=DynamicModule[{x, y, z, k, mylist},
mylist = {};
x = 1;
y = 2;
z = 3;

Panel[

Style[
Grid[{  
{
Grid[{ 
{Row[{"(",InputField[Dynamic[x]] , " * ", InputField[Dynamic[y]],") *",InputField[Dynamic[z]] ,
		Text[" = "],
		Dynamic[x], " * (", Dynamic[y], " * ", Dynamic[z], ")"
		Text[" = "],
		Dynamic[k = getResult[assocprod, x, y, z]; k], 
Dynamic@Refresh[AppendTo[mylist, Row[{ Text["("], x, Text[" * "], y, Text[") * "], z ,Text[" = "], x, Text[" * ("], y, Text[" * "], z,Text[") = "], k }]];" ",TrackedSymbols:>{x,y,z}]
}]},
{
	Row[{
		PopupMenu[Dynamic[x],{emoji[[1]],emoji[[2]],emoji[[3]],emoji[[4]],emoji[[5]],emoji[[6]],emoji[[7]],emoji[[8]],emoji[[9]],emoji[[10]],emoji[[11]]} ],
		 " ",
		PopupMenu[Dynamic[y],{emoji[[12]], emoji[[13]], emoji[[14]], emoji[[15]], emoji[[16]], emoji[[17]], emoji[[18]], emoji[[19]], emoji[[20]], emoji[[21]] }],
		" ", 
		PopupMenu[Dynamic[z],{emoji[[22]], emoji[[23]], emoji[[24]], emoji[[25]], emoji[[26]], emoji[[27]], emoji[[28]], emoji[[29]], emoji[[30]], emoji[[31]] }]
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

AssociativaSomma[]:=DynamicModule[{x, y, z, k, mylist},
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
{Row[{"(",InputField[Dynamic[x]] , " + ", InputField[Dynamic[y]],") +",InputField[Dynamic[z]] ,
		Text[" = "],
		Dynamic[x], " + (", Dynamic[y], " + ", Dynamic[z], ")"
		Text[" = "],
		Dynamic[k = getResult[assocsomm, x, y, z]; k], 
Dynamic@Refresh[AppendTo[mylist, Row[{ Text["("], x, Text[" + "], y, Text[") + "], z ,Text[" = "], x, Text[" + ("], y, Text[" + "], z,Text[") = "], k }]];" ",TrackedSymbols:>{x,y,z}]
}]},
{
	Row[{
		PopupMenu[Dynamic[x],{emoji[[1]],emoji[[2]],emoji[[3]],emoji[[4]],emoji[[5]],emoji[[6]],emoji[[7]],emoji[[8]],emoji[[9]],emoji[[10]],emoji[[11]]} ],
		 " ",
		PopupMenu[Dynamic[y],{emoji[[12]], emoji[[13]], emoji[[14]], emoji[[15]], emoji[[16]], emoji[[17]], emoji[[18]], emoji[[19]], emoji[[20]], emoji[[21]] }],
		" ", 
		PopupMenu[Dynamic[z],{emoji[[22]], emoji[[23]], emoji[[24]], emoji[[25]], emoji[[26]], emoji[[27]], emoji[[28]], emoji[[29]], emoji[[30]], emoji[[31]] }]
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


ProdottoPotenze[]:=DynamicModule[{x,k,powerx,powery,mylist},mylist={};
x=2;
powerx=1;
powery=1;

Panel[Style[
Grid[{
{Grid[{{
Row[{InputField[Dynamic[x]]^InputField[Dynamic[powerx],FieldSize->2], Text[" * "], InputField[Dynamic[x],Enabled->False]^InputField[Dynamic[powery],FieldSize->2],
	Text[" = "],Dynamic[x]^Dynamic[powerx],Text[" * "],Dynamic[x]^Dynamic[powery],
	Text[" = "],Dynamic[k=getResult[prodpotb,x, powerx,powery];k],
	Dynamic@Refresh[AppendTo[mylist,
		Row[{x^Row[{powerx}], Text[" * "], x^Row[{powery}], Text[" = "], x^Row[{powerx, " + ",powery}], Text[" = "], x^Row[{powerx+powery}], Text[" = "],k}]];" ",TrackedSymbols:>{x,powerx,powery}]}]},
{Row[{
	PopupMenu[Dynamic[x],{emoji[[1]],emoji[[2]],emoji[[3]],emoji[[4]],emoji[[5]],emoji[[6]],emoji[[7]],emoji[[8]],emoji[[9]],emoji[[10]],emoji[[11]]} ]}
]}},ItemSize->{60}],
Dynamic@Panel[Column[mylist,Background->LightBlue,Spacings->{1,1},ItemSize->{0,0},Alignment->{Center,Center}],Background->LightBlue]}}],
DefaultOptions->{InputField->{ContinuousAction->True,FieldSize->{{5,30},{1,Infinity}}}}],ImageSize->{panelSizex,panelSizex}]]



DivisionePotenze[]:=DynamicModule[{x,k,powerx,powery,mylist},mylist={};
x=2;
powerx=1;
powery=1;

Panel[Style[
Grid[{
{Grid[{{
Row[{InputField[Dynamic[x]]^InputField[Dynamic[powerx],FieldSize->2], Text[" / "], InputField[Dynamic[x],Enabled->False]^InputField[Dynamic[powery],FieldSize->2],
	Text[" = "],Dynamic[x]^Dynamic[powerx],Text[" / "],Dynamic[x]^Dynamic[powery],
	(*Devo prevedere un caso alternativo alla getResult, per i casi in cui b \[EGrave] pi\[UGrave] grande di c (non esiste una faccina corrispondente all'indice 1/3)*)
	(*Non posso farlo nella getresult perch\[EAcute] non si pu\[OGrave] restituire un'immagine elevata ad una potenza*)
	Text[" = "],Dynamic[k=If[powerx>=powery, getResult[divpotb,x,powerx,powery], x^(powerx-powery)];k],
	Dynamic@Refresh[AppendTo[mylist,
		Row[{x^Row[{powerx}], Text[" / "], x^Row[{powery}], Text[" = "], x^Row[{powerx, " - ",powery}], Text[" = "], x^Row[{powerx-powery}], Text[" = "],k}]
	];" ",TrackedSymbols:>{x,powerx,powery}]}]},
	{Row[{
	PopupMenu[
			Dynamic[x],{emoji[[2]],emoji[[3]],emoji[[4]],emoji[[5]],emoji[[6]],emoji[[7]],emoji[[8]],emoji[[9]],emoji[[10]],emoji[[11]]} 
		]}]}},
		ItemSize->{60}],
	Dynamic@Panel[Column[mylist,Background->LightBlue,Spacings->{1,1},ItemSize->{0,0},Alignment->{Center,Center}],Background->LightBlue]}}],
	DefaultOptions->{InputField->{ContinuousAction->True,FieldSize->{{5,30},{1,Infinity}}}}],ImageSize->{panelSizex,panelSizex}]
	]


PotenzediPotenze[]:=DynamicModule[{x,k,powerx,powery,mylist},mylist={};
x=2;
powerx=1;
powery=1;

Panel[Style[
Grid[{
{Grid[{{
Row[{InputField[Dynamic[x]]^InputField[Dynamic[powerx],FieldSize->2]^ InputField[Dynamic[powery],FieldSize->2],
	Text[" = "],Dynamic[x]^Dynamic[powerx]^Dynamic[powery],
	Text[" = "],Dynamic[k=getResult[potpot,x,powerx,powery];k],
	Dynamic@Refresh[AppendTo[mylist,
		Row[{x^Row[{Text["[ "], powerx, Text[" ]"]}]^Row[{Text["( "], powery, Text[" )"]}], Text[" = "], x^Row[{Text["( "], powerx, Text[" * "], powery, Text[" )"]}], Text[" = "],
        x^Row[{powerx*powery}], Text[" = "],k}]];" ",TrackedSymbols:>{x,powerx,powery}]}]},
	{Row[{
	PopupMenu[Dynamic[x],{emoji[[1]],emoji[[2]],emoji[[3]],emoji[[4]],emoji[[5]],emoji[[6]],emoji[[7]],emoji[[8]],emoji[[9]],emoji[[10]],emoji[[11]]} ]}]}},ItemSize->{60}],Dynamic@Panel[Column[mylist,Background->LightBlue,Spacings->{1,1},ItemSize->{0,0},Alignment->{Center,Center}],Background->LightBlue]}}],DefaultOptions->{InputField->{ContinuousAction->True,FieldSize->{{5,30},{1,Infinity}}}}],ImageSize->{panelSizex,panelSizex}]]

ProdPotEsp[]:=DynamicModule[{x,y,k,powerx,mylist},mylist={};
x=2;
y=3;
powerx=1;


Panel[Style[
Grid[{
{Grid[{{
Row[{InputField[Dynamic[x]]^InputField[Dynamic[powerx],FieldSize->2], Text[" * "], InputField[Dynamic[y]]^InputField[Dynamic[powerx],FieldSize->2, Enabled->False],
	Text[" = ("],Dynamic[x], Text[" * "], Dynamic[y], Text[" )"]^Dynamic[powerx], Text[" = "],Dynamic[k=getResult[prodpotesp,x, y, powerx];k],
	Dynamic@Refresh[AppendTo[mylist,
		Row[{x^Row[{powerx}], Text[" * "], y^Row[{Text[powerx]}], Text[" = ("],Dynamic[x], Text[" * "], Dynamic[y], Text[" )"]^Dynamic[powerx], Text[" = "], (x*y)^Row[{powerx}], Text[" = "], k}]];" ",TrackedSymbols:>{x,y,powerx}]}]},
	{Row[{
	PopupMenu[Dynamic[x],{emoji[[1]],emoji[[2]],emoji[[3]],emoji[[4]],emoji[[5]],emoji[[6]],emoji[[7]],emoji[[8]],emoji[[9]],emoji[[10]],emoji[[11]]}],
		" ", 
		PopupMenu[Dynamic[y],{emoji[[21]], emoji[[22]], emoji[[23]], emoji[[24]], emoji[[25]], emoji[[26]], emoji[[27]], emoji[[28]], emoji[[29]], emoji[[30]] }]}]}},ItemSize->{60}],Dynamic@Panel[Column[mylist,Background->LightBlue,Spacings->{1,1},ItemSize->{0,0},Alignment->{Center,Center}],Background->LightBlue]}}],DefaultOptions->{InputField->{ContinuousAction->True,FieldSize->{{5,30},{1,Infinity}}}}],ImageSize->{panelSizex,panelSizex}]]



DivPotEsp[]:=DynamicModule[{x,y,k,powerx,mylist},mylist={};
x=2;
y=3;
powerx=1;

Panel[Style[
Grid[{
{Grid[{{
Row[{InputField[Dynamic[x]]^InputField[Dynamic[powerx],FieldSize->2], Text[" / "], InputField[Dynamic[y]]^InputField[Dynamic[powerx],FieldSize->2, Enabled->False],
	Text[" = ("],Dynamic[x], Text[" / "], Dynamic[y], Text[" )"]^Dynamic[powerx], Text[" = "],
		(*Per evitare lo stesso problema di prima, dato che non esiste una faccina corrispondente a 2/5*)
		Dynamic[k=If[ImageQ[x]&&ImageQ[y], 
				If[getEmoji[x]>=getEmoji[y], getResult[divpotesp,x,y,powerx], (x/y)^(powerx)],
				getResult[divpotesp,x,y,powerx]
		];k],
	Dynamic@Refresh[AppendTo[mylist,
		Row[{x^Row[{powerx}], Text[" / "], y^Row[{Text[powerx]}], Text[" = ("],Dynamic[x], Text[" / "], Dynamic[y], Text[" )"]^Dynamic[powerx], Text[" = "], (x/y)^Row[{powerx}], Text[" = "], k}]];" ",TrackedSymbols:>{x,y,powerx}]}]},
	{Row[{
	PopupMenu[Dynamic[x],{emoji[[1]],emoji[[2]],emoji[[3]],emoji[[4]],emoji[[5]],emoji[[6]],emoji[[7]],emoji[[8]],emoji[[9]],emoji[[10]],emoji[[11]]}],
		" ", 
		PopupMenu[Dynamic[y],{emoji[[21]], emoji[[22]], emoji[[23]], emoji[[24]], emoji[[25]], emoji[[26]], emoji[[27]], emoji[[28]], emoji[[29]], emoji[[30]] }]
}]}
},ItemSize->{60}],Dynamic@Panel[Column[mylist,Background->LightBlue,Spacings->{1,1},ItemSize->{0,0},Alignment->{Center,Center}],Background->LightBlue]}}],DefaultOptions->{InputField->{ContinuousAction->True,FieldSize->{{5,30},{1,Infinity}}}}],ImageSize->{panelSizex,panelSizex}]]





(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


End[]


(* ::InheritFromParent:: *)
(**)


CommutativaSomma[]


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

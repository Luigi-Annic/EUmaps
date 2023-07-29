data center;
   length function $ 8;
   retain flag 0 xsys ysys '2' hsys '3' when 'a';
   set maps.uscenter
       (where=(fipstate(state) ne 'DC')
       drop=long lat);
   style = "'Albany AMT/bold'";
   function='label';
   text=fipstate(state);
   size=2.5;
   position='5';
   if ocean='Y' then
      do;
         position='6';
         output;
         function='move';
         flag=1;
      end;
   else if flag=1 then
      do;
         function='draw';
         size=.25;
         flag=0;
      end;
   output;
run;
/* creates a heatmap for each dummy we enter in the macro variable */
%let dummy= snus;

proc freq data= prova1;
tables &dummy*_state / nopercent norow;
ods output CrossTabFreqs= crossfreq;
run;

/* colpercent contains percentages of interest (frequency of stroke in each state)*/
data crossfreq1;
set crossfreq;
where &dummy=1;
rename _state= state;
keep _state &dummy colpercent;
run;
*ods rtf file='C:\Users\Elia Gonzato\Desktop\curiosity cup\heart\Pic\prova1.rtf';

/* Le seguenti MV sono i limiti delle categorie di prevalenza della mappa.
Vengono richiamate sia nelle etichette del format che nell'if-else,
quindi se si vuole vedere come viene la mappa con diverse specificazione
delle categorie, è sufficiente modificare queste Macro Variables */

%let a= 2; %let b= 3; %let c=4; %let d=5; %let e=6;

proc format;
value map 1= "&a.% and lower"
		  2= "&a.% - &b.%"
		  3= "&b.% - &c.%"
		  4= "&c.% - &d.%"
		  5= "&d.% - &e.%"
		  6= "&e.% and higher"
;
run;

data crossfreq1;
set crossfreq1;
if ColPercent < &a then class= 1;
else if ColPercent < &b then class= 2;
else if ColPercent < &c then class= 3;
else if ColPercent < &d then class= 4;
else if ColPercent < &e then class= 5;
else if ColPercent > &e then class= 6;
format class map.;
run;

pattern1 value=solid color= "white";
pattern2 value=solid color= "yellow";
pattern3 value=solid color= "gold";
pattern4 value=solid color= "orange";
pattern5 value=solid color= "red";
pattern6 value=solid color= "brown";

/* per tornare al pattern di default:
goptions reset=pattern;
*/

legend1 label= none;

title "Prevalence of &dummy";
proc gmap data=crossfreq1 map=maps.us;
   id state;
   choro class / annotate=center legend= legend1;
run;
quit;
title;


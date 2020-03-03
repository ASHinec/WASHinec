*-------------------------------------------------------*
*														*
*		Estimación del indicador de agua segura  		*
*														*
* Fuente: ENEMDU										*
* Año: 2016, 2019										*
* Contrucción: Juan Carlos Palacios/Fausto Jácome		*
* Consultoría Unicef-INEC calidad del agua				*
*														*
*-------------------------------------------------------*

clear all
set more off
set matsize 11000

global dir "C:\Users\Fausto Jácome\Dropbox\Proyecto Agua-Unicef"

global in "$dir\Datos\ASH"
global outR "$dir\Productos\Producto 4\3 Resultados"
*global outR "C:\Users\Fausto Jácome\Documents\Consultoría"

use "$in\ASH 2019\BDD\Originales\201903_enemdu.dta", clear
merge m:1 id_hogar using "$in\ASH 2019\BDD\Originales\rma_enemdu_mar19.dta", keep(3) 
drop _*
destring zona sector, replace

*svyset id_upm [iw=fexpper], strata(plan_muestreo) singleunit(certainty)

/*Calidad de agua*/

***Tipo de suministro***
recode vi17 (1/3 7 9=1 "tipo A") (4 8 10 12=2 "tipo B") (11 13=3 "tipo C"), gen(agua_cp1)
replace agua_cp1=1 if inlist(vi17,5,6) & inrange(vi16,1,3)
replace agua_cp1=2 if inlist(vi17,5,6) & inrange(vi16,4,7)

recode vi17 (1/7 9 12=1 "tipo A") (8 10=2 "tipo B") (11 13=3 "tipo C"), gen(agua_cp1b)

***Calidad del agua***
gen vtest=vi26==1 & inrange(ra01e_f,24,48) & ra03_f==2 & ra04_f==2
gen 	agua_cp2=vtest==1 & (ra022_f==2 | ra023_f==2)
replace agua_cp2=. if vtest==0

gen vtestb=vi21==1 & inrange(ra01e_v,20,48) & ra03_v==2 & ra04_v==2
gen 	agua_cp2b=vtestb==1 & (ra022_v==2 | ra023_v==2)
replace agua_cp2b=. if vtestb==0

***Cercanía***
gen 	agua_cp3=1 if inlist(vi17,5,6) | inlist(vi17a,1,2)
replace agua_cp3=2 if vi17a==3 & vi17b <=30
replace agua_cp3=3 if vi17a==3 & vi17b>30 & vi17b!=999

***Suficiencia***
gen		agua_cp4=vi17c==1 if vi17c!=3


/*Higiene*/
***Instalación***
gen higiene_cp1=vi29==1 if !(vi29==2 & inlist(vi32,2,4))

***Agua en la instalación***
gen higiene_cp2=vi30==1 if !inlist(vi32,1,2,4) 

***Jabón***
gen higiene_cp3=vi31a==1 | vi31b==1 if !inlist(vi32,1,2,4) 

tempfile bdd19
save `bdd19'

use "$in\ASH 2016\122016_enemdubdd_completa.dta", clear
destring zona sector, replace
merge m:1 ciudad zona sector panelm vivienda hogar using "$in\ASH 2016\bdd_ASH_2016.dta", keep(3) 
drop fexp
egen id_upm=group(ciudad zona sector)
*svyset id_upm [iw=fexp2], strata(plan_muestreo) singleunit(certainty)

/*Calidad de agua*/

***Tipo de suministro***
recode vi17 (1/3 7 9=1 "tipo A") (4 8 10 12=2 "tipo B") (11 13=3 "tipo C"), gen(agua_cp1)
replace agua_cp1=1 if inlist(vi17,5,6) & inrange(vi16,1,3)
replace agua_cp1=2 if inlist(vi17,5,6) & inrange(vi16,4,7)

recode vi17 (1/7 9 12=1 "tipo A") (8 10=2 "tipo B") (11 13=3 "tipo C"), gen(agua_cp1b)

***Calidad del agua***
gen vtest=vi20a==1 & inrange(regs04,24,48) & regsp03==2 & regsp04==2
gen 	agua_cp2=vtest==1 & (regsp01==2 | regsp02==2)
replace agua_cp2=. if vtest==0

***Cercanía***
gen 	agua_cp3=1 if inlist(vi17,5,6) | inlist(vi17a,1,2)
replace agua_cp3=2 if vi17a==3 & vi17b <=30
replace agua_cp3=3 if vi17a==3 & vi17b>30 & vi17b!=999

***Suficiencia***
gen		agua_cp4=vi17c==1 if vi17c!=3


/*Higiene*/
***Instalación***
gen higiene_cp1=vi21==1 if !(vi21==2 & inlist(vi24,2,4))

***Agua en la instalación***
gen higiene_cp2=vi22==1 if !inlist(vi24,1,2,4) 

***Jabón***
gen higiene_cp3=vi23a==1 | vi23b==1 if !inlist(vi24,1,2,4) 


*Se unen las bases 2016 y 2019
append using `bdd19', gen(t) force

****Agua segura****
local as "agua_cp1 agua_cp1b agua_cp1 agua_cp1b agua_cp2 agua_cp2 agua_cp2b agua_cp2b"

forval i=1/4 {
*di "`:word `i' of `as'' `:word `=`i'+4' of `as''"
gen 	i_agua`i'=1 if `:word `i' of `as''==1 & `:word `=`i'+4' of `as''==1 & agua_cp3==1 & agua_cp4==1
replace	i_agua`i'=2 if `:word `i' of `as''==1 & `:word `=`i'+4' of `as''==1 & ((agua_cp3==1 & agua_cp4==0) | agua_cp3==2)
replace	i_agua`i'=3 if `:word `i' of `as''==1 & `:word `=`i'+4' of `as''==0 & inlist(agua_cp3,1,2)
replace	i_agua`i'=4 if `:word `i' of `as''==1 & agua_cp3==3
replace	i_agua`i'=5 if `:word `i' of `as''==2
replace i_agua`i'=6 if `:word `i' of `as''==3
if `i'>=3 replace i_agua`i'=. if t==0
}

label def i_agua ///
1 "seguro" ///
2 "básico 1" ///
3 "básico 2" ///
4 "limitado" ///
5 "no mejorado" ///
6 "superficial", replace
label values i_agua* i_agua

label def agua_cp3 ///
1 "Vivienda/terreno" ///
2 "<= a 30 minutos" ///
3 "> a 30 minutos", replace
label values agua_cp3 agua_cp3

label def sino 1 "sí" 0 "no"
label values agua_cp2 agua_cp4 sino

label var agua_cp1 "Tipo de suministro - oficial"
label var agua_cp1b "Tipo de suministro - modificado"
label var agua_cp2	"Calidad del agua - fuente"
label var agua_cp2b	"Calidad del agua - vaso"
label var agua_cp3 "Cercanía del suministro"
label var agua_cp4 "Suficiencia"
label var i_agua1 	"Agua segura-cal fuente"
label var i_agua2 	"Agua segura-cal fuente_mod ts"
label var i_agua3 	"Agua segura-cal vaso"
label var i_agua4 	"Agua segura-cal vaso_mod ts"


****Higiene****
gen 	i_higiene=higiene_cp1*higiene_cp2*higiene_cp3
replace i_higiene=2 if higiene_cp1==1 & higiene_cp2*higiene_cp3==0
replace i_higiene=3 if higiene_cp1==0
replace i_higiene=. if higiene_cp2+higiene_cp3==.& higiene_cp1!=0

label var higiene_cp1 "Instalación lavarse manos"
label var higiene_cp2 "Agua lavarse manos"
label var higiene_cp3 "Jabón lavarse manos"
label var i_higiene "Higiene"
label values higiene_cp* sino

*Seteo de la muestra
replace conglomerado=0 if t==0
replace fexp2=fexpper if fexp2==.
drop fexpper fexpviv
egen t_strata=group(plan_muestreo t)
egen t_upm=group(ciudad zona sector conglomerado t) 
svyset t_upm [pw=fexp2], strata(t_strata) singleunit(certainty)

**Variables de cruce**
*Etnia
drop etnia
recode p15 (1=1 "Indígena") (2/4=2 "Afro") (5=3 "Montubio") (6=4 "Mestizo") (7/8=5 "Blanco & otro"), gen(etnia)

*Etina jefe
tempvar v1
gen `v1'=etnia if p04==1
egen etniaj=max(`v1'), by(t id_hogar)
label val etniaj etnia 
replace etnia=etniaj if p03<5

*Nivel de instrucción 
recode p10a (1=1) (2/5=2) (6/7=3) (8/10=4), gen(inst)
replace inst=3 if p10a==5 & p10b>7 & p10b<=10
replace inst=inst-1 if p10b==0 & inlist(p10a,2,3,4,5,6,8,9)
label var inst "Nivel de instrucción"
label define inst 1"Ninguno" 2"Primaria" 3"Secundaria" 4"Superior"
label val inst inst
*Nivel de instrucción del jefe
tempvar v1
gen `v1'=inst if p04==1
egen instj=max(`v1'), by(t id_hogar)
label val instj inst 
*Grupos de edad
recode p03 (0/4=1 "0-4 años") (5/17=2 "5-17 años") (18/29=3 "18-29 años") (30/64=4 "30-64 años") (65/98=5 "+65 años") (99=.), gen(gedad)
*Quintil de ingreso
tempvar a1 a2 
xtile `a1'=ingpc [aw=fexp2] if t==0, n(5)
xtile `a2'=ingpc [aw=fexp2] if t==1, n(5)
gen 	q5_ingpc=`a1'
replace q5_ingpc=`a2' if t==1


*Seteo para exportar a Excel
local nombre_ex "pruebas hip 16-19_homologada"
putexcel set "$outR\\`nombre_ex'", replace

*****Test de hipótesis 2016-2019****
gen total=1
local subp "total p02 gedad etnia instj rn area q5_ingpc"

local indic "i_agua1 i_higiene"
foreach ind of local indic {
local rown ""

foreach var of local subp {
if "`var'"=="rn" local filtro "if rn!=4"
else 			 local filtro ""

levelsof `var' `filtro', local(name)
*levelsof i_agua
*local nn=r(levels)

svy: prop `ind' `filtro', over(`var' t)

mat e= e(b)'
mata st_matrix("se",sqrt(diagonal(st_matrix("e(V)"))))
mat A=e,se

mat AA=J(`=rowsof(A)/2',4,.)	// /2 por los t=0,1
forval ii=1/`=rowsof(A)' {
if mod(`ii',2)==1 mat AA[`=ceil(`ii'/2)',1]=A[`ii',1..2]
else 			  mat AA[`=ceil(`ii'/2)',3]=A[`ii',1..2]
}
mat AA=(AA,AA[1...,3]-AA[1...,1])

local nom=1
cap mat drop p
foreach tt in `e(namelist)' {
*di in red "`tt'"
	forval i=1/`:word count `name'' {
	local rown "`rown' `=strtoname("`:word `nom' of `e(label1)'' `:label (`var') `i''",1)'"
	cap nois test [`tt']_subpop_`=2*`i'-1'=[`tt']_subpop_`=2*`i''
	if !_rc mat p=nullmat(p)\r(p)
	else 	mat p=nullmat(p)\.
	}
local ++nom
}
mat R`ind'=nullmat(R`ind')\(AA,p)
}
mat rownames R`ind'=`rown'
mat colnames R`ind'=u_16 se_16 u_19 se_19 dif p
putexcel set "$outR\\`nombre_ex'", sheet(`ind'_t, replace) modify 
putexcel A1= "`:variable label `ind''"
putexcel A2=matrix(R`ind'), names
}

*local subp "total p02 gedad etnia etniaj inst instj rn area mig"


local indic "agua_cp1 agua_cp1b agua_cp2 agua_cp3 agua_cp4 higiene_cp1 higiene_cp2 higiene_cp3"
foreach ind of local indic {
tab `ind', gen(`ind'_)
levelsof `ind', local(le)
	foreach var of local subp {
	if "`var'"=="rn" local filtro "if rn!=4"
	else 			 local filtro ""

	levelsof `var' `filtro', local(name)
		foreach lev of local name { 
			forval i=1/`:word count `le'' {
			svy, subpop(if `var'==`lev'): mean `ind'_`i' `filtro', over(t)
			mat A=r(table)
			mat A=el(A,1,1),el(A,2,1),el(A,1,2),el(A,2,2),el(A,1,2)-el(A,1,1)
			test [`ind'_`i']0=[`ind'_`i']1
			mat A=A,r(p)
			mat rownames A=`=strtoname("cp`ind'_`i' `:label (`var') `lev''",1)'
			mat C`ind'=nullmat(C`ind')\A
			}
		}
	}
mat colnames C`ind'=u_16 se_16 u_19 se_19 dif p 	
drop `ind'_*
putexcel set "$outR\\`nombre_ex'", sheet(C_`ind') modify 
putexcel A1="`:variable label `ind''"
putexcel A2=matrix(C`ind'), names
}

****Datos 2019****
keep if t==1

foreach v of varlist i_agua* agua_cp* i_higiene higiene_cp* {
local rown ""
foreach var of local subp {
levelsof `var' `filtro', local(name)
svy: prop `v', over(`var')
mat e=r(table)'
mat `v'=nullmat(`v')\e[1...,1..6]

local nom=1
foreach tt in `e(namelist)' {
*di in red "`tt'"
	forval i=1/`:word count `name'' {
	local rown "`rown' `=strtoname("`:word `nom' of `e(label1)''",1)':`=strtoname("`:label (`var') `i''",1)'"
	}
local ++nom
}
}
*di "`rown'"
mat rownames `v'=`rown'
putexcel set "$outR\\`nombre_ex'", sheet(`v') modify 
putexcel A1="`:variable label `v''"
putexcel A2=matrix(`v'), names
}

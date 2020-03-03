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

global dir "C:\Users\Juan\Dropbox\Trabajos\Proyecto Agua-Unicef"

global in "$dir\Datos\ASH"
global out "$dir\Productos\Producto 4\2 BDD creadas"
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


**Variables de cruce**
*Sexo jefe
tempvar v1
gen `v1'=p02 if p04==1
egen sexj=max(`v1'), by(t id_hogar)
label val sexj p02
*Etnia
drop etnia
recode p15 (1=1 "Indígena") (2/4=2 "Afro") (5=3 "Montubio") (6=4 "Mestizo") (7/8=5 "Blanco & otro"), gen(etnia)
*Etina jefe
tempvar v1
gen `v1'=etnia if p04==1
egen etniaj=max(`v1'), by(t id_hogar)
label val etniaj etnia 
*Correción de etnia para menores de 5 años
replace etnia=etniaj if etnia==.
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
recode p03 (0/17=1 "0-17 años") (18/64=2 "18-64 años") (65/98=3 "+65 años") (99=.), gen(gedad)
*Tamaño del hogar
egen N=count(id_hogar), by(t id_hogar)
*Variable de migración los últimos 5 años
gen 	mig=	 (p16a==2 & p16b<=4 & p17a==2)
replace mig=2 if (p16a==2 & p16b<=4 & p17a==1)
replace mig=3 if (p16a==1 | p16b>4)

label var mig "Estatus migratorio últimos 5 años"
label define mig 1"Migrante internacional" ///
				 2"Migrante interno" ///
				 3"No migrante en últimos 5 años"
label val mig mig
*Hogares con menores de 5 años
gen m5=p03<=5
egen m5h=max(m5), by(id_hogar t)
*Quintil por ingresos
xtile q5_ingpc=ingpc [pw=fexp], n(5)

*Aceso a saneamiento (no se conoce con certeza el tratamiento de aguas servidas, entonces solo se crea categoría básico)
gen 	sane=1 if (vi13==1 & vi15==2) 												/*excusado y alcantarillado (vi13) de uso exclusivo (vi15) si las aguas servidas son tratadas (falta)*/ 
replace sane=1 if	///
			(inrange(vi13,2,3) & inrange(vi13a,2,3) & vi13c==2 & vi15==2) 	| /// 	/*pozo sÃ©ptico o pozo ciego (vi13) los desechos no van a un lugar abierto (vi13a) y no se ha vaciado el pozo (vi13c) de uso exclusivo (vi15)*/
			(vi13==4 & vi13b==1 & vi13c==2 & vi15==2)					  	/*letrina (vi13) con losa (vi13b) que no se haya vaciado (vi13c) de uso exclusivo (vi15) */
replace sane=1 if (inrange(vi13,2,3) & (vi13a==1 | (inrange(vi13a,2,3) & vi13c==1)) & vi15==2) 	| /// /*pozo sÃ©ptico o pozo ciego (vi13) los desechos van a un lugar abierto (vi13a) o se ha vaciado el pozo (vi13c) de uso exclusivo (vi15)*/
			(vi13==4 & vi13b==1 & vi13c==1 & vi15==2)					  	/*letrina (vi13) con losa (vi13b) que se ha vaciado (vi13c) de uso exclusivo (vi15) */
replace sane=2 if (vi13==1 & vi15==1) 										| /// 	/*excusado y alcantarillado (vi13) de uso compartido (vi15)*/ 
			(inrange(vi13,2,3) & inrange(vi13a,2,3) & vi13c==2 & vi15==1) 	| /// 	/*pozo sÃ©ptico o pozo ciego (vi13) los desechos no van a un lugar abierto (vi13a) y no se ha vaciado el pozo (vi13c) de uso compartido (vi15)*/
			(vi13==4 & vi13b==1 & vi13c==2 & vi15==1)					  	| /// 	/*letrina (vi13) con losa (vi13b) que no se haya vaciado (vi13c) de uso compartido (vi15) */
			(vi13==5 & vi13d==2 & inrange(vi14,1,3))						/*no tiene (vi13) usa instalaciÃ³n cercana/prestada(vi13d) que es alcantarillado, pozo sÃ©ptico o pozo ciego (vi14)*/
replace sane=3 if ///
			(inrange(vi13,2,3) & (vi13a==1 | (inrange(vi13a,2,3) & vi13c==1)) & vi15==1) 	| /// /*pozo sÃ©ptico o pozo ciego (vi13) los desechos van a un lugar abierto (vi13a) o se ha vaciado el pozo (vi13c) de uso compartido (vi15)*/
			(vi13==4 & vi13b==1 & vi13c==1 & vi15==1)					  	| /// 	/*letrina (vi13) con losa (vi13b) que se ha vaciado (vi13c) de uso compartido (vi15) */
			(vi13==4 & vi13b==2)											| /// 	/*letrina (vi13) sin losa (vi13b)*/
			(vi13==5 & vi13d==2 & vi14==4)											/*no tiene (vi13) usa instalaciÃ³n cercana/prestada(vi13d) que es letrina (vi14)*/
replace sane=4 if (vi13==5 & vi13d==1)												/*no tiene (vi13) al aire libre (vi13d)*/

label def sane ///
1 "Básico"					///
2 "Limitado"				///
3 "No mejorado"				///
4 "Al aire libre", replace
label value sane* sane

*Seteo de la muestra
replace conglomerado=0 if t==0
replace fexp2=fexpper if fexp2==.
drop fexpper fexpviv
egen t_strata=group(plan_muestreo t)
egen t_upm=group(ciudad zona sector conglomerado t) 
svyset t_upm [pw=fexp2], strata(t_strata) singleunit(certainty)

*Seteo para exportar a Excel
local nombre_ex "pruebas hip 16-19"
putexcel set "$outR\\`nombre_ex'", sheet(Transicion) modify

*Transición agua fuente a vaso
svy: mean agua_cp2 agua_cp2b
test agua_cp2=agua_cp2b
mat agua_tra=e(b), _b[agua_cp2b]-_b[agua_cp2], r(p)
foreach v in area rn etnia q5_ingpc {
	levelsof `v', local(le)
	foreach l in `le' {
		svy: mean agua_cp2 agua_cp2b if `v'==`l'
		test agua_cp2=agua_cp2b
		mat agua_tra=agua_tra\(e(b), _b[agua_cp2b]-_b[agua_cp2], r(p))
	}
}

mat colnames agua_tra="Calidad en la fuente" "Calidad en el vaso" "Cambio vaso - fuente" "p(cambio=0)"
mat rownames agua_tra="Nacional" "Urbano" "Rural" "Sierra" "Costa" "Amazonía" "Insular" ///
					  "Indígena" "Afroecuatoriano" "Montubio" "Mestizo" "Blanco & otro" ///
					  "Quintil 1" "Quintil 2" "Quintil 3" "Quintil 4" "Quintil 5"
putexcel A1="Transición calidad de fuente a vaso"
putexcel A2=matrix(agua_tra), names

*Capacidad de descontaminación por tratamiento en el hogar
svy: tab agua_cp2 agua_cp2b if t==1, row 											// Calidad en fuente y calidad en vaso
svy: tab vi19 agua_cp2b if t==1 & agua_cp2==0, row 									// Calidad en vaso según tratamiento | agua de mala calidad en la fuente.
svy: tab vi19 agua_cp2b if t==1 & agua_cp2==1, row 									// Calidad en vaso según tratamiento | agua de buena calidad en la fuente.
svy: reg agua_cp2b i.vi19 if t==1 & agua_cp2==1

***Modelo de explicación de la calidad del agua en el vaso
keep if t==1

gen idcanton=int(ciudad/100)
gen ly=ln(ingpc)
merge m:1 idcanton using "$out\atra_17.dta", keep(1 3) gen(_mGAD)
replace tra=0 if inrange(vi17,4,13)
replace vi19=. if vi19==4

**Factores inmediatos
logit agua_cp2b i.agua_cp2##i.vi19 i.i_higiene i.sane, vce(r)
estimates store l0
outreg2 using "$outR/Reg_agua.xls", replace label(,proper) excel sideway
test _b[agua_cp2b:2.vi19]-_b[agua_cp2b:1.agua_cp2#2.vi19]=0
test _b[agua_cp2b:3.vi19]-_b[agua_cp2b:1.agua_cp2#3.vi19]=0

margins, dydx(vi19) at(agua_cp2==0) post
estimates store l11
estimates restore l0
margins, dydx(vi19) at(agua_cp2==1) post
estimates store l12
estimates restore l0
margins, dydx(agua_cp2 i.i_higiene i.sane) post
estimates store l2

coefplot l11 l12 l2, xline(0) legend(label(2 "Sin calidad en la fuente") label(4 "Con calidad en la fuente") label(6 "Efectos mg promedio") position(6) rows(1)) ///
ylabel(1 "Tratamiento - Hervir" 2 "Tratamiento - Otro" 3 "Calidad en la fuente" 4 "Lav. manos - Sin agua o jabón" 5 "Lav. manos - Sin instalación" /// 
6 "Saneamiento - Limitado" 7 "Saneamiento - No mejorado" 8 "Saneamiento - DCA") name(G1, replace) 
graph export "$outR\ef_mg1.png", width(1600) replace
outreg2 using "$outR/mfx_agua.xls", replace label(,proper) excel sideway

*Solo entubada
logit agua_cp2b i.agua_cp2##i.vi19 i.i_higiene i.sane if inrange(vi17,1,3), vce(r)
outreg2 using "$outR/Reg_agua_entubada.xls", replace label(,proper) excel sideway

**Factores ampliados
logit agua_cp2	c.tra i.agua_cp1 i.agua_cp3 i.agua_cp4, vce(r) 								//Modelo de calidad en fuente
logit agua_cp2b	i.vi19 i.i_higiene i.sane c.tra i.agua_cp1 i.agua_cp3 i.agua_cp4, vce(r)	//Modelo ampliado de calidad en vaso
outreg2 using "$outR/Reg_agua.xls", label(,proper) excel sideway

margins, dydx(*) post
coefplot, xline(0) ylabel( ///
1 "Tratamiento - Hervir" 2 "Tratamiento - Otro" 3 "Lav. manos - Sin agua o jabón" 4 "Lav. manos - Sin instalación" /// 
5 "Saneamiento - Limitado" 6 "Saneamiento - No mejorado" 7 "Saneamiento - DCA" 8 "% agua distribuida con calidad" /// 
9 "Fuente agua - Tipo B" 10 "Fuente agua - Tipo C" 11 "Cercanía - <30 minutos" 12 "Suficiencia - Sí") name(G2,replace) 
graph export "$outR\ef_mg2.png", width(1600) replace
outreg2 using "$outR/mfx_agua.xls", label(,proper) excel sideway

*Solo entubada
logit agua_cp2b	i.vi19 i.i_higiene i.sane c.tra i.agua_cp1 i.agua_cp3 i.agua_cp4 if inrange(vi17,1,3), vce(r)
outreg2 using "$outR/Reg_agua_entubada.xls", label(,proper) excel sideway

*Factores sociales +  geográficos
logit agua_cp2b i.area i.rn ly i.etnia N i.m5h i.sexj i.instj, vce(r)
outreg2 using "$outR/Reg_agua.xls", label(,proper) excel sideway

margins, dydx(*) post
coefplot, xline(0) ylabel( ///
1 "Área - Rural" 2 "Región - Costa" 3 "Región - Amazonía" 4 "Región - Insular" /// 
5 "ln(Ingreso per cápita)" 6 "Etnia - Afroecuatoriano" 7 "Etnia - Montubio" 8 "Etnia - Mestizo" /// 
9 "Etnia - Blanco & otro" 10 "Tamaño del hogar" 11 "Hogar con menores de 5 años" 12 "Jefe de hogar - Mujer" 13 "Jefe de hogar - Ed.Primaria" /// 
14 "Jefe de hogar - Ed.Secundaria" 15 "Jefe de hogar - Ed.Superior") name(G3,replace) 
outreg2 using "$outR/mfx_agua.xls", label(,proper) excel sideway
graph export "$outR\ef_mg3.png", width(1600) replace

*Solo entubada
logit agua_cp2b i.area i.rn ly i.etnia N i.m5h i.sexj i.instj if inrange(vi17,1,3), vce(r)
outreg2 using "$outR/Reg_agua_entubada.xls", label(,proper) excel sideway

***Modelos de explicación de transición entre calidad en la fuente y caldiad en el vaso
**Mejora en la calidad
gen mejor=agua_cp2==0 & agua_cp2b==1
*Factores inmediatos
logit mejor i.vi19 i.i_higiene i.sane if agua_cp2==0, vce(r)
outreg2 using "$outR/Reg_cambio.xls", label(,proper) excel sideway replace
margins, dydx(*) post
coefplot, xline(0) ylabel( ///
1 "Tratamiento - Hervir" 2 "Tratamiento - Otro" 3 "Lav. manos - Sin agua o jabón" 4 "Lav. manos - Sin instalación" /// 
5 "Saneamiento - Limitado" 6 "Saneamiento - DCA") title("Probabilidad de mejorar calidad" "dado que calidad en fuente es mala") name(G4,replace) 

**Empeoramiento en la calidad
gen peor=agua_cp2==1 & agua_cp2b==0
*Factores inmediatos
logit peor i.vi19 i.i_higiene i.sane if agua_cp2==1, vce(r)
outreg2 using "$outR/Reg_cambio.xls", label(,proper) excel sideway
margins, dydx(*) post
coefplot, xline(0) ylabel( ///
1 "Tratamiento - Hervir" 2 "Tratamiento - Otro" 3 "Lav. manos - Sin agua o jabón" 4 "Lav. manos - Sin instalación" /// 
5 "Saneamiento - Limitado" 6 "Saneamiento - No Mejorado" 7 "Saneamiento - DCA") title("Probabilidad de empeorar calidad" "dado que calidad en fuente es buena") name(G5,replace) 
graph combine G4 G5

graph export "$outR\ef_mg4.png", width(1600) replace

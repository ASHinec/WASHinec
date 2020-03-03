*-------------------------------------------------------*
*														*
*	Descomposición del indicador de agua segura  		*
*														*
* Fuente: ENEMDU										*
* AÃ±o: 2016, 2019										*
* Contrucción: Juan Carlos Palacios/Fausto Jácome		*
* Consultoría Unicef-INEC calidad del agua				*
*														*
*-------------------------------------------------------*

clear all
set more off
set matsize 11000

global dir "C:\Users\Juan\Dropbox\Trabajos\Proyecto Agua-Unicef"

global in "$dir\Datos\ASH"
global outR "$dir\Productos\Producto 4\3 Resultados"

*Matriz de combinaciones para Shapley
clear
set obs 4
gen x=_n
permin x, k(4)
mkmat _all, mat(per)

use "$in\ASH 2019\BDD\Originales\201903_enemdu.dta", clear
merge m:1 id_hogar using "$in\ASH 2019\BDD\Originales\rma_enemdu_mar19.dta", keep(3) 
drop _*
destring zona sector, replace

/*Calidad de agua*/

***Tipo de suministro***
recode vi17 (1/3 7 9=1 "tipo A") (4 8 10 12=2 "tipo B") (11 13=3 "tipo C"), gen(agua_cp1)
replace agua_cp1=1 if inlist(vi17,5,6) & inrange(vi16,1,3)
replace agua_cp1=2 if inlist(vi17,5,6) & inrange(vi16,4,7)

***Calidad del agua***
gen vtest=vi26==1 & inrange(ra01e_f,24,48) & ra03_f==2 & ra04_f==2
gen 	agua_cp2=vtest==1 & (ra022_f==2 | ra023_f==2)
replace agua_cp2=. if vtest==0

***Cercanía***
gen 	agua_cp3=1 if inlist(vi17,5,6) | inlist(vi17a,1,2)
replace agua_cp3=2 if vi17a==3 & vi17b <=30
replace agua_cp3=3 if vi17a==3 & vi17b>30 & vi17b!=999

***Suficiencia***
gen		agua_cp4=vi17c==1 if vi17c!=3

tempfile bdd19
save `bdd19'

use "$in\ASH 2016\122016_enemdubdd_completa.dta", clear
destring zona sector, replace
merge m:1 ciudad zona sector panelm vivienda hogar using "$in\ASH 2016\bdd_ASH_2016.dta", keep(3) 
drop fexp
egen id_upm=group(ciudad zona sector)

/*Calidad de agua*/

***Tipo de suministro***
recode vi17 (1/3 7 9=1 "tipo A") (4 8 10 12=2 "tipo B") (11 13=3 "tipo C"), gen(agua_cp1)
replace agua_cp1=1 if inlist(vi17,5,6) & inrange(vi16,1,3)
replace agua_cp1=2 if inlist(vi17,5,6) & inrange(vi16,4,7)

***Calidad del agua***
gen vtest=vi20a==1 & inrange(regs04,24,48) & regsp03==2 & regsp04==2
gen 	agua_cp2=vtest==1 & (regsp01==2 | regsp02==2)
replace agua_cp2=. if vtest==0

***CercanÃ­a***
gen 	agua_cp3=1 if inlist(vi17,5,6) | inlist(vi17a,1,2)
replace agua_cp3=2 if vi17a==3 & vi17b <=30
replace agua_cp3=3 if vi17a==3 & vi17b>30 & vi17b!=999

***Suficiencia***
gen		agua_cp4=vi17c==1 if vi17c!=3

*Se unen las bases 2016 y 2019
append using `bdd19', gen(t) force

****Agua segura****

gen 	i_agua=1 if agua_cp1==1 & agua_cp2==1 & agua_cp3==1 & agua_cp4==1
replace	i_agua=2 if agua_cp1==1 & agua_cp2==1 & ((agua_cp3==1 & agua_cp4==0) | agua_cp3==2)
replace	i_agua=3 if agua_cp1==1 & agua_cp2==0 & inlist(agua_cp3,1,2)
replace	i_agua=4 if agua_cp1==1 & agua_cp3==3
replace	i_agua=5 if agua_cp1==2
replace i_agua=6 if agua_cp1==3

label def i_agua ///
1 "Seguro" ///
2 "Básico 1" ///
3 "Básico 2" ///
4 "Limitado" ///
5 "No mejorado" ///
6 "Superficial", replace
label values i_agua* i_agua

label def agua_cp3 ///
1 "Vivienda/terreno" ///
2 "<= a 30 minutos" ///
3 "> a 30 minutos", replace
label values agua_cp3 agua_cp3

label def sino 1 "sí" 0 "no"
label values agua_cp2 agua_cp4 sino

label var agua_cp1 "Tipo de suministro - oficial"
label var agua_cp2	"Calidad del agua - fuente"
label var agua_cp3 "Cercanía del suministro"
label var agua_cp4 "Suficiencia"
label var i_agua 	"Agua segura-cal fuente"


forval i=1/4 {
gen acp`i'=(agua_cp`i'==1)
}

egen 	k_agua=rowtotal(acp*), missing
replace k_agua=. if i_agua==.
gen tot=1

*Seteo de la muestra
replace conglomerado=0 if t==0
replace fexp2=fexpper if fexp2==.
drop fexpper fexpviv
egen t_strata=group(plan_muestreo t)
egen t_upm=group(ciudad zona sector conglomerado t) 
svyset t_upm [pw=fexp2], strata(t_strata) singleunit(certainty)

*Descomposición de Shapley por año
forval t=0/1 {
preserve
keep if t==`t'
svy: total tot, over(k_agua)
mat esc=e(b)'
svy: prop k_agua
mat esc=esc,e(b)'
mat colname esc=total propor

	forval j=1/`=rowsof(per)' {
	qui total fexp2 if i_agua!=.
	mat A=e(b)
	mat B=e(b)
	mat N=0
		forval i=1/4 {
		local cod`i'=""
		if `i'==1 	local cod`i'="& acp`=el(per,`j',`i')'==1"
		else 		local cod`i'="`cod`=`i'-1'' & acp`=el(per,`j',`i')'==1"
		di in red "`cod`i''"
		qui total fexp2 if i_agua!=. `cod`i''
		mat A=A\(B-e(b))
		mat B=e(b)
		mat N=N\ `=el(per,`j',`i')'
			if `i'==4 {
			mat A=A,N
			mata : st_matrix("A", sort(st_matrix("A"), (2)))
			}
		}
	if `j'!=`=rowsof(per)' 	mat R=nullmat(R),A[1...,1]
	else 					mat R=nullmat(R),A
	}

mata: A=st_matrix("R")[1...,cols(st_matrix("R"))],mean(st_matrix("R")[1...,1..cols(st_matrix("R"))-1]',1)'
mata: A=A,A[1...,2]:/A[1,2]
mata: st_matrix("A",A)
mat esc_`t'=esc
mat A_`t'=A
cap mat drop R
restore

*Universalización hipotética por componente
forv c=0/4{
	preserve
	cap replace agua_cp`c'=1
	cap drop i_aguah
	gen 	i_aguah=1 if agua_cp1==1 & agua_cp2==1 & agua_cp3==1 & agua_cp4==1
	replace	i_aguah=2 if agua_cp1==1 & agua_cp2==1 & ((agua_cp3==1 & agua_cp4==0) | agua_cp3==2)
	replace	i_aguah=3 if agua_cp1==1 & agua_cp2==0 & inlist(agua_cp3,1,2)
	replace	i_aguah=4 if agua_cp1==1 & agua_cp3==3
	replace	i_aguah=5 if agua_cp1==2
	replace i_aguah=6 if agua_cp1==3
	recode i_aguah (1=1) (nonm=0), g(apgms)
	svy:mean apgms if t==`t'
	mat hypU_`t'=nullmat(hypU_`t')\e(b)
	restore
}

putexcel set "$outR\escalera y descomp", sheet(Año `t') modify 
putexcel A1=("Escalera")
putexcel G1=("Descomposición")
putexcel A10=("Universalización")

putexcel A2=matrix(esc_`t', names)
putexcel G2=matrix(A_`t', names)
putexcel A11=matrix(hypU_`t', names)
}

*Relación con el ingreso - solo año 19
keep if t==1
recode i_agua (1=1) (nonm=0), g(apgms)

gen ly=ln(ingpc)
label var ly "ln(Ingreso per cápita)"
cap sum ingpc if t==1, d
replace ly=. if ingpc<r(p1) | ingpc>r(p99)
lpoly apgms ly if t==1 [w=round(fexp2)], nosca bwidth(.12) ytitle("% de personas con acceso a APGMS") title("")
graph export "$outR/ly-apgms.png", width(800) replace

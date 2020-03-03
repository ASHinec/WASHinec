*-------------------------------------------------------*
*														*
*	Cantones cuya agua distribuida por red pública		*
*					es tratada							*
* Fuente: Censo GADs municipales						*
* Año: 2016/7											*
* Contrucción: Juan Carlos Palacios/Fausto Jácome		*
* Consultoría Unicef-INEC calidad del agua				*
*														*
*-------------------------------------------------------*

clear all
set more off

global dir "C:\Users\Juan\Dropbox\Trabajos\Proyecto Agua-Unicef"

global in "$dir\Datos\Censo GADs"
global out "$dir\Productos\Producto 4\2 BDD creadas"
global outR "$dir\Productos\Producto 4\3 Resultados"

cap mkdir "$out"
cap mkdir "$outR"

*---------------------------------------*
*		Base de junta de aguas			*
*---------------------------------------*
*Llamar base 2017
use "$in\2017\JA_2017.dta", clear

gen 	atra=vtd if carap>0 & carap!=. & cacap>=carap
collapse (sum) atra vtd, by(idcanton)
keep idcanton atra vtd
destring idcanton, replace
tempfile R
save `R'

*---------------------------------------*
*		Base de censo GADS				*
*---------------------------------------*
*Llamar base de 2016
use "$in\2016\apa_2016.dta", clear

*Fuentes con riesgo de contaminación o contaminadas por falta de protección por cantón
egen fc=rsum(ma3292 ma3293)
gen rc=fc/ma32

*Proporción de agua distribuida que cumple norma INEN 1108
gen 	atra_=ma365
replace atra_=ma310 if ma365>ma310				// Ajuste por volúmenes de tratamiento superiores a volúmenes de distribución
gen		pct_tra=ma3671/ma36						// Porcentaje de plantas de tratamiento con norma INEN (se asume iguales proporciones de producción por planta)
gen		atra=pct_tra*atra_
replace atra=0 if atra==.
rename 	ma310 vtd
keep idcanton atra vtd
append using `R'
collapse (sum) atra vtd, by(idcanton)
gen 	tra=atra/vtd
replace tra=0 if tra==.
save "$out\atra_16.dta", replace

*Llamar base de 2017
use "$in\2017\apa_2017vf.dta", clear

*Fuentes con riesgo de contaminación o contaminadas por falta de protección por cantón
egen fc=rsum(ma31382 ma31383)
gen rc=fc/ma3131
replace rc=1 if rc>1 & rc!=.

*Proporción de agua distribuida que cumple norma INEN 1108
gen 	atra_=ma3165vac
replace atra_=ma3110 if ma3165vac>ma3110		// Ajuste por volúmenes de tratamiento superiores a volúmenes de distribución
recode 	ma3167nic ma3162 (mis=0)
gen		pct_tra=ma3167nic/ma3162				// Porcentaje de plantas de tratamiento con norma INEN (se asume iguales proporciones de producción por planta)
												// Nota: Cantón 0904 (Pedro Carbo) tiene 3 plantas con cumplimiento INEN, pero solo 2 plantas en total
replace pct_tra=1 if pct_tra>1 & pct_tra!=.
gen		atra=pct_tra*atra_
replace atra=0 if atra==.
rename 	ma3110 vtd
keep idcanton atra vtd rc
append using `R'
collapse (sum) atra vtd rc, by(idcanton)
gen 	tra=atra/vtd
replace tra=0 if tra==.

**Mapas
*Mapa de % de fuentes contaminadas o en riesgo de contaminación por falta de protección por cantón
merge 1:1 idcanton using "$out\nxcantones.dta", keep (1 3)
spmap  rc using "$out\shp_canton.dta", id(_ID) clmethod(c) clbreaks(0 0.05 0.25 0.50 1)  fcolor(Heat) ndfcolor(gray) /// 
legend(title("Leyenda", size(*0.5)) position(7)) legstyle(2) title("Proporción de fuentes con riesgo de contaminación o" "contaminadas por falta de protección", size(medsmall)) ///
name("m1", replace)

*Mapa de % de agua tratada según norma INEN 1108
spmap  tra using "$out\shp_canton.dta", id(_ID) clmethod(c) clbreaks(0 0.25 0.50 0.9 1)  fcolor(Blues2) ndfcolor(gray) /// 
legend(title("Leyenda", size(*0.5)) position(7)) legstyle(2) title("Proporción de agua distribuida de red pública que cumple" "norma INEN 1108", size(medsmall)) ///
name("m2", replace)

graph combine m1 m2, r(1)
graph export "$outR\Mapa_Censo.png", replace width(1500)
graph drop _all

keep idcanton atra vtd tra
save "$out\atra_17.dta", replace

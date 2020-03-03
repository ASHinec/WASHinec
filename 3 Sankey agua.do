*-------------------------------------------------------*
*														*
*				Sankey flujo de agua	  				*
*														*
* Fuente: ENEMDU										*
* Año: 2019												*
* Contrucción: Juan Carlos Palacios/Fausto Jácome		*
* Consultoría Unicef-INEC calidad del agua				*
*														*
*-------------------------------------------------------*

clear all
set more off

global dir "C:\Users\Fausto Jácome\Dropbox\Proyecto Agua-Unicef"

global in "$dir\Datos\ASH"
global outR "$dir\Productos\Producto 4\3 Resultados"

use "$in\ASH 2019\BDD\Originales\201903_enemdu.dta", clear
gen idcanton=floor(ciudad/100)
merge m:1 idcanton using "$dir\Productos\Producto 4\2 BDD creadas\atra_17.dta", keep(3)	nogen //se eliminan 619 obs de cantones 90
merge m:1 id_hogar using "$in\ASH 2019\BDD\Originales\rma_enemdu_mar19.dta", keep(3)
drop _* fexp fexpviv

/*Fuente para beber (fuente)*/
recode vi17 (1/3=1 "A tub") (7 9=2 "A ntub") (4 8 10 12=3 "B") (11 13=4 "C"), gen(fuente)
replace fuente=2 if inlist(vi17,5,6) & inrange(vi16,1,3)
replace fuente=3 if inlist(vi17,5,6) & inrange(vi16,4,7)

/*Tratamiento (trata)*/
gen trata=tra if fuente==1

/*Calidad fuente (calidad_f)*/
gen vtest=vi26==1 & inrange(ra01e_f,20,48) & ra03_f==2 & ra04_f==2
gen 	calidad_f=vtest==1 & (ra022_f==2 | ra023_f==2) & ra023_f!=1
replace calidad_f=. if vtest==0

/*Tratamiento hogar (trata_h)*/
gen trata_h=inlist(vi19,2,3) if vi19!=4

/*Calidad bebida (calidad_v)*/
gen vtestb=vi21==1 & inrange(ra01e_v,20,48) & ra03_v==2 & ra04_v==2
gen 	calidad_v=vtestb==1 & (ra022_v==2 | ra023_v==2) & ra023_v!=1
replace calidad_v=. if vtestb==0


local varl fuente calidad_f trata_h calidad_v
foreach var of local varl {
levelsof `var', local(`=substr("`var'",-3,.)')
replace fexpper=. if `var'==.
}

gen tot=1
total trata [iw=fexp]
mat sk=e(b)
total tot [iw=fexp] if fuente==1
mat sk=sk\(e(b)-sk)\e(b)
total tot [iw=fexp]
mat sk[3,1]=-(el(sk,3,1)-el(e(b),1,1))

local nombre ""
local i=1
foreach var of local varl {
	if `i'<`:word count `varl''{
	local var2 "`:word `=`i'+1' of `varl''"
		foreach lev of local `=substr("`var'",-3,.)' {
		di in red "`var'_`lev' `var2' "
		total tot [iw=fexp] if `var'==`lev', over(`var2')
		mat sk=sk\e(b)'
		levelsof `var2'
			foreach niv in `r(levels)' {
			local nombre "`nombre' `var'_`lev':`var2'_`niv'"
			}
		}
	}
local ++i
}

mat rownames sk=trata:fuente1 ntrata:fuente1 sdtrata:fuente2 `nombre'

putexcel set "$outR\sankey.xlsx", replace
putexcel A1=matrix(sk), names


/*
/*gsort idcanton -fexp
by idcanton: egen double trata=pc(fexp), prop
by idcanton: replace trata=sum(trata)
replace trata=trata<=tra*/

egen numper=count(ciudad), by(id_hogar)
egen np_tot=total(numper*fexp), by(idcanton)
egen fx_tot=total(fexp), by(idcanton)
gen double np_fx=np_tot/fx_tot
drop np_tot fx_tot
replace tra=. if fuente!=1
gen tra_1=tra*(numper/np_fx)

local vr "id_upm"		// Agrupamiento por conglomerado

sort id_per 
forval i=1/1 {
*bysort `vr': replace agu_t`i'=cond(_n==1,rbinomial(1,tra),agu_t`i'[1])
gen 	agu_t=rbinomial(1,tra)
replace agu_t=1 if tra==1
replace agu_t=0 if tra==0
replace agu_t=. if vi13!=1


drop agu_t
}

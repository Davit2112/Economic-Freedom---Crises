clear
import delimited C:\Users\User\Documents\OneDrive\Desktop\MP\krk.csv
cd "D:\"
   
sort country
by country: gen id = 1 if _n==1
replace id = sum(id)
replace id = . if missing(country)
xtset id year

xtlogit bank efw, i(id) fe nolog 
estimates store fe_base
xtlogit bank efw, i(id) re nolog
estimates store re_base
hausman fe_base re_base, eq(1:1)
xtlogit bank gov_size leg_prop sound_mon trade_freed regul, i(id) fe nolog
estimates store fe_base2
xtlogit bank gov_size leg_prop sound_mon trade_freed regul, i(id) re nolog
estimates store re_base2
hausman fe_base2 re_base2, eq(1:1)
outreg2 using test.doc, append stats(coef)


xtlogit bank gov_size leg_prop sound_mon trade_freed regul bank_partner, i(id) fe nolog 
estimates store fe_1
xtlogit bank gov_size leg_prop sound_mon trade_freed regul bank_partner, i(id) re nolog
estimates store re_1
hausman fe_1 re_1, eq(1:1)

xtlogit bank gov_size leg_prop sound_mon trade_freed regul bank_partner public_deposit, i(id) fe nolog 

estimates store fe_2
xtlogit bank gov_size leg_prop sound_mon trade_freed regul bank_partner public_deposit, i(id) re nolog
estimates store re_2

hausman fe_2 re_2, eq(1:1)

xtlogit bank gov_size leg_prop sound_mon trade_freed regul bank_partner public_deposit bank_past, i(id) fe nolog 

estimates store fe_3
xtlogit bank gov_size leg_prop sound_mon trade_freed regul bank_partner public_deposit bank_past, i(id) re nolog

estimates store re_3

hausman fe_3 re_3, eq(1:1)

xtlogit bank gov_size leg_prop sound_mon trade_freed regul bank_partner public_deposit bank_past gdp_per_cap, i(id) fe nolog 

estimates store fe_4
xtlogit bank gov_size leg_prop sound_mon trade_freed regul bank_partner public_deposit bank_past gdp_per_cap, i(id) re nolog

estimates store re_4


hausman fe_4 re_4, eq(1:1)


estimates table fe_base re_base fe_base2 re_base2 fe_1 re_1 , b(%9.3f) star(.05 .01 .001)



*****************************************************


xtlogit currency efw, i(id) fe nolog
estimates store fe_base
xtlogit currency efw, i(id) re nolog
estimates store re_base
hausman fe_base re_base, eq(1:1)

xtlogit currency gov_size leg_prop sound_mon trade_freed regul, i(id) fe nolog
estimates store fe_base2
xtlogit currency gov_size leg_prop sound_mon trade_freed regul, i(id) re nolog
estimates store re_base2
hausman fe_base2 re_base2, eq(1:1)


xtlogit currency gov_size leg_prop sound_mon trade_freed regul currency_partner, i(id) fe nolog 
estimates store fe_1
xtlogit currency gov_size leg_prop sound_mon trade_freed regul currency_partner, i(id) re nolog
estimates store re_1
hausman fe_1 re_1, eq(1:1)

xtlogit currency gov_size leg_prop sound_mon trade_freed regul currency_partner public_deposit, i(id) fe nolog 
estimates store fe_2
xtlogit currency gov_size leg_prop sound_mon trade_freed regul currency_partner public_deposit, i(id) re nolog
estimates store re_2
hausman fe_2 re_2, eq(1:1)

xtlogit currency gov_size leg_prop sound_mon trade_freed regul currency_partner public_deposit currency_past, i(id) fe nolog 
estimates store fe_3
xtlogit currency gov_size leg_prop sound_mon trade_freed regul currency_partner public_deposit currency_past, i(id) re nolog
estimates store re_3
hausman fe_3 re_3, eq(1:1)

xtlogit currency gov_size leg_prop sound_mon trade_freed regul currency_partner public_deposit currency_past gdp_per_cap, i(id) fe nolog 
estimates store fe_4
xtlogit currency gov_size leg_prop sound_mon trade_freed regul currency_partner public_deposit currency_past gdp_per_cap, i(id) re nolog
estimates store re_4
hausman fe_4 re_4, eq(1:1)


estimates table fe_base re_base fe_base2 re_base2 fe_1 re_1 , b(%9.3f) star(.05 .01 .001)




*****************************************************


xtlogit debt efw, i(id) fe nolog
outreg2 using currency.doc, replace stats(coef)
estimates store fe_base
xtlogit debt efw, i(id) re nolog
outreg2 using currency.doc, append stats(coef)
estimates store re_base
hausman fe_base re_base, eq(1:1)

xtlogit debt gov_size leg_prop sound_mon trade_freed regul, i(id) fe nolog
outreg2 using currency.doc, append stats(coef)
estimates store fe_base2
xtlogit debt gov_size leg_prop sound_mon trade_freed regul, i(id) re nolog
outreg2 using currency.doc, append stats(coef)
estimates store re_base2
hausman fe_base2 re_base2, eq(1:1)


xtlogit debt gov_size leg_prop sound_mon trade_freed regul debt_partner, i(id) fe nolog 
estimates store fe_1
xtlogit debt gov_size leg_prop sound_mon trade_freed regul debt_partner, i(id) re nolog
estimates store re_1
hausman fe_1 re_1, eq(1:1)


xtlogit debt gov_size leg_prop sound_mon trade_freed regul debt_partner debt_past, i(id) fe nolog 
estimates store fe_3
xtlogit debt gov_size leg_prop sound_mon trade_freed regul debt_partner debt_past, i(id) re nolog
estimates store re_3
hausman fe_3 re_3, eq(1:1)


xtlogit debt gov_size leg_prop sound_mon trade_freed regul debt_partner debt_past  debt_to_gdp,  i(id) fe nolog 
outreg2 using terst.doc, append stats(coef)
estimates store fe_2

xtlogit debt gov_size leg_prop sound_mon trade_freed regul debt_partner debt_past  debt_to_gdp,  i(id) re nolog
outreg2 using terst.doc, append stats(coef)

estimates store re_2
hausman fe_2 re_2, eq(1:1)

estimates table fe_base re_base fe_base2 re_base2 fe_1 re_1 , b(%9.3f) star(.05 .01 .001)

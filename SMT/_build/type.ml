type pterms = PFun of string * pterms | PVar of string | PList of pterms * pterms;;
type ppredicat = PEqual of pterms * pterms | PDiff of pterms * pterms;;
type pformule = PPred of ppredicat | POr of pformule * pformule | PAnd of pformule * pformule
              | PNot of pformule | PImply of pformule * pformule;;

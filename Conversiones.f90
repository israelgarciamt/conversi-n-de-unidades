!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!Funciones de conversiòn!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


Function CtoF(c) result(f)
implicit none
  INTENT (IN)::c
  DOUBLE PRECISION:: c,f
	F=(9.0/5.0)*c+32.0 
End Function

Function FtoC(f) result(c)
implicit none
  INTENT (IN)::f
  DOUBLE PRECISION:: f,c
	c=(F-32.0)*(5.0/9.0) 
End Function

Function mmtoin(mm) result(in)
implicit none
  INTENT (IN)::mm
  DOUBLE PRECISION:: mm,in,mtoin,mmtom
	in=mtoin(mmtom(mm)) 
End Function

Function mtoin(m) result(in)
implicit none
  INTENT (IN)::m
  DOUBLE PRECISION:: m,in
	in=39.3701*m 
End Function


Function intomm(in) result(mm)
implicit none
  INTENT (IN)::in
  DOUBLE PRECISION::in,mm,mtomm,intom
	mm=mtomm(intom(in))
End Function


Function intom(in) result(m)
implicit none
  INTENT (IN)::in
  DOUBLE PRECISION::in,m
	m=0.0254*in
End Function



Function mtomm(m) result(mm)
implicit none
  INTENT (IN)::m
  DOUBLE PRECISION::mm,m
	mm=m*1000
End Function


Function mmtom(mm) result(m)
implicit none
  INTENT (IN)::mm
  DOUBLE PRECISION::mm,m
	m=mm/1000.0
End Function


Function kgstogalmin(kgs,t) result(galmin)
implicit none
  INTENT (IN)::kgs,t
  DOUBLE PRECISION::kgs,galmin,t,kgtogal
	galmin=kgtogal(kgs,t)/(1/60.0)
End Function



Function kgtogal(kg,t) result(gal)
implicit none
  INTENT (IN)::kg,t
  DOUBLE PRECISION::kg,gal,WATERDENSITY,t
	gal=kg*(2.64172*WATERDENSITY(t))
End Function


Function fttom(ft) result(m)
implicit none
  INTENT (IN)::ft
  DOUBLE PRECISION::ft,m
	m=0.3048*ft
End Function



Function kgstoft3min(kgs,t,spsatm) result(ft3min)
implicit none
  INTENT (IN)::kgs
  DOUBLE PRECISION::kgs,ft3min,t,airdensity,spsatm
  ft3min=kgs*(35.3147*airdensity(t,spsatm))/(1.0/60.0)
End Function


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



Function ft3mintokgs(ft3min,t,spsatm) result(kgs)
implicit none
  INTENT (IN)::ft3min,t,spsatm
  DOUBLE PRECISION::kgs,ft3min,t,airdensity,spsatm
  kgs=((1.0/60.0)*ft3min)/(35.3147*airdensity(t,spsatm))
End Function

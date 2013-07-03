subroutine runge6(ipif, deltat, tpgp, tpgm, hpgm,&
                  hpgp, err)
    implicit none
#include "jeveux.h"
#include "asterfort/fointa.h"
    integer :: ipif
    real(kind=8) :: deltat, tpgp, tpgm, hpgm, hpgp, err
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CALCUL DE h+ PAR RUNGE-KUTTA6
!     RESOLUTION DE DH/DT = f(HYDR,TEMP)
! ----------------------------------------------------------------------
! IN  IPIF  : POINTEUR DANS LE MATERIAU CODE (FONCTION OU NAPPE)
! IN  DELTAT  : PAS DE TEMPS
! IN  TPGP : TEMP A ITERATION NEWTON COURANTE
! IN  TPGM : TEMP A ITERATION PRECEDENTE
! IN  HPGM : HYDR A ITERATION PRECEDENTE
! OUT HPGP  : RESULTAT DE L'INTEGRATION HYDR À ITERATION NEWTON COURANTE
! OUT ERR : ERREUR ABSOLUE
! ----------------------------------------------------------------------
!
!
!
    real(kind=8) :: f1, f2, f3, f4, f5, f6, valpa(2)
    real(kind=8) :: k1, k2, k3, k4, k5, k6, hpgpe
    real(kind=8) :: a2, a3, a4, a5, a6
    real(kind=8) :: b21, b31, b32, b41, b42, b43
    real(kind=8) :: b51, b52, b53, b54
    real(kind=8) :: b61, b62, b63
    real(kind=8) :: b64, b65, c1, c2
    real(kind=8) :: c3, c4, c5, c6
    real(kind=8) :: ce1, ce2, ce3
    real(kind=8) :: ce4, ce5, ce6
    character(len=24) :: nompa(2)
! ----------------------------------------------------------------------
! DEFINITION DES COEFFICIENTS cf: numerical recipes
    a2=2.d-1
    a3=3.d-1
    a4=6.d-1
    a5=1.d0
    a6=8.75d-1
    b21=2.d-1
    b31=3.d0/40.d0
    b32=9.d0/40.d0
    b41=3.d-1
    b42=-9.d-1
    b43=1.2d0
    b51=-11.d0/54.d0
    b52=2.5d0
    b53=-70.d0/27.d0
    b54=35.d0/27.d0
    b61=1631.d0/55296.d0
    b62=175.d0/512.d0
    b63=575.d0/13824.d0
    b64=44275.d0/110592.d0
    b65=253.d0/4096.d0
    c1=37.d0/378.d0
    c2=0.d0
    c3=250.d0/621.d0
    c4=125.d0/594.d0
    c5=0.d0
    c6=512.d0/1771.d0
    ce1=20825.d0/27648.d0
    ce2=0.d0
    ce3=18575.d0/48384.d0
    ce4=13525.d0/55296.d0
    ce5=277.d0/14336.d0
    ce6=2.5d-1
! CALCUL DE K1
    valpa(1)=hpgm
    valpa(2)=tpgm
    nompa(1)='HYDR'
    nompa(2)='TEMP'
    call fointa(ipif, 2, nompa, valpa, f1)
    k1= deltat*f1
! CALCUL DE K2
    valpa(1)=hpgm+k1*b21
    valpa(2)=tpgm+(tpgp-tpgm)*a2
    nompa(1)='HYDR'
    nompa(2)='TEMP'
    call fointa(ipif, 2, nompa, valpa, f2)
    k2= deltat*f2
! CALCUL DE K3
    valpa(1)=hpgm+b31*k1+b32*k2
    valpa(2)=tpgm+(tpgp-tpgm)*a3
    nompa(1)='HYDR'
    nompa(2)='TEMP'
    call fointa(ipif, 2, nompa, valpa, f3)
    k3= deltat*f3
! CALCUL DE K4
    valpa(1)=hpgm+b41*k1+b42*k2+b43*k3
    valpa(2)=tpgm+(tpgp-tpgm)*a4
    nompa(1)='HYDR'
    nompa(2)='TEMP'
    call fointa(ipif, 2, nompa, valpa, f4)
    k4= deltat*f4
! CALCUL DE K5
    valpa(1)=hpgm+b51*k1+b52*k2+b53*k3+b54*k4
    valpa(2)=tpgm+(tpgp-tpgm)*a5
    nompa(1)='HYDR'
    nompa(2)='TEMP'
    call fointa(ipif, 2, nompa, valpa, f5)
    k5= deltat*f5
! CALCUL DE K6
    valpa(1)=hpgm+b61*k1+b62*k2+b63*k3+b64*k4+b65*k5
    valpa(2)=tpgm+(tpgp-tpgm)*a6
    nompa(1)='HYDR'
    nompa(2)='TEMP'
    call fointa(ipif, 2, nompa, valpa, f6)
    k6= deltat*f6
! CALCUL DE HPGP
    hpgp=hpgm+c1*k1+c2*k2+c3*k3+c4*k4+c5*k5+c6*k6
!--------------------
!
! CALCUL DE L ERREUR
    hpgpe=hpgm+ce1*k1+ce2*k2+ce3*k3+&
     &      ce4*k4+ce5*k5+ce6*k6
    err=abs(hpgp-hpgpe)
end subroutine

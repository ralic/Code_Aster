function lcroy1()
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    real(kind=8) :: lcroy1
!
! ********************************************************************
! *       INTEGRATION DE LA LOI DE ROUSSELIER LOCAL                  *
! *  CALCUL DE BORNES INF ET SUP DE LA FONCTION SEUIL(Y) QUAND S(0)>0*
! *  ET RESOLUTION DE SEUIL(Y)=0                                     *
! *  PAR UNE METHODE DE NEWTON AVEC BORNES CONTROLEES ET DICHOTOMIE  *
! *  - LA BORNE INFERIEURE EST TELLE QUE:                            *
! *    2*MU*EQTR-R(PM+DPINF)=3*MU*DPINF      => ON EN DEDUIT DPINF   *
! *    YINF*EXP(YINF)=K*FONC*DPINF/SIG1      => ON EN DEDUIT YINF    *
! *  - LA BORNE SUPERIEURE EST TELLE QUE:                            *
! *    YSUP*EXP(YSUP) =K*FONC*(2*MU*EQTR-S(YINF))/3*MU*SIG1          *
! ********************************************************************
!
! OUT  Y   : VALEUR DE Y TEL QUE SEUIL(Y)=0
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!  COMMON LOI DE COMPORTEMENT ROUSSELIER
!
#include "asterfort/lcrofg.h"
#include "asterfort/lcroty.h"
#include "asterfort/rcfonc.h"
#include "asterfort/utmess.h"
    integer :: itemax, jprolp, jvalep, nbvalp
    real(kind=8) :: prec, young, nu, sigy, sig1, rousd, f0, fcr, acce
    real(kind=8) :: pm, rpm, fonc, fcd, dfcddj, dpmaxi,typoro
    common /lcrou/ prec,young,nu,sigy,sig1,rousd,f0,fcr,acce,&
     &               pm,rpm,fonc,fcd,dfcddj,dpmaxi,typoro,&
     &               itemax, jprolp, jvalep, nbvalp
! ----------------------------------------------------------------------
!  COMMON GRANDES DEFORMATIONS CANO-LORENTZ
!
    integer :: ind1(6), ind2(6)
    real(kind=8) :: kr(6), rac2, rc(6)
    real(kind=8) :: lambda, mu, deuxmu, unk, troisk, cother
    real(kind=8) :: jm, dj, jp, djdf(3, 3)
    real(kind=8) :: etr(6), dvetr(6), eqetr, tretr, detrdf(6, 3, 3)
    real(kind=8) :: dtaude(6, 6)
!
    common /gdclc/&
     &          ind1,ind2,kr,rac2,rc,&
     &          lambda,mu,deuxmu,unk,troisk,cother,&
     &          jm,dj,jp,djdf,&
     &          etr,dvetr,eqetr,tretr,detrdf,&
     &          dtaude
!
! ----------------------------------------------------------------------
    integer :: iter
    real(kind=8) :: seuil, dseuil, s
    real(kind=8) :: y, dp, yinf, ysup, t, rp, pente, aire
    real(kind=8) :: r8bid
!
! 1 - CALCUL DU MINORANT
!
    if (deuxmu*eqetr-rpm .le. 0) then
!
! LE TERME DE TRACE N'EST PAS NEGLIGEABLE
!
        yinf = 0
    else
!
! RESOLUTION DE L'EQUATION SANS LE TERME DE TRACE
! LCROTY RESOUD UNE EQUATION DU TYPE Y*EXP(Y)=CONSTANTE
!
        call rcfonc('E', 1, jprolp, jvalep, nbvalp,&
                    r8bid, young, nu, pm, rp,&
                    pente, aire, 2.d0*mu*eqetr, dp)
        yinf = lcroty(dp*unk*fonc/sig1, prec, itemax)
    endif
!
! 2 - CALCUL DU MAJORANT
! LCROTY RESOUD UNE EQUATION DU TYPE Y*EXP(Y)=CONSTANTE
!
    call lcrofg(yinf, dp, s, seuil, dseuil)
    if (seuil .lt. 0.d0) yinf=0.d0
    call lcrofg(yinf, dp, s, seuil, dseuil)
!
    t = (deuxmu*eqetr-s)/(3.d0*mu)
    ysup = lcroty(t*unk*fonc/sig1, prec, itemax)
    call lcrofg(ysup, dp, s, seuil, dseuil)
!
! 3 - RESOLUTION PAR UNE METHODE DE NEWTON ENTRE LES BORNES
!
    y = ysup
    do 10 iter = 1, itemax
        if (abs(seuil)/sigy .le. prec) goto 100
!
        y = y - seuil/dseuil
        if (y .le. yinf .or. y .ge. ysup) y=(yinf+ysup)/2
!
        call lcrofg(y, dp, s, seuil, dseuil)
!
        if (seuil .ge. 0) yinf = y
        if (seuil .le. 0) ysup = y
!
10  end do
    call utmess('F', 'ALGORITH3_55')
!
!
100  continue
    lcroy1 = y
end function

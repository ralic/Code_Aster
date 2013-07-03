function lcesrf(am, weps, r, v, prec,&
                itemax, iret)
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
    implicit none
    real(kind=8) :: lcesrf
#include "asterfort/lcesvf.h"
    integer :: itemax, iret
    real(kind=8) :: weps, r, v, am, prec
! ----------------------------------------------------------------------
!   ENDO_SCALAIRE:       RESOLUTION DE -DRDA(A)*WEPS = V + R*A
! ----------------------------------------------------------------------
!  IN  AM      VALEUR DE L'ENDOMMAGEMENT EN T- (BORNE INF)
!  IN  WEPS    ENERGIE DE DEFORMATION ELASTIQUE
!  IN  R       PARAMETRE D'AUGMENTATION
!  IN  V       CSTE DU SECOND MEMBRE (K-PHI EN PRATIQUE)
!  IN  PREC    PRECISION : A ET A+PREC ENCADRENT LA SOLUTION
!  IN  ITEMAX  NOMBRE D'ITERATIONS MAXI AUTORISE
!  OUT IRET    CONVERGENCE (0=OK, 1=PB)
!  OUT ITER    NOMBRE D'ITERATIONS (POUR INFO)
! ----------------------------------------------------------------------
    integer :: iter
    real(kind=8) :: amin, amax, ai, p2, p1p, p0, an, cn, ln, sens, pente, da
    real(kind=8) :: capp, aest, acvg, ccvg, lcvg
    real(kind=8) :: un
! ----------------------------------------------------------------------
    real(kind=8) :: pk, pm, pp
    common /lces/ pk,pm,pp
! ----------------------------------------------------------------------
!
!    INITIALISATION
    iret = 0
    un = 1.d0
!
!    INTERVALLE DE RECHERCHE
    amin = max(am,-v/r)
    amax = min(un,(pm*weps-v)/r)
!
!    POINT D'INFLEXION
    if (pm-2*pp-3 .ge. 0) then
        ai = -1
    else
        p2 = (1+pm*pp)*(1+2*pp)
        p1p = 1+pm*pp
        p0 = pm-2*pp-3
        ai = (-p1p+sqrt(p1p**2-p0*p2))/p2
    endif
!
!    INITIALISATION NEWTON
    an = max(amin,ai)
    an = min(amax,an)
    cn = -weps*lcesvf(1,an)
    ln = v+r*an
    sens = sign(un,cn-ln)
!
!    OPTIMISATION DE L'ESTIMATION INITIALE
    if (sens .ge. 0) then
        capp = pm*(1+pp)*(2+pm*(1+pp))
        p2 = r*capp + (2*pp+1)*pm*weps
        p1p = (r+v*capp)/2 -pp*pm*weps
        p0 = v - pm*weps
        aest = (-p1p+sqrt(p1p**2-p0*p2))/p2
        if (aest .gt. an) then
            an = aest
            cn = -weps*lcesvf(1,an)
            ln = v+r*an
        endif
    endif
!
!    METHODE DE NEWTON
    do 10 iter = 1, itemax
        pente = weps*lcesvf(2,an)
        da = (cn-ln)/(r+pente)
        an = an+da
!
        if (abs(da) .lt. prec) then
            acvg = an+sens*prec
            ccvg = -weps*lcesvf(1,acvg)
            lcvg = v+r*acvg
            if (sens*(lcvg-ccvg) .ge. 0) then
                lcesrf = an
                goto 9999
            endif
        endif
!
        cn = -weps*lcesvf(1,an)
        ln = v+r*an
10  end do
    iret = 1
!
9999  continue
end function

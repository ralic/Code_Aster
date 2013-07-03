function prgonf(biot, betam, pref, p1)
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! ======================================================================
! ROUTINE PRGONF
! CALCUL DE LA PRESSION DE GONFLEMENT POUR L ARGILE GONFLANTE
! ======================================================================
!
    implicit none
#include "asterc/r8pi.h"
#include "asterfort/erfcfo.h"
    real(kind=8) :: biot, betam, pref, p1, prgonf, derf
    real(kind=8) :: pi, rpi, s, rbetam
!
    pi = r8pi()
    rpi=sqrt(pi)
    rbetam=sqrt(betam)
    betam=rbetam*rbetam
    s=p1/pref
    derf = (1.d0-erfcfo(s*rbetam))
    if (s .gt. 0.d0) then
        prgonf=pref*biot*((rpi/(2.d0*rbetam))*derf +(0.5d0/betam)*(&
        1.d0-exp(-betam*s*s)))
    else
        prgonf=pref*biot*s
    endif
end function

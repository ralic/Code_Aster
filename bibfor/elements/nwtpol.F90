subroutine nwtpol(deg, coef, rac)
!
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!======================================================================
!
!     RECHERCHE DU ZERO D UN POLYNOME PAR LA METHODE DE NEWTON
!
! IN  DEG : DEGRE DU POLYNOME
! IN  COEF : COEFFICIENT DU POLYNOME
!
! OUT RAC : VALEUR DE LA RACINE TROUVEE
!
#include "asterfort/dpolyh.h"
#include "asterfort/dpolyn.h"
    integer :: deg, itermx, i
    real(kind=8) :: coef(deg+1), rac
    real(kind=8) :: rac0, tole, fx, dfx, err, fx0
!
    itermx = 10
    tole = 1.d-15
!
!     EVALUATION DU POLYNOME A X = RAC
    fx = dpolyh(deg,coef,rac)
!
    err = abs(fx)
    rac0 = rac
    fx0 = fx
!
    do 9, i = 1, itermx+1
!
!     CRITERE DE CONVERGENCE
    if (err .le. tole) goto 10
!
!     EVALUATION DE LA DERIVEE DU POLYNOME A X = RAC
    dfx = dpolyn(deg,coef,rac)
!
    if (abs(dfx) .lt. 1.d-12) then
        if (abs(fx0) .lt. abs(fx)) rac = rac0
        goto 10
    endif
!
    rac = rac - fx/dfx
    fx = dpolyh(deg,coef,rac)
    err = abs(fx)
    9 end do
!
10  continue
!
end subroutine

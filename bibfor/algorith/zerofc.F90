subroutine zerofc(func, xmin, xmax, prec, niter,&
                  dp, iret, nit)
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
! ======================================================================
    implicit none
! aslint: disable=W0307
#include "asterfort/zeroco.h"
    interface
        function func(x)
            real(kind=8) :: x
            real(kind=8) :: func
        end function
    end interface
    integer :: niter, iret
    real(kind=8) :: xmin, xmax, prec, dp
! ----------------------------------------------------------------------
!     RECHERCHE DU ZERO DE func. ON SAIT QUE VAL0=func(0) < 0 ET func CROISSANTE
!     APPEL A ZEROCO (METHODE DE CORDE)
!
! IN  func       : FONCTION func
! IN  XMIN    : VALEUR DE X POUR LAQUELLE func(X) < 0 (XMIN = 0 EN GENERAL)
! IN  XMAX    : ESTIMATION DE LA VALEUR DE X POUR LAQUELLE func > 0
! IN  PREC    : PRECISION ABSOLUE : LA SOLUTION EST TELLE QUE func(DP)<PREC
! IN  NITER   : NOMBRE D'ITERATIONS MAXIMUM
! OUT DP      : SOLUTION : ACCROISSEMENT DE LA VARIABLE INTERNE P
! OUT IRET    : CODE RETOUR : IRET = 0 : OK
!             :               SINON : PB
! OUT NIT     : NOMBRE D'ITERATIONS NECESSAIRE POUR CONVERGER
!
    real(kind=8) :: x(4), y(4)
    integer :: i, nit
! DEB ------------------------------------------------------------------
!
    nit = 0
    iret = 1
    x(1) = xmin
    y(1) = func(xmin)
    x(2) = xmax
    y(2) = func(xmax)
    x(3) = x(1)
    y(3) = y(1)
    x(4) = x(2)
    y(4) = y(2)
!
    do 20 i = 1, niter
!
!       SOLUTION TROUVEE : ON SORT
        if (abs(y(4)) .lt. prec) then
            iret = 0
            nit=i
            goto 9999
        endif
!
        call zeroco(x, y)
!
        dp = x(4)
        y(4) = func(dp)
!
20  end do
!
9999  continue
end subroutine

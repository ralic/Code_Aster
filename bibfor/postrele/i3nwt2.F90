subroutine i3nwt2(epsi, seuil, maxitr, fk, x,&
                  iret)
    implicit none
!
#include "asterfort/i3efk2.h"
    integer :: maxitr, iret
    real(kind=8) :: epsi, seuil, fk(4, *), x(*)
!
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     RESOLUTION DE FK(X) = 0 PAR NEWTON-RAPHSON
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  SEUIL  : R : CRITERE D' ARRET
! IN  MAXITR : I : NOMBRE MAX D' ITERATION AUTORISE
! IN  FK     : R : TABLE(1..4,1..2) DES COEF DE LA TRANSFORMATION
! VAR X      : R : TABLE(1..2)      DES COORDONNEES DU POINT
! OUT IRET   : I : CODE RETOUR :  0 --> CONVERGENCE A SEUIL
!                                 1 --> MAX_ITER UTILISE
!                                -1 --> UN JACOBIEN NUL  RENCONTRE
!     ------------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: valfk(3, 1), d, d1, d2, j11, j12, j21, j22
    logical :: fini
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    fini = .false.
    i = 1
    iret = 0
10  continue
    if (.not. fini) then
        call i3efk2(fk, 1, x(1), x(2), valfk)
        j11 = fk(2,1) + fk(4,1)*x(2)
        j12 = fk(3,1) + fk(4,1)*x(1)
        j21 = fk(2,2) + fk(4,2)*x(2)
        j22 = fk(3,2) + fk(4,2)*x(1)
        d = j11*j22 - j12*j21
        if (abs(d) .lt. epsi) then
            fini = .true.
            iret = -1
        else
            d = 1/d
            d1 = (valfk(1,1)*j22 - valfk(2,1)*j12)*d
            d2 = (valfk(2,1)*j11 - valfk(1,1)*j21)*d
            d = x(1)*x(1) + x(2)*x(2)
            x(1) = x(1) - d1
            x(2) = x(2) - d2
            d1 = d1*d1 + d2*d2
            i = i + 1
            if (d1 .le. d*seuil*seuil) then
                fini = .true.
                iret = 0
            else
                if (i .gt. maxitr) then
                    fini = .true.
                    iret = 1
                endif
            endif
        endif
        goto 10
    endif
    call i3efk2(fk, 1, x(1), x(2), valfk)
    d = valfk(1,1)*valfk(1,1) + valfk(2,1)*valfk(2,1)
end subroutine

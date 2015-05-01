subroutine nmveso(rb, nb, rp, np, drbdb,&
                  drbdp, drpdb, drpdp, dp, dbeta,&
                  nr, cplan)
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
! ======================================================================
!-----------------------------------------------------------------------
    implicit none
!
#include "asterf_types.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcicma.h"
#include "asterfort/lcpsvn.h"
#include "asterfort/mgauss.h"
    integer :: nb, np, nr
    aster_logical :: cplan
    real(kind=8) :: rb(nb), rp(np), drbdb(nb, nb), drbdp(nb, np)
    real(kind=8) :: dp(np), dbeta(nb), drpdp(np, np), drpdb(np, nb)
! ----------------------------------------------------------------------
!     INTEGRATION DE LA LOI DE COMPORTEMENT VISCO PLASTIQUE DE
!     CHABOCHE AVEC ENDOMAGEMENT
!     METHODE ITERATIVE D'EULER IMPLICITE
!
!     GENERATION ET RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY)
!-----------------------------------------------------------------------
    integer :: nmod, i, iret
    real(kind=8) :: zero, un, mun, det
    parameter  ( nmod = 25 )
    parameter  ( zero = 0.d0   )
    parameter  ( un   = 1.d0   )
    parameter  ( mun   = -1.d0   )
!
    real(kind=8) :: drdy(nmod, nmod), r(nmod)
!
!
!-----------------------------------------------------------------------
!-- 1. INITIALISATIONS
!-- 1.1. INITIALISATION DE L OPERATEUR LINEAIRE DU SYSTEME
!                     DRDY = ( DRBDB, DRBDP )
!                            ( DRPDB, DRPDP )
!
    call lcicma(drbdb, nb, nb, nb, nb,&
                1, 1, drdy, nmod, nmod,&
                1, 1)
    call lcicma(drbdp, nb, np, nb, np,&
                1, 1, drdy, nmod, nmod,&
                1, nb+1)
    call lcicma(drpdb, np, nb, np, nb,&
                1, 1, drdy, nmod, nmod,&
                nb+1, 1)
    call lcicma(drpdp, np, np, np, np,&
                1, 1, drdy, nmod, nmod,&
                nb+1, nb+1)
!
!-- 1.2. INITIALISATION R = ( -RB , -RP )
!
    call lcpsvn(nb, mun, rb, r)
    call lcpsvn(np, mun, rp, r(nb+1))
!
!-- 2. RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY)
!
    if (cplan) then
        r(3) = zero
        do 110 i = 1, nr
            drdy(i,3) = zero
            drdy(3,i) = zero
110     continue
        drdy(3,3) = un
    endif
!
    call mgauss('NFVP', drdy, r, nmod, nr,&
                1, det, iret)
    call lceqvn(nb, r, dbeta)
    call lceqvn(np, r(nb+1), dp)
!
end subroutine

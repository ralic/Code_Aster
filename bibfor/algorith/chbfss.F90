subroutine chbfss(sig, x1, x2, id, ddfdds)
    implicit none
!       ----------------------------------------------------------------
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
!       ----------------------------------------------------------------
!       3D / 1D / DP / CP
!       DERIVEE / S / S DE LA FONCTION SEUIL A (SIG , X1 , X2 ) DONNES
!
!       IN  SIG    :  TENSEUR CONTRAINTE
!       IN  X1     :  TENSEUR CINEMATIQUE 1
!       IN  X2     :  TENSEUR CINEMATIQUE 2
!                                                     T
!       OUT D2FD2S :  DDFDDS = 1/S ( 3/2 ID - DFDS DFDS )
!                     DFDS   = 3 (D-X1-X2) / 2 S
!                                            T
!                     ID     = I4 - 1/3 I2 I2
!                                           T           1/2
!                     S      = (3/2(D-X1-X2) (D-X1-X2))
!                     D      = SIG - 1/3 TR(SIG) I
!       ----------------------------------------------------------------
#include "asterfort/chbfs.h"
#include "asterfort/lcdevi.h"
#include "asterfort/lcdima.h"
#include "asterfort/lcdive.h"
#include "asterfort/lcnrts.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprte.h"
    integer :: n, nd
    real(kind=8) :: dfds(6), sig(6), x1(6), x2(6), dev(6), s
    real(kind=8) :: ddfdds(6, 6), dfds2(6, 6)
    real(kind=8) :: id(6, 6)
    common /tdim/   n , nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call lcdevi(sig, dev)
    call lcdive(dev, x1, dev)
    call lcdive(dev, x2, dev)
    s = lcnrts ( dev )
    call chbfs(sig, x1, x2, dfds)
    call lcprte(dfds, dfds, dfds2)
    call lcprsm(1.5d0, id, ddfdds)
    call lcdima(ddfdds, dfds2, ddfdds)
    call lcprsm(1.d0/ s, ddfdds, ddfdds)
end subroutine

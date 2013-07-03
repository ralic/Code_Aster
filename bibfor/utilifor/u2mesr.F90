subroutine u2mesr(ch1, idmess, nr, valr)
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     ROUTINE CHAPEAU D'APPEL A U2MESG POUR NE PASSER QUE DES REELS
!
!     ------------------------------------------------------------------
    implicit none
#include "asterc/isnnem.h"
#include "asterfort/u2mesg.h"
    character(len=*) :: ch1, idmess
    integer :: nr
    real(kind=8) :: valr(*)
!     ------------------------------------------------------------------
    character(len=8) :: valk(1)
    integer :: nk, ni, vali(1)
!     ------------------------------------------------------------------
    ni = 0
    nk = 0
    valk(1) = ' '
    vali(1) = isnnem()
    call u2mesg(ch1, idmess, nk, valk, ni,&
                vali, nr, valr)
!     ------------------------------------------------------------------
end subroutine

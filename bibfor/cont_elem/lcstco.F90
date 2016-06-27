subroutine lcstco(algo_reso_geom, indi_cont, lagrc_)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterf_types.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! 
    integer, intent(out) :: algo_reso_geom
    integer, intent(out) :: indi_cont
    real(kind=8), optional, intent(out) :: lagrc_
!
! --------------------------------------------------------------------------------------------------
!
! Contact (LAC) - Elementary computations
!
! Get indicators
!
! --------------------------------------------------------------------------------------------------
!
! Out indi_cont        : contact indicator
!                        -1 No pairing
!                         0 Paired - No contact
!                        +1 Paired - Contact
! Out algo_reso_geom   : algorithm for geometry loop
!                         0 - fixed point
!                         1 - Newton
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jpcf
    real(kind=8) :: lagrc
!
! --------------------------------------------------------------------------------------------------
!
    call jevech('PCONFR', 'L', jpcf)
    algo_reso_geom = nint(zr(jpcf-1+25))
    indi_cont      = nint(zr(jpcf-1+12))
    lagrc          =     (zr(jpcf-1+13))
    if (present(lagrc_)) then
        lagrc_ = lagrc
    endif
!
end subroutine

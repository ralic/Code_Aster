subroutine surfco(sdcont, mesh)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/infniv.h"
#include "asterfort/surfc1.h"
#include "asterfort/surfc2.h"
#include "asterfort/surfc3.h"
#include "asterfort/surfcl.h"
#include "asterfort/surfcp.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
!
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: mesh
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Print debug
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdcont           : name of contact concept (DEFI_CONTACT)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: unit_msg, lvel_msg
    character(len=24) :: sdcont_defi
    integer :: cont_form
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(unit_msg, lvel_msg)
!
! - Datastructure for contact definition
!
    sdcont_defi = sdcont(1:8)//'.CONTACT'
!
! - Parameters
!
    cont_form   = cfdisi(sdcont_defi,'FORMULATION')
!
! - Debug print
!
    if (lvel_msg .ge. 2) then
        call surfcp(sdcont, unit_msg)
        if (cont_form .eq. 1) then
            call surfcl(sdcont, mesh, unit_msg)
            call surfc1(sdcont, unit_msg)
        else if (cont_form .eq. 2) then
            call surfcl(sdcont, mesh, unit_msg)
            call surfc2(sdcont, mesh)
        else if (cont_form .eq. 3) then
            call surfc3(sdcont, mesh, unit_msg)
        else if (cont_form .eq. 5) then
            ASSERT(.false.)
        else
            ASSERT(.false.)
        endif
    endif
!
end subroutine

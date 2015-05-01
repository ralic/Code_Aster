subroutine nmextc(sd_inout, keyw_fact, i_keyw_fact, field_type, l_extr)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/nmetob.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: sd_inout
    character(len=16), intent(in) :: keyw_fact
    integer, intent(in) :: i_keyw_fact
    character(len=24), intent(out) :: field_type
    aster_logical, intent(out) :: l_extr
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Read field type
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_inout         : datastructure for input/output parameters
! In  keyw_fact        : factor keyword to read extraction parameters
! In  i_keyw_fact      : index of keyword to read extraction parameters
! Out field_type       : type of field (name in results datastructure)
! Out l_extr           : .true. if field can been extracted
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nchp, n1
    integer :: i_field
!
! --------------------------------------------------------------------------------------------------
!
    l_extr = .true.
!
! - Read
!
    call getvtx(keyw_fact, 'NOM_CHAM', iocc=i_keyw_fact, nbval=0, nbret=n1)
    nchp = -n1
    ASSERT(nchp.eq.1)
!
! - Get name of field (type)
!
    call getvtx(keyw_fact, 'NOM_CHAM', iocc=i_keyw_fact, scal=field_type)
!
! - Get index of field in sd_inout
!
    call nmetob(sd_inout, field_type, i_field)
!
! - Can been monitored ?
!
    if (i_field .eq. 0) then
        l_extr = .false.
    else
        l_extr = .true.
    endif
!
end subroutine

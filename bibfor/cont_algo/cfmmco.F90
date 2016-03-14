subroutine cfmmco(ds_contact, i_zone, coef_type_, action, valr)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=*), intent(in) :: coef_type_
    character(len=1), intent(in) :: action
    integer, intent(in) :: i_zone
    real(kind=8), intent(inout) :: valr
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete method - Get or set coefficients
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  coef_type        : type of coefficient (E_N or E_T)
! In  action           : read (L) or write (E) coefficient
! In  i_zone           : index of contact zone
! IO  valr             : coefficeint (to read or write)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ztaco
    character(len=24) :: sdcont_tabcof
    real(kind=8), pointer :: v_sdcont_tabcof(:) => null()
    character(len=24) :: coef_type
    integer :: nb_cont_zone
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Get parameters
!
    coef_type    = coef_type_
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi,'NZOCO')
    ASSERT(i_zone .le. nb_cont_zone)
    ASSERT(i_zone .ge. 1)
!
! --- ACCES SD
!
    ztaco = cfmmvd('ZTACO')
    sdcont_tabcof = ds_contact%sdcont_solv(1:14)//'.TABL.COEF'
    call jeveuo(sdcont_tabcof, 'E', vr = v_sdcont_tabcof)
!
    if (action .eq. 'E') then
        if (coef_type .eq. 'E_N') then
            v_sdcont_tabcof(ztaco*(i_zone-1)+1) = valr
        else if (coef_type .eq.'E_T') then
            v_sdcont_tabcof(ztaco*(i_zone-1)+2) = valr
        else
            ASSERT(.false.)
        endif
    else if (action.eq.'L') then
        valr = 0.d0
        if (coef_type .eq. 'E_N') then
            valr = v_sdcont_tabcof(ztaco*(i_zone-1)+1)
        else if (coef_type .eq.'E_T') then
            valr = v_sdcont_tabcof(ztaco*(i_zone-1)+2)
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine

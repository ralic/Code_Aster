subroutine caraxi(sdcont, model, model_ndim)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/jeveuo.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfdisi.h"
#include "asterfort/mmmaxi.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: model
    integer, intent(in) :: model_ndim
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Check if elements of contact zones are axisymmetric
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont      : name of contact concept (DEFI_CONTACT)
! In  model       : name of model
! In  model_ndim  : dimension of model
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_defi
    character(len=24) :: cont_paraci, cont_contma
    integer, pointer :: v_cont_paraci(:) => null()
    aster_logical :: l_verif_all, l_elem_axis
    integer :: nb_cont_elem
!
! --------------------------------------------------------------------------------------------------
!
!
! - Datastructure for contact definition
!
    sdcont_defi = sdcont(1:8)//'.CONTACT'
    cont_paraci = sdcont_defi(1:16)//'.PARACI'
    cont_contma = sdcont_defi(1:16)//'.MAILCO'
    call jeveuo(cont_paraci, 'E', vi=v_cont_paraci)
!
! - Parameters
!
    l_verif_all  = cfdisl(sdcont_defi,'ALL_VERIF')
    nb_cont_elem = cfdisi(sdcont_defi,'NMACO')
!
! - Only if not verification on all zones !
!
    if (.not.l_verif_all) then
        l_elem_axis = .false.
        if (model_ndim .eq. 2) then
            l_elem_axis = mmmaxi(model, cont_contma, nb_cont_elem)
        endif
        if (l_elem_axis) then
            v_cont_paraci(16) = 1
        else
            v_cont_paraci(16) = 0
        endif
    endif
!
end subroutine

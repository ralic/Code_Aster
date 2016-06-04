subroutine mmprel(sdcont, mesh, model, ligret)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/ajellt.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/wkvect.h"
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
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: model
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: ligret
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Continue method - Create slave elements
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  model            : name of model
! In  mesh             : name of mesh
! In  ligret           : special LIGREL for slaves elements (CONTINUE formulation)
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_frot_zone, l_veri
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_mailco
    integer, pointer :: v_sdcont_mailco(:) => null()
    character(len=16) :: modeli, phenom
    integer :: jdecme, i_zone
    integer :: nb_cont_zone, model_ndim, nb_cont_elem, nt_elem_slav
    integer :: i_elem_slav, elem_slav_idx, elem_slav_nume, nb_elem_slav
    aster_logical :: l_verif_all
    character(len=24) :: list_elem
    integer, pointer :: v_list_elem(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    list_elem = '&&MMPREL.LISTE_MAILLES'
    call dismoi('PHENOMENE', model, 'MODELE', repk=phenom)
!
! - Datastructure for contact definition
!
    sdcont_defi   = sdcont(1:8)//'.CONTACT'
    sdcont_mailco = sdcont_defi(1:16)//'.MAILCO'
    call jeveuo(sdcont_mailco, 'L', vi = v_sdcont_mailco)
!
! - Parameters
!
    model_ndim   = cfdisi(sdcont_defi,'NDIM')
    nb_cont_elem = cfdisi(sdcont_defi,'NMACO')
    nt_elem_slav = cfdisi(sdcont_defi,'NTMAEC')
    nb_cont_zone = cfdisi(sdcont_defi,'NZOCO')
    l_verif_all  = cfdisl(sdcont_defi,'ALL_VERIF')
!
! - Add elements
!
    if (.not.l_verif_all) then
!
! ----- Create list of slave elements
!       
        call wkvect(list_elem, 'V V I', nt_elem_slav, vi = v_list_elem)
!
! ----- Set list of slave elements
!
        do i_zone = 1, nb_cont_zone
!
! --------- Type of model
!
            l_frot_zone = mminfl(sdcont_defi,'FROTTEMENT_ZONE',i_zone)
            l_veri = mminfl(sdcont_defi,'VERIF',i_zone)
            if (model_ndim .eq. 2) then
                if (l_frot_zone) then
                    modeli = 'FRIC_SL_2D'
                else
                    modeli = 'CONT_SL_2D'
                endif
            else if (model_ndim.eq. 3) then
                if (l_frot_zone) then
                    modeli = 'FRIC_SL_3D'
                else
                    modeli = 'CONT_SL_3D'
                endif
            else
                ASSERT(.false.)
            endif
!
! --------- Type of model
!
            if (.not.l_veri) then
                nb_elem_slav = mminfi(sdcont_defi,'NBMAE' ,i_zone)
                jdecme       = mminfi(sdcont_defi,'JDECME',i_zone)
                        ASSERT(nb_elem_slav.le.nt_elem_slav)
                do i_elem_slav = 1, nb_elem_slav
                    elem_slav_idx  = jdecme+i_elem_slav
                    elem_slav_nume = v_sdcont_mailco(elem_slav_idx)
                    v_list_elem(i_elem_slav) = elem_slav_nume
                end do
                call ajellt(ligret, mesh, nb_elem_slav, list_elem, ' ',&
                            phenom, modeli, 0, ' ')
            endif
        end do
        call jedetr(list_elem)
    endif
!
end subroutine

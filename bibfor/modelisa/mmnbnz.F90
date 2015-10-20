subroutine mmnbnz(mesh, sdcont_defi, i_zone, nb_cont_poin)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfnumm.h"
#include "asterfort/mminfi.h"
#include "asterfort/mmelin.h"
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
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: i_zone
    integer, intent(out) :: nb_cont_poin
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Utility
!
! Continue/Discrete - Count contact point by zonee
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  i_zone           : index of contact zone
! Out nb_cont_poin     : number of contact points
!
! --------------------------------------------------------------------------------------------------
!
    integer :: cont_form, elem_slav_indx, elem_slav_nume, type_inte, nb_poin_elem
    integer :: i_elem_slav, nb_elem_slav, jdecme
!
! --------------------------------------------------------------------------------------------------
!
    nb_cont_poin = 0
    cont_form    = cfdisi(sdcont_defi, 'FORMULATION')
!
! - Count
!
    if (cont_form .eq. 1) then
        nb_cont_poin = mminfi(sdcont_defi, 'NBNOE' , i_zone)
    else if (cont_form.eq.2) then
        nb_elem_slav = mminfi(sdcont_defi, 'NBMAE', i_zone)
        jdecme       = mminfi(sdcont_defi, 'JDECME', i_zone)
        type_inte    = mminfi(sdcont_defi, 'INTEGRATION', i_zone)
        do i_elem_slav = 1, nb_elem_slav
            elem_slav_indx = i_elem_slav + jdecme
            call cfnumm(sdcont_defi, elem_slav_indx, elem_slav_nume)
            call mmelin(mesh, elem_slav_nume, type_inte, nb_poin_elem)
            nb_cont_poin = nb_cont_poin + nb_poin_elem
        end do
    else
        ASSERT(.false.)
    endif
!
end subroutine

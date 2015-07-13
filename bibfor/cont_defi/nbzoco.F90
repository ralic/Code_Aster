subroutine nbzoco(keywf, mesh, i_zone, nb_cont_surf)
!
implicit none
!
#include "asterfort/getvtx.h"
#include "asterfort/verima.h"
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
    character(len=16), intent(in) :: keywf
    integer, intent(in) :: i_zone
    integer, intent(out) :: nb_cont_surf
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Count number of surfaces of contact
!
! --------------------------------------------------------------------------------------------------
!
! In  keywf            : factor keyword to read
! In  mesh             : name of mesh
! In  i_zone           : index of contact zone
! Out nb_cont_surf     : number of surfaces of contact
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_group_mast, nb_group_slav, nb_mast, nb_slav
    character(len=24) :: list_elem
!
! --------------------------------------------------------------------------------------------------
!
    nb_cont_surf  = 0
    nb_group_mast = 0
    nb_group_slav = 0
    nb_mast       = 0
    nb_slav       = 0
!
! - Number of master elements
!
    call getvtx(keywf, 'GROUP_MA_MAIT', iocc=i_zone, vect=list_elem,&
                nbret=nb_group_mast)
    if (nb_group_mast .ne. 0) then
        call verima(mesh, list_elem, nb_group_mast, 'GROUP_MA')
    endif
    call getvtx(keywf, 'MAILLE_MAIT', iocc=i_zone, vect=list_elem,&
                nbret=nb_mast)
    if (nb_mast .ne. 0) then
        call verima(mesh, list_elem, nb_mast, 'MAILLE')
    endif
!
! - Number of slave elements
!
    call getvtx(keywf, 'GROUP_MA_ESCL', iocc=i_zone, vect=list_elem,&
                nbret=nb_group_slav)
    if (nb_group_slav .ne. 0) then
        call verima(mesh, list_elem, nb_group_slav, 'GROUP_MA')
    endif
    call getvtx(keywf, 'MAILLE_ESCL', iocc=i_zone, vect=list_elem,&
                nbret=nb_slav)
    if (nb_group_slav .ne. 0) then
        call verima(mesh, list_elem, nb_slav, 'MAILLE')
    endif
!
! - Total number of contact surfaces
!
    if (nb_group_mast .ne. 0) then
        nb_cont_surf = nb_cont_surf + 1
    endif
    if (nb_group_slav .ne. 0) then
        nb_cont_surf = nb_cont_surf + 1
    endif
    if (nb_mast .ne. 0) then
        nb_cont_surf = nb_cont_surf + 1
    endif
    if (nb_slav .ne. 0) then
        nb_cont_surf = nb_cont_surf + 1
    endif
!
end subroutine

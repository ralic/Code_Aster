subroutine numoch(list_matr_elem, nb_matr_elem, list_ligr, nb_ligr)
!
implicit none
!
#include "asterfort/dismoi.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/as_allocate.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=24), intent(in) :: list_matr_elem(*)
    integer, intent(in) :: nb_matr_elem
    character(len=24), pointer, intent(out) :: list_ligr(:)
    integer, intent(out) :: nb_ligr
!
! --------------------------------------------------------------------------------------------------
!
! Factor
!
! Create list of LIGREL for numbering - For matr_elem
!
! --------------------------------------------------------------------------------------------------
!
! In  list_matr_elem : list of elementary matrixes
! In  nb_matr_elem   : number of elementary matrixes
! In  list_ligr      : pointer to list of LIGREL
! In  nb_ligr        : number of LIGREL in list
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: matr_elem, ligr_name, resu_elem
    integer :: nb_list_ligr, i_list_ligr
    integer :: i_matr_elem, i_resu_elem, iret, nb_subs, nb_resu_elem
    aster_logical :: l_found
    character(len=24), pointer :: resu_elem_noli(:) => null()
    character(len=24), pointer :: list_resu_elem(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_ligr      = 0
    nb_list_ligr = 2
    do i_matr_elem = 1, nb_matr_elem
        matr_elem = list_matr_elem(i_matr_elem)(1:19)
        call jeexin(matr_elem//'.RELR', iret)
        if (iret .ne. 0) then
            call jelira(matr_elem//'.RELR', 'LONUTI', nb_resu_elem)
            nb_list_ligr = nb_list_ligr + nb_resu_elem
        endif
    end do
!
! - Create object
!
    AS_ALLOCATE(vk24 = list_ligr, size = nb_list_ligr)
!
! - Set ligrel in object
!
    nb_ligr = 0
    do i_matr_elem = 1, nb_matr_elem
        matr_elem = list_matr_elem(i_matr_elem)(1:19)
!
! ----- Substructuration matrix
!
        call dismoi('NB_SS_ACTI', matr_elem, 'MATR_ELEM', repi=nb_subs)
        if (nb_subs .gt. 0) then
            call dismoi('NOM_MODELE', matr_elem, 'MATR_ELEM', repk=ligr_name)
            ligr_name = ligr_name(1:8)//'.MODELE'
            l_found   = .false.
            do i_list_ligr = 1, nb_ligr
                if (ligr_name .eq. list_ligr(i_list_ligr)) then
                    l_found   = .true.
                endif
            end do
            if (.not.l_found) then
                nb_ligr = nb_ligr + 1
                list_ligr(nb_ligr) = ligr_name
            endif
        endif
!
! ----- Standard matrix
!
        call jeexin(matr_elem//'.RELR', iret)
        if (iret .ne. 0) then
            call jeveuo(matr_elem//'.RELR', 'L', vk24 = list_resu_elem)
            call jelira(matr_elem//'.RELR', 'LONUTI', nb_resu_elem)
            do i_resu_elem = 1, nb_resu_elem
                resu_elem = list_resu_elem(i_resu_elem)(1:19)
                call jeexin(resu_elem//'.NOLI', iret)
                if (iret .ne. 0) then
                    call jeveuo(resu_elem//'.NOLI', 'L', vk24 = resu_elem_noli)
                    ligr_name = resu_elem_noli(1)(1:19)
                    l_found   = .false.
                    do i_list_ligr = 1, nb_ligr
                        if (ligr_name .eq. list_ligr(i_list_ligr)) then
                            l_found   = .true.
                        endif
                    end do
                    if (.not.l_found) then
                        nb_ligr = nb_ligr + 1
                        list_ligr(nb_ligr) = ligr_name
                     endif
                endif
            end do
        endif
    end do
!
end subroutine

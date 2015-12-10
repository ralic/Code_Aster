subroutine nmdoct(list_load, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/focste.h"
#include "asterfort/jeveuo.h"
#include "asterfort/liscad.h"
#include "asterfort/lisccr.h"
#include "asterfort/liscli.h"
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
    character(len=19), intent(in) :: list_load
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear algorithm - Initializations
!
! Prepare list of loads (and late elements) for contact
!
! --------------------------------------------------------------------------------------------------
!
! In  list_load        : list of loads
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_info_maxi =99
    character(len=24) :: list_info_type(nb_info_maxi)
    integer :: nb_load_init, nb_load_new, nb_info_type
    integer :: i_neum_lapl, i_load
    character(len=8) :: ligrel_link_slav, ligrel_link, ligrel_link_cont
    character(len=19) :: list_load_new
    character(len=24) :: lload_info
    character(len=8) :: load_name, load_func, func_const
    real(kind=8) :: coef
    integer, pointer :: v_load_info(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    list_load_new    = '&&NMDOCT.LISCHA'   
!
    if (ds_contact%l_contact) then
!
! ----- Read previous list of load
!
        lload_info = list_load(1:19)//'.INFC'
        call jeveuo(lload_info, 'L', vi = v_load_info)
        nb_load_init = v_load_info(1)
        nb_load_new  = nb_load_init
!
! ----- Prepare constant function
!
        func_const = '&&NMDOCT'
        coef       = 1.d0
        call focste(func_const, 'TOUTRESU', coef, 'V')
!
! ----- Add list of elements for slave surface (create in DEFI_CONTACT)
!
        if (ds_contact%l_elem_slav) then
            ligrel_link_slav = ds_contact%ligrel_elem_slav
            nb_load_new      = nb_load_new + 1
        endif
!
! ----- Add list of contact elements (create in MECA_NON_LINE)
!
        if (ds_contact%l_elem_cont) then
            ligrel_link_cont = ds_contact%ligrel_elem_cont(1:8)
            nb_load_new      = nb_load_new + 1
        endif
!
! ----- Add list of linear relation
!
        if (ds_contact%l_dof_rela) then
            ligrel_link = ds_contact%ligrel_dof_rela
            nb_load_new = nb_load_new + 1
        endif
!
! ----- Add LIGREL to list of loads
!
        if (nb_load_new .ne. nb_load_init) then
!
! --------- Create new datastructure
!
            call lisccr('MECA', list_load_new, nb_load_new, 'V')
!
! --------- Copy old datastructure in new one
!
            do i_load = 1, nb_load_init
                nb_info_type = nb_info_maxi
                call liscli(list_load, i_load, nb_info_maxi, list_info_type, load_name,&
                            load_func, nb_info_type, i_neum_lapl)
                call liscad('MECA'      , list_load_new , i_load, load_name, load_func, &
                            nb_info_type, list_info_type, i_neum_laplz = i_neum_lapl)
            end do
!
! --------- Add elements (slave)
!
            i_load = nb_load_init
            if (ds_contact%l_elem_slav) then
                ASSERT(ds_contact%l_elem_cont)
                i_load = i_load + 1
                call liscad('MECA'        ,list_load_new, i_load, ligrel_link_slav, func_const,&
                            info_typez = 'ELEM_TARDIF')
            endif
!
! --------- Add elements (contact)
!
            if (ds_contact%l_elem_cont) then
                i_load = i_load + 1
                call liscad('MECA'        ,list_load_new, i_load, ligrel_link_cont, func_const,&
                            info_typez = 'ELEM_TARDIF')
            endif
!
! --------- Add list of linear relations
!
            if (ds_contact%l_dof_rela) then
                i_load = i_load + 1
                call liscad('MECA'        ,list_load_new, i_load, ligrel_link, func_const,&
                            info_typez = 'DIRI_CSTE')
            endif
            ASSERT(i_load .eq. nb_load_new)
!
! --------- Copy and clean
!
            call lisccr('MECA', list_load, nb_load_new, 'V')
            call copisd(' ', 'V', list_load_new, list_load)
            call detrsd('LISTE_CHARGES', list_load_new)
        endif
    endif
!
end subroutine

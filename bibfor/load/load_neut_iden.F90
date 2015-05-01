subroutine load_neut_iden(nb_type_neumz, load_name, list_load_keyw, nb_load_exist_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/exisd.h"
#include "asterfort/jeexin.h"
#include "asterfort/load_neut_data.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: load_name
    integer, intent(in) :: nb_type_neumz
    aster_logical, intent(out) :: list_load_keyw(nb_type_neumz)
    integer, optional, intent(out) :: nb_load_exist_
!
! --------------------------------------------------------------------------------------------------
!
! Neumann loads computation - Thermic
!
! Identify type of loads present in load datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  load_name        : name of load
! In  nb_type_neumz    : maximum number of Neumann load type
! Out nb_load_exist    : number of loads existing in load datastructure
! Out list_load_keyw   : list of loads existing in load datastructure (AFFE_CHAR_THER keyword)
!                         
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_type_neum
    parameter (nb_type_neum = 10)
!
    character(len=24) :: load_keyw, obje_name
    character(len=19) :: cart_name(2)
    character(len=10) :: load_obje(2)
    integer :: nb_obje, nb_load_exist, i_obje, i_type_neum, iret
    aster_logical :: l_load_exist
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(nb_type_neumz.eq.nb_type_neum)
    list_load_keyw(1:nb_type_neumz) = .false.
    nb_load_exist = 0
!
    do i_type_neum = 1, nb_type_neum
        call load_neut_data(i_type_neum, nb_type_neum,&
                            load_obje_ = load_obje, load_keyw_ = load_keyw, nb_obje_ = nb_obje)
        ASSERT(nb_obje.le.2)
        ASSERT(nb_obje.ge.1)
        l_load_exist = nb_obje.gt.0
        do i_obje = 1, nb_obje
            if (load_keyw.eq.'EVOL_CHAR') then
                ASSERT(nb_obje.eq.1)
                obje_name = load_name(1:8)//'.CHTH'//load_obje(i_obje)
                call jeexin(obje_name, iret)
                l_load_exist = l_load_exist.and.(iret.ne.0)
            else
                cart_name = load_name(1:8)//'.CHTH'//load_obje(i_obje)
                call exisd('CHAMP_GD', cart_name(i_obje), iret)
                l_load_exist = l_load_exist.and.(iret.eq.1)
            endif
        end do
        if (l_load_exist) then
            nb_load_exist = nb_load_exist + 1
            list_load_keyw(i_type_neum) = .true.
        endif
    end do
!
    if (present(nb_load_exist_)) then
        nb_load_exist_ = nb_load_exist
    endif
!
end subroutine

subroutine comp_meca_name(nb_vari     , l_excl      , vari_excl   , l_kit_meta,&
                          comp_code_py, rela_code_py, meta_code_py,&
                          v_vari_name)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/lcinfo.h"
#include "asterc/lcvari.h"
#include "asterfort/assert.h"
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
    integer, intent(in) :: nb_vari
    aster_logical, intent(in) :: l_excl
    character(len=16), intent(in) :: vari_excl
    aster_logical, intent(in) :: l_kit_meta
    character(len=16), intent(in) :: comp_code_py
    character(len=16), intent(in) :: rela_code_py
    character(len=16), intent(in) :: meta_code_py
    character(len=16), pointer, intent(in) :: v_vari_name(:)
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Name of internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_vari          : number of internal variables 
! In  l_excl           : .true. if exception case (no names for internal variables)
! In  vari_excl        : name of internal variables if l_excl
! In  l_kit_meta       : .true. if metallurgy
! In  comp_code_py     : composite coded comportment (coding in Python)
! In  rela_code_py     : coded comportment for RELATION (coding in Python)
! In  meta_code_py     : coded comportment for metallurgy (coding in Python)
! In  v_vari_name      : pointer to names of internal variables
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_vari_meta, nb_vari_rela, idummy
    character(len=6) :: phas_name(10)
    character(len=8) :: rela_name(30)
    integer :: i_vari, i_vari_meta, i_vari_rela
!
! --------------------------------------------------------------------------------------------------
!
    if (l_excl) then
        v_vari_name(1:nb_vari) = vari_excl
    else
        if (l_kit_meta) then
            call lcinfo(meta_code_py, idummy, nb_vari_meta)
            call lcinfo(rela_code_py, idummy, nb_vari_rela)
            ASSERT(nb_vari_meta.le.10)
            ASSERT(nb_vari_rela.le.30)
            call lcvari(meta_code_py, nb_vari_meta, phas_name)
            call lcvari(rela_code_py, nb_vari_rela, rela_name)
            i_vari = 0
            do i_vari_meta = 1, nb_vari_meta
                do i_vari_rela = 1, nb_vari_rela
                    i_vari = i_vari + 1
                    v_vari_name(i_vari) = phas_name(i_vari_meta)//'##'//rela_name(i_vari_rela)
                enddo
            enddo
            do i_vari_rela = 1, nb_vari_rela
                i_vari = i_vari + 1
                v_vari_name(i_vari) = rela_name(i_vari_rela)
            enddo
            ASSERT(i_vari.eq.(nb_vari-1))
            v_vari_name(nb_vari) = 'INDIPLAS'
        else
            call lcvari(comp_code_py, nb_vari, v_vari_name)
        endif 
    endif
!
end subroutine

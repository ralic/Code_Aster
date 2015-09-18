subroutine asmari(list_func_acti, hval_meelem, nume_dof, list_load, ds_algopara,&
                  matr_rigi)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/asmatr.h"
#include "asterfort/assert.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmchex.h"
#include "asterfort/matr_asse_syme.h"
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
    integer, intent(in) :: list_func_acti(*)
    character(len=19), intent(in) :: hval_meelem(*)
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: list_load
    character(len=19), intent(in) :: matr_rigi
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Computation
!
! Assembling rigidity matrix
!
! --------------------------------------------------------------------------------------------------
!
! In  list_func_acti   : list of active functionnalities
! In  hval_meelem      : hat variable for elementary matrixes
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  list_load        : name of datastructure for list of loads
! In  ds_algopara      : datastructure for algorithm parameters
! In  matr_rigi        : name of rigidity matrix
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_matr_elem
    character(len=19) :: merigi, mediri, meeltc, meeltf
    character(len=19) :: list_matr_elem(8)
    aster_logical :: l_cont_elem, l_frot_elem, l_cont_all_verif
!
! --------------------------------------------------------------------------------------------------
!
    nb_matr_elem = 0
!
! - Active functionnalities
!
    l_cont_elem      = isfonc(list_func_acti,'ELT_CONTACT')
    l_frot_elem      = isfonc(list_func_acti,'ELT_FROTTEMENT')
    l_cont_all_verif = isfonc(list_func_acti,'CONT_ALL_VERIF')
!
! - Rigidity MATR_ELEM
!
    call nmchex(hval_meelem, 'MEELEM', 'MERIGI', merigi)
    nb_matr_elem = nb_matr_elem + 1
    list_matr_elem(nb_matr_elem) = merigi
!
! - Boundary conditions MATR_ELEM
!
    call nmchex(hval_meelem, 'MEELEM', 'MEDIRI', mediri)
    nb_matr_elem = nb_matr_elem + 1
    list_matr_elem(nb_matr_elem) = mediri
!
! - Contact/friction MATR_ELEM
!
    if (l_cont_elem) then
        if (.not.l_cont_all_verif) then
            call nmchex(hval_meelem, 'MEELEM', 'MEELTC', meeltc)
            nb_matr_elem = nb_matr_elem + 1
            list_matr_elem(nb_matr_elem) = meeltc
            if (l_frot_elem) then
                call nmchex(hval_meelem, 'MEELEM', 'MEELTF', meeltf)
                nb_matr_elem = nb_matr_elem + 1
                list_matr_elem(nb_matr_elem) = meeltf
            endif
        endif
    endif
!
! - Assembly MATR_ELEM
!
    ASSERT(nb_matr_elem.le.8)
    call asmatr(nb_matr_elem, list_matr_elem, ' ', nume_dof, &
                list_load, 'ZERO', 'V', 1, matr_rigi)
!
! - Symmetry of rigidity matrix
!
    if (ds_algopara%l_matr_rigi_syme) then
        call matr_asse_syme(matr_rigi)
    endif
!
end subroutine

subroutine romMultiParaChck(ds_multipara)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/dismoi.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(ROM_DS_MultiPara), intent(in) :: ds_multipara
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Check data for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_multipara     : datastructure for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: syme
    integer :: nb_matr, nb_vari_para, nb_vale_para
    integer :: i_matr, i_vari_para
    character(len=24) :: nume_dof_ref, nume_dof 
!
! --------------------------------------------------------------------------------------------------
!
    nb_matr      = ds_multipara%nb_matr
    nb_vari_para = ds_multipara%nb_vari_para
!
! - Check numbering
!
    if (ds_multipara%matr_name(1) .ne. ' ') then
        call dismoi('NOM_NUME_DDL', ds_multipara%matr_name(1), 'MATR_ASSE', repk=nume_dof_ref)
        do i_matr = 2, nb_matr
            call dismoi('NOM_NUME_DDL', ds_multipara%matr_name(i_matr), 'MATR_ASSE', repk=nume_dof)
            if (nume_dof .ne. nume_dof_ref) then
                call utmess('F','ROM2_21')
            endif
        end do
    endif
!
! - Only symmetric matrix
!
    if (ds_multipara%matr_name(1) .ne. ' ') then
        do i_matr = 1, nb_matr
            call dismoi('TYPE_MATRICE', ds_multipara%matr_name(i_matr), 'MATR_ASSE', repk=syme)
            if (syme .eq. 'NON_SYM') then
                call utmess('F','ROM2_22')
            endif
        end do
    endif
!
! - If no definition of variation => no functions !
!
    if (nb_vari_para .eq. 0) then
        do i_matr = 1, nb_matr
            if (ds_multipara%matr_coef(i_matr)%l_func) then
                call utmess('F','ROM2_25', sk = ds_multipara%matr_name(i_matr))
            endif
        end do
        if (ds_multipara%vect_coef%l_func) then
            call utmess('F','ROM2_31')
        endif
    endif
!
! - Same number of values for each parameter
!
    if (nb_vari_para .ne. 0) then
        nb_vale_para = ds_multipara%vari_para(1)%nb_vale_para
        do i_vari_para = 2, nb_vari_para
            if (ds_multipara%vari_para(i_vari_para)%nb_vale_para .ne. nb_vale_para) then
                call utmess('F','ROM2_29')
            endif
        end do
    endif 
!
end subroutine

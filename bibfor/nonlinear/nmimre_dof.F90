subroutine nmimre_dof(nume_dof , ds_conv  , vale_rela, vale_maxi     , vale_refe     ,&
                      vale_comp, vale_frot, vale_geom, ieq_rela      , ieq_maxi      ,&
                      ieq_refe , noddlm   , ieq_comp , name_node_frot, name_node_geom)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/impcmp.h"
#include "asterfort/impcom.h"
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
    character(len=24), intent(in) :: nume_dof
    type(NL_DS_Conv), intent(inout) :: ds_conv
    integer, intent(in) :: ieq_rela
    integer, intent(in) :: ieq_maxi
    integer, intent(in) :: ieq_refe
    integer, intent(in) :: ieq_comp
    real(kind=8), intent(in) :: vale_rela
    real(kind=8), intent(in) :: vale_maxi
    real(kind=8), intent(in) :: vale_refe
    real(kind=8), intent(in) :: vale_comp
    real(kind=8), intent(in) :: vale_frot
    real(kind=8), intent(in) :: vale_geom
    character(len=8), intent(in) :: noddlm
    character(len=16), intent(in) :: name_node_frot
    character(len=16), intent(in) :: name_node_geom
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Convergence management
!
! Save informations about residuals into convergence datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  nume_dof         : name of numbering (NUME_DDL)
! IO  ds_conv          : datastructure for convergence management
! In  ieq_rela         : number of equation where RESI_GLOB_RELA is maximum
! In  vale_rela        : value of RESI_GLOB_RELA
! In  ieq_maxi         : number of equation where RESI_GLOB_MAXI is maximum
! In  vale_maxi        : value of RESI_GLOB_MAXI
! In  ieq_refe         : number of equation where RESI_REFE_RELA is maximum
! In  vale_refe        : value of RESI_REFE_RELA
! In  ieq_comp         : number of equation where RESI_COMP_RELA is maximum
! In  vale_comp        : value of RESI_COMP_RELA
! In  vale_frot        : value of friction trigger (contact)
! In  vale_geom        : value of geometry trigger (contact)
! In  name_node_frot   : name of node where friction trigger is maximum (contact)
! In  name_node_geom   : name of node where geometry trigger is maximum (contact)
! In  noddlm           : name of component where RESI_COMP_RELA is maximum
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: name_dof_rela, name_dof_maxi, name_dof_refe, name_dof_comp
    integer :: i_resi, nb_resi
    real(kind=8) :: vale_calc
    character(len=16) :: locus_calc
    character(len=16) :: resi_type
!
! --------------------------------------------------------------------------------------------------
!
    nb_resi = ds_conv%nb_resi
!
! - Get names of dof where residuals is maximum
!
    call impcmp(ieq_rela, nume_dof, name_dof_rela)
    call impcmp(ieq_maxi, nume_dof, name_dof_maxi)
    call impcmp(ieq_refe, nume_dof, name_dof_refe)
    call impcom(ieq_comp, noddlm  , name_dof_comp)
!
! - Save into convergence datastructure
!
    do i_resi = 1, nb_resi
        resi_type  = ds_conv%list_resi(i_resi)%type
        locus_calc = ' '
        vale_calc  = r8vide()
        if (resi_type .eq. 'RESI_GLOB_RELA') then
            vale_calc  = vale_rela
            locus_calc = name_dof_rela
        else if (resi_type.eq.'RESI_GLOB_MAXI') then
            vale_calc  = vale_maxi
            locus_calc = name_dof_maxi
        else if (resi_type.eq.'RESI_REFE_RELA') then
            vale_calc  = vale_refe
            locus_calc = name_dof_refe
        else if (resi_type.eq.'RESI_COMP_RELA') then
            vale_calc  = vale_comp
            locus_calc = name_dof_comp
        else if (resi_type.eq.'RESI_FROT') then
            vale_calc  = vale_frot
            locus_calc = name_node_frot
        else if (resi_type.eq.'RESI_GEOM') then
            vale_calc  = vale_geom
            locus_calc = name_node_geom
        else
            ASSERT(.false.)
        endif
        ds_conv%list_resi(i_resi)%vale_calc  = vale_calc
        ds_conv%list_resi(i_resi)%locus_calc = locus_calc
    end do
!
end subroutine

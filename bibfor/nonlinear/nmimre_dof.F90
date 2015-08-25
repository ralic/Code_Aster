subroutine nmimre_dof(nume_dof , sdconv   , vale_rela, vale_maxi     , vale_refe     ,&
                      vale_comp, vale_frot, vale_geom, ieq_rela      , ieq_maxi      ,&
                      ieq_refe , noddlm   , ieq_comp , name_node_frot, name_node_geom)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/impcmp.h"
#include "asterfort/impcom.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
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
    character(len=24), intent(in) :: sdconv
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
! In  sdconv           : name of datastructure convergence
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
    character(len=24) :: sdconv_lieu, sdconv_vale, sdconv_name
    character(len=16) :: name_dof_rela, name_dof_maxi, name_dof_refe, name_dof_comp
    integer :: i_resi, nb_resi
    real(kind=8) :: vale
    character(len=16) :: dof
    character(len=9) :: resi_name
    character(len=16), pointer :: v_sdconv_lieu(:) => null()
    character(len=16), pointer :: v_sdconv_name(:) => null()
    real(kind=8), pointer :: v_sdconv_vale(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    sdconv_lieu = sdconv(1:19)//'.LIEU'
    sdconv_vale = sdconv(1:19)//'.VALE'
    sdconv_name = sdconv(1:19)//'.NAME'
    call jeveuo(sdconv_lieu, 'E', vk16 = v_sdconv_lieu)
    call jeveuo(sdconv_vale, 'E', vr   = v_sdconv_vale)
    call jeveuo(sdconv_name, 'L', vk16 = v_sdconv_name)
    call jelira(sdconv_name, 'LONMAX', ival=nb_resi)
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
        resi_name = v_sdconv_name(i_resi)(1:9)
        dof       = ' '
        vale      = r8vide()
        if (resi_name .eq. 'RESI_RELA') then
            vale = vale_rela
            dof  = name_dof_rela
        else if (resi_name.eq.'RESI_MAXI') then
            vale = vale_maxi
            dof  = name_dof_maxi
        else if (resi_name.eq.'RESI_REFE') then
            vale = vale_refe
            dof  = name_dof_refe
        else if (resi_name.eq.'RESI_COMP') then
            vale = vale_comp
            dof  = name_dof_comp
        else if (resi_name.eq.'FROT_NEWT') then
            vale = vale_frot
            dof  = name_node_frot
        else if (resi_name.eq.'GEOM_NEWT') then
            vale = vale_geom
            dof  = name_node_geom
        else
            ASSERT(.false.)
        endif
        v_sdconv_vale(i_resi) = vale
        v_sdconv_lieu(i_resi) = dof
    end do
!
end subroutine

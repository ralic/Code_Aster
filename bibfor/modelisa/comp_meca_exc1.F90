subroutine comp_meca_exc1(defo_comp, mult_comp, nb_vari_exte, l_kit_meta, l_cristal,&
                          l_exte_comp, nb_vari)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=16), intent(in) :: defo_comp
    character(len=16), intent(in) :: mult_comp
    integer, intent(in) :: nb_vari_exte
    aster_logical, intent(in) :: l_kit_meta
    aster_logical, intent(in) :: l_cristal
    aster_logical, intent(in) :: l_exte_comp
    integer, intent(inout) :: nb_vari
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Exception for number of internal variables - Return modified value of number of internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  defo_comp    : DEFORMATION comportment
! In  mult_comp    : *CRISTAL comportment
! In  nb_vari_exte : number of internal variable if external computing for comportment
! In  l_kit_meta   : .true. if metallurgy
! In  l_cristal    : .true. if *CRISTAL comportment
! In  l_exte_comp  : .true. if external computing for comportment
! I&O nb_vari      : number of internal variables
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: sdcomp
    integer :: nb_vari_cris
    integer, pointer :: cpri(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - KIT META
!
    if (l_kit_meta) then
        if (defo_comp .eq. 'SIMO_MIEHE') nb_vari = nb_vari + 1
        if (defo_comp .eq. 'GDEF_LOG') nb_vari = nb_vari + 6
    endif
!
! - Multi comportment (*CRISTAL)
!
    if (l_cristal) then
        sdcomp = mult_comp(1:8)
        call jeveuo(sdcomp//'.CPRI', 'L', vi=cpri)
        nb_vari_cris = cpri(3)
        nb_vari = nb_vari + nb_vari_cris
        if (defo_comp .eq. 'SIMO_MIEHE') nb_vari = nb_vari + 3 + 9
    endif
!
! - External comportment (UMAT/ZMAT/MFRONT)
!
    if (l_exte_comp) then
        nb_vari = nb_vari_exte + nb_vari
    endif
!
end subroutine

subroutine ddi_kit_nvar(rela_flua   , rela_plas   , rela_cpla   , rela_coup     , nb_vari_flua,&
                        nb_vari_plas, nb_vari_cpla, nb_vari_coup, nume_comp_plas)
!
implicit none
!
#include "jeveux.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterc/lcdiscard.h"
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
    character(len=16), intent(in) :: rela_flua
    character(len=16), intent(in) :: rela_plas
    character(len=16), intent(in) :: rela_cpla
    character(len=16), intent(in) :: rela_coup
    integer, intent(out) :: nb_vari_flua
    integer, intent(out) :: nb_vari_plas
    integer, intent(out) :: nb_vari_cpla
    integer, intent(out) :: nb_vari_coup
    integer, intent(out) :: nume_comp_plas
!
! --------------------------------------------------------------------------------------------------
!
! KIT_DDI
!
! Number of internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_flua        : relation for creeping
! In  rela_plas        : relation for plasticity
! In  rela_cpla        : relation for plane stress (GLRC)
! In  rela_coup        : relation for coupling (GLRC)
! Out nb_vari_flua     : number of internal variables for creeping
! Out nb_vari_plas     : number of internal variables for plasticity
! Out nb_vari_cpla     : number of internal variables for plane stress
! Out nb_vari_coup     : number of internal variables for coupling
! Out nume_comp_plas   : number LCxxxx subroutine for plasticity
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: rela_py
    integer :: ibid
!
! --------------------------------------------------------------------------------------------------
!
    nb_vari_flua   = 0
    nb_vari_plas   = 0
    nb_vari_cpla   = 0
    nb_vari_coup   = 0
    nume_comp_plas = 0
    if (rela_flua .ne. ' ') then
        call lccree(1, rela_flua, rela_py)
        call lcinfo(rela_py, ibid, nb_vari_flua)
        call lcdiscard(rela_py)
    endif
    if (rela_plas .ne. ' ') then
        call lccree(1, rela_plas, rela_py)
        call lcinfo(rela_py, nume_comp_plas, nb_vari_plas)
        call lcdiscard(rela_py)
    endif
    if (rela_cpla .ne. ' ') then
        call lccree(1, rela_cpla, rela_py)
        call lcinfo(rela_py, ibid, nb_vari_cpla)
        call lcdiscard(rela_py)
    endif
    if (rela_coup .ne. ' ') then
        call lccree(1, rela_coup, rela_py)
        call lcinfo(rela_py, ibid, nb_vari_coup)
        call lcdiscard(rela_py)
    endif
!
end subroutine

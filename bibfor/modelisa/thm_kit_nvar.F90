subroutine thm_kit_nvar(rela_thmc, rela_hydr, rela_meca, rela_ther, nb_vari_thmc, &
                        nb_vari_hydr, nb_vari_meca, nb_vari_ther)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
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
! person_in_charge: sylvie.granet at edf.fr
!
    character(len=16), intent(in) :: rela_thmc
    character(len=16), intent(in) :: rela_hydr
    character(len=16), intent(in) :: rela_meca
    character(len=16), intent(in) :: rela_ther
    integer, intent(out) :: nb_vari_thmc
    integer, intent(out) :: nb_vari_hydr
    integer, intent(out) :: nb_vari_meca
    integer, intent(out) :: nb_vari_ther
!
! --------------------------------------------------------------------------------------------------
!
! THM
!
! Number of internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_thmc    : relation for coupling
! In  rela_hydr    : relation for hydraulic
! In  rela_meca    : relation for mechanic
! In  rela_ther    : relation for thermic
! Out nb_vari_thmc : number of internal variables for coupling
! Out nb_vari_hydr : number of internal variables for hydraulic
! Out nb_vari_meca : number of internal variables for mechanic
! Out nb_vari_ther : number of internal variables for thermic
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: rela_thmc_py, rela_ther_py, rela_hydr_py, rela_meca_py
    integer :: ibid
!
! --------------------------------------------------------------------------------------------------
!
    nb_vari_thmc = 0
    nb_vari_ther = 0
    nb_vari_hydr = 0
    nb_vari_meca = 0
!
        call lccree(1, rela_thmc, rela_thmc_py)
        call lcinfo(rela_thmc_py, ibid, nb_vari_thmc)


        call lccree(1, rela_ther, rela_ther_py)
        call lcinfo(rela_ther_py, ibid, nb_vari_ther)


        call lccree(1, rela_hydr, rela_hydr_py)
        call lcinfo(rela_hydr_py, ibid, nb_vari_hydr)


        call lccree(1, rela_meca, rela_meca_py)
        call lcinfo(rela_meca_py, ibid, nb_vari_meca)
!
end subroutine

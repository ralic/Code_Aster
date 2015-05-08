subroutine cg_kit_nvar(rela_cg, nb_vari_cg)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterc/lcdiscard.h"
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
!
    character(len=16), intent(in) :: rela_cg(2)
    integer, intent(out) :: nb_vari_cg(2)
!
! --------------------------------------------------------------------------------------------------
!
! KIT_CG
!
! Number of internal variables
!
! --------------------------------------------------------------------------------------------------
!
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: rela_py
    integer :: ibid
!
! --------------------------------------------------------------------------------------------------
!
    nb_vari_cg(1) = 0
    nb_vari_cg(2) = 0
!
    call lccree(1, rela_cg(1), rela_py)
    call lcinfo(rela_py, ibid, nb_vari_cg(1))
    call lcdiscard(rela_py)
    call lccree(1, rela_cg(2), rela_py)
    call lcinfo(rela_py, ibid, nb_vari_cg(2))
    call lcdiscard(rela_py)
!
end subroutine

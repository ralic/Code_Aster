subroutine meta_kit_nvar(rela_meta, nb_vari_meta)
!
implicit none
!
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
    character(len=16), intent(in) :: rela_meta
    integer, intent(out) :: nb_vari_meta
!
! --------------------------------------------------------------------------------------------------
!
! META_*
!
! Number of internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_meta        : relations for META
! Out nb_vari_meta     : number of internal variables for META
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: rela_meta_py
    integer :: idummy
!
! --------------------------------------------------------------------------------------------------
!
    nb_vari_meta = 0
    call lccree(1, rela_meta, rela_meta_py)
    call lcinfo(rela_meta_py, idummy, nb_vari_meta)
    call lcdiscard(rela_meta_py)
!
end subroutine

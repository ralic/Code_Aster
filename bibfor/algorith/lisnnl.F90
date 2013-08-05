subroutine lisnnl(phenoz, charge, prefob)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit      none
#include "asterfort/assert.h"
    character(len=*) :: phenoz
    character(len=8) :: charge
    character(len=13) :: prefob
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! NOM DU PREFIXE DE L'OBJET DE LA CHARGE
!
! ----------------------------------------------------------------------
!
! IN  PHENOM : TYPE DE PHENOMENE (MECANIQUE, THERMIQUE, ACOUSTIQUE)
! IN  CHARGE : NOM DE LA CHARGE (AFFE_CHAR_*)
! OUT PREFOB : PREFIXE DE L'OBJET DE LA CHARGE
!
! ----------------------------------------------------------------------
!
    character(len=16) :: phenom
!
! ----------------------------------------------------------------------
!
    phenom = phenoz
    if (phenom .eq. 'MECANIQUE') then
        prefob = charge(1:8)//'.CHME'
    else if (phenom.eq.'THERMIQUE') then
        prefob = charge(1:8)//'.CHTH'
    else if (phenom.eq.'ACOUSTIQUE') then
        prefob = charge(1:8)//'.CHAC'
    else
        ASSERT(.false.)
    endif
!
end subroutine

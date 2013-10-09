function xpheop(modele)
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
!
    implicit none
    character(len=9) :: xpheop
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
    character(len=8) :: modele
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE
!
! IN  : MODELE : NOM DU MODELE
! OUT : XPHEOP : NOM DE L'OPTION SELON LE PHENOMENE
!                -> 'FULL_MECA' POUR LA MECANIQUE
!                -> 'RIGI_THER' POUR LA THERMIQUE
! ----------------------------------------------------------------------
!
    character(len=24) :: pheno
!
    call dismoi('PHENOMENE', modele, 'MODELE', repk=pheno)
    if (pheno .eq. 'MECANIQUE') then
        xpheop = 'FULL_MECA'
    else if (pheno.eq.'THERMIQUE') then
        xpheop = 'RIGI_THER'
    else
        ASSERT(.false.)
    endif
!
end function

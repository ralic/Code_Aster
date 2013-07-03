subroutine lisnca(phenoz, charge, genchz, carte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
#include "asterfort/assert.h"
#include "asterfort/lisdef.h"
#include "asterfort/lisnnl.h"
    character(len=*) :: genchz, phenoz
    character(len=8) :: charge
    character(len=19) :: carte
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! NOM DE LA CARTE ASSOCIEE A LA CHARGE
!
! ----------------------------------------------------------------------
!
! IN  PHENOM : TYPE DE PHENOMENE (MECANIQUE, THERMIQUE, ACOUSTIQUE)
! IN  CHARGE : NOM DE LA CHARGE (AFFE_CHAR_*)
! IN  GENCHA : GENRE DE LA CHARGE (VOIR LISDEF)
! OUT CARTE  : NOM DE LA CARTE ASSOCIEE
!
! ----------------------------------------------------------------------
!
    character(len=6) :: nomcar
    integer :: ibid, itypob
    character(len=13) :: prefob
!
! ----------------------------------------------------------------------
!
    call lisnnl(phenoz, charge, prefob)
    call lisdef('CART', genchz, ibid, nomcar, itypob)
    call assert(itypob.eq.1)
    carte = prefob(1:13)//nomcar(1:6)
end subroutine

subroutine dfftan(ndim, baslo, inoff, vtan)
!
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dffdir.h"
#include "asterfort/dffnor.h"
#include "asterfort/provec.h"
    real(kind=8) :: baslo(*), vtan(3)
    integer :: ndim, inoff
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
! FONCTION REALISEE (OPERATEURS DEFI_FOND_FISS, CALC_G) :
!
!      RETOURNE LE VECTEUR TANGENT AU FOND DE FISSURE (3EME VECTEUR DE
!      LA BASE LOCALE EN FOND DE FISSURE) EN UN NOEUD
!
! IN
!   NDIM   : DIMENSION DU MAILLAGE
!   BASLO  : VALEURS DE LA BASE LOCALE EN FOND DE FISSURE
!   INOFF  : INDICE LOCAL DU NOEUD DE LA BASE DEMANDE
!
! OUT
!   VTAN   : VALEURS DU TANGENT AU FOND DE FISSURE EN CE NOEUD
!
!-----------------------------------------------------------------------
!
    real(kind=8) :: vdir(3), vnor(3)
!
!-----------------------------------------------------------------------
!
    ASSERT((ndim.eq.2).or.(ndim.eq.3))
!
    if (ndim .eq. 3) then
!
        call dffdir(ndim, baslo, inoff, vdir)
!
        call dffnor(ndim, baslo, inoff, vnor)
!
        call provec(vdir, vnor, vtan)
!
    else if (ndim.eq.2) then
!
        vtan(1) = 0.d0
        vtan(2) = 0.d0
        vtan(3) = 1.d0
!
    endif
!
end subroutine

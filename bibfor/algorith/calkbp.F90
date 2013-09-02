subroutine calkbp(nno, ndim, w, dff1, kbp)
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
! person_in_charge: sebastien.fayolle at edf.fr
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/r8inir.h"
    integer :: ndim, nno
    real(kind=8) :: w, dff1(nno, ndim)
    real(kind=8) :: kbp(ndim, nno)
!-----------------------------------------------------------------------
!     BUT:  CALCUL LE TERME DE COUPLAGE KBP
!     ON UTILISE UNE FORMULATION AVEC UN SEULE POINT DE GAUSS
!     SITUE AU BARYCENTRE DE L ELEMENT ET DONT LE POIDS EST EGAL
!-----------------------------------------------------------------------
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  W      : POIDS DU POINT DE GAUSS
! IN  DFF1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! OUT KBP    : MATRICE KBP
!-----------------------------------------------------------------------
!
    integer :: ia, na
    real(kind=8) :: pbulle
!-----------------------------------------------------------------------
!
!
! - INITIALISATION
    call r8inir(nno*ndim, 0.d0, kbp, 1)
!
    if (ndim .eq. 3) then
        pbulle = 4.d0
    else if (ndim .eq. 2) then
        pbulle = 3.d0
    else
        ASSERT(.false.)
    endif
!
! - TERME KBP
! - BOUCLE SUR LES NOEUDS DE PRESSION
    do 100 na = 1, nno
! - BOUCLE SUR LA DIMENSION
        do 99 ia = 1, ndim
            kbp(ia,na) = - w/pbulle*dff1(na,ia)
99      continue
100  end do
!
end subroutine

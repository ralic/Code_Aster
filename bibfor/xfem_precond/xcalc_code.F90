function xcalc_code(nfiss, he_inte, he_real)
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!-----------------------------------------------------------------------
! BUT : CALCULER UN INDICE DE DOMAINE :
!       CONSTRUIRE UNE INJECTION P-UPLET <HE> => ENTIER <ID>
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!   - NFISS  : TAILLE DU P-UPLET
!   - HE     : P-UPLET DE SIGNE HEAVISIDE
!   - ID     : ENTIER A CALCULER
!
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/assert.h"
!-----------------------------------------------------------------------
    integer :: nfiss, xcalc_code
    integer, optional :: he_inte(*)
    real(kind=8), optional :: he_real(*)
!-----------------------------------------------------------------------
    integer :: base_codage
    parameter (base_codage=4)
    integer :: ifiss
!-----------------------------------------------------------------------
!
    ASSERT(nfiss.le.4)
!
    ASSERT(.not.(present(he_inte).and.present(he_real)))
!
    xcalc_code=0
    if ( present(he_inte) ) then
      do ifiss=1, nfiss
        xcalc_code=xcalc_code + (he_inte(ifiss)+2)*base_codage**(nfiss-ifiss)
      enddo
    elseif (present(he_real) ) then
      do ifiss=1, nfiss
        xcalc_code=xcalc_code + (int(he_real(ifiss))+2)*base_codage**(nfiss-ifiss)
      enddo
    else
      ASSERT(.false.)
    endif
!
end function

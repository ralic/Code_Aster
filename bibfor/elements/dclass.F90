subroutine dclass(n, tab, iran)
!
    implicit none
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!======================================================================
!
!     EVALUE L ORDRE DES RACINES DANS IRAN
!     TRI PAR UNE METHODE SOMMAIRE
!     A NE PAS UTILISER AVEC DE GROS TABLEAUX
!
! IN  N : NOMBRE DE RACINE DU POLYNOME
! IN  TAB : TABLEAU DES RACINES DU POLYNOME
!
! OUT IRAN : RANG DE LA RACINE
!
#include "asterfort/dclsma.h"
    integer :: n, iran(*)
!
    real(kind=8) :: tab(*)
!
    if (n .le. 1) then
        iran(1) = 1
    else
        call dclsma(n, tab, iran)
    endif
!
end subroutine

subroutine modthm(nomte, modint)
    implicit      none
#include "asterfort/lxlgut.h"
    character(len=3) :: modint
    character(len=16) :: nomte
! =====================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =====================================================================
! --- DETERMINATION DES MODELISATIONS LUMPEE, NON LUMPEE OU MIXTE -----
! =====================================================================
    integer :: dimete
! =====================================================================
!
!     REPERAGE DE LA LONGUEUR UTILE DE LA CHAINE 'NOMTE'
!
    dimete = lxlgut(nomte)
!
!     SI ON EST DANS UNE MODELISATION PERMANENTE, LA FIN EST '_P'
!     ON DOIT DONC TESTER SUR L'ANTEPENULTIEME CARACTERE
!
    if (nomte(dimete-1:dimete) .eq. '_P') then
        dimete = dimete - 2
    endif
!
!     REPERAGE DU MODE D'INTEGRATION
!
    if (nomte(dimete:dimete) .eq. 'D') then
        modint = 'LUM'
    else if (nomte(dimete:dimete).eq.'S') then
        modint = 'RED'
    else
        modint = 'CLA'
    endif
! =====================================================================
end subroutine

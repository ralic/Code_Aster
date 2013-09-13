subroutine mdnoma(nomamd, lnomam, nomast, codret)
!_____________________________________________________________________
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!        FORMAT MED : ELABORATION D'UN NOM DE MAILLAGE DANS LE FICHIER
!               - -                    --     --
!_____________________________________________________________________
!
!   LA REGLE EST LA SUIVANTE :
!
!     LE NOM EST LIMITE A 64 CARACTERES DANS MED. ON UTILISE ICI
!     AU MAXIMUM 8 CARACTERES.
!                 12345678901234567890123456789012
!                 AAAAAAAA
!       AAAAAAAA : NOM DU MAILLAGE DANS ASTER
!
!
!     SORTIES :
!       NOMAMD : NOM DU MAILLAGE DANS LE FICHIER MED
!       LNOMAM : LONGUEUR UTILE DU NOM DU MAILLAGE DANS LE FICHIER MED
!       CODRET : CODE DE RETOUR DE L'OPERATION
!                0 --> TOUT VA BIEN
!                1 --> LA DECLARATION DU NOM DE L'OBJET NE CONVIENT PAS
!               10 --> AUTRE PROBLEME
!    ENTREES :
!       NOMAST : NOM DU MAILLAGE ASTER
!_____________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
    character(len=64) :: nomamd
    character(len=8) :: nomast
!
    integer :: lnomam
    integer :: codret
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
!
!
    integer :: iaux
!
!====
! 1. PREALABLES
!====
!
    codret = 0
!
!====
! 2. CREATION DU NOM
!====
!
    if (codret .eq. 0) then
!
! 2.1. ==> BLANCHIMENT INITIAL
!               12345678901234567890123456789012
        nomamd = '                                '//'                          '
!
! 2.2. ==> NOM DU MAILLAGE
!
        iaux = lxlgut(nomast)
        ASSERT(iaux.ge.1 .and. iaux.le.8)
        nomamd(1:iaux) = nomast(1:iaux)
        lnomam = iaux
!
    endif
!
!====
! 3. BILAN
!====
!
    if (codret .ne. 0) then
        call utmess('E', 'MED_62')
    endif
!
end subroutine

subroutine ingrma(sdmail, nomma, lgrma, nbgrma, codret)
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
!-----------------------------------------------------------------------
!     RETOURNE LA LISTE DES GROUPES DE MAILLES CONTENANT UNE MAILLE
!     PARTICULIERE DONT ON DONNE LE NOM OU LE NUMERO
!-----------------------------------------------------------------------
!     ENTREES:
!        SDMAIL : NOM DE LA SD MAILLAGE
!        NOMMA  : NOM DE LA MAILLE
!     SORTIES:
!        LGRMA  : ADR DU TABLEAU DES GROUP_MA CONTENANT LA MAILLE
!        NBGRMA : NBRE DE GROUPES DANS CETTE LISTE
!        CODRET : 0 SI OK, <>0 SI ERREUR
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
    character(len=8) :: sdmail, nomma
    integer :: lgrma(*), nbgrma, codret
!
! 0.2. ==> JEVEUX
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=24) :: nommai, grpmai
    character(len=1) :: k1b
    integer :: i, j, ier, num, nbg, nbmag, jgrma
!
!====
! 1. PREALABLES
!====
!
! 1.1. ==> INITIALISATIONS
!
    codret = 0
    nbgrma = 0
    nommai = sdmail//'.NOMMAI         '
    grpmai = sdmail//'.GROUPEMA       '
!
! 1.2. ==> VERIFICATIONS
!
    call jeexin(nommai, ier)
    ASSERT(ier.ne.0)
!
    call jeexin(grpmai, ier)
    ASSERT(ier.ne.0)
!
    num = 0
    call jenonu(jexnom(nommai, nomma), num)
    ASSERT(num.ne.0)
!
!====
! 2. BOUCLE SUR LES GROUP_MA
!====
!
    call jelira(grpmai, 'NOMUTI', nbg, k1b)
    do 200 i = 1, nbg
        call jeveuo(jexnum(grpmai, i), 'L', jgrma)
        call jelira(jexnum(grpmai, i), 'LONUTI', nbmag, k1b)
!     --- BCLE SUR LES MAILLES DU GROUP_MA
        do 210 j = 1, nbmag
            if (zi(jgrma-1+j) .eq. num) then
                nbgrma = nbgrma + 1
                lgrma(nbgrma) = i
            endif
210      continue
200  end do
!
!====
! 99. SORTIE
!====
!
!
end subroutine

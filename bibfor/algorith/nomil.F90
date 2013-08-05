function nomil(typma, ia)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/u2mesk.h"
    character(len=8) :: typma
    integer :: nomil, ia
!
!-----------------------------------------------------------------------
!     BUT: RETOURNE LE NUMÉRO LOCAL DU NOEUD MILIEU PORTÉ SUR L'ARETE
!                   D'UNE MAILLE QUADRATIQUE
!
! ARGUMENTS D'ENTRÉE:
!      TYPMA     : TYPE DE LA MAILLE
!      IA        : NUMÉRO LOCAL DE L'ARETE
!
!-----------------------------------------------------------------------
!
!
! --- VARIABLES
!
    integer :: tab(12)
!
    ASSERT(ia.gt.0)
!
    if (typma .eq. 'HEXA20') then
        ASSERT(ia.le.12)
        tab(1)=9
        tab(2)=10
        tab(3)=11
        tab(4)=12
        tab(5)=17
        tab(6)=18
        tab(7)=19
        tab(8)=20
        tab(9)=13
        tab(10)=14
        tab(11)=15
        tab(12)=16
    else if (typma.eq.'PENTA15') then
        ASSERT(ia.le.9)
        tab(1)=7
        tab(2)=8
        tab(3)=9
        tab(4)=13
        tab(5)=14
        tab(6)=15
        tab(7)=10
        tab(8)=11
        tab(9)=12
    else if (typma.eq.'PYRAM13') then
        ASSERT(ia.le.8)
        tab(1)=6
        tab(2)=7
        tab(3)=8
        tab(4)=9
        tab(5)=10
        tab(6)=11
        tab(7)=12
        tab(8)=13
    else if (typma.eq.'TETRA10') then
        ASSERT(ia.le.6)
        tab(1)=5
        tab(2)=7
        tab(3)=8
        tab(4)=6
        tab(5)=9
        tab(6)=10
    else if (typma.eq.'QUAD8') then
        ASSERT(ia.le.4)
        tab(1)=5
        tab(2)=6
        tab(3)=7
        tab(4)=8
    else if (typma.eq.'TRIA6') then
        ASSERT(ia.le.3)
        tab(1)=4
        tab(2)=5
        tab(3)=6
    else
        call u2mesk('F', 'ALGORITH8_92', 1, typma)
    endif
!
    nomil=tab(ia)
!
end function

subroutine conare(typma, ar, nbar)
    implicit none
#include "asterfort/utmess.h"
    integer :: ar(12, 3), nbar
    character(len=8) :: typma
! ----------------------------------------------------------------------
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
!                       RENVOIE LA MATRICE DE CONNECTIVITÉ DES
!                       ARETES D'UNE MAILLE DE TYPE TYPMA
!
!    ENTREE :
!              TYPMA : TYPE DE LA MAILLE (TYPE_MAILLE)
!
!    SORTIE :
!              AR   : MATRICE DE CONNECTIVITÉ DES ARETES
!              NBAR : NOMBRE D'ARETES
!......................................................................
!
    integer :: i, j
!......................................................................
!
    do 100 i = 1, 12
        do 110 j = 1, 3
            ar(i,j)=0
110      continue
100  end do
!
    if (typma .eq. 'HEXA8') then
        nbar=12
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE HEXA8
        ar(1,1)=1
        ar(1,2)=2
        ar(2,1)=2
        ar(2,2)=3
        ar(3,1)=3
        ar(3,2)=4
        ar(4,1)=4
        ar(4,2)=1
        ar(5,1)=5
        ar(5,2)=6
        ar(6,1)=6
        ar(6,2)=7
        ar(7,1)=7
        ar(7,2)=8
        ar(8,1)=8
        ar(8,2)=5
        ar(9,1)=1
        ar(9,2)=5
        ar(10,1)=2
        ar(10,2)=6
        ar(11,1)=3
        ar(11,2)=7
        ar(12,1)=4
        ar(12,2)=8
    else if (typma.eq.'HEXA20') then
        nbar=12
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE HEXA20
        ar(1,1)=1
        ar(1,2)=2
        ar(1,3)=9
        ar(2,1)=2
        ar(2,2)=3
        ar(2,3)=10
        ar(3,1)=3
        ar(3,2)=4
        ar(3,3)=11
        ar(4,1)=4
        ar(4,2)=1
        ar(4,3)=12
        ar(5,1)=5
        ar(5,2)=6
        ar(5,3)=17
        ar(6,1)=6
        ar(6,2)=7
        ar(6,3)=18
        ar(7,1)=7
        ar(7,2)=8
        ar(7,3)=19
        ar(8,1)=8
        ar(8,2)=5
        ar(8,3)=20
        ar(9,1)=1
        ar(9,2)=5
        ar(9,3)=13
        ar(10,1)=2
        ar(10,2)=6
        ar(10,3)=14
        ar(11,1)=3
        ar(11,2)=7
        ar(11,3)=15
        ar(12,1)=4
        ar(12,2)=8
        ar(12,3)=16
    else if (typma.eq.'PENTA6') then
        nbar=9
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE PENTA6 OU PENTA15
        ar(1,1)=1
        ar(1,2)=2
        ar(2,1)=2
        ar(2,2)=3
        ar(3,1)=3
        ar(3,2)=1
        ar(4,1)=4
        ar(4,2)=5
        ar(5,1)=5
        ar(5,2)=6
        ar(6,1)=6
        ar(6,2)=4
        ar(7,1)=1
        ar(7,2)=4
        ar(8,1)=2
        ar(8,2)=5
        ar(9,1)=3
        ar(9,2)=6
    else if (typma.eq.'PENTA15') then
        nbar=9
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE PENTA6 OU PENTA15
        ar(1,1)=1
        ar(1,2)=2
        ar(1,3)=7
        ar(2,1)=2
        ar(2,2)=3
        ar(2,3)=8
        ar(3,1)=3
        ar(3,2)=1
        ar(3,3)=9
        ar(4,1)=4
        ar(4,2)=5
        ar(4,3)=13
        ar(5,1)=5
        ar(5,2)=6
        ar(5,3)=14
        ar(6,1)=6
        ar(6,2)=4
        ar(6,3)=15
        ar(7,1)=1
        ar(7,2)=4
        ar(7,3)=10
        ar(8,1)=2
        ar(8,2)=5
        ar(8,3)=11
        ar(9,1)=3
        ar(9,2)=6
        ar(9,3)=12
    else if (typma.eq.'PYRAM5') then
        nbar=8
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE PYRAM5 OU PYRAM13
        ar(1,1)=1
        ar(1,2)=2
        ar(2,1)=2
        ar(2,2)=3
        ar(3,1)=3
        ar(3,2)=4
        ar(4,1)=1
        ar(4,2)=4
        ar(5,1)=1
        ar(5,2)=5
        ar(6,1)=2
        ar(6,2)=5
        ar(7,1)=3
        ar(7,2)=5
        ar(8,1)=4
        ar(8,2)=5
    else if (typma.eq.'PYRAM13') then
        nbar=8
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE PYRAM5 OU PYRAM13
        ar(1,1)=1
        ar(1,2)=2
        ar(1,3)=6
        ar(2,1)=2
        ar(2,2)=3
        ar(2,3)=7
        ar(3,1)=3
        ar(3,2)=4
        ar(3,3)=8
        ar(4,1)=1
        ar(4,2)=4
        ar(4,3)=9
        ar(5,1)=1
        ar(5,2)=5
        ar(5,3)=10
        ar(6,1)=2
        ar(6,2)=5
        ar(6,3)=11
        ar(7,1)=3
        ar(7,2)=5
        ar(7,3)=12
        ar(8,1)=4
        ar(8,2)=5
        ar(8,3)=13
    else if (typma.eq.'TETRA4') then
        nbar=6
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE TETRA4 OU TETRA10
        ar(1,1)=1
        ar(1,2)=2
        ar(2,1)=1
        ar(2,2)=3
        ar(3,1)=1
        ar(3,2)=4
        ar(4,1)=2
        ar(4,2)=3
        ar(5,1)=2
        ar(5,2)=4
        ar(6,1)=3
        ar(6,2)=4
    else if (typma.eq.'TETRA10') then
        nbar=6
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE TETRA4 OU TETRA10
        ar(1,1)=1
        ar(1,2)=2
        ar(1,3)=5
        ar(2,1)=1
        ar(2,2)=3
        ar(2,3)=7
        ar(3,1)=1
        ar(3,2)=4
        ar(3,3)=8
        ar(4,1)=2
        ar(4,2)=3
        ar(4,3)=6
        ar(5,1)=2
        ar(5,2)=4
        ar(5,3)=9
        ar(6,1)=3
        ar(6,2)=4
        ar(6,3)=10
    else if (typma.eq.'QUAD4') then
        nbar=4
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE QUAD4
        ar(1,1)=1
        ar(1,2)=2
        ar(2,1)=2
        ar(2,2)=3
        ar(3,1)=3
        ar(3,2)=4
        ar(4,1)=4
        ar(4,2)=1
    else if (typma.eq.'QUAD8') then
        nbar=4
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE QUAD8
        ar(1,1)=1
        ar(1,2)=2
        ar(1,3)=5
        ar(2,1)=2
        ar(2,2)=3
        ar(2,3)=6
        ar(3,1)=3
        ar(3,2)=4
        ar(3,3)=7
        ar(4,1)=4
        ar(4,2)=1
        ar(4,3)=8
    else if (typma.eq.'TRIA3') then
        nbar=3
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE TRIA3
        ar(1,1)=1
        ar(1,2)=2
        ar(2,1)=2
        ar(2,2)=3
        ar(3,1)=3
        ar(3,2)=1
    else if (typma.eq.'TRIA6') then
        nbar=3
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE TRIA6
        ar(1,1)=1
        ar(1,2)=2
        ar(1,3)=4
        ar(2,1)=2
        ar(2,2)=3
        ar(2,3)=5
        ar(3,1)=3
        ar(3,2)=1
        ar(3,3)=6
    else if (typma.eq.'SEG2') then
        nbar=1
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE SEG2
        ar(1,1)=1
        ar(1,2)=2
    else if (typma.eq.'SEG3') then
        nbar=1
!       CONNECTIVITÉ DES ARETES POUR UNE MAILLE SEG3
        ar(1,1)=1
        ar(1,2)=2
        ar(1,3)=3
    else if (typma.eq.'POI1') then
        nbar=0
    else
        call utmess('F', 'ALGORITH2_23', sk=typma)
    endif
!
end subroutine

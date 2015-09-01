subroutine confac(typma, ft, nbft, f, nbf, quad)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/utmess.h"
    integer :: ft(12, 3), nbft, f(6, 8), nbf
    character(len=8) :: typma
    aster_logical, intent(in), optional :: quad
!     ------------------------------------------------------------------
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
! person_in_charge: samuel.geniaut at edf.fr
!
!    RENVOIE LA MATRICE DE CONNECTIVITÉ :
!                   1) DES FACES TRIANGULARISEES D'UNE MAILLE
!                   2) DES FACES (USUELLES) D'UNE MAILLE
!
!    (REMARQUE : POUR UN TETRA, CES 2 MATRICES SONT IDENTIQUES)
!
!
!    ENTREE :
!              TYPMA : TYPE DE LA MAILLE
!              QUAD  : ON DEMANDE LES CONNECTIVITES DES FACES EN QUADRATIQUE
!
!    SORTIE :
!              FT   : MATRICE DE CONNECTIVITÉ DES FACES TRIANGULARISEES
!              NBFT : NOMBRE DE FACES TRIANGULARISEES
!              F    : MATRICE DE CONNECTIVITÉ DES FACES
!              NBF  : NOMBRE DE FACES
!     ------------------------------------------------------------------
!
    integer :: i, j
    aster_logical :: milieu
! ----------------------------------------------------------------------
!
    do i = 1, 12
        do j = 1, 3
            ft(i,j)=0
        end do
    end do
    do i = 1, 6
        do j = 1, 8
            f(i,j)=0
        end do
    end do
    milieu = .false.
    if (present(quad)) milieu = .true.
!
    if (typma(1:4) .eq. 'HEXA') then
        nbft=12
!       CONNECTIVITÉ DES FACES TRIANGLES POUR UNE MAILLE HEXA8 OU HEXA20
        ft(1,1)=1
        ft(1,2)=2
        ft(1,3)=3
        ft(2,1)=3
        ft(2,2)=4
        ft(2,3)=1
        ft(3,1)=1
        ft(3,2)=2
        ft(3,3)=6
        ft(4,1)=6
        ft(4,2)=5
        ft(4,3)=1
        ft(5,1)=3
        ft(5,2)=4
        ft(5,3)=8
        ft(6,1)=8
        ft(6,2)=7
        ft(6,3)=3
        ft(7,1)=5
        ft(7,2)=6
        ft(7,3)=7
        ft(8,1)=7
        ft(8,2)=8
        ft(8,3)=5
        ft(9,1)=1
        ft(9,2)=4
        ft(9,3)=8
        ft(10,1)=8
        ft(10,2)=5
        ft(10,3)=1
        ft(11,1)=2
        ft(11,2)=3
        ft(11,3)=7
        ft(12,1)=7
        ft(12,2)=6
        ft(12,3)=2
!
        nbf=6
!       CONNECTIVITÉ DES FACES POUR UNE MAILLE HEXA8 OU HEXA20
        f(1,1)=1
        f(1,2)=2
        f(1,3)=3
        f(1,4)=4
        f(2,1)=1
        f(2,2)=2
        f(2,3)=6
        f(2,4)=5
        f(3,1)=3
        f(3,2)=4
        f(3,3)=8
        f(3,4)=7
        f(4,1)=5
        f(4,2)=6
        f(4,3)=7
        f(4,4)=8
        f(5,1)=1
        f(5,2)=4
        f(5,3)=8
        f(5,4)=5
        f(6,1)=2
        f(6,2)=3
        f(6,3)=7
        f(6,4)=6
        if (milieu) then
           f(1,5)=9
           f(1,6)=10
           f(1,7)=11
           f(1,8)=12
           f(2,5)=9
           f(2,6)=14
           f(2,7)=17
           f(2,8)=13
           f(3,5)=11
           f(3,6)=16
           f(3,7)=19
           f(3,8)=15
           f(4,5)=17
           f(4,6)=18
           f(4,7)=19
           f(4,8)=20
           f(5,5)=12
           f(5,6)=16
           f(5,7)=20
           f(5,8)=13
           f(6,5)=10
           f(6,6)=15
           f(6,7)=18
           f(6,8)=14
        endif
!
    else if (typma(1:5).eq.'PENTA') then
        nbft=8
!     CONNECTIVITÉ DES FACES TRIANGLES POUR UNE MAILLE PENTA6 OU PENTA15
        ft(1,1)=1
        ft(1,2)=2
        ft(1,3)=3
        ft(2,1)=4
        ft(2,2)=5
        ft(2,3)=6
        ft(3,1)=1
        ft(3,2)=3
        ft(3,3)=6
        ft(4,1)=6
        ft(4,2)=4
        ft(4,3)=1
        ft(5,1)=2
        ft(5,2)=3
        ft(5,3)=6
        ft(6,1)=6
        ft(6,2)=5
        ft(6,3)=2
        ft(7,1)=1
        ft(7,2)=2
        ft(7,3)=5
        ft(8,1)=5
        ft(8,2)=4
        ft(8,3)=1
!
        nbf=5
!       CONNECTIVITÉ DES FACES POUR UNE MAILLE PENTA6 OU PENTA15
        f(1,1)=1
        f(1,2)=2
        f(1,3)=3
        f(1,4)=0
        f(2,1)=4
        f(2,2)=5
        f(2,3)=6
        f(2,4)=0
        f(3,1)=1
        f(3,2)=3
        f(3,3)=6
        f(3,4)=4
        f(4,1)=2
        f(4,2)=3
        f(4,3)=6
        f(4,4)=5
        f(5,1)=1
        f(5,2)=2
        f(5,3)=5
        f(5,4)=4
        if (milieu) then
           f(1,4)=7
           f(1,5)=8
           f(1,6)=9
           f(1,7)=0
           f(2,4)=13
           f(2,5)=14
           f(2,6)=15
           f(2,7)=0
           f(3,5)=9
           f(3,6)=12
           f(3,7)=15
           f(3,8)=10
           f(4,5)=8
           f(4,6)=12
           f(4,7)=14
           f(4,8)=11
           f(5,5)=7
           f(5,6)=11
           f(5,7)=13
           f(5,8)=10
        endif
!
    else if (typma(1:5).eq.'PYRAM') then
        nbft=6
!     CONNECTIVITÉ DES FACES TRIANGLES POUR UNE MAILLE TETRA4 OU TETRA10
        ft(1,1)=1
        ft(1,2)=2
        ft(1,3)=5
        ft(2,1)=2
        ft(2,2)=3
        ft(2,3)=5
        ft(3,1)=3
        ft(3,2)=4
        ft(3,3)=5
        ft(4,1)=4
        ft(4,2)=1
        ft(4,3)=5
        ft(5,1)=1
        ft(5,2)=2
        ft(5,3)=3
        ft(6,1)=1
        ft(6,2)=3
        ft(6,3)=4
!
        nbf=5
!       CONNECTIVITÉ DES FACES POUR UNE MAILLE PYRAM5 OU PYRAM13
!       F : [ 1 2 5
!             2 3 5
!             3 4 5
!             4 1 5
!             1 2 3 4]
        f(1,1)=1
        f(1,2)=2
        f(1,3)=5
        f(1,4)=0
        f(2,1)=2
        f(2,2)=3
        f(2,3)=5
        f(2,4)=0
        f(3,1)=3
        f(3,2)=4
        f(3,3)=5
        f(3,4)=0
        f(4,1)=4
        f(4,2)=1
        f(4,3)=5
        f(4,4)=0
        f(5,1)=1
        f(5,2)=2
        f(5,3)=3
        f(5,4)=4
        if (milieu) then
           f(1,4)=6
           f(1,5)=11
           f(1,6)=10
           f(1,7)=0
           f(2,4)=7
           f(2,5)=12
           f(2,6)=11
           f(2,7)=0
           f(3,4)=8
           f(3,5)=13
           f(3,6)=12
           f(3,7)=0
           f(4,4)=9
           f(4,5)=10
           f(4,6)=13
           f(4,7)=0
           f(5,5)=6
           f(5,6)=7
           f(5,7)=8
           f(5,8)=9
        endif
!
    else if (typma(1:5).eq.'TETRA') then
        nbft=4
!     CONNECTIVITÉ DES FACES TRIANGLES POUR UNE MAILLE TETRA4 OU TETRA10
        ft(1,1)=1
        ft(1,2)=3
        ft(1,3)=2
        ft(2,1)=2
        ft(2,2)=3
        ft(2,3)=4
        ft(3,1)=1
        ft(3,2)=4
        ft(3,3)=3
        ft(4,1)=1
        ft(4,2)=2
        ft(4,3)=4
!
        nbf=4
!       CONNECTIVITÉ DES FACES POUR UNE MAILLE TETRA4 OU TETRA10
        f(1,1)=1
        f(1,2)=3
        f(1,3)=2
        f(1,4)=0
        f(2,1)=2
        f(2,2)=3
        f(2,3)=4
        f(2,4)=0
        f(3,1)=1
        f(3,2)=4
        f(3,3)=3
        f(3,4)=0
        f(4,1)=1
        f(4,2)=2
        f(4,3)=4
        f(4,4)=0
        if (milieu) then
           f(1,4)=7
           f(1,5)=6
           f(1,6)=5
           f(1,7)=0
           f(2,4)=6
           f(2,5)=10
           f(2,6)=9
           f(2,7)=0
           f(3,4)=8
           f(3,5)=10
           f(3,6)=7
           f(3,7)=0
           f(4,4)=5
           f(4,5)=9
           f(4,6)=8
           f(4,7)=0
        endif
!
    else if (typma(1:4).eq.'QUAD') then
        nbft=2
!     CONNECTIVITE DES TRIANGLES POUR UNE MAILLE QUAD4 OU QUAD8
        ft(1,1)=1
        ft(1,2)=2
        ft(1,3)=3
        ft(2,1)=3
        ft(2,2)=4
        ft(2,3)=1
!
        nbf=1
!       CONNECTIVITE DES FACES POUR UNE MAILLE QUAD4 OU QUAD8
        f(1,1)=1
        f(1,2)=2
        f(1,3)=3
        f(1,4)=4
        if (milieu) then
           f(1,5)=5
           f(1,6)=6
           f(1,7)=7
           f(1,8)=8
        endif
    else if (typma(1:4).eq.'TRIA') then
        nbft=1
!     CONNECTIVITE DES TRIANGLES POUR UNE MAILLE TRIA3 OU TRIA6
        ft(1,1)=1
        ft(1,2)=3
        ft(1,3)=2
!
        nbf=1
!       CONNECTIVITE DES FACES POUR UNE MAILLE TRIA3 OU TRIA6
        f(1,1)=1
        f(1,2)=3
        f(1,3)=2
        f(1,4)=0
        if (milieu) then
           f(1,4)=4
           f(1,5)=5
           f(1,6)=6
           f(1,7)=0
        endif
    else
        call utmess('F', 'ALGORITH2_24', sk=typma)
    endif
!
end subroutine

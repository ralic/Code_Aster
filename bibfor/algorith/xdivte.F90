subroutine xdivte(elp, cnset, nse, nnose)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
    integer :: cnset(*), nse, nnose
    character(len=8) :: elp
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
!                      CONNECTIVITÉ DES ÉLÉMENTS TETRAS À PARTIR
!                               D'UN ÉLÉMENT PARENT X-FEM
!                          (VOIR BOOK III 19/04/04)
!
!     ENTREE
!       ELP     : TYPE DE MAILLE
!
!     SORTIE
!       CNSET   : CONNECTIVITÉ DES NOEUDS DE LA MAILLE
!       NSE     : NOMBRE DE SOUS-TÉTRAS (SOUS TRIA)
!       NNOSE   : NOMBRE DE NOEUDS DU SOUS TETRA (SOUS TRIA)
!     ------------------------------------------------------------------
!
    integer :: ino, ise, connec(6, 10)
! ----------------------------------------------------------------------
!
    if (elp .eq. 'HE8') then
        connec(1,1)=6
        connec(1,2)=3
        connec(1,3)=2
        connec(1,4)=4
        connec(2,1)=4
        connec(2,2)=5
        connec(2,3)=1
        connec(2,4)=2
        connec(3,1)=2
        connec(3,2)=5
        connec(3,3)=6
        connec(3,4)=4
        connec(4,1)=5
        connec(4,2)=4
        connec(4,3)=8
        connec(4,4)=6
        connec(5,1)=3
        connec(5,2)=6
        connec(5,3)=7
        connec(5,4)=8
        connec(6,1)=3
        connec(6,2)=8
        connec(6,3)=4
        connec(6,4)=6
        nse=6
        nnose=4
    else if (elp.eq.'H20') then
        connec(1,1)=6
        connec(1,2)=3
        connec(1,3)=2
        connec(1,4)=4
        connec(1,5)=23
        connec(1,6)=10
        connec(1,7)=14
        connec(1,8)=27
        connec(1,9)=11
        connec(1,10)=21
        connec(2,1)=4
        connec(2,2)=5
        connec(2,3)=1
        connec(2,4)=2
        connec(2,5)=25
        connec(2,6)=13
        connec(2,7)=12
        connec(2,8)=21
        connec(2,9)=22
        connec(2,10)=9
        connec(3,1)=2
        connec(3,2)=5
        connec(3,3)=6
        connec(3,4)=4
        connec(3,5)=22
        connec(3,6)=17
        connec(3,7)=14
        connec(3,8)=21
        connec(3,9)=25
        connec(3,10)=27
        connec(4,1)=5
        connec(4,2)=4
        connec(4,3)=8
        connec(4,4)=6
        connec(4,5)=25
        connec(4,6)=16
        connec(4,7)=20
        connec(4,8)=17
        connec(4,9)=27
        connec(4,10)=26
        connec(5,1)=3
        connec(5,2)=6
        connec(5,3)=7
        connec(5,4)=8
        connec(5,5)=23
        connec(5,6)=18
        connec(5,7)=15
        connec(5,8)=24
        connec(5,9)=26
        connec(5,10)=19
        connec(6,1)=3
        connec(6,2)=8
        connec(6,3)=4
        connec(6,4)=6
        connec(6,5)=24
        connec(6,6)=16
        connec(6,7)=11
        connec(6,8)=23
        connec(6,9)=26
        connec(6,10)=27
        nse=6
        nnose=10
    else if (elp.eq.'PE6') then
        connec(1,1)=5
        connec(1,2)=4
        connec(1,3)=6
        connec(1,4)=1
        connec(2,1)=1
        connec(2,2)=2
        connec(2,3)=3
        connec(2,4)=6
        connec(3,1)=6
        connec(3,2)=2
        connec(3,3)=5
        connec(3,4)=1
        nse=3
        nnose=4
!
    else if (elp.eq.'P15') then
        connec(1,1)=5
        connec(1,2)=4
        connec(1,3)=6
        connec(1,4)=1
        connec(1,5)=13
        connec(1,6)=15
        connec(1,7)=14
        connec(1,8)=16
        connec(1,9)=10
        connec(1,10)=18
        connec(2,1)=1
        connec(2,2)=2
        connec(2,3)=3
        connec(2,4)=6
        connec(2,5)=7
        connec(2,6)=8
        connec(2,7)=9
        connec(2,8)=18
        connec(2,9)=17
        connec(2,10)=12
        connec(3,1)=6
        connec(3,2)=2
        connec(3,3)=5
        connec(3,4)=1
        connec(3,5)=17
        connec(3,6)=11
        connec(3,7)=14
        connec(3,8)=18
        connec(3,9)=7
        connec(3,10)=16
        nse=3
        nnose=10
    else if (elp.eq.'PY5') then
!       SOUS-TETRAS
!       CONNEC = [1 2 3 5
!                 1 3 4 5]
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=3
        connec(1,4)=5
        connec(2,1)=1
        connec(2,2)=3
        connec(2,3)=4
        connec(2,4)=5
        nse=2
        nnose=4
    else if (elp.eq.'P13') then
!       SOUS-TETRAS
!       CONNEC = [1 2 3 5
!                 1 3 4 5]
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=3
        connec(1,4)=5
        connec(1,5)=6
        connec(1,6)=7
        connec(1,7)=14
        connec(1,8)=10
        connec(1,9)=11
        connec(1,10)=12
        connec(2,1)=1
        connec(2,2)=3
        connec(2,3)=4
        connec(2,4)=5
        connec(2,5)=14
        connec(2,6)=8
        connec(2,7)=9
        connec(2,8)=10
        connec(2,9)=12
        connec(2,10)=13
        nse=2
        nnose=10
    else if (elp.eq.'TE4') then
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=3
        connec(1,4)=4
        nse=1
        nnose=4
    else if (elp.eq.'T10') then
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=3
        connec(1,4)=4
        connec(1,5)=5
        connec(1,6)=6
        connec(1,7)=7
        connec(1,8)=8
        connec(1,9)=9
        connec(1,10)=10
        nse=1
        nnose=10
    else if (elp.eq.'QU4') then
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=4
        connec(2,1)=2
        connec(2,2)=3
        connec(2,3)=4
        nse=2
        nnose=3
    else if (elp.eq.'QU8') then
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=4
        connec(1,4)=5
        connec(1,5)=9
        connec(1,6)=8
        connec(2,1)=2
        connec(2,2)=3
        connec(2,3)=4
        connec(2,4)=6
        connec(2,5)=7
        connec(2,6)=9
        nse=2
        nnose=6
    else if (elp.eq.'TR3') then
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=3
        nse=1
        nnose=3
    else if (elp.eq.'TR6') then
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=3
        connec(1,4)=4
        connec(1,5)=5
        connec(1,6)=6
        nse=1
        nnose=6
    else if (elp.eq.'SE2') then
        connec(1,1)=1
        connec(1,2)=2
        nse=1
        nnose=2
    else if (elp.eq.'SE3') then
        connec(1,1)=1
        connec(1,2)=2
        connec(1,3)=3
        nse=1
        nnose=3
    else
!       TYPE D'ELEMENT FINI PAS TRAITE
        ASSERT(.false.)
    endif
!
    do  ise = 1, nse
        do 20 ino = 1, nnose
            cnset(nnose*(ise-1)+ino)=connec(ise,ino)
20      continue
    end do
!
end subroutine

subroutine vfnulo(maxfa, maxar, ndim, nnos, nface,&
                  nbnofa, nosar, nosfa, narfa)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! DONNE LA NUMEROTATION LOCALE DES SOMMETS DES FACES DE VF
! LA FACE EST UN ELEMENT DE BORD DE DIMENSION DIM-1
!
! IN NDIM DIMENSION D ESPACE
! IN NFACE NOMBRE DE FACES
! IN NNOS NOMBRE DE SOMMETS
! MAXFA NOMBRE MAX DE FACES
! MAXAR NOMBRE MAX DE ARETES
! NOSAR(IAR ,1:2)  LES DESDUS SOMMETS DE L ARETE IAR
! OUT NBNOFA(1:NFACE) : NOMBRE DE SOMMETS DE LA FACE
! OUT NOSFA(IFA :1,NFACE,J : 1,NBNOFA(IFA)) J EME SOMMET DE LA FACE IFA
!     (EN NUMEROTATION LOCALE)
! OUT NARFA(IFA :1,NFACE,J : 1,NBNOFA(IFA)) J EME ARETE DE LA FACE IFA
!     (EN NUMEROTATION LOCALE)
!
!
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/utmess.h"
    integer :: maxfa, maxar, ndim, nnos, nface
    integer :: nbnofa(1:nface)
    integer :: nosar(1:maxar, 2)
    integer :: nosfa(1:maxfa, *)
    integer :: narfa(1:maxfa, *)
!
    character(len=8) :: elrefe
    integer :: ifa
!
    call elref1(elrefe)
!
    if (ndim .eq. 2) then
        ASSERT(nnos.eq.nface)
        do 1 ifa = 1, nface
            nbnofa(ifa)=2
            nosfa(ifa,1)=ifa
            if ((ifa+1) .le. nnos) then
                nosfa(ifa,2)=ifa+1
            else
                nosfa(ifa,2)=ifa+1-nnos
            endif
 1      continue
    else
        if (elrefe .eq. 'H27') then
            ASSERT(nface.eq.6)
            ASSERT(nnos.eq.8)
            do 2 ifa = 1, 6
                nbnofa(ifa)=4
 2          continue
! SOMMETS DE ARETE
            nosar(1,1)=1
            nosar(1,2)=2
            nosar(2,1)=2
            nosar(2,2)=3
            nosar(3,1)=3
            nosar(3,2)=4
            nosar(4,1)=4
            nosar(4,2)=1
            nosar(5,1)=1
            nosar(5,2)=5
            nosar(6,1)=2
            nosar(6,2)=6
            nosar(7,1)=3
            nosar(7,2)=7
            nosar(8,1)=4
            nosar(8,2)=8
            nosar(9,1)=5
            nosar(9,2)=6
            nosar(10,1)=6
            nosar(10,2)=7
            nosar(11,1)=7
            nosar(11,2)=8
! SOMMETS DE FACE
            nosar(12,1)=8
            nosar(12,2)=5
            nosfa(1,1)=1
            nosfa(1,2)=2
            nosfa(1,3)=3
            nosfa(1,4)=4
            nosfa(2,1)=1
            nosfa(2,2)=5
            nosfa(2,3)=6
            nosfa(2,4)=2
            nosfa(3,1)=2
            nosfa(3,2)=3
            nosfa(3,3)=7
            nosfa(3,4)=6
            nosfa(4,1)=7
            nosfa(4,2)=3
            nosfa(4,3)=4
            nosfa(4,4)=8
            nosfa(5,1)=1
            nosfa(5,2)=4
            nosfa(5,3)=8
            nosfa(5,4)=5
            nosfa(6,1)=5
            nosfa(6,2)=6
            nosfa(6,3)=7
            nosfa(6,4)=8
! ARETES DE FACE
            narfa(1,1)=1
            narfa(1,2)=2
            narfa(1,3)=3
            narfa(1,4)=4
            narfa(2,1)=5
            narfa(2,2)=9
            narfa(2,3)=6
            narfa(2,4)=1
            narfa(3,1)=2
            narfa(3,2)=7
            narfa(3,3)=10
            narfa(3,4)=6
            narfa(4,1)=7
            narfa(4,2)=3
            narfa(4,3)=8
            narfa(4,4)=11
            narfa(5,1)=4
            narfa(5,2)=8
            narfa(5,3)=12
            narfa(5,4)=5
            narfa(6,1)=9
            narfa(6,2)=10
            narfa(6,3)=11
            narfa(6,4)=12
        else if (elrefe.eq.'T9') then
            ASSERT(nface.eq.4)
            ASSERT(nnos.eq.4)
            do 3 ifa = 1, 4
                nbnofa(ifa)=3
 3          continue
! SOMMETS DE ARETE
            nosar(1,1)=1
            nosar(1,2)=2
            nosar(2,1)=2
            nosar(2,2)=3
            nosar(3,1)=3
            nosar(3,2)=1
            nosar(4,1)=1
            nosar(4,2)=4
            nosar(5,1)=2
            nosar(5,2)=4
            nosar(6,1)=3
            nosar(6,2)=4
! SOMMETS DE FACE
            nosfa(1,1)=2
            nosfa(1,2)=3
            nosfa(1,3)=4
            nosfa(2,1)=3
            nosfa(2,2)=4
            nosfa(2,3)=1
            nosfa(3,1)=4
            nosfa(3,2)=1
            nosfa(3,3)=2
            nosfa(4,1)=1
            nosfa(4,2)=2
            nosfa(4,3)=3
! ARETES DE FACE
            narfa(1,1)=2
            narfa(1,2)=6
            narfa(1,3)=5
            narfa(2,1)=6
            narfa(2,2)=4
            narfa(2,3)=3
            narfa(3,1)=4
            narfa(3,2)=1
            narfa(3,3)=5
            narfa(4,1)=1
            narfa(4,2)=2
            narfa(4,3)=3
        else
            call utmess('F', 'VOLUFINI_12', sk=elrefe)
        endif
    endif
end subroutine

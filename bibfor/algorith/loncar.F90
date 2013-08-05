subroutine loncar(ndim, typma, coord, l)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: ndim
    real(kind=8) :: coord(*), l
    character(len=8) :: typma
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
!                      LONGUEUR CARACTÉRISTIQUE D'UNE MAILLE
!
!     ENTREE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       TYPMA   : TYPE DE MAILLE (TYPE_MAILLE)
!       COORD   : COORDONNÉES DES NOEUDS (X Y Z SI NDIM = 3
!                                         X Y   SI NDIM = 2)
!
!     SORTIE
!       L      : LONGUEUR CARACTÉRISTIQUE
!     ------------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: ar(3), m(3)
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     ATTENTION,
!     NDIM EST LA DIMENSION DU MAILLAGE
!     POUR LES MAILLES DE BORD, CE N'EST PAS LA DIMENSION DE LA MAILLE
!
    ASSERT(ndim.eq.2.or.ndim.eq.3)
!
    if (typma(1:4) .eq. 'HEXA') then
!
!       LA LONGUEUR CARACTÉRISTIQUE EST LA GRANDE DIAGONALE N1-N7
        l=sqrt( (coord(1)-coord(19))**2 + (coord(2)-coord(20))**2&
        + (coord(3)-coord(21))**2 )
!
    else if (typma(1:5).eq.'PENTA') then
!
!       LA LONGUEUR CARACTÉRISTIQUE EST ((N3-N1)*(N3-N2)*(N3-N6))^(1/3)
        ar(1)=sqrt((coord(7)-coord(1))**2 + (coord(8)-coord(2))**2&
        + (coord(9)-coord(3))**2 )
        ar(2)=sqrt((coord(7)-coord(4))**2 + (coord(8)-coord(5))**2&
        + (coord(9)-coord(6))**2 )
        ar(3)=sqrt((coord(7)-coord(16))**2 + (coord(8)-coord(17))**2&
        + (coord(9)-coord(18))**2 )
        l=(ar(1)*ar(2)*ar(3))**(1.d0/3.d0)
!
    else if (typma(1:5).eq.'PYRAM') then
!
!       M : MILIEU DE LA FACE QUADRANGLE
        do 5 i = 1, 3
            m(i) = (coord( 3*(1-1)+i) + coord(3*(2-1)+i) + coord(3*(3- 1)+i) + coord(3*(4-1)+i )&
                   ) / 4.d0
 5      continue
!
!       LA LONGUEUR CARACTÉRISTIQUE EST ((N1-N3)*(M-N5))^(1/2)
        ar(1)=sqrt( (coord(3*(3-1)+1)-coord(3*(1-1)+1))**2 + (coord(3*&
        (3-1)+2)-coord(3*(1-1)+2))**2 + (coord(3*(3-1)+3)-coord(3*(1-&
        1)+3))**2 )
        ar(2)=sqrt( ( m(1)-coord(3*(5-1)+1))**2 + ( m(2)-coord(3*(5-1)&
        +2))**2 + ( m(3)-coord(3*(5-1)+3))**2 )
        l=sqrt(ar(1)*ar(2))
!
    else if (typma(1:5).eq.'TETRA') then
!
!       LA LONGUEUR CARACTÉRISTIQUE EST ((N1-N2)*(N1-N3)*(N1-N4))^(1/3)
        do 10 i = 1, 3
            ar(i)=sqrt((coord(1)-coord(3*i+1))**2 + (coord(2)-coord(3*&
            i+2))**2 + (coord(3)-coord(3*i+3))**2 )
10      continue
        l=(ar(1)*ar(2)*ar(3))**(1.d0/3.d0)
!
    else if (typma(1:4).eq.'QUAD') then
!
!     LA LONGUEUR CARACTÉRISTIQUE EST ((N1-N2)*(N1-N3))^(1/2)
        do 20 i = 1, 2
            ar(i) = (coord(1)-coord(ndim*i+1))**2 + (coord(2)-coord( ndim*i+2) )**2
            if (ndim .eq. 3) ar(i) = ar(i) + (coord(3)-coord(ndim*i+3)) **2
20      continue
        l=(sqrt(ar(1)*ar(2)))**(1.d0/2.d0)
!
    else if (typma(1:4).eq.'TRIA') then
!
!     LA LONGUEUR CARACTÉRISTIQUE EST ((N1-N2)*(N1-N3))^(1/2)
        do 30 i = 1, 2
            ar(i) = (coord(1)-coord(ndim*i+1))**2 + (coord(2)-coord( ndim*i+2) )**2
            if (ndim .eq. 3) ar(i) = ar(i) + (coord(3)-coord(ndim*i+3)) **2
30      continue
        l=(sqrt(ar(1)*ar(2)))**(1.d0/2.d0)
!
    else if (typma(1:3).eq.'SEG') then
!
!       LA LONGUEUR CARACTÉRISTIQUE EST (N1-N2)^(1/2)
        l = (coord(1)-coord(ndim+1))**2 + (coord(2)-coord(ndim+2))**2
        if (ndim .eq. 3) l = l + (coord(3)-coord(ndim+3))**2
        l = sqrt(l)
!
    else
!
!       TYPE D'ELEMENT FINI PAS TRAITE
        ASSERT(.false.)
!
    endif
!
    call jedema()
end subroutine

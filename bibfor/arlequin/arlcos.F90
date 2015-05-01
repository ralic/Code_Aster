subroutine arlcos(numa, connex, loncum, coord, dime,&
                  cnoeud)
!
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
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
!
    integer :: numa, connex(*), loncum(*)
    integer :: dime
    real(kind=8) :: coord(3, *), cnoeud(dime, *)
!
! ----------------------------------------------------------------------
!
! CONSTRUCTION DE BOITES ENGLOBANTES POUR UN GROUPE DE MAILLES
!
! RETOURNE LES COORDONNEES DES NOEUDS DE LA MAILLE POUR LES ELEMENTS
! SOLIDES
!
! ----------------------------------------------------------------------
!
!
! IN  NUMA   : NUMERO ABSOLU DE LA MAILLE DANS LE MAILLAGE
! IN  CONNEX : CONNEXITE DES MAILLES
! IN  LONCUM : LONGUEUR CUMULEE DE CONNEX
! IN  COORD  : COORDONNEES DES NOEUDS
! IN  DIME   : DIMENSION DE L'ESPACE
! OUT CNOEUD : COORD DES NOEUDS (X1, [Y1, Z1], X2, ...)
!
! ----------------------------------------------------------------------
!
    integer :: ino, idim, nuno, jdec, nbno
!
! ----------------------------------------------------------------------
!
    jdec = loncum(numa)
    nbno = loncum(numa+1) - jdec
!
    if ((nbno < 1) .or. (nbno > 27)) then
        ASSERT( .false. )
    endif
!
    do 10 ino = 1, nbno
        nuno = connex(jdec-1+ino)
        do 11 idim = 1, dime
            cnoeud(idim,ino) = coord(idim,nuno)
 11     end do
 10 end do
!
end subroutine

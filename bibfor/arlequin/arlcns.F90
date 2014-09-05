subroutine arlcns(nummai, connex, loncum, nbno, cxno)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: nummai, connex(*), loncum(*)
    integer :: nbno
    integer :: cxno(nbno)
!
! ----------------------------------------------------------------------
!
! CONSTRUCTION DE BOITES ENGLOBANTES POUR UN GROUPE DE MAILLES
!
! RETOURNE LES NUMEROS ABSOLUS DES NOEUDS DE LA CONNECTIVITE DE LA
! MAILLE POUR LES ELEMENTS SOLIDES
!
! ----------------------------------------------------------------------
!
!
! IN  NUMMAI : NUMERO ABSOLU DE LA MAILLE DANS LE MAILLAGE
! IN  CONNEX : CONNEXITE DES MAILLES
! IN  LONCUM : LONGUEUR CUMULEE DE CONNEX
! IN  NBNO   : NOMBRE DE NOEUDS DE LA MAILLE
! OUT CXNO   : CONNECTIVITE DE LA MAILLE
!
! ----------------------------------------------------------------------
!
    integer :: ino, jdec
!
! ----------------------------------------------------------------------
!
    jdec = loncum(nummai)
!
    if ((nbno < 1) .or. (nbno > 27)) then
        ASSERT( .false. )
    endif
!
    do 10 ino = 1, nbno
        cxno(ino) = connex(jdec-1+ino)
 10 end do
!
end subroutine

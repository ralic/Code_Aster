subroutine inivec(vec, neq, id, nbcp)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
!    P. RICHARD     DATE 27/11/90
!-----------------------------------------------------------------------
!  BUT:  INITIALISER TOUTES LES COMPOSANTES D'UN VECTEUR A ZERO SAUF
!   CELLES D'UNE LISTE EGALES A UN
    implicit none
#include "asterfort/utmess.h"
!
!-----------------------------------------------------------------------
!
! VEC      /M/: VECTEUR A INITIALISER
! NEQ      /I/: DIMENSION DU VECTEUR
! ID       /I/: LISTE DES RANGS DES COMPOSANTES NON NULLES
! NBCP     /I/: NOMBRE DE COMPOSANTES A INITIALISER A UN
!
!-----------------------------------------------------------------------
!
    integer :: i, j, nbcp, id(nbcp), neq
    real(kind=8) :: vec(neq)
!
!-----------------------------------------------------------------------
!
    do 10 i = 1, neq
        vec(i)=0.d0
10  end do
!
    do 20 j = 1, nbcp
        if (id(j) .gt. neq) then
            call utmess('A', 'ALGORITH4_35')
        else
            vec(id(j))=1.d0
        endif
20  end do
!
end subroutine

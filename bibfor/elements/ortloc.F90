subroutine ortloc(dsidep, i1, j1, r)
    implicit none
#include "asterfort/utbtab.h"
    real(kind=8) :: trav(3, 3), xab(3, 3), dsidep(6, 6), r(9)
    integer :: i, j, i1, j1
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!       PASSAGE DE LA MATRICE TANGENTE DU REPERE D'ORTHOTROPIE AU REPERE
!       LOCAL DE L'ELEMENT. CE PASSAGE SE FAIT PAR SOUS MATRICES
!
!       IN      DSIDEP = MATRICE TANGENTE
!               R      = MATRICE DE CHANGEMENT DE REPERE
!       OUT     DSIDEP = MATRICE TANGENTE
!-----------------------------------------------------------------------
!
    do 10 i = 1, 3
        do 20 j = 1, 3
            trav(i,j) = dsidep(i+i1,j+j1)
20      continue
10  end do
!
    call utbtab('ZERO', 3, 3, trav, r,&
                xab, trav)
!
    do 30 i = 1, 3
        do 40 j = 1, 3
            dsidep(i+i1,j+j1) = trav(i,j)
40      continue
30  end do
!
end subroutine

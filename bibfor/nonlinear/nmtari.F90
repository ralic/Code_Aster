subroutine nmtari(type, ndimsi, mat, sigel, vim,&
                  epm, dp, sp, xi, dsidep)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterfort/matini.h"
#include "asterfort/nmtacr.h"
    integer :: ndimsi, type
    real(kind=8) :: mat(14), sigel(ndimsi), vim(9), epm(ndimsi), dp, sp, xi
    real(kind=8) :: dsidep(6, 6)
!
! ----------------------------------------------------------------------
!  TAHERI : CALCUL DE LA RIGIDITE TANGENTE
! ----------------------------------------------------------------------
! IN  TYPE   0: ELAS, 1:PSEUDO-ELAS, 2: PLAS, 3: PLAS+G
! IN  NDIMSI DIMENSION DES TENSEURS
! IN  SIGEL  DEVIATEUR DES CONTRAINTES ELASTIQUES
! IN  VIM    VARIABLES INTERNES EN T-
! IN  EPM    DEFORMATION PLASTIQUE EN T-
! IN  DP     INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
! IN  SP     CONTRAINTE DE PIC
! IN  XI     PILOTAGE DE EPN
! OUT DSIDEP MATRICE TANGENTE
! ----------------------------------------------------------------------
!
    integer :: i, j, mode
    real(kind=8) :: tmp
    real(kind=8) :: tang(6, 6)
    real(kind=8) :: f, g, fdp, gdp, fds, gds, fdx, gdx, dpmax, sig(6)
!
!
!    MATRICE ELASTIQUE
!
    call matini(6, 6, 0.d0, dsidep)
!
    tmp = (mat(1) - mat(2)) / 3.d0
    do 30 i = 1, 3
        do 40 j = 1, 3
            dsidep(i,j) = tmp
40      continue
30  end do
!
    do 50 i = 1, ndimsi
        dsidep(i,i) = dsidep(i,i) + mat(2)
50  end do
!
!
!    CONTRIBUTION NON LINEAIRE SYMETRISEE
!
    if (type .ge. 2) then
        if (type .eq. 2) mode = 5
        if (type .eq. 3) mode = 6
        call nmtacr(mode, ndimsi, mat, sigel, vim,&
                    epm, dp, sp, xi, f,&
                    g, fds, gds, fdp, gdp,&
                    fdx, gdx, dpmax, sig, tang)
        do 60 i = 1, ndimsi
            do 70 j = 1, ndimsi
                dsidep(i,j)=dsidep(i,j)-mat(2)**2*(tang(i,j)+tang(j,i)&
                )/2.d0
70          continue
60      continue
    endif
!
!
end subroutine

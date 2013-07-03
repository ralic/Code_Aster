subroutine dxktan(delas, mp1, mp2, nbackn, ncrit,&
                  dcc1, dcc2, dsidep)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     REALISE LE CALCUL DE LA MATRICE TANGENTE DANS LE CAS DE LA LOI
!     DE COMPORTEMENT GLRC
!
! IN  DELAS  : MATRICE ELASTIQUE EN MEMBRANE, FLEXION ET COUPLAGE
! IN  MP1    : MOMENTS LIMITES ELASTIQUES EN FLEXION POSITIVE
! IN  MP2    : MOMENTS LIMITES ELASTIQUES EN FLEXION NEGATIVE
! IN  NBACKN : MOMENT DE RAPPEL
! IN  NCRIT  : TYPE DU CRITERE DE PLASTICITE
! IN  DCC1   : MATRICE ELASTIQUE + CONSTANTES DE PRAGER (FLEXION +)
! IN  DCC2   : MATRICE ELASTIQUE + CONSTANTES DE PRAGER (FLEXION -)
!
! OUT DSIDEP : MATRICE TANGENTE
! ----------------------------------------------------------------------
!
#include "asterfort/dfplas.h"
#include "asterfort/dxprd1.h"
#include "asterfort/dxprd2.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprte.h"
#include "asterfort/pmat.h"
#include "asterfort/pmavec.h"
#include "blas/dcopy.h"
    integer :: ncrit, i, j, n, nd
!
    real(kind=8) :: mp1(3), mp2(3), nbackn(6)
    real(kind=8) :: dfpla1(6)
    real(kind=8) :: dfpla2(6)
    real(kind=8) :: delas(6, 6), dsidep(6, 6)
    real(kind=8) :: mat(6, 6), mata(6, 6), matb(6, 6), matc(6, 6), matd(6, 6)
    real(kind=8) :: dcc1(3, 3), dcc2(3, 3), dc1(6, 6), dc2(6, 6)
    real(kind=8) :: vect(6)
    real(kind=8) :: scal, scala, scalb
    common /tdim/ n,nd
!
!     INITIALISATION
    n=6
!
    do 100 i = 1, 6
        dfpla1(i) = 0.d0
        dfpla2(i) = 0.d0
        do 110 j = 1, 6
            dc1(i,j) = 0.d0
            dc2(i,j) = 0.d0
110      continue
100  end do
!
    call dcopy(36, delas, 1, dc1, 1)
    call dcopy(36, delas, 1, dc2, 1)
!
    do 120 i = 1, 3
        do 130 j = 1, 3
            dc1(i+3,j+3) = dcc1(i,j)
            dc2(i+3,j+3) = dcc2(i,j)
130      continue
120  end do
!
    if (ncrit .eq. 0) then
!     CAS ELASTIQUE
        call dcopy(36, delas, 1, dsidep, 1)
!
    else if (ncrit .eq. 1) then
        call dfplas(nbackn(4), mp1, dfpla1(4))
        call lcprte(dfpla1, dfpla1, mata)
        call pmat(6, delas, mata, matb)
        call pmat(6, matb, delas, matc)
        call pmavec('ZERO', 6, dc1, dfpla1, vect)
        call lcprsc(dfpla1, vect, scal)
!
        do 10 i = 1, 6
            do 20 j = 1, 6
                dsidep(i,j)=delas(i,j)-matc(i,j)/scal
20          continue
10      continue
!
    else if (ncrit .eq. 2) then
        call dfplas(nbackn(4), mp2, dfpla2(4))
        call lcprte(dfpla2, dfpla2, mata)
        call pmat(6, delas, mata, matb)
        call pmat(6, matb, delas, matc)
        call pmavec('ZERO', 6, dc2, dfpla2, vect)
        call lcprsc(dfpla2, vect, scal)
!
        do 30 i = 1, 6
            do 40 j = 1, 6
                dsidep(i,j)=delas(i,j)-matc(i,j)/scal
40          continue
30      continue
    else if (ncrit .eq. 12) then
!
!     NUMERATEUR
        call dfplas(nbackn(4), mp1, dfpla1(4))
        call dfplas(nbackn(4), mp2, dfpla2(4))
        call dxprd1(dfpla1, dfpla2, dc2, dfpla2, dfpla1,&
                    mata)
        call dxprd1(dfpla1, dfpla1, dc2, dfpla2, dfpla2,&
                    matb)
        call dxprd1(dfpla2, dfpla1, dc1, dfpla1, dfpla2,&
                    matc)
        call dxprd1(dfpla2, dfpla2, dc1, dfpla1, dfpla1,&
                    matd)
!     DENOMINATEUR
        call dxprd2(dfpla1, dc1, dfpla1, dfpla2, dc2,&
                    dfpla2, scala)
        call dxprd2(dfpla2, dc1, dfpla1, dfpla1, dc2,&
                    dfpla2, scalb)
!
        scal = scala*scalb
!
        do 50 i = 1, 6
            do 60 j = 1, 6
                mat(i,j)=(mata(i,j)-matb(i,j)+matc(i,j)-matd(i,j))/&
                scal
60          continue
50      continue
!
        call pmat(6, delas, mat, mata)
        call pmat(6, mata, delas, matb)
!
        do 70 i = 1, 6
            do 80 j = 1, 6
                dsidep(i,j)=delas(i,j)-matb(i,j)
80          continue
70      continue
!
    endif
end subroutine

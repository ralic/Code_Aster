subroutine lcmmro(tampon, omp, nvi, vind, vinf)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     Stockage variables internes rotation reseau
!     ----------------------------------------------------------------
#include "asterc/r8miem.h"
#include "asterfort/r8inir.h"
#include "blas/dcopy.h"
    integer :: i, j, nvi, k
    real(kind=8) :: omp(3), dtheta, iden(3, 3), nax(3, 3), q(3, 3)
    real(kind=8) :: tampon(*), omegap(3, 3), omegae(3, 3), omega(3, 3), dq(3, 3)
    real(kind=8) :: vind(nvi), vinf(nvi), l(3, 3), qm(3, 3)
    data iden/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
!
!     LA MATRICE DE ROTATION QM EST STOCKEE DANS VIND (N-19 A N-9)
    do 24 i = 1, 3
        do 24 j = 1, 3
            qm(i,j)=vind(nvi-19+3*(i-1)+j)+iden(i,j)
24      continue
!
!     TAMPON CONTIENT L(3,3)
    do 21 i = 1, 3
        do 21 j = 1, 3
            l(i,j)=tampon(3*(i-1)+j)
21      continue
    do 22 i = 1, 3
        do 22 j = 1, 3
            omega(i,j)=0.5d0*(l(i,j)-l(j,i))
22      continue
!     LE VECTEUR  ROTATION PLASTIQUE EST STOCKE DANS VINF (N-9 A N-7)
    call r8inir(9, 0.d0, omegap, 1)
    omegap(2,3)=-omp(1)
    omegap(3,2)=+omp(1)
    omegap(1,3)=+omp(2)
    omegap(3,1)=-omp(2)
    omegap(1,2)=-omp(3)
    omegap(2,1)=+omp(3)
    do 23 i = 1, 3
        do 23 j = 1, 3
            omegae(i,j)=omega(i,j)-omegap(i,j)
23      continue
!     ANGLE = NORME DU VECTEUR AXIAL
    dtheta=sqrt(omegae(1,2)**2+omegae(1,3)**2+omegae(2,3)**2)
!
    call dcopy(9, iden, 1, dq, 1)
    if (dtheta .gt. r8miem()) then
        do 25 i = 1, 3
            do 25 j = 1, 3
                nax(i,j)=omegae(i,j)/dtheta
25          continue
        do 26 i = 1, 3
            do 26 j = 1, 3
                dq(i,j)=dq(i,j)+sin(dtheta)*nax(i,j)
26          continue
        do 27 i = 1, 3
            do 27 j = 1, 3
                do 27 k = 1, 3
                    dq(i,j)=dq(i,j)+(1.d0-cos(dtheta))*nax(i,k)*nax(k,&
                    j)
27              continue
    endif
    call r8inir(9, 0.d0, q, 1)
    do 28 i = 1, 3
        do 28 j = 1, 3
            do 28 k = 1, 3
                q(i,j)=q(i,j)+dq(i,k)*qm(k,j)
28          continue
!
! LA MATRICE DE ROTATION EST STOCKEE DANS VINF (N-18 A N-10)
    do 29 i = 1, 3
        do 29 j = 1, 3
            vinf(nvi-19+3*(i-1)+j)=(q(i,j)-iden(i,j))
29      continue
!
! LE VECTEUR D-ROTATION PLASTIQUE EST STOCKE DANS VINF (N-9 A N-7)
!
    vinf(nvi-9) = omp(1)
    vinf(nvi-8) = omp(2)
    vinf(nvi-7) = omp(3)
!
! LE VECTEUR D-ROTATION ELASTIQUE EST STOCKE DANS VINF (N-6 A N-4)
!
    vinf(nvi-6) = omegae(3,2)
    vinf(nvi-5) = omegae(1,3)
    vinf(nvi-4) = omegae(2,1)
!
    vinf(nvi-3) = dtheta+vind(nvi-3)
!
end subroutine

subroutine e1e2nn(nno, dfde, dfdk, e1n, e2n,&
                  nxn, nyn, nzn, normn, j1n,&
                  j2n, san, can)
    implicit none
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
!...................................................................
!
!  BUT: CALCUL DES VECTEURS DE BASE NATURELLE SUR L'ELEMENT PHYSIQUE
!
!  ENTREES  ---> DERIVEES DE FONCTIONS DE FORME
!                COORDONNES DES NOEUDS
!                NOMBRE DE NOEUDS
!...................................................................
#include "jeveux.h"
#include "asterfort/jevech.h"
    real(kind=8) :: jac(9), nxn(9), nyn(9), nzn(9), san(9)
    real(kind=8) :: normn(3, 9), e1n(3, 9), e2n(3, 9), uni1n(3, 9)
    real(kind=8) :: dfde(9, 9), dfdk(9, 9), j1n(9), j2n(9), can(9)
    real(kind=8) :: sx(9, 9), sy(9, 9), sz(9, 9), uni2n(3, 9)
    integer :: igeom, i, ino, j, jno, k, nno
!
!
    call jevech('PGEOMER', 'L', igeom)
!
! CALCUL DE E1 E2 AUX NOEUDS
    do 10 i = 1, nno
        e1n(1,i)=0.0d0
        e1n(2,i)=0.0d0
        e1n(3,i)=0.0d0
!
        e2n(1,i)=0.0d0
        e2n(2,i)=0.0d0
        e2n(3,i)=0.0d0
!
        do 11 j = 1, nno
            e1n(1,i)= e1n(1,i)+dfde(j,i)*zr(igeom + 3*(j-1) -1+1)
            e1n(2,i)= e1n(2,i)+dfde(j,i)*zr(igeom + 3*(j-1) -1+2)
            e1n(3,i)= e1n(3,i)+dfde(j,i)*zr(igeom + 3*(j-1) -1+3)
!
            e2n(1,i)= e2n(1,i)+dfdk(j,i)*zr(igeom + 3*(j-1) -1+1)
            e2n(2,i)= e2n(2,i)+dfdk(j,i)*zr(igeom + 3*(j-1) -1+2)
            e2n(3,i)= e2n(3,i)+dfdk(j,i)*zr(igeom + 3*(j-1) -1+3)
!
11      continue
        j1n(i)=sqrt(e1n(1,i)**2+e1n(2,i)**2+e1n(3,i)**2)
!
        uni1n(1,i)=e1n(1,i)/j1n(i)
        uni1n(2,i)=e1n(2,i)/j1n(i)
        uni1n(3,i)=e1n(3,i)/j1n(i)
!
10  end do
! CALCUL DE LA NORMALE AUX NOEUDS
!    CALCUL DU PRODUIT VECTORIEL DE E1 E2
    do 12 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 13 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
13      continue
12  end do
    do 14 i = 1, nno
        nxn(i)=0.0d0
        nyn(i)=0.0d0
        nzn(i)=0.0d0
        do 15 j = 1, nno
            do 16 k = 1, nno
                nxn(i)= nxn(i)+dfde(j,i)*dfdk(k,i)*sx(j,k)
                nyn(i)= nyn(i)+dfde(j,i)*dfdk(k,i)*sy(j,k)
                nzn(i)= nzn(i)+dfde(j,i)*dfdk(k,i)*sz(j,k)
16          continue
15      continue
!       CALCUL DU JACOBIEN AUX NOEUDS
!
        jac(i) = sqrt (nxn(i)*nxn(i) + nyn(i)*nyn(i) + nzn(i)*nzn(i))
!
! CALCUL DE LA NORMALE UNITAIRE AUX NOEUDS
!
        normn(1,i)=nxn(i)/jac(i)
        normn(2,i)=nyn(i)/jac(i)
        normn(3,i)=nzn(i)/jac(i)
!
! CALCUL DU VECTEUR UNI2 ORTHOGONAL AUX NOEUDS A E1N
!
        uni2n(1,i)= -uni1n(2,i)*normn(3,i) +uni1n(3,i)*normn(2,i)
        uni2n(2,i)=-uni1n(3,i)*normn(1,i) +uni1n(1,i)*normn(3,i)
        uni2n(3,i)=-uni1n(1,i)*normn(2,i) +uni1n(2,i)*normn(1,i)
!
14  continue
! CALCUL DE LA NORME DE E1 E2 AUX NOEUDS
    do 17 i = 1, nno
!
        j2n(i)=sqrt(e2n(1,i)**2+e2n(2,i)**2+e2n(3,i)**2)
!
        if (j2n(i) .lt. (1.d-15)) then
            can(i) = 0.d0
            san(i) = 0.d0
        else
            can(i)=(uni2n(1,i)*e2n(1,i)+uni2n(2,i)*e2n(2,i) +uni2n(3,&
            i)*e2n(3,i))/j2n(i)
            san(i)=(uni1n(1,i)*e2n(1,i)+uni1n(2,i)*e2n(2,i) +uni1n(3,&
            i)*e2n(3,i))/j2n(i)
        endif
!
!
17  continue
end subroutine

subroutine ceobfb(bm, epsm, lambda, mu, ecrob,&
                  bdim, fb, nofbm, fbm)
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
! person_in_charge: ludovic.idoux at edf.fr
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/diago2.h"
#include "asterfort/diago3.h"
#include "asterfort/r8inir.h"
    real(kind=8) :: epsm(6), bm(6), fb(6), fbm(6), nofbm
    real(kind=8) :: lambda, mu, ecrob
    integer :: bdim
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT DU MODELE D'ENDOMMAGEMENT ANISOTROPE
!     ROUTINE DE CALCUL DE LA FORCE THERMODYNAMIQUE FB
!
!  IN BM     : TENSEUR D'ENDOMMAGEMENT DE TRACTION
!  IN EPSM   : TENSEUR DE DEFORMATION
!  IN LAMBDA : /
!  IN MU     : / COEFFICIENTS DE LAME
!  IN ECROB  : PARAMETRE DU MODELE
!  IN BDIM   : DIMENSION DE L ESPACE
!
! OUT FB     : FORCE THERMODYNAMIQUE
! OUT FBM    : PARTIE POSITIVE DE FB
! OUT NOFBM  : NORME DE FBM
! ----------------------------------------------------------------------
!
    integer :: i, j, k
    integer :: t(3, 3), r(2, 2)
!
    real(kind=8) :: cc(6), cpe(6), ccp(6), eps(6), b(6), fbs(3)
    real(kind=8) :: deux, treb, kron(6)
    real(kind=8) :: vecc(3, 3), valcc(3), vecfb(3, 3), valfb(3)
    real(kind=8) :: vecfbs(2, 2), valfbs(2)
!
    data  kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
    t(1,1)=1
    t(1,2)=4
    t(1,3)=5
    t(2,1)=4
    t(2,2)=2
    t(2,3)=6
    t(3,1)=5
    t(3,2)=6
    t(3,3)=3
!
    deux=2.d0
!
    do 100 i = 1, 6
        b(i)=bm(i)
        eps(i)=epsm(i)
100  end do
!
! CALCUL DE FB
!
    call r8inir(6, 0.d0, cc, 1)
!
    do 9 i = 1, 3
        do 10 j = i, 3
            do 11 k = 1, 3
                cc(t(i,j))=cc(t(i,j))+b(t(i,k))*eps(t(k,j))+ b(t(j,k))&
                *eps(t(k,i))
11          continue
10      continue
 9  end do
    call diago3(cc, vecc, valcc)
    call r8inir(6, 0.d0, ccp, 1)
    call r8inir(6, 0.d0, cpe, 1)
    do 12 i = 1, 3
        if (valcc(i) .lt. 0.d0) then
            valcc(i)=0.d0
        endif
12  end do
    do 13 i = 1, 3
        do 14 j = i, 3
            do 15 k = 1, 3
                ccp(t(i,j))=ccp(t(i,j))+vecc(i,k)*valcc(k)*vecc(j,k)
15          continue
14      continue
13  end do
    do 16 i = 1, 3
        do 17 j = i, 3
            do 18 k = 1, 3
                cpe(t(i,j))=cpe(t(i,j))+ ccp(t(i,k))*eps(t(k,j))+&
                ccp(t(j,k))*eps(t(k,i))
18          continue
17      continue
16  end do
!
    call r8inir(6, 0.d0, fb, 1)
    treb=0.d0
    do 301 i = 1, 3
        treb=treb+cc(i)/deux
301  end do
    if (treb .gt. 0.d0) then
        do 19 i = 1, 6
            fb(i)=-lambda*treb*eps(i)
19      continue
    endif
    do 20 i = 1, 6
        fb(i)=fb(i)-mu/deux*cpe(i)+ecrob*(kron(i)-b(i))
20  end do
!
! CALCUL DE LA PARTIE POSITIVE DE FBM ET DE SA NORME NOFB
!
    call r8inir(6, 0.d0, fbm, 1)
    if (bdim .eq. 3) then
        call diago3(fb, vecfb, valfb)
        nofbm=0.d0
!
        do 129 i = 1, 3
            if (valfb(i) .gt. 0.d0) then
                valfb(i)=0.d0
            endif
            nofbm=nofbm+valfb(i)*valfb(i)
129      continue
!
        do 126 i = 1, 3
            do 127 j = i, 3
                do 128 k = 1, 3
                    fbm(t(i,j))=fbm(t(i,j))+vecfb(i,k)*valfb(k)*vecfb(&
                    j,k)
128              continue
127          continue
126      continue
!
    else if (bdim.eq.2) then
        r(1,1)=1
        r(2,2)=2
        r(1,2)=3
        r(2,1)=3
        fbs(1)=fb(1)
        fbs(2)=fb(2)
        fbs(3)=fb(4)
!
        call diago2(fbs, vecfbs, valfbs)
!
        nofbm=0.d0
        do 29 i = 1, 2
            if (valfbs(i) .gt. 0.d0) then
                valfbs(i)=0.d0
            endif
            nofbm=nofbm+valfbs(i)*valfbs(i)
29      continue
!
        do 26 i = 1, 2
            do 27 j = i, 2
                do 28 k = 1, 2
                    fbm(r(i,j))=fbm(r(i,j))+vecfbs(i,k)*valfbs(k)*&
                    vecfbs(j,k)
28              continue
27          continue
26      continue
!
    else if (bdim.eq.1) then
        if (fb(1) .lt. 0.d0) then
            fbm(1)=fb(1)
        endif
        nofbm=fbm(1)**2
!
    endif
!
    if (abs(nofbm) .lt. r8prem()) then
        nofbm=0.d0
    endif
!
end subroutine

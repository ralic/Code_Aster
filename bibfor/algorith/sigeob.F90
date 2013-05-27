subroutine sigeob(eps, bt, endo, ndim, lambda,&
                  mu, sigm)
!
    implicit none
    include 'asterfort/diago3.h'
    include 'asterfort/r8inir.h'
    integer :: ndim
    real(kind=8) :: eps(6), bt(6), lambda, mu
    real(kind=8) :: sigm(6), endo
!
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!
!----------------------------------------------------------------------
!     CALCUL DE LA CONTRAINTE POUR LA LOI DE COMPORTEMENT
!     ENDO_ORTH_BETON
!
!     IN  EPS      : DEFORMATION
!     IN  B        : TENSEUR D ENDOMMAGEMENT DE TRACTION
!     IN  ENDO     : ENDOMMAGEMENT SCALAIRE DE COMPRESSION
!     IN  NDIM     : DIMENSION 3(3D) OU 2(2D)
!     IN  LAMBDA MU: COEFFICIENT DE LAME
!     OUT SIGM     : CONTRAINTE
!----------------------------------------------------------------------
!
    real(kind=8) :: rac2, deux, un
    real(kind=8) :: treb, treps, be(6), beeb(6), b(6)
    real(kind=8) :: to(6), tu(6), vp(3), vpe(3)
    real(kind=8) :: valbe(3), vecbe(3, 3)
    real(kind=8) :: valeps(3), veceps(3, 3), phid
    integer :: i, j, k, t(3, 3)
!
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
    deux=2.d0
    rac2=sqrt(deux)
    deux=2.d0
    un=1.d0
!
    phid = (un-endo)**deux
!
!      CALL DIAGO3(BT,VECB,VALB)
!      CALL R8INIR(3,0.D0,VB,1)
!      DO 32 I=1,NDIM
!          VB(I)=VALB(I)
! 32   CONTINUE
!
    call r8inir(6, 0.d0, b, 1)
!      DO 33 I=1,NDIM
!        DO 34 J=I,NDIM
!          DO 35 K=1,NDIM
!            B(T(I,J))=B(T(I,J))+VECB(I,K)*VB(K)*VECB(J,K)
! 35       CONTINUE
! 34     CONTINUE
! 33   CONTINUE
    do 32 i = 1, 6
        b(i)=bt(i)
32  end do
    call r8inir(6, 0.d0, sigm, 1)
    call r8inir(6, 0.d0, be, 1)
    do 1 i = 1, ndim
        do 2 j = i, ndim
            do 3 k = 1, ndim
                be(t(i,j))=be(t(i,j))+b(t(i,k))*eps(t(k,j))
 3          continue
 2      continue
 1  end do
!
    treb=0.d0
    do 4 i = 1, ndim
        treb=treb+be(i)
 4  end do
!
    treps=0.d0
    do 5 i = 1, ndim
        treps=treps+eps(t(i,i))
 5  end do
    if (treb .ge. 0.d0) then
        do 6 i = 1, ndim
            do 7 j = i, ndim
                sigm(t(i,j))=sigm(t(i,j))+lambda*treb*b(t(i,j))
 7          continue
 6      continue
    endif
    if (treps .lt. 0.d0) then
        do 8 i = 1, ndim
            sigm(t(i,i))=sigm(t(i,i))+phid*lambda*treps
 8      continue
    endif
    call r8inir(6, 0.d0, beeb, 1)
    do 9 i = 1, ndim
        do 10 j = i, ndim
            do 11 k = 1, ndim
                beeb(t(i,j))=beeb(t(i,j))+ b(t(i,k))*eps(t(k,j))+b(t(&
                j,k))*eps(t(k,i))
11          continue
10      continue
 9  end do
!
    call diago3(beeb, vecbe, valbe)
    call r8inir(3, 0.d0, vp, 1)
    do 12 i = 1, ndim
        if (valbe(i) .gt. 0.d0) then
            vp(i)=valbe(i)
        else
            vp(i)=0.d0
        endif
12  end do
!
    call r8inir(6, 0.d0, to, 1)
    do 13 i = 1, ndim
        do 14 j = i, ndim
            do 15 k = 1, ndim
                to(t(i,j))=to(t(i,j))+vecbe(i,k)*vp(k)*vecbe(j,k)
15          continue
14      continue
13  end do
!
    do 16 i = 1, ndim
        do 17 j = i, ndim
            do 18 k = 1, ndim
                sigm(t(i,j))=sigm(t(i,j))+mu/2*(to(t(i,k))*b(t(k,j))+&
                to(t(j,k))*b(t(k,i)))
18          continue
17      continue
16  end do
    call diago3(eps, veceps, valeps)
    call r8inir(3, 0.d0, vpe, 1)
!
    do 19 i = 1, ndim
        if (valeps(i) .lt. 0.d0) then
            vpe(i)=valeps(i)
        else
            vpe(i)=0.d0
        endif
19  end do
!
    call r8inir(6, 0.d0, tu, 1)
    do 20 i = 1, ndim
        do 21 j = i, ndim
            do 22 k = 1, ndim
                tu(t(i,j))=tu(t(i,j))+veceps(i,k)*vpe(k)*veceps(j,k)
22          continue
21      continue
20  end do
!
    do 23 i = 1, ndim
        do 24 j = i, ndim
            sigm(t(i,j))=sigm(t(i,j))+deux*mu*phid*tu(t(i,j))
24      continue
23  end do
!
!
    sigm(4)=rac2*sigm(4)
    sigm(5)=rac2*sigm(5)
    sigm(6)=rac2*sigm(6)
!
!
end subroutine

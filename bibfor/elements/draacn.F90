subroutine draacn(deg, poly, nbroot, root)
!
    implicit none
!
!-----------------------------------------------------------------------
! TOLE CRS_1404
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
!======================================================================
!
!     EVALUE LES RACINES DU POLYNOME DE DEGRE DEG PAR L ALGORITHME
!     DE BAIRSTOW AVEC POLISSAGE DES RACINES PAR L ALGO DE NEWTON
!
!
! IN  DEG : DEGRE DU POLYNOME
! IN  POLY : COEFFICIENT DU POLYNOME
!
! OUT NBROOT : NOMBRE DE RACINE DU POLYNOME
! OUT ROOT : RACINE DU POLYNOME
!
    include 'asterfort/dclass.h'
    include 'asterfort/draac2.h'
    include 'asterfort/nwtpol.h'
    include 'asterfort/r8inir.h'
    integer :: i, j, k, n, nroot, iran(20)
    integer :: i2, kkk
    integer :: deg, nbroot
!
    real(kind=8) :: poly(deg+1), root(deg)
    real(kind=8) :: a(deg+1), b(deg+3), c(deg+3)
    real(kind=8) :: dp1, dq, d, e, f, p1, q, ttt, u, x, y, z
    real(kind=8) :: realro(deg), ccoef, coef(deg+1)
!
    real(kind=8) :: zero
!
    n = deg
    zero = 1.0d-8
    ccoef=0.d0
!
!     COPIE DES COEFFICIENTS DU POLYNOME ET
!     RECHERCHE DU PLUS GRAND COEFFICIENT EN VALEUR ABSOLUE
    do 20, i = 1, deg+1
    a(i) = poly(i)
    ccoef = max(ccoef,abs(a(i)))
    20 end do
!
    if (abs(a(1)) .le. zero*ccoef) then
        do 24, n = deg, 1, -1
        do 23, i = 1, n
        a(i) = a(i+1)
23      continue
!
        a(n+1)=0.d0
24      continue
    else
        ccoef=a(1)
        do 30, i = 1,n+1
        a(i)=a(i)/ccoef
30      continue
    endif
!
    p1 = 0.d0
    q = 0.d0
    k = 100
    e = 1.d-4
!
!     INITIALISATION DES TABLEAUX
    call r8inir(deg, 0.0d0, root, 1)
    call r8inir(deg, 0.0d0, realro, 1)
!
    nbroot=0
!
    do 59, n = n, 3, -2
!
    j=0
    f=e+1.0d0
!
    do 49, kkk = 1,10000000
    if (f .le. e) goto 50
!
    if (j .gt. k) goto 60
!
    j=j+1
    b(1) = 0.d0
    b(2) = 0.d0
    c(1) = 0.d0
    c(2) = 0.d0
!
    do 40, i2 = 1, n+1
    i = i2 + 2
    b(i) = a(i-2)-p1*b(i-1)-q*b(i-2)
    c(i) = -b(i-1)-p1*c(i-1)-q*c(i-2)
40  continue
!
    x = b(n+2)
    y = b(n+3)
    z = c(n+1)
    ttt = c(n+2)
    u = c(n+3)
    d = ttt*ttt-z*(u+x)
!
    if (d .eq. 0.d0) goto 60
!
    dp1=(z*y-x*ttt)/d
    dq=(-x*(q*z+p1*ttt)-y*ttt)/d
    p1=p1+dp1
    q=q+dq
    f=(abs(dp1)+abs(dq))/(abs(p1)+abs(q))
49  continue
!
50  continue
!
!     RECHERCHE DES RACINES DU POLYNOME DU SECOND DEGRE
!     Y = X**2 + P1 * X + Q
    call draac2(1.d0, p1, q, realro(nbroot+1), realro(nbroot+2),&
                nroot)
!
    nbroot=nbroot+nroot
!
    do 55, i = 1,n-1
    a(i) = b(i+2)
55  continue
    59 end do
!
60  continue
!
    if (n .eq. 2) then
!     RECHERCHE DES RACINES DU POLYNOME DU SECOND DEGRE
!     Y = A(1) * X**2 + A(2) * X + A(3)
        call draac2(a(1), a(2), a(3), realro(nbroot+1), realro(nbroot+2),&
                    nroot)
        nbroot = nbroot+nroot
    else if (n .eq. 1) then
        nbroot = nbroot + 1
        realro(nbroot) = -a(2)/a(1)
    endif
!
    if (nbroot .gt. 0) then
        do 65, i=1,deg+1
        coef(i) = poly(deg-i+2)
65      continue
!
        do 67, i=1,nbroot
!     RAFFINE LA RECHERCHE DES RACINES PAR LA METHODE DE NEWTON
        call nwtpol(deg, coef, realro(i))
67      continue
    endif
!
    if (nbroot .gt. 1) then
!     EVALUE L ORDRE DES RACINES DANS IRAN
        call dclass(nbroot, realro, iran)
!
        do 70, i = 1,nbroot
        root(i)=realro(iran(i))
70      continue
    endif
!
end subroutine

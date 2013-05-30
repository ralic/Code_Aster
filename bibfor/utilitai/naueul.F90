subroutine naueul(angnau, angeul)
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
!     PASSAGE DES ANGLES NAUTIQUES AUX ANGLES D'EULER
!     IN : ANGNAU(3) : 3 ANGLES NAUTIQUES
!     OUT : ANGEUL(3) : 3 ANGLES D'EULER ENTRE 0 ET 2PI
!
    include 'asterc/r8miem.h'
    include 'asterc/r8pi.h'
    include 'asterfort/angrot.h'
    include 'asterfort/matrot.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/utpvlg.h'
    real(kind=8) :: angnau(3), pn(3, 3), angeul(3), xl(3), yl(3), zl(3)
    real(kind=8) :: xg(3), yg(3), zg(3), x1(3), phi, zlzg(3)
    real(kind=8) :: phi1, phi2, nv3
    integer :: i
!
!     MATRICE ROTATION ASSOCIEE AUX ANGLES NAUTIQUES
!
    call matrot(angnau, pn)
!
!     3 VECTEURS UNITAIRES DE BASE
!
    call r8inir(3, 0.d0, xl, 1)
    call r8inir(3, 0.d0, yl, 1)
    call r8inir(3, 0.d0, zl, 1)
    xl(1)=1.d0
    yl(2)=1.d0
    zl(3)=1.d0
!
!     EXPRESSION DES COORDONNEES DES 3 VECTEURS DE LA BASE LOCALE
!     DANS LE REPERE GLOBAL
    call utpvlg(1, 3, pn, xl, xg)
    call utpvlg(1, 3, pn, yl, yg)
    call utpvlg(1, 3, pn, zl, zg)
!
!     CALCUL DE X1
    call provec(zl, zg, zlzg)
!
    call normev(zlzg, nv3)
    if (nv3 .lt. r8miem()) then
        do 20 i = 1, 3
            x1(i)=xg(i)
20      continue
    else
        do 30 i = 1, 3
            x1(i)=zlzg(i)
30      continue
    endif
!
!     PREMIER ANGLE D'EULER : PHI1 (OU PSI )
    call angrot(xl, x1, zl, phi1)
!     DEUXIEME ANGLE D'EULER : PHI (OU THETA)
    call angrot(zl, zg, x1, phi)
!     DEUXIEME ANGLE D'EULER : PHI2 (OU PHI)
    call angrot(x1, xg, zg, phi2)
!
    angeul(1)=phi1
    angeul(2)=phi
    angeul(3)=phi2
    if (angeul(1) .lt. 0.d0) angeul(1)=angeul(1)+2.0d0*r8pi()
    if (angeul(2) .lt. 0.d0) angeul(2)=angeul(2)+2.0d0*r8pi()
    if (angeul(3) .lt. 0.d0) angeul(3)=angeul(3)+2.0d0*r8pi()
!
!
end subroutine

subroutine i3chgr(a, b, t1, t2, n)
    implicit   none
    include 'asterc/r8prem.h'
    include 'asterfort/rvegal.h'
    integer :: n
    real(kind=8) :: a(*), b(*), t1(*), t2(*)
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     ------------------------------------------------------------------
!     CHANGEMENT DE REPERE POUR PLACER (AB) SUIVANT L' AXE 3
!     ------------------------------------------------------------------
! IN  A      : R : COORDONNEES DE A
! IN  B      : R : COORDONNEES DE B
! IN  N      : I : NOMBRE DE NOEUD DU MAILLAGE
! IN  T1     : R : COORDONNEES DES NOEUDS REPERE GLOBALE
! OUT T2     : R : COORDONNEES DES NOEUDS NOUVEAU REPERE
!     ------------------------------------------------------------------
!
    integer :: i, j
    real(kind=8) :: epsi, un, xa, xb, ya, yb, za, zb, ct, st, cp, sp, d
    logical :: egalx, egaly
!
!======================================================================
!
    epsi = r8prem()
    un = 1.0d0
    xa = a(1)
    ya = a(2)
    za = a(3)
    xb = b(1)
    yb = b(2)
    zb = b(3)
    j = 0
!
    call rvegal(epsi, 'R', xa, xb, egalx,&
                d)
    call rvegal(epsi, 'R', ya, yb, egaly,&
                d)
!
    if (.not. (egalx .and. egaly)) then
        do 10, i = 1, n, 1
        t2(j+1) = t1(j+1) - xa
        t2(j+2) = t1(j+2) - ya
        t2(j+3) = t1(j+3) - za
        j = j + 3
10      continue
        xb = xb - xa
        yb = yb - ya
        zb = zb - za
        d = un/sqrt(xb*xb + yb*yb)
        ct = xb*d
        st = yb*d
        xb = ct*xb + st*yb
        d = un/sqrt(xb*xb + zb*zb)
        cp = xb*d
        sp = zb*d
        xa = ct*cp
        ya = ct*sp
        za = st*cp
        d = st*sp
        j = 0
        do 20, i = 1, n, 1
        xb = t2(j+1)
        yb = t2(j+2)
        zb = t2(j+3)
        t2(j+1) = ya*xb + d*yb - cp*zb
        t2(j+2) = -st*xb + ct*yb
        t2(j+3) = xa*xb + za*yb + sp*zb
        j = j + 3
20      continue
    else
        if (zb .ge. za) then
            do 30, i = 1, n, 1
            t2(j+1) = t1(j+1) - xa
            t2(j+2) = t1(j+2) - ya
            t2(j+3) = t1(j+3) - za
            j = j + 3
30          continue
        else
            do 32, i = 1, n, 1
            t2(j+1) = t1(j+1) - xb
            t2(j+2) = t1(j+2) - yb
            t2(j+3) = t1(j+3) - zb
            j = j + 3
32          continue
            a(1) = xb
            a(2) = yb
            a(3) = zb
            b(1) = xa
            b(2) = ya
            b(3) = za
        endif
    endif
end subroutine

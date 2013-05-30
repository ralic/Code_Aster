subroutine elrfdf(elrefz, x, dimd, dff, nno,&
                  ndim)
    implicit none
    include 'asterfort/assert.h'
    integer :: dimd, nno, ndim
    real(kind=8) :: x(*), dff(3, *)
    character(len=*) :: elrefz
! ----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
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
!
!
! ======================================================================
! TOLE CRP_20
!
! BUT:   CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES
!        AU POINT DE COORDONNEES XI,YI,ZI
!
! ----------------------------------------------------------------------
!   IN   ELREFZ : NOM DE L'ELREFE (K8)
!        X      : POINT DE CALCUL DES F FORMES ET DERIVEES
!        DIMD   : DIMENSION DE DFF
!   OUT  DFF    : FONCTIONS DE FORMES EN XI,YI,ZI
!        NNO    : NOMBRE DE NOEUDS
!        NDIM : DIMENSION TOPOLOGIQUE DE L'ELREFE
!   -------------------------------------------------------------------
    character(len=8) :: elrefe
    integer :: i, j
    real(kind=8) :: zero, undemi, un, deux, trois, quatre, uns4, uns8
    real(kind=8) :: x0, y0, z0, al, x1, x2, x3, x4, d1, d2, d3, d4
    real(kind=8) :: pface1, pface2, pface3, pface4, z01, z02, z04
    real(kind=8) :: pmili1, pmili2, pmili3, pmili4, huit
    real(kind=8) :: u, al31, al32, al33, dal31, dal32, dal33
    real(kind=8) :: r, r1, r2, a, b, c, d, e, f, g, h, o, p, q, s, t
!
! -----  FONCTIONS FORMULES
    al31(u) = 0.5d0*u*(u-1.0d0)
    al32(u) = -(u+1.0d0)*(u-1.0d0)
    al33(u) = 0.5d0*u*(u+1.0d0)
    dal31(u) = 0.5d0*(2.0d0*u-1.0d0)
    dal32(u) = -2.0d0*u
    dal33(u) = 0.5d0*(2.0d0*u+1.0d0)
! DEB ------------------------------------------------------------------
    elrefe = elrefz
!
    zero = 0.0d0
    undemi = 0.5d0
    un = 1.0d0
    deux = 2.0d0
    trois = 3.0d0
    quatre = 4.0d0
    huit = 8.0d0
    uns4 = un/quatre
    uns8 = un/huit
!
!     ------------------------------------------------------------------
    if (elrefe .eq. 'HE8') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 8
        ndim = 3
!
        dff(1,1) = - (un-y0)* (un-z0)*uns8
        dff(2,1) = - (un-x0)* (un-z0)*uns8
        dff(3,1) = - (un-x0)* (un-y0)*uns8
        dff(1,2) = (un-y0)* (un-z0)*uns8
        dff(2,2) = - (un+x0)* (un-z0)*uns8
        dff(3,2) = - (un+x0)* (un-y0)*uns8
        dff(1,3) = (un+y0)* (un-z0)*uns8
        dff(2,3) = (un+x0)* (un-z0)*uns8
        dff(3,3) = - (un+x0)* (un+y0)*uns8
        dff(1,4) = - (un+y0)* (un-z0)*uns8
        dff(2,4) = (un-x0)* (un-z0)*uns8
        dff(3,4) = - (un-x0)* (un+y0)*uns8
        dff(1,5) = - (un-y0)* (un+z0)*uns8
        dff(2,5) = - (un-x0)* (un+z0)*uns8
        dff(3,5) = (un-x0)* (un-y0)*uns8
        dff(1,6) = (un-y0)* (un+z0)*uns8
        dff(2,6) = - (un+x0)* (un+z0)*uns8
        dff(3,6) = (un+x0)* (un-y0)*uns8
        dff(1,7) = (un+y0)* (un+z0)*uns8
        dff(2,7) = (un+x0)* (un+z0)*uns8
        dff(3,7) = (un+x0)* (un+y0)*uns8
        dff(1,8) = - (un+y0)* (un+z0)*uns8
        dff(2,8) = (un-x0)* (un+z0)*uns8
        dff(3,8) = (un-x0)* (un+y0)*uns8
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'HH8') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 8
        ndim = 3
        r1 = 0.2165063509461097D0
        a = r1 - 0.375d0*x0
        b = r1 - 0.375d0*y0
        c = r1 - 0.375d0*z0
        d = r1 + 0.375d0*x0
        e = r1 + 0.375d0*y0
        f = r1 + 0.375d0*z0
        r2 = 1.7320508075688773D0
        x0 = x0 * r2
        y0 = y0 * r2
        z0 = z0 * r2
!
        dff(1,1) = b*(z0 - un)
        dff(2,1) = a*(z0 - un)
        dff(3,1) = a*(y0 - un)
        dff(1,2) = -b*(z0 - un)
        dff(2,2) = d*(z0 - un)
        dff(3,2) = d*(y0 - un)
        dff(1,3) = e*(un - z0)
        dff(2,3) = -d*(z0 - un)
        dff(3,3) = -d*(un + y0)
        dff(1,4) = -e*(un - z0)
        dff(2,4) = -a*(z0 - un)
        dff(3,4) = -a*(un + y0)
        dff(1,5) = -b*(un + z0)
        dff(2,5) = -a*(un + z0)
        dff(3,5) = -a*(y0 - un)
        dff(1,6) = b*(un + z0)
        dff(2,6) = -d*(un + z0)
        dff(3,6) = -d*(y0 - un)
        dff(1,7) = e*(un + z0)
        dff(2,7) = d*(un + z0)
        dff(3,7) = d*(un + y0)
        dff(1,8) = -e*(un + z0)
        dff(2,8) = a*(un + z0)
        dff(3,8) = a*(un + y0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'H16') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 16
        ndim = 3
!
        a = undemi*(un - x0)
        b = undemi*(un - y0)
        c = undemi*(un - z0)
        d = a + x0
        e = b + y0
        f = c + z0
        g = undemi*(un - y0*y0)
        h = undemi*(un - z0*z0)
        o = undemi*y0
        p = undemi*z0
!
        dff(1,1) = b*c*( e + p )
        dff(2,1) = a*c*( y0 + p )
        dff(3,1) = a*b*( z0 + o )
        dff(1,2) = -b*c*( e + p )
        dff(2,2) = d*c*( y0 + p )
        dff(3,2) = d*b*( z0 + o )
        dff(1,3) = e*c*(-b - p )
        dff(2,3) = d*c*( y0 - p )
        dff(3,3) = d*e*( z0 - o )
        dff(1,4) = -e*c*( -b - p )
        dff(2,4) = a*c*( y0 - p )
        dff(3,4) = a*e*( z0 - o )
        dff(1,5) = b*f*( e - p )
        dff(2,5) = a*f*( y0 - p )
        dff(3,5) = a*b*( z0 - o )
        dff(1,6) = -b*f*( e - p )
        dff(2,6) = d*f*( y0 - p )
        dff(3,6) = d*b*( z0 - o )
        dff(1,7) = e*f*( -b + p )
        dff(2,7) = d*f*( y0 + p )
        dff(3,7) = d*e*( z0 + o )
        dff(1,8) = -e*f*(-b + p )
        dff(2,8) = a*f*( y0 + p )
        dff(3,8) = a*e*( z0 + o )
!
        y0 = -deux*y0
        z0 = -deux*z0
!
        dff(1,9) = g*c
        dff(2,9) = d*y0*c
        dff(3,9) = -d*g
        dff(1,10) = -g*c
        dff(2,10) = a*y0*c
        dff(3,10) = -a*g
        dff(1,11) = -b*h
        dff(2,11) = -a*h
        dff(3,11) = a*b*z0
        dff(1,12) = b*h
        dff(2,12) = -d*h
        dff(3,12) = d*b*z0
        dff(1,13) = e*h
        dff(2,13) = d*h
        dff(3,13) = d*e*z0
        dff(1,14) = -e*h
        dff(2,14) = a*h
        dff(3,14) = a*e*z0
        dff(1,15) = g*f
        dff(2,15) = d*y0*f
        dff(3,15) = d*g
        dff(1,16) = -g*f
        dff(2,16) = a*y0*f
        dff(3,16) = a*g
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'H18') then
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 18
        ndim = 3
!
        a = undemi*(un - x0)
        b = undemi*y0*(y0 - un)
        c = undemi*z0*(z0 - un)
        d = a + x0
        e = b + y0
        f = c + z0
        g = undemi*(un - y0*y0)
        h = undemi*(un - z0*z0)
!
        dff(1,1) = -undemi*b*c
        dff(2,1) = a*c*(y0-undemi)
        dff(3,1) = a*b*(z0-undemi)
        dff(1,2) = undemi*b*c
        dff(2,2) = d*c*(y0-undemi)
        dff(3,2) = d*b*(z0-undemi)
        dff(1,3) = undemi*e*c
        dff(2,3) = d*c*(y0+undemi)
        dff(3,3) = d*e*(z0-undemi)
        dff(1,4) = -undemi*e*c
        dff(2,4) = a*c*(y0+undemi)
        dff(3,4) = a*e*(z0-undemi)
        dff(1,5) = -undemi*b*f
        dff(2,5) = a*f*(y0-undemi)
        dff(3,5) = a*b*(z0+undemi)
        dff(1,6) = undemi*b*f
        dff(2,6) = d*f*(y0-undemi)
        dff(3,6) = d*b*(z0+undemi)
        dff(1,7) = undemi*e*f
        dff(2,7) = d*f*(y0+undemi)
        dff(3,7) = d*e*(z0+undemi)
        dff(1,8) = -undemi*e*f
        dff(2,8) = a*f*(y0+undemi)
        dff(3,8) = a*e*(z0+undemi)
!
        y0 = deux*y0
        z0 = deux*z0
!
        dff(1,9) = g*c
        dff(2,9) = -d*y0*c
        dff(3,9) = d*g*(z0-un)
        dff(1,10) = -g*c
        dff(2,10) = -a*y0*c
        dff(3,10) = a*g*(z0-un)
        dff(1,11) = -b*h
        dff(2,11) = a*h*(y0-un)
        dff(3,11) = -a*b*z0
        dff(1,12) = b*h
        dff(2,12) = d*h*(y0-un)
        dff(3,12) = -d*b*z0
        dff(1,13) = e*h
        dff(2,13) = d*h*(y0+un)
        dff(3,13) = -d*e*z0
        dff(1,14) = -e*h
        dff(2,14) = a*h*(y0+un)
        dff(3,14) = -a*e*z0
        dff(1,15) = g*f
        dff(2,15) = -d*y0*f
        dff(3,15) = d*g*(z0+un)
        dff(1,16) = -g*f
        dff(2,16) = -a*y0*f
        dff(3,16) = a*g*(z0+un)
!
        y0 = deux*y0
        z0 = deux*z0
!
        dff(1,17) = deux*g*h
        dff(2,17) = -d*y0*h
        dff(3,17) = -d*g*z0
        dff(1,18) = -deux*g*h
        dff(2,18) = -a*y0*h
        dff(3,18) = -a*g*z0
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'H20') then
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 20
        ndim = 3
!
        dff(1,1) = - (un-y0)* (un-z0)* (-deux*x0-y0-z0-un)*uns8
        dff(2,1) = - (un-x0)* (un-z0)* (-x0-deux*y0-z0-un)*uns8
        dff(3,1) = - (un-x0)* (un-y0)* (-x0-y0-deux*z0-un)*uns8
        dff(1,2) = (un-y0)* (un-z0)* (deux*x0-y0-z0-un)*uns8
        dff(2,2) = - (un+x0)* (un-z0)* (x0-deux*y0-z0-un)*uns8
        dff(3,2) = - (un+x0)* (un-y0)* (x0-y0-deux*z0-un)*uns8
        dff(1,3) = (un+y0)* (un-z0)* (deux*x0+y0-z0-un)*uns8
        dff(2,3) = (un+x0)* (un-z0)* (x0+deux*y0-z0-un)*uns8
        dff(3,3) = - (un+x0)* (un+y0)* (x0+y0-deux*z0-un)*uns8
        dff(1,4) = - (un+y0)* (un-z0)* (-deux*x0+y0-z0-un)*uns8
        dff(2,4) = (un-x0)* (un-z0)* (-x0+deux*y0-z0-un)*uns8
        dff(3,4) = - (un-x0)* (un+y0)* (-x0+y0-deux*z0-un)*uns8
        dff(1,5) = - (un-y0)* (un+z0)* (-deux*x0-y0+z0-un)*uns8
        dff(2,5) = - (un-x0)* (un+z0)* (-x0-deux*y0+z0-un)*uns8
        dff(3,5) = (un-x0)* (un-y0)* (-x0-y0+deux*z0-un)*uns8
        dff(1,6) = (un-y0)* (un+z0)* (deux*x0-y0+z0-un)*uns8
        dff(2,6) = - (un+x0)* (un+z0)* (x0-deux*y0+z0-un)*uns8
        dff(3,6) = (un+x0)* (un-y0)* (x0-y0+deux*z0-un)*uns8
        dff(1,7) = (un+y0)* (un+z0)* (deux*x0+y0+z0-un)*uns8
        dff(2,7) = (un+x0)* (un+z0)* (x0+deux*y0+z0-un)*uns8
        dff(3,7) = (un+x0)* (un+y0)* (x0+y0+deux*z0-un)*uns8
        dff(1,8) = - (un+y0)* (un+z0)* (-deux*x0+y0+z0-un)*uns8
        dff(2,8) = (un-x0)* (un+z0)* (-x0+deux*y0+z0-un)*uns8
        dff(3,8) = (un-x0)* (un+y0)* (-x0+y0+deux*z0-un)*uns8
        dff(1,9) = -deux*x0* (un-y0)* (un-z0)*uns4
        dff(2,9) = - (un-x0*x0)* (un-z0)*uns4
        dff(3,9) = - (un-x0*x0)* (un-y0)*uns4
        dff(1,10) = (un-y0*y0)* (un-z0)*uns4
        dff(2,10) = -deux*y0* (un+x0)* (un-z0)*uns4
        dff(3,10) = - (un+x0)* (un-y0*y0)*uns4
        dff(1,11) = -deux*x0* (un+y0)* (un-z0)*uns4
        dff(2,11) = (un-x0*x0)* (un-z0)*uns4
        dff(3,11) = - (un-x0*x0)* (un+y0)*uns4
        dff(1,12) = - (un-y0*y0)* (un-z0)*uns4
        dff(2,12) = -deux*y0* (un-x0)* (un-z0)*uns4
        dff(3,12) = - (un-x0)* (un-y0*y0)*uns4
        dff(1,13) = - (un-z0*z0)* (un-y0)*uns4
        dff(2,13) = - (un-x0)* (un-z0*z0)*uns4
        dff(3,13) = -deux*z0* (un-y0)* (un-x0)*uns4
        dff(1,14) = (un-z0*z0)* (un-y0)*uns4
        dff(2,14) = - (un+x0)* (un-z0*z0)*uns4
        dff(3,14) = -deux*z0* (un-y0)* (un+x0)*uns4
        dff(1,15) = (un-z0*z0)* (un+y0)*uns4
        dff(2,15) = (un+x0)* (un-z0*z0)*uns4
        dff(3,15) = -deux*z0* (un+y0)* (un+x0)*uns4
        dff(1,16) = - (un-z0*z0)* (un+y0)*uns4
        dff(2,16) = (un-x0)* (un-z0*z0)*uns4
        dff(3,16) = -deux*z0* (un+y0)* (un-x0)*uns4
        dff(1,17) = -deux*x0* (un-y0)* (un+z0)*uns4
        dff(2,17) = - (un-x0*x0)* (un+z0)*uns4
        dff(3,17) = (un-x0*x0)* (un-y0)*uns4
        dff(1,18) = (un-y0*y0)* (un+z0)*uns4
        dff(2,18) = -deux*y0* (un+x0)* (un+z0)*uns4
        dff(3,18) = (un+x0)* (un-y0*y0)*uns4
        dff(1,19) = -deux*x0* (un+y0)* (un+z0)*uns4
        dff(2,19) = (un-x0*x0)* (un+z0)*uns4
        dff(3,19) = (un-x0*x0)* (un+y0)*uns4
        dff(1,20) = - (un-y0*y0)* (un+z0)*uns4
        dff(2,20) = -deux*y0* (un-x0)* (un+z0)*uns4
        dff(3,20) = (un-x0)* (un-y0*y0)*uns4
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'H27') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 27
        ndim = 3
!
        dff(1,1) = dal31(x0)*al31(y0)*al31(z0)
        dff(2,1) = al31(x0)*dal31(y0)*al31(z0)
        dff(3,1) = al31(x0)*al31(y0)*dal31(z0)
        dff(1,2) = dal33(x0)*al31(y0)*al31(z0)
        dff(2,2) = al33(x0)*dal31(y0)*al31(z0)
        dff(3,2) = al33(x0)*al31(y0)*dal31(z0)
        dff(1,3) = dal33(x0)*al33(y0)*al31(z0)
        dff(2,3) = al33(x0)*dal33(y0)*al31(z0)
        dff(3,3) = al33(x0)*al33(y0)*dal31(z0)
        dff(1,4) = dal31(x0)*al33(y0)*al31(z0)
        dff(2,4) = al31(x0)*dal33(y0)*al31(z0)
        dff(3,4) = al31(x0)*al33(y0)*dal31(z0)
        dff(1,5) = dal31(x0)*al31(y0)*al33(z0)
        dff(2,5) = al31(x0)*dal31(y0)*al33(z0)
        dff(3,5) = al31(x0)*al31(y0)*dal33(z0)
        dff(1,6) = dal33(x0)*al31(y0)*al33(z0)
        dff(2,6) = al33(x0)*dal31(y0)*al33(z0)
        dff(3,6) = al33(x0)*al31(y0)*dal33(z0)
        dff(1,7) = dal33(x0)*al33(y0)*al33(z0)
        dff(2,7) = al33(x0)*dal33(y0)*al33(z0)
        dff(3,7) = al33(x0)*al33(y0)*dal33(z0)
        dff(1,8) = dal31(x0)*al33(y0)*al33(z0)
        dff(2,8) = al31(x0)*dal33(y0)*al33(z0)
        dff(3,8) = al31(x0)*al33(y0)*dal33(z0)
        dff(1,9) = dal32(x0)*al31(y0)*al31(z0)
        dff(2,9) = al32(x0)*dal31(y0)*al31(z0)
        dff(3,9) = al32(x0)*al31(y0)*dal31(z0)
        dff(1,10) = dal33(x0)*al32(y0)*al31(z0)
        dff(2,10) = al33(x0)*dal32(y0)*al31(z0)
        dff(3,10) = al33(x0)*al32(y0)*dal31(z0)
        dff(1,11) = dal32(x0)*al33(y0)*al31(z0)
        dff(2,11) = al32(x0)*dal33(y0)*al31(z0)
        dff(3,11) = al32(x0)*al33(y0)*dal31(z0)
        dff(1,12) = dal31(x0)*al32(y0)*al31(z0)
        dff(2,12) = al31(x0)*dal32(y0)*al31(z0)
        dff(3,12) = al31(x0)*al32(y0)*dal31(z0)
        dff(1,13) = dal31(x0)*al31(y0)*al32(z0)
        dff(2,13) = al31(x0)*dal31(y0)*al32(z0)
        dff(3,13) = al31(x0)*al31(y0)*dal32(z0)
        dff(1,14) = dal33(x0)*al31(y0)*al32(z0)
        dff(2,14) = al33(x0)*dal31(y0)*al32(z0)
        dff(3,14) = al33(x0)*al31(y0)*dal32(z0)
        dff(1,15) = dal33(x0)*al33(y0)*al32(z0)
        dff(2,15) = al33(x0)*dal33(y0)*al32(z0)
        dff(3,15) = al33(x0)*al33(y0)*dal32(z0)
        dff(1,16) = dal31(x0)*al33(y0)*al32(z0)
        dff(2,16) = al31(x0)*dal33(y0)*al32(z0)
        dff(3,16) = al31(x0)*al33(y0)*dal32(z0)
        dff(1,17) = dal32(x0)*al31(y0)*al33(z0)
        dff(2,17) = al32(x0)*dal31(y0)*al33(z0)
        dff(3,17) = al32(x0)*al31(y0)*dal33(z0)
        dff(1,18) = dal33(x0)*al32(y0)*al33(z0)
        dff(2,18) = al33(x0)*dal32(y0)*al33(z0)
        dff(3,18) = al33(x0)*al32(y0)*dal33(z0)
        dff(1,19) = dal32(x0)*al33(y0)*al33(z0)
        dff(2,19) = al32(x0)*dal33(y0)*al33(z0)
        dff(3,19) = al32(x0)*al33(y0)*dal33(z0)
        dff(1,20) = dal31(x0)*al32(y0)*al33(z0)
        dff(2,20) = al31(x0)*dal32(y0)*al33(z0)
        dff(3,20) = al31(x0)*al32(y0)*dal33(z0)
        dff(1,21) = dal32(x0)*al32(y0)*al31(z0)
        dff(2,21) = al32(x0)*dal32(y0)*al31(z0)
        dff(3,21) = al32(x0)*al32(y0)*dal31(z0)
        dff(1,22) = dal32(x0)*al31(y0)*al32(z0)
        dff(2,22) = al32(x0)*dal31(y0)*al32(z0)
        dff(3,22) = al32(x0)*al31(y0)*dal32(z0)
        dff(1,23) = dal33(x0)*al32(y0)*al32(z0)
        dff(2,23) = al33(x0)*dal32(y0)*al32(z0)
        dff(3,23) = al33(x0)*al32(y0)*dal32(z0)
        dff(1,24) = dal32(x0)*al33(y0)*al32(z0)
        dff(2,24) = al32(x0)*dal33(y0)*al32(z0)
        dff(3,24) = al32(x0)*al33(y0)*dal32(z0)
        dff(1,25) = dal31(x0)*al32(y0)*al32(z0)
        dff(2,25) = al31(x0)*dal32(y0)*al32(z0)
        dff(3,25) = al31(x0)*al32(y0)*dal32(z0)
        dff(1,26) = dal32(x0)*al32(y0)*al33(z0)
        dff(2,26) = al32(x0)*dal32(y0)*al33(z0)
        dff(3,26) = al32(x0)*al32(y0)*dal33(z0)
        dff(1,27) = dal32(x0)*al32(y0)*al32(z0)
        dff(2,27) = al32(x0)*dal32(y0)*al32(z0)
        dff(3,27) = al32(x0)*al32(y0)*dal32(z0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'PE6') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 6
        ndim = 3
        al = (un-y0-z0)
!
        dff(1,1) = -y0*undemi
        dff(2,1) = (un-x0)*undemi
        dff(3,1) = zero
        dff(1,2) = -z0*undemi
        dff(2,2) = zero
        dff(3,2) = (un-x0)*undemi
        dff(1,3) = -al*undemi
        dff(2,3) = - (un-x0)*undemi
        dff(3,3) = - (un-x0)*undemi
        dff(1,4) = y0*undemi
        dff(2,4) = (un+x0)*undemi
        dff(3,4) = zero
        dff(1,5) = z0*undemi
        dff(2,5) = zero
        dff(3,5) = (un+x0)*undemi
        dff(1,6) = al*undemi
        dff(2,6) = - (un+x0)*undemi
        dff(3,6) = - (un+x0)*undemi
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'PH6') then
!
        r = 1.7320508075688773D0
        x0 = x(1) * r
        y0 = x(2) * r
        z0 = x(3) * r
        r1 = 0.2886751345948129D0
        r2 = 0.86602540378443865D0
        nno = 6
        ndim = 3
!
        dff(1,1) = -(un - z0)
        dff(2,1) = -(un - z0)
        dff(3,1) = -(r2 - (x0 - r1) - (y0 - r1))
        dff(1,2) = un - z0
        dff(2,2) = zero
        dff(3,2) = -(x0 - r1)
        dff(1,3) = zero
        dff(2,3) = un - z0
        dff(3,3) = -(y0 - r1)
        dff(1,4) = -(un + z0)
        dff(2,4) = -(un + z0)
        dff(3,4) = r2 - (x0 - r1) - (y0 - r1)
        dff(1,5) = un + z0
        dff(2,5) = zero
        dff(3,5) = x0 - r1
        dff(1,6) = zero
        dff(2,6) = un + z0
        dff(3,6) = y0 - r1
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'P12') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 12
        ndim = 3
!
        a = un - y0 - z0
        b = undemi*(un - x0)
        c = b + x0
        d = quatre*a*b
        e = quatre*y0*b
        f = quatre*z0*b
        g = quatre*a*c
        h = quatre*y0*c
        o = quatre*z0*c
!
        dff(1,1) = y0*(undemi - y0)
        dff(2,1) = e - b
        dff(3,1) = zero
        dff(1,2) = z0*(undemi - z0)
        dff(2,2) = zero
        dff(3,2) = f - b
        dff(1,3) = a*(undemi - a)
        dff(2,3) = b - d
        dff(3,3) = b - d
        dff(1,4) = -y0*(undemi - y0)
        dff(2,4) = h - c
        dff(3,4) = zero
        dff(1,5) = -z0*(undemi - z0)
        dff(2,5) = zero
        dff(3,5) = o - c
        dff(1,6) = -a*(undemi - a)
        dff(2,6) = c - g
        dff(3,6) = c - g
        dff(1,7) = -deux*y0*z0
        dff(2,7) = f
        dff(3,7) = e
        dff(1,8) = -deux*z0*a
        dff(2,8) = -f
        dff(3,8) = d - f
        dff(1,9) = -deux*a*y0
        dff(2,9) = d - e
        dff(3,9) = -e
        dff(1,10) = deux*y0*z0
        dff(2,10) = o
        dff(3,10) = h
        dff(1,11) = deux*z0*a
        dff(2,11) = -o
        dff(3,11) = g - o
        dff(1,12) = deux*a*y0
        dff(2,12) = g - h
        dff(3,12) = -h
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'P14') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 14
        ndim = 3
!
        a = un - y0 - z0
        b = undemi*(un - x0)
        c = b + x0
        d = quatre*a*b
        e = quatre*y0*b
        f = quatre*z0*b
        g = quatre*a*c
        h = quatre*y0*c
        o = quatre*z0*c
        p = 1.5d0*a*y0*z0
        q = trois*z0*(a - y0)
        o = trois*y0*(a - z0)
        s = q*b
        t = o*b
        q = q*c
        o = o*c
!
        dff(1,1) = y0*(undemi - y0) - p
        dff(2,1) = s + e - b
        dff(3,1) = t
        dff(1,2) = z0*(undemi - z0) - p
        dff(2,2) = s
        dff(3,2) = t + f - b
        dff(1,3) = a*(undemi - a) - p
        dff(2,3) = s + b - d
        dff(3,3) = t + b - d
        dff(1,4) = -(y0*(undemi - y0) - p)
        dff(2,4) = q + h - c
        dff(3,4) = o
        dff(1,5) = -(z0*(undemi - z0) - p)
        dff(2,5) = q
        dff(3,5) = o + o - c
        dff(1,6) = -(a*(undemi - a) - p)
        dff(2,6) = q + c - g
        dff(3,6) = o + c - g
!
        p = quatre * p
        s =-quatre * s
        t =-quatre * t
        q =-quatre * q
        o =-quatre * o
!
        dff(1,7) = -deux*y0*z0 + p
        dff(2,7) = s + f
        dff(3,7) = t + e
        dff(1,8) = -deux*z0*a + p
        dff(2,8) = s - f
        dff(3,8) = t + d - f
        dff(1,9) = -deux*a*y0 + p
        dff(2,9) = s + d - e
        dff(3,9) = t - e
        dff(1,10) = -(-deux*y0*z0 + p)
        dff(2,10) = q + o
        dff(3,10) = o + h
        dff(1,11) = -(-deux*z0*a + p)
        dff(2,11) = q - o
        dff(3,11) = o + g - o
        dff(1,12) = -(-deux*a*y0 + p)
        dff(2,12) = q + g - h
        dff(3,12) = o - h
        dff(1,13) = -2.25d0*p
        dff(2,13) = -2.25d0*s
        dff(3,13) = -2.25d0*t
        dff(1,14) = 2.25d0*p
        dff(2,14) = -2.25d0*q
        dff(3,14) = -2.25d0*o
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'P15') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 15
        ndim = 3
        al = un - y0 - z0
!
        dff(1,1) = (-y0* (deux*y0-deux-x0)-y0* (un-x0))/deux
        dff(2,1) = (un-x0)* (quatre*y0-deux-x0)/deux
        dff(3,1) = zero
        dff(1,2) = -z0* (deux*z0-un-deux*x0)/deux
        dff(2,2) = zero
        dff(3,2) = (un-x0)* (quatre*z0-deux-x0)/deux
        dff(1,3) = al* (deux*x0+deux*y0+deux*z0-un)/deux
        dff(2,3) = (x0-un)* (-x0-quatre*y0-quatre*z0+deux)/deux
        dff(3,3) = (x0-un)* (-x0-quatre*y0-quatre*z0+deux)/deux
        dff(1,4) = y0* (deux*y0-un+deux*x0)/deux
        dff(2,4) = (un+x0)* (quatre*y0-deux+x0)/deux
        dff(3,4) = zero
        dff(1,5) = z0* (deux*z0-un+deux*x0)/deux
        dff(2,5) = zero
        dff(3,5) = (un+x0)* (quatre*z0-deux+x0)/deux
        dff(1,6) = al* (deux*x0-deux*y0-deux*z0+un)/deux
        dff(2,6) = (x0+un)* (-x0+quatre*y0+quatre*z0-deux)/deux
        dff(3,6) = (x0+un)* (-x0+quatre*y0+quatre*z0-deux)/deux
        dff(1,7) = -deux*y0*z0
        dff(2,7) = deux*z0* (un-x0)
        dff(3,7) = deux*y0* (un-x0)
        dff(1,8) = -deux*al*z0
        dff(2,8) = -deux*z0* (un-x0)
        dff(3,8) = (deux*al-deux*z0)* (un-x0)
        dff(1,9) = -deux*y0*al
        dff(2,9) = (deux*al-deux*y0)* (un-x0)
        dff(3,9) = -deux*y0* (un-x0)
        dff(1,10) = -deux*y0*x0
        dff(2,10) = (un-x0*x0)
        dff(3,10) = zero
        dff(1,11) = -deux*z0*x0
        dff(2,11) = zero
        dff(3,11) = (un-x0*x0)
        dff(1,12) = -deux*al*x0
        dff(2,12) = - (un-x0*x0)
        dff(3,12) = - (un-x0*x0)
        dff(1,13) = deux*y0*z0
        dff(2,13) = deux*z0* (un+x0)
        dff(3,13) = deux*y0* (un+x0)
        dff(1,14) = deux*al*z0
        dff(2,14) = -deux*z0* (un+x0)
        dff(3,14) = (deux*al-deux*z0)* (un+x0)
        dff(1,15) = deux*y0*al
        dff(2,15) = (deux*al-deux*y0)* (un+x0)
        dff(3,15) = -deux*y0* (un+x0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'P18') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 18
        ndim = 3
!
        dff(1,1) = y0*(deux*x0-un)*(deux*y0-un)/deux
        dff(2,1) = x0*(x0-un)*(quatre*y0-un)/deux
        dff(3,1) = zero
        dff(1,2) = z0*(deux*x0-un)*(deux*z0-un)/deux
        dff(2,2) = zero
        dff(3,2) = x0*(x0-un)*(quatre*z0-un)/deux
        dff(1,3) = (deux*x0-un)*(z0+y0-un)*(deux*z0+deux*y0-un)/deux
        dff(2,3) = x0*(x0-un)*(quatre*z0+quatre*y0-trois)/deux
        dff(3,3) = x0*(x0-un)*(quatre*z0+quatre*y0-trois)/deux
        dff(1,4) = y0*(deux*x0+un)*(deux*y0-un)/deux
        dff(2,4) = x0*(x0+un)*(quatre*y0-un)/deux
        dff(3,4) = zero
        dff(1,5) = z0*(deux*x0+un)*(deux*z0-un)/deux
        dff(2,5) = zero
        dff(3,5) = x0*(x0+un)*(quatre*z0-un)/deux
        dff(1,6) = (deux*x0+un)*(z0+y0-un)*(deux*z0+deux*y0-un)/deux
        dff(2,6) = x0*(x0+un)*(quatre*z0+quatre*y0-trois)/deux
        dff(3,6) = x0*(x0+un)*(quatre*z0+quatre*y0-trois)/deux
        dff(1,7) = deux*y0*z0*(deux*x0-un)
        dff(2,7) = deux*x0*z0*(x0-un)
        dff(3,7) = deux*x0*y0*(x0-un)
        dff(1,8) = -deux*z0*(deux*x0-un)*(z0+y0-un)
        dff(2,8) = -deux*x0*z0*(x0-un)
        dff(3,8) = -deux*x0*(x0-un)*(deux*z0+y0-un)
        dff(1,9) = -deux*y0*(deux*x0-un)*(z0+y0-un)
        dff(2,9) = -deux*x0*(x0-un)*(deux*y0+z0-un)
        dff(3,9) = -deux*x0*y0*(x0-un)
        dff(1,10) = -deux*x0*y0*(deux*y0-un)
        dff(2,10) = -(x0-un)*(x0+un)*(quatre*y0-un)
        dff(3,10) = zero
        dff(1,11) = -deux*x0*z0*(deux*z0-un)
        dff(2,11) = zero
        dff(3,11) = -(x0-un)*(x0+un)*(quatre*z0-un)
        dff(1,12) = -deux*x0*(z0+y0-un)*(deux*z0+deux*y0-un)
        dff(2,12) = -(x0-un)*(x0+un)*(quatre*z0+quatre*y0-trois)
        dff(3,12) = -(x0-un)*(x0+un)*(quatre*z0+quatre*y0-trois)
        dff(1,13) = deux*y0*z0*(deux*x0+un)
        dff(2,13) = deux*x0*z0*(x0+un)
        dff(3,13) = deux*x0*y0*(x0+un)
        dff(1,14) = -deux*z0*(deux*x0+un)*(z0+y0-un)
        dff(2,14) = -deux*x0*z0*(x0+un)
        dff(3,14) = -deux*x0*(x0+un)*(deux*z0+y0-un)
        dff(1,15) = -deux*y0*(deux*x0+un)*(z0+y0-un)
        dff(2,15) = -deux*x0*(x0+un)*(deux*y0+z0-un)
        dff(3,15) = -deux*x0*y0*(x0+un)
        dff(1,16) = -huit*x0*y0*z0
        dff(2,16) = -quatre*z0*(x0-un)*(x0+un)
        dff(3,16) = -quatre*y0*(x0-un)*(x0+un)
        dff(1,17) = huit*x0*z0*(z0+y0-un)
        dff(2,17) = quatre*z0*(x0-un)*(x0+un)
        dff(3,17) = quatre*(x0-un)*(x0+un)*(deux*z0+y0-un)
        dff(1,18) = huit*x0*y0*(z0+y0-un)
        dff(2,18) = quatre*(x0-un)*(x0+un)*(deux*y0+z0-un)
        dff(3,18) = quatre*y0*(x0-un)*(x0+un)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TE4') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 4
        ndim = 3
!
        dff(1,1) = zero
        dff(2,1) = un
        dff(3,1) = zero
        dff(1,2) = zero
        dff(2,2) = zero
        dff(3,2) = un
        dff(1,3) = -un
        dff(2,3) = -un
        dff(3,3) = -un
        dff(1,4) = un
        dff(2,4) = zero
        dff(3,4) = zero
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'T10') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 10
        ndim = 3
        al = un - x0 - y0 - z0
!
        dff(1,1) = zero
        dff(2,1) = quatre*y0 - un
        dff(3,1) = zero
        dff(1,2) = zero
        dff(2,2) = zero
        dff(3,2) = quatre*z0 - un
        dff(1,3) = un - quatre*al
        dff(2,3) = un - quatre*al
        dff(3,3) = un - quatre*al
        dff(1,4) = quatre*x0 - un
        dff(2,4) = zero
        dff(3,4) = zero
        dff(1,5) = zero
        dff(2,5) = quatre*z0
        dff(3,5) = quatre*y0
        dff(1,6) = -quatre*z0
        dff(2,6) = -quatre*z0
        dff(3,6) = quatre* (al-z0)
        dff(1,7) = -quatre*y0
        dff(2,7) = quatre* (al-y0)
        dff(3,7) = -quatre*y0
        dff(1,8) = quatre*y0
        dff(2,8) = quatre*x0
        dff(3,8) = zero
        dff(1,9) = quatre*z0
        dff(2,9) = zero
        dff(3,9) = quatre*x0
        dff(1,10) = quatre* (al-x0)
        dff(2,10) = -quatre*x0
        dff(3,10) = -quatre*x0
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'PY5') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 5
        ndim = 3
        z01 = un - z0
        z04 = (un-z0)*quatre
!
        pface1 = x0 + y0 + z0 - un
        pface2 = -x0 + y0 + z0 - un
        pface3 = -x0 - y0 + z0 - un
        pface4 = x0 - y0 + z0 - un
!
        if (abs(z0-un) .lt. 1.0d-6) then
            do 20 i = 1, 5
                do 10 j = 1, 2
                    dff(j,i) = zero
10              continue
20          continue
!
            dff(1,1) = undemi
            dff(1,3) = -undemi
!
            dff(2,2) = undemi
            dff(2,4) = -undemi
!
            dff(3,1) = -1.d0/4.d0
            dff(3,2) = -1.d0/4.d0
            dff(3,3) = -1.d0/4.d0
            dff(3,4) = -1.d0/4.d0
            dff(3,5) = un
!
        else
!
            dff(1,1) = (-pface2-pface3)/z04
            dff(1,2) = (pface3-pface4)/z04
            dff(1,3) = (pface1+pface4)/z04
            dff(1,4) = (pface2-pface1)/z04
            dff(1,5) = zero
!
            dff(2,1) = (pface3-pface2)/z04
            dff(2,2) = (-pface3-pface4)/z04
            dff(2,3) = (pface4-pface1)/z04
            dff(2,4) = (pface1+pface2)/z04
            dff(2,5) = zero
!
            dff(3,1) = (pface2+pface3+pface2*pface3/z01)/z04
            dff(3,2) = (pface3+pface4+pface3*pface4/z01)/z04
            dff(3,3) = (pface4+pface1+pface4*pface1/z01)/z04
            dff(3,4) = (pface1+pface2+pface1*pface2/z01)/z04
            dff(3,5) = un
        endif
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'P13') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 13
        ndim = 3
        z01 = un - z0
        z02 = (un-z0)*deux
!
        pface1 = x0 + y0 + z0 - un
        pface2 = -x0 + y0 + z0 - un
        pface3 = -x0 - y0 + z0 - un
        pface4 = x0 - y0 + z0 - un
!
        pmili1 = x0 - undemi
        pmili2 = y0 - undemi
        pmili3 = -x0 - undemi
        pmili4 = -y0 - undemi
!
        if (abs(z0-un) .lt. 1.0d-6) then
            do 40 i = 1, 13
                do 30 j = 1, 2
                    dff(j,i) = zero
30              continue
40          continue
!
            dff(1,1) = -undemi
            dff(1,3) = undemi
            dff(1,9) = deux
            dff(1,11) = -deux
!
            dff(2,2) = -undemi
            dff(2,4) = undemi
            dff(2,10) = deux
            dff(2,12) = -deux
!
            dff(3,1) = un/quatre
            dff(3,2) = un/quatre
            dff(3,3) = un/quatre
            dff(3,4) = un/quatre
            dff(3,6) = zero
            dff(3,7) = zero
            dff(3,8) = zero
            dff(3,9) = zero
!
            do 50 i = 10, 13
                dff(3,i) = -un
50          continue
!
            dff(3,5) = trois
!
        else
!
            dff(1,1) = (pface2*pface3- (pface2+pface3)*pmili1)/z02
            dff(1,2) = (pface3-pface4)*pmili2/z02
            dff(1,3) = ((pface1+pface4)*pmili3-pface4*pface1)/z02
            dff(1,4) = (pface2-pface1)*pmili4/z02
            dff(1,5) = zero
            dff(1,6) = (pface3*pface4+pface2*pface4-pface2*pface3)/ z02
            dff(1,7) = (pface4*pface1-pface3*pface1-pface3*pface4)/ z02
            dff(1,8) = (pface4*pface1-pface1*pface2-pface4*pface2)/ z02
            dff(1,9) = (pface1*pface3+pface1*pface2-pface3*pface2)/ z02
            dff(1,10) = (-pface3-pface2)*z0/z01
            dff(1,11) = (pface3-pface4)*z0/z01
            dff(1,12) = (pface1+pface4)*z0/z01
            dff(1,13) = (pface2-pface1)*z0/z01
!
            dff(2,1) = (pface3-pface2)*pmili1/z02
            dff(2,2) = (pface3*pface4- (pface3+pface4)*pmili2)/z02
            dff(2,3) = (pface4-pface1)*pmili3/z02
            dff(2,4) = ((pface2+pface1)*pmili4-pface1*pface2)/z02
            dff(2,5) = zero
            dff(2,6) = (pface2*pface4+pface2*pface3-pface3*pface4)/ z02
            dff(2,7) = (pface4*pface1+pface3*pface1-pface3*pface4)/ z02
            dff(2,8) = (pface1*pface2-pface4*pface2-pface4*pface1)/ z02
            dff(2,9) = (pface1*pface2-pface2*pface3-pface1*pface3)/ z02
            dff(2,10) = (pface3-pface2)*z0/z01
            dff(2,11) = (-pface4-pface3)*z0/z01
            dff(2,12) = (pface4-pface1)*z0/z01
            dff(2,13) = (pface2+pface1)*z0/z01
!
            dff(3,1) = (pface2+pface3+pface2*pface3/z01)*pmili1/z02
            dff(3,2) = (pface3+pface4+pface3*pface4/z01)*pmili2/z02
            dff(3,3) = (pface1+pface4+pface1*pface4/z01)*pmili3/z02
            dff(3,4) = (pface2+pface1+pface1*pface2/z01)*pmili4/z02
            dff(3,5) = quatre*z0 - un
            dff(3,6) = - (pface3*pface4+pface2*pface4+pface2*pface3+ pface2*pface3*pface4/z01&
                       )/z02
            dff(3,7) = - (pface4*pface1+pface3*pface1+pface3*pface4+ pface3*pface4*pface1/z01&
                       )/z02
            dff(3,8) = - (pface1*pface2+pface4*pface2+pface4*pface1+ pface4*pface1*pface2/z01&
                       )/z02
            dff(3,9) = - (pface2*pface3+pface1*pface3+pface1*pface2+ pface1*pface2*pface3/z01&
                       )/z02
            dff(3,10) = pface2*pface3/z01/z01 + (pface3+pface2)*z0/ z01
            dff(3,11) = pface3*pface4/z01/z01 + (pface4+pface3)*z0/ z01
            dff(3,12) = pface4*pface1/z01/z01 + (pface1+pface4)*z0/ z01
            dff(3,13) = pface1*pface2/z01/z01 + (pface1+pface2)*z0/ z01
        endif
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TR3') then
!
        nno = 3
        ndim = 2
!
        dff(1,1) = -un
        dff(2,1) = -un
        dff(1,2) = +un
        dff(2,2) = zero
        dff(1,3) = zero
        dff(2,3) = +un
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TH3') then
!
        nno = 3
        ndim = 2
!
        dff(1,1) = -deux
        dff(2,1) = -deux
        dff(1,2) = deux
        dff(2,2) = zero
        dff(1,3) = zero
        dff(2,3) = deux
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TR6') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 6
        ndim = 2
        al = un - x0 - y0
!
        dff(1,1) = un - quatre*al
        dff(1,2) = -un + quatre*x0
        dff(1,3) = zero
        dff(1,4) = quatre* (al-x0)
        dff(1,5) = quatre*y0
        dff(1,6) = -quatre*y0
        dff(2,1) = un - quatre*al
        dff(2,2) = zero
        dff(2,3) = -un + quatre*y0
        dff(2,4) = -quatre*x0
        dff(2,5) = quatre*x0
        dff(2,6) = quatre* (al-y0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TW6') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 6
        ndim = 2
!
        r = 2.8284271247461901D0
        al = undemi - x0 - y0
        x0 = deux * x0
        y0 = deux * y0
!
        dff(1,1) = al
        dff(2,1) = al + y0
        dff(1,2) = y0 - un
        dff(2,2) = x0 - un
        dff(1,3) = al + x0
        dff(2,3) = al
        dff(1,4) = deux*(x0 - un)
        dff(2,4) = zero
        dff(1,5) = zero
        dff(2,5) = deux*(y0 - un)
        dff(1,6) = -r*al
        dff(2,6) = -r*al
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TR7') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 7
        ndim = 2
!
        dff(1,1) = -trois + 4.0d0*x0 + 7.0d0*y0 - 6.0d0*x0*y0 - 3.0d0* y0*y0
        dff(1,2) = -un + 4.0d0*x0 + 3.0d0*y0 - 6.0d0*x0*y0 - 3.0d0*y0* y0
        dff(1,3) = 3.0d0*y0*( un - 2.0d0*x0 - y0 )
        dff(1,4) = 4.0d0*(un - 2.0d0*x0 - 4.0d0*y0 + 6.0d0*x0*y0 + 3.0d0*y0*y0)
        dff(1,5) = 4.0d0*y0*( -2.0d0 + 6.0d0*x0 + 3.0d0*y0 )
        dff(1,6) = 4.0d0*y0*( -4.0d0 + 6.0d0*x0 + 3.0d0*y0 )
        dff(1,7) = 27.d0*y0*( un - 2.0d0*x0 - y0 )
!
        dff(2,1) = -trois + 4.0d0*y0 + 7.0d0*x0 - 6.0d0*x0*y0 - 3.0d0* x0*x0
        dff(2,2) = 3.0d0*x0*( un - 2.0d0*y0 - x0 )
        dff(2,3) = -un + 4.0d0*y0 + 3.0d0*x0 - 6.0d0*x0*y0 - 3.0d0*x0* x0
        dff(2,4) = 4.0d0*x0*( -4.0d0 + 6.0d0*y0 + 3.0d0*x0 )
        dff(2,5) = 4.0d0*x0*( -2.0d0 + 6.0d0*y0 + 3.0d0*x0 )
        dff(2,6) = 4.0d0*(un - 2.0d0*y0 - 4.0d0*x0 + 6.0d0*x0*y0 + 3.0d0*x0*x0)
        dff(2,7) = 27.d0*x0*( un - 2.0d0*y0 - x0 )
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QU4') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 4
        ndim = 2
!
        dff(1,1) = -(un-y0)*uns4
        dff(2,1) = -(un-x0)*uns4
        dff(1,2) = (un-y0)*uns4
        dff(2,2) = -(un+x0)*uns4
        dff(1,3) = (un+y0)*uns4
        dff(2,3) = (un+x0)*uns4
        dff(1,4) = -(un+y0)*uns4
        dff(2,4) = (un-x0)*uns4
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QH4') then
!
        r = 0.4330127018922193D0
        x0 = x(1) * 0.75d0
        y0 = x(2) * 0.75d0
        nno = 4
        ndim = 2
!
        dff(1,1) = -(r - y0)
        dff(2,1) = -(r - x0)
        dff(1,2) = r - y0
        dff(2,2) = -(r + x0)
        dff(1,3) = r + y0
        dff(2,3) = r + x0
        dff(1,4) = -(r + y0)
        dff(2,4) = r - x0
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QU6') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 6
        ndim = 2
!
        dff(1,1) = undemi*(un-y0)*(x0-undemi)
        dff(2,1) = -uns4*x0*(x0-un)
        dff(1,2) = undemi*(un-y0)*(x0+undemi)
        dff(2,2) = -(uns4*x0*(x0-un) + undemi*x0)
        dff(1,3) = (undemi*(un-y0) + y0)*(x0+undemi)
        dff(2,3) = uns4*x0*(x0-un) + undemi*x0
        dff(1,4) = (undemi*(un-y0) + y0)*(x0-undemi)
        dff(2,4) = uns4*x0*(x0-un)
        dff(1,5) = x0*(y0 - un)
        dff(2,5) = -undemi*(un-x0*x0)
        dff(1,6) = -x0*(un + y0)
        dff(2,6) = undemi*(un-x0*x0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QU8') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 8
        ndim = 2
!
        dff(1,1) = -uns4* (un-y0)* (-deux*x0-y0)
        dff(2,1) = -uns4* (un-x0)* (-deux*y0-x0)
        dff(1,2) = uns4* (un-y0)* ( deux*x0-y0)
        dff(2,2) = -uns4* (un+x0)* (-deux*y0+x0)
        dff(1,3) = uns4* (un+y0)* (deux*x0+y0)
        dff(2,3) = uns4* (un+x0)* (deux*y0+x0)
        dff(1,4) = -uns4* (un+y0)* (-deux*x0+y0)
        dff(2,4) = uns4* (un-x0)* ( deux*y0-x0)
        dff(1,5) = -deux*x0* (un-y0)*undemi
        dff(2,5) = - (un-x0*x0)*undemi
        dff(1,6) = (un-y0*y0)*undemi
        dff(2,6) = -deux*y0* (un+x0)*undemi
        dff(1,7) = -deux*x0* (un+y0)*undemi
        dff(2,7) = (un-x0*x0)*undemi
        dff(1,8) = - (un-y0*y0)*undemi
        dff(2,8) = -deux*y0* (un-x0)*undemi
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QU9') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 9
        ndim = 2
!
        dff(1,1) = dal31(x0)*al31(y0)
        dff(2,1) = al31(x0)*dal31(y0)
        dff(1,2) = dal33(x0)*al31(y0)
        dff(2,2) = al33(x0)*dal31(y0)
        dff(1,3) = dal33(x0)*al33(y0)
        dff(2,3) = al33(x0)*dal33(y0)
        dff(1,4) = dal31(x0)*al33(y0)
        dff(2,4) = al31(x0)*dal33(y0)
        dff(1,5) = dal32(x0)*al31(y0)
        dff(2,5) = al32(x0)*dal31(y0)
        dff(1,6) = dal33(x0)*al32(y0)
        dff(2,6) = al33(x0)*dal32(y0)
        dff(1,7) = dal32(x0)*al33(y0)
        dff(2,7) = al32(x0)*dal33(y0)
        dff(1,8) = dal31(x0)*al32(y0)
        dff(2,8) = al31(x0)*dal32(y0)
        dff(1,9) = dal32(x0)*al32(y0)
        dff(2,9) = al32(x0)*dal32(y0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'PO1') then
        nno = 1
        ndim = 0
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'SE2') then
        nno = 2
        ndim = 1
!
        dff(1,1) = -undemi
        dff(1,2) = undemi
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'SE3') then
        x0 = x(1)
        nno = 3
        ndim = 1
!
        dff(1,1) = x0 - undemi
        dff(1,2) = x0 + undemi
        dff(1,3) = -deux*x0
!     ------------------------------------------------------------------
    else if (elrefe.eq.'SE4') then
        x0 = x(1)
        nno = 4
        ndim = 1
!
        x1 = -un
        x2 = un
        x3 = -un/trois
        x4 = un/trois
        d1 = (x1-x2)* (x1-x3)* (x1-x4)
        dff(1,1) = ((x0-x2)* (x0-x3)+ (x0-x2)* (x0-x4)+ (x0-x3)* (x0- x4))/d1
        d2 = (x2-x1)* (x2-x3)* (x2-x4)
        dff(1,2) = ((x0-x1)* (x0-x3)+ (x0-x1)* (x0-x4)+ (x0-x3)* (x0- x4))/d2
        d3 = (x3-x1)* (x3-x2)* (x3-x4)
        dff(1,3) = ((x0-x1)* (x0-x2)+ (x0-x1)* (x0-x4)+ (x0-x2)* (x0- x4))/d3
        d4 = (x4-x1)* (x4-x2)* (x4-x3)
        dff(1,4) = ((x0-x1)* (x0-x2)+ (x0-x1)* (x0-x3)+ (x0-x2)* (x0- x3))/d4
!
!     ------------------------------------------------------------------
    else
        call assert(.false.)
    endif
!
!
!     ------------------------------------------------------------------
!
    call assert(dimd.ge.(nno*ndim))
!
end subroutine

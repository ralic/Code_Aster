subroutine elrfvf(elrefz, x, dimf, ff, nno)
    implicit none
    include 'asterfort/assert.h'
    integer :: dimf, nno
    real(kind=8) :: x(*), ff(*)
    character(len=*) :: elrefz
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
!
! BUT:   CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES
!        AU POINT DE COORDONNEES X,Y,Z
!
! ----------------------------------------------------------------------
!   IN   ELREFZ : NOM DE L'ELREFE (K8)
!        X      : VECTEUR DU POINT DE CALCUL DES F FORMES ET DERIVEES
!        DIMF   : DIMENSION DE FF
!   OUT  FF     : FONCTIONS DE FORMES EN X,Y,Z
!        NNO    : NOMBRE DE NOEUDS
!   -------------------------------------------------------------------
    character(len=8) :: elrefe
    integer :: i
    real(kind=8) :: al31, al32, al33, u, x0, y0, z0, al, z01, z02, z04, pface1
    real(kind=8) :: pface2
    real(kind=8) :: pface3, pface4, pmili1, pmili2, pmili3, pmili4
    real(kind=8) :: x1, x2, x3, x4, d1, d2, d3, d4
    real(kind=8) :: zero, undemi, un, deux, quatre, uns4, uns8
!
! -----  FONCTIONS FORMULES
    al31(u) = 0.5d00*u* (u-1.d00)
    al32(u) = - (u+1.d00)* (u-1.d00)
    al33(u) = 0.5d00*u* (u+1.d00)
! DEB ------------------------------------------------------------------
!
    elrefe = elrefz
    zero = 0.0d0
    undemi = 0.5d0
    un = 1.0d0
    deux = 2.0d0
    quatre = 4.0d0
    uns4 = un/4.0d0
    uns8 = un/8.0d0
!
!     ------------------------------------------------------------------
    if (elrefe .eq. 'HE8') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 8
!
        ff(1) = (un-x0)* (un-y0)* (un-z0)*uns8
        ff(2) = (un+x0)* (un-y0)* (un-z0)*uns8
        ff(3) = (un+x0)* (un+y0)* (un-z0)*uns8
        ff(4) = (un-x0)* (un+y0)* (un-z0)*uns8
        ff(5) = (un-x0)* (un-y0)* (un+z0)*uns8
        ff(6) = (un+x0)* (un-y0)* (un+z0)*uns8
        ff(7) = (un+x0)* (un+y0)* (un+z0)*uns8
        ff(8) = (un-x0)* (un+y0)* (un+z0)*uns8
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'H20') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 20
!
        ff(1) = (un-x0)* (un-y0)* (un-z0)* (-x0-y0-z0-deux)*uns8
        ff(2) = (un+x0)* (un-y0)* (un-z0)* (x0-y0-z0-deux)*uns8
        ff(3) = (un+x0)* (un+y0)* (un-z0)* (x0+y0-z0-deux)*uns8
        ff(4) = (un-x0)* (un+y0)* (un-z0)* (-x0+y0-z0-deux)*uns8
        ff(5) = (un-x0)* (un-y0)* (un+z0)* (-x0-y0+z0-deux)*uns8
        ff(6) = (un+x0)* (un-y0)* (un+z0)* (x0-y0+z0-deux)*uns8
        ff(7) = (un+x0)* (un+y0)* (un+z0)* (x0+y0+z0-deux)*uns8
        ff(8) = (un-x0)* (un+y0)* (un+z0)* (-x0+y0+z0-deux)*uns8
        ff(9) = (un-x0*x0)* (un-y0)* (un-z0)*uns4
        ff(10) = (un+x0)* (un-y0*y0)* (un-z0)*uns4
        ff(11) = (un-x0*x0)* (un+y0)* (un-z0)*uns4
        ff(12) = (un-x0)* (un-y0*y0)* (un-z0)*uns4
        ff(13) = (un-x0)* (un-y0)* (un-z0*z0)*uns4
        ff(14) = (un+x0)* (un-y0)* (un-z0*z0)*uns4
        ff(15) = (un+x0)* (un+y0)* (un-z0*z0)*uns4
        ff(16) = (un-x0)* (un+y0)* (un-z0*z0)*uns4
        ff(17) = (un-x0*x0)* (un-y0)* (un+z0)*uns4
        ff(18) = (un+x0)* (un-y0*y0)* (un+z0)*uns4
        ff(19) = (un-x0*x0)* (un+y0)* (un+z0)*uns4
        ff(20) = (un-x0)* (un-y0*y0)* (un+z0)*uns4
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'H27') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 27
!
        ff(1) = al31(x0)*al31(y0)*al31(z0)
        ff(2) = al33(x0)*al31(y0)*al31(z0)
        ff(3) = al33(x0)*al33(y0)*al31(z0)
        ff(4) = al31(x0)*al33(y0)*al31(z0)
        ff(5) = al31(x0)*al31(y0)*al33(z0)
        ff(6) = al33(x0)*al31(y0)*al33(z0)
        ff(7) = al33(x0)*al33(y0)*al33(z0)
        ff(8) = al31(x0)*al33(y0)*al33(z0)
        ff(9) = al32(x0)*al31(y0)*al31(z0)
        ff(10) = al33(x0)*al32(y0)*al31(z0)
        ff(11) = al32(x0)*al33(y0)*al31(z0)
        ff(12) = al31(x0)*al32(y0)*al31(z0)
        ff(13) = al31(x0)*al31(y0)*al32(z0)
        ff(14) = al33(x0)*al31(y0)*al32(z0)
        ff(15) = al33(x0)*al33(y0)*al32(z0)
        ff(16) = al31(x0)*al33(y0)*al32(z0)
        ff(17) = al32(x0)*al31(y0)*al33(z0)
        ff(18) = al33(x0)*al32(y0)*al33(z0)
        ff(19) = al32(x0)*al33(y0)*al33(z0)
        ff(20) = al31(x0)*al32(y0)*al33(z0)
        ff(21) = al32(x0)*al32(y0)*al31(z0)
        ff(22) = al32(x0)*al31(y0)*al32(z0)
        ff(23) = al33(x0)*al32(y0)*al32(z0)
        ff(24) = al32(x0)*al33(y0)*al32(z0)
        ff(25) = al31(x0)*al32(y0)*al32(z0)
        ff(26) = al32(x0)*al32(y0)*al33(z0)
        ff(27) = al32(x0)*al32(y0)*al32(z0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'PE6') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 6
!
        ff(1) = undemi*y0* (un-x0)
        ff(2) = undemi*z0* (un-x0)
        ff(3) = undemi* (un-y0-z0)* (un-x0)
        ff(4) = undemi*y0* (un+x0)
        ff(5) = undemi*z0* (un+x0)
        ff(6) = undemi* (un-y0-z0)* (un+x0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'P15') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 15
        al = un - y0 - z0
!
        ff(1) = y0* (un-x0)* ((deux*y0)-deux-x0)/deux
        ff(2) = z0* (un-x0)* ((deux*z0)-deux-x0)/deux
        ff(3) = al* (x0-un)* (x0+ (deux*y0)+ (deux*z0))/deux
        ff(4) = y0* (un+x0)* ((deux*y0)-deux+x0)/deux
        ff(5) = z0* (un+x0)* ((deux*z0)-deux+x0)/deux
        ff(6) = al* (-x0-un)* (-x0+ (deux*y0)+ (deux*z0))/deux
!
        ff(7) = deux*y0*z0* (un-x0)
        ff(8) = deux*z0*al* (un-x0)
        ff(9) = deux*y0*al* (un-x0)
!
        ff(10) = y0* (un-x0*x0)
        ff(11) = z0* (un-x0*x0)
        ff(12) = al* (un-x0*x0)
!
        ff(13) = deux*y0*z0* (un+x0)
        ff(14) = deux*z0*al* (un+x0)
        ff(15) = deux*y0*al* (un+x0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'P18') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 18
!
        ff(1) = x0*y0*(x0-un)*(deux*y0-un)/deux
        ff(2) = x0*z0*(x0-un)*(deux*z0-un)/deux
        ff(3) = x0*(x0-un)*(z0+y0-un)*(deux*z0+deux*y0-un)/deux
        ff(4) = x0*y0*(x0+un)*(deux*y0-un)/deux
        ff(5) = x0*z0*(x0+un)*(deux*z0-un)/deux
        ff(6) = x0*(x0+un)*(z0+y0-un)*(deux*z0+deux*y0-un)/deux
!
        ff(7) = deux*x0*y0*z0*(x0-un)
        ff(8) = -deux*x0*z0*(x0-un)*(z0+y0-un)
        ff(9) = -deux*x0*y0*(x0-un)*(z0+y0-un)
!
        ff(10) = -y0*(x0-un)*(x0+un)*(deux*y0-un)
        ff(11) = -z0*(x0-un)*(x0+un)*(deux*z0-un)
        ff(12) = -(x0-un)*(x0+un)*(z0+y0-un)*(deux*z0+deux*y0-un)
!
        ff(13) = deux*x0*y0*z0*(x0+un)
        ff(14) = -deux*x0*z0*(x0+un)*(z0+y0-un)
        ff(15) = -deux*x0*y0*(x0+un)*(z0+y0-un)
!
        ff(16) = -quatre*y0*z0*(x0-un)*(x0+un)
        ff(17) = quatre*z0*(x0-un)*(x0+un)*(z0+y0-un)
        ff(18) = quatre*y0*(x0-un)*(x0+un)*(z0+y0-un)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TE4') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 4
!
        ff(1) = y0
        ff(2) = z0
        ff(3) = un - x0 - y0 - z0
        ff(4) = x0
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'T10') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 10
        al = un - x0 - y0 - z0
!
        ff(1) = (deux*y0-un)*y0
        ff(2) = (deux*z0-un)*z0
        ff(3) = (deux*al-un)*al
        ff(4) = (deux*x0-un)*x0
        ff(5) = quatre*z0*y0
        ff(6) = quatre*z0*al
        ff(7) = quatre*al*y0
        ff(8) = quatre*x0*y0
        ff(9) = quatre*x0*z0
        ff(10) = quatre*x0*al
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'PY5') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 5
        z04 = (un-z0)*quatre
!
        pface1 = x0 + y0 + z0 - un
        pface2 = -x0 + y0 + z0 - un
        pface3 = -x0 - y0 + z0 - un
        pface4 = x0 - y0 + z0 - un
!
        if (abs(z0-un) .lt. 1.0d-6) then
            do 10 i = 1, 4
                ff(i) = zero
10          continue
            ff(5) = un
        else
            ff(1) = pface2*pface3/z04
            ff(2) = pface3*pface4/z04
            ff(3) = pface1*pface4/z04
            ff(4) = pface1*pface2/z04
            ff(5) = z0
        endif
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'P13') then
!
        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 13
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
            do 20 i = 1, 13
                ff(i) = zero
20          continue
            ff(5) = un
        else
            ff(1) = pface2*pface3*pmili1/z02
            ff(2) = pface3*pface4*pmili2/z02
            ff(3) = pface4*pface1*pmili3/z02
            ff(4) = pface1*pface2*pmili4/z02
            ff(5) = deux*z0* (z0-undemi)
            ff(6) = -pface2*pface3*pface4/z02
            ff(7) = -pface3*pface4*pface1/z02
            ff(8) = -pface4*pface1*pface2/z02
            ff(9) = -pface1*pface2*pface3/z02
            ff(10) = z0*pface2*pface3/z01
            ff(11) = z0*pface3*pface4/z01
            ff(12) = z0*pface4*pface1/z01
            ff(13) = z0*pface1*pface2/z01
        endif
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TR3') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 3
!
        ff(1) = un - x0 - y0
        ff(2) = x0
        ff(3) = y0
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TR6') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 6
        al = un - x0 - y0
!
        ff(1) = -al* (un-deux*al)
        ff(2) = -x0* (un-deux*x0)
        ff(3) = -y0* (un-deux*y0)
        ff(4) = quatre*x0*al
        ff(5) = quatre*x0*y0
        ff(6) = quatre*y0*al
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'TR7') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 7
!
        ff(1) = un - 3.0d0*(x0+y0) + 2.0d0*(x0*x0+y0*y0) + 7.0d0*x0* y0 - 3.0d0*x0*y0*(x0+y0)
        ff(2) = x0*( -un + 2.0d0*x0 + 3.0d0*y0 - 3.0d0*y0*(x0+y0) )
        ff(3) = y0*( -un + 3.0d0*x0 + 2.0d0*y0 - 3.0d0*x0*(x0+y0) )
        ff(4) = quatre*x0*( un - x0 - 4.0d0*y0 + 3.0d0*y0*(x0+y0) )
        ff(5) = quatre*x0*y0*( -deux + 3.0d0*(x0+y0) )
        ff(6) = quatre*y0*( un - y0 - 4.0d0*x0 + 3.0d0*x0*(x0+y0) )
        ff(7) = 27.0d0*x0*y0*( un - x0 - y0 )
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QU4') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 4
!
        ff(1) = uns4* (un-x0)* (un-y0)
        ff(2) = uns4* (un+x0)* (un-y0)
        ff(3) = uns4* (un+x0)* (un+y0)
        ff(4) = uns4* (un-x0)* (un+y0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QU8') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 8
!
        ff(1) = uns4* (un-x0)* (un-y0)* (-un-x0-y0)
        ff(2) = uns4* (un+x0)* (un-y0)* (-un+x0-y0)
        ff(3) = uns4* (un+x0)* (un+y0)* (-un+x0+y0)
        ff(4) = uns4* (un-x0)* (un+y0)* (-un-x0+y0)
        ff(5) = undemi* (un-x0*x0)* (un-y0)
        ff(6) = undemi* (un-y0*y0)* (un+x0)
        ff(7) = undemi* (un-x0*x0)* (un+y0)
        ff(8) = undemi* (un-y0*y0)* (un-x0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'QU9') then
!
        x0 = x(1)
        y0 = x(2)
        nno = 9
!
        ff(1) = al31(x0)*al31(y0)
        ff(2) = al33(x0)*al31(y0)
        ff(3) = al33(x0)*al33(y0)
        ff(4) = al31(x0)*al33(y0)
        ff(5) = al32(x0)*al31(y0)
        ff(6) = al33(x0)*al32(y0)
        ff(7) = al32(x0)*al33(y0)
        ff(8) = al31(x0)*al32(y0)
        ff(9) = al32(x0)*al32(y0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'PO1') then
        nno = 1
        ff(1) = un
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'SE2') then
!
        x0 = x(1)
        nno = 2
!
        ff(1) = (un-x0)/deux
        ff(2) = (un+x0)/deux
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'SE3') then
!
        x0 = x(1)
        nno = 3
!
        ff(1) = - (un-x0)*x0/deux
        ff(2) = (un+x0)*x0/deux
        ff(3) = (un+x0)* (un-x0)
!
!     ------------------------------------------------------------------
    else if (elrefe.eq.'SE4') then
        nno = 4
        x0 = x(1)
!
        x1 = -1.d0
        x2 = 1.d0
        x3 = -1.d0/3.d0
        x4 = 1.d0/3.d0
        d1 = (x1-x2)* (x1-x3)* (x1-x4)
!
        ff(1) = (x0-x2)* (x0-x3)* (x0-x4)/d1
        d2 = (x2-x1)* (x2-x3)* (x2-x4)
!
        ff(2) = (x0-x1)* (x0-x3)* (x0-x4)/d2
        d3 = (x3-x1)* (x3-x2)* (x3-x4)
!
        ff(3) = (x0-x1)* (x0-x2)* (x0-x4)/d3
        d4 = (x4-x1)* (x4-x2)* (x4-x3)
!
        ff(4) = (x0-x1)* (x0-x2)* (x0-x3)/d4
!
!     ------------------------------------------------------------------
!
    else
        call assert(.false.)
    endif
!
    call assert(dimf.ge.nno)
!
!
end subroutine

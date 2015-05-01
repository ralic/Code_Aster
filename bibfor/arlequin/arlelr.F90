subroutine arlelr(elrefz,x,dimf,ff,nno)
! ----------------------------------------------------------------------



! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.


! ======================================================================

    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"

    integer :: dimf,nno
    real(kind=8) :: x(*),ff(*)
    character(len=8) elrefz

! BUT:   CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES
!        AU POINT DE COORDONNEES X,Y,Z

! ----------------------------------------------------------------------
!   IN   ELREFZ : NOM DE L'ELREFE (K8)
!        X      : VECTEUR DU POINT DE CALCUL DES F FORMES ET DERIVEES
!        DIMF   : DIMENSION DE FF
!   OUT  FF     : FONCTIONS DE FORMES EN X,Y,Z
!        NNO    : NOMBRE DE NOEUDS
!   -------------------------------------------------------------------
    character(len=8) :: elrefe
    real(kind=8) :: nte4(12),nte10(30),nhe8(24),nh20(60),npe6(18),npe15(45)
    real(kind=8) :: x0,y0,z0,al,zero,un,deux,quatre,uns2,uns4,uns8

! ------------------------------------------------------------------
    call jemarq()

    elrefe = elrefz
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
    quatre = 4.0d0
    uns2 = un/2.0d0
    uns4 = un/4.0d0
    uns8 = un/8.0d0
!     ------------------------------------------------------------------
    if (elrefe == 'HE8') then

        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 8

        nhe8(1) = (un-x0)* (un-y0)* (un-z0)*uns8
        nhe8(2) = zero
        nhe8(3) = zero
        nhe8(4) = (un+x0)* (un-y0)* (un-z0)*uns8
        nhe8(5) = zero
        nhe8(6) = zero
        nhe8(7) = (un+x0)* (un+y0)* (un-z0)*uns8
        nhe8(8) = zero
        nhe8(9) = zero
        nhe8(10) = (un-x0)* (un+y0)* (un-z0)*uns8
        nhe8(11) = zero
        nhe8(12) = zero
        nhe8(13) = (un-x0)* (un-y0)* (un+z0)*uns8
        nhe8(14) = zero
        nhe8(15) = zero
        nhe8(16) = (un+x0)* (un-y0)* (un+z0)*uns8
        nhe8(17) = zero
        nhe8(18) = zero
        nhe8(19) = (un+x0)* (un+y0)* (un+z0)*uns8
        nhe8(20) = zero
        nhe8(21) = zero
        nhe8(22) = (un-x0)* (un+y0)* (un+z0)*uns8
        nhe8(23) = zero
        nhe8(24) = zero

        ff(1:24) = nhe8(1:24)
        ff(25) = zero
        ff(26:48) = nhe8(1:23)
        ff(49:50) = zero
        ff(51:72) = nhe8(1:22)
!     ------------------------------------------------------------------
    else if (elrefe == 'H20') then

        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 20

        nh20(1) = (un-x0)* (un-y0)* (un-z0)* (-x0-y0-z0-deux)*uns8
        nh20(2) = zero
        nh20(3) = zero
        nh20(4) = (un+x0)* (un-y0)* (un-z0)* (x0-y0-z0-deux)*uns8
        nh20(5) = zero
        nh20(6) = zero
        nh20(7) = (un+x0)* (un+y0)* (un-z0)* (x0+y0-z0-deux)*uns8
        nh20(8) = zero
        nh20(9) = zero
        nh20(10) = (un-x0)* (un+y0)* (un-z0)* (-x0+y0-z0-deux)*uns8
        nh20(11) = zero
        nh20(12) = zero
        nh20(13) = (un-x0)* (un-y0)* (un+z0)* (-x0-y0+z0-deux)*uns8
        nh20(14) = zero
        nh20(15) = zero
        nh20(16) = (un+x0)* (un-y0)* (un+z0)* (x0-y0+z0-deux)*uns8
        nh20(17) = zero
        nh20(18) = zero
        nh20(19) = (un+x0)* (un+y0)* (un+z0)* (x0+y0+z0-deux)*uns8
        nh20(20) = zero
        nh20(21) = zero
        nh20(22) = (un-x0)* (un+y0)* (un+z0)* (-x0+y0+z0-deux)*uns8
        nh20(23) = zero
        nh20(24) = zero
        nh20(25) = (un-x0*x0)* (un-y0)* (un-z0)*uns4
        nh20(26) = zero
        nh20(27) = zero
        nh20(28) = (un+x0)* (un-y0*y0)* (un-z0)*uns4
        nh20(29) = zero
        nh20(30) = zero
        nh20(31) = (un-x0*x0)* (un+y0)* (un-z0)*uns4
        nh20(32) = zero
        nh20(33) = zero
        nh20(34) = (un-x0)* (un-y0*y0)* (un-z0)*uns4
        nh20(35) = zero
        nh20(36) = zero
        nh20(37) = (un-x0)* (un-y0)* (un-z0*z0)*uns4
        nh20(38) = zero
        nh20(39) = zero
        nh20(40) = (un+x0)* (un-y0)* (un-z0*z0)*uns4
        nh20(41) = zero
        nh20(42) = zero
        nh20(43) = (un+x0)* (un+y0)* (un-z0*z0)*uns4
        nh20(44) = zero
        nh20(45) = zero
        nh20(46) = (un-x0)* (un+y0)* (un-z0*z0)*uns4
        nh20(47) = zero
        nh20(48) = zero
        nh20(49) = (un-x0*x0)* (un-y0)* (un+z0)*uns4
        nh20(50) = zero
        nh20(51) = zero
        nh20(52) = (un+x0)* (un-y0*y0)* (un+z0)*uns4
        nh20(53) = zero
        nh20(54) = zero
        nh20(55) = (un-x0*x0)* (un+y0)* (un+z0)*uns4
        nh20(56) = zero
        nh20(57) = zero
        nh20(58) = (un-x0)* (un-y0*y0)* (un+z0)*uns4
        nh20(59) = zero
        nh20(60) = zero

        ff(1:60) = nh20(1:60)
        ff(61) = zero
        ff(62:120) = nh20(1:59)
        ff(121:122) = zero
        ff(123:180) = nh20(1:58)
!     ------------------------------------------------------------------
    else if (elrefe == 'PE6') then

        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 6

        npe6(1) = uns2*y0* (un-x0)
        npe6(2) = zero
        npe6(3) = zero
        npe6(4) = uns2*z0* (un-x0)
        npe6(5) = zero
        npe6(6) = zero
        npe6(7) = uns2* (un-y0-z0)* (un-x0)
        npe6(8) = zero
        npe6(9) = zero
        npe6(10) = uns2*y0* (un+x0)
        npe6(11) = zero
        npe6(12) = zero
        npe6(13) = uns2*z0* (un+x0)
        npe6(14) = zero
        npe6(15) = zero
        npe6(16) = uns2* (un-y0-z0)* (un+x0)
        npe6(17) = zero
        npe6(18) = zero

        ff(1:18) = npe6(1:18)
        ff(19) = zero
        ff(20:36) = npe6(1:17)
        ff(37:38) = zero
        ff(39:54) = npe6(1:16)
!     ------------------------------------------------------------------
    else if (elrefe == 'P15') then

        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 15
        al = un - y0 - z0

        npe15(1) = uns2*y0*(un-x0)*((deux*y0)-deux-x0)
        npe15(2) = zero
        npe15(3) = zero
        npe15(4) = uns2*z0*(un-x0)*((deux*z0)-deux-x0)
        npe15(5) = zero
        npe15(6) = zero
        npe15(7) = uns2*al*(x0-un)*(x0+(deux*y0)+(deux*z0))
        npe15(8) = zero
        npe15(9) = zero
        npe15(10) = uns2*y0*(un+x0)*((deux*y0)-deux+x0)
        npe15(11) = zero
        npe15(12) = zero
        npe15(13) = uns2*z0*(un+x0)*((deux*z0)-deux+x0)
        npe15(14) = zero
        npe15(15) = zero
        npe15(16) = uns2*al*(-x0-un)*(-x0+(deux*y0)+(deux*z0))
        npe15(17) = zero
        npe15(18) = zero
        npe15(19) = deux*y0*z0*(un-x0)
        npe15(20) = zero
        npe15(21) = zero
        npe15(22) = deux*z0*al*(un-x0)
        npe15(23) = zero
        npe15(24) = zero
        npe15(25) = deux*y0*al*(un-x0)
        npe15(26) = zero
        npe15(27) = zero
        npe15(28) = y0*(un-x0*x0)
        npe15(29) = zero
        npe15(30) = zero
        npe15(31) = z0*(un-x0*x0)
        npe15(32) = zero
        npe15(33) = zero
        npe15(34) = al*(un-x0*x0)
        npe15(35) = zero
        npe15(36) = zero
        npe15(37) = deux*y0*z0*(un+x0)
        npe15(38) = zero
        npe15(39) = zero
        npe15(40) = deux*z0*al*(un+x0)
        npe15(41) = zero
        npe15(42) = zero
        npe15(43) = deux*y0*al*(un+x0)
        npe15(44) = zero
        npe15(45) = zero

        ff(1:45) = npe15(1:45)
        ff(46) = zero
        ff(47:90) = npe15(1:44)
        ff(91:92) = zero
        ff(93:135) = npe15(1:43)
!     ------------------------------------------------------------------
    else if (elrefe == 'TE4') then

        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 4

        nte4(1) = y0
        nte4(2) = zero
        nte4(3) = zero
        nte4(4) = z0
        nte4(5) = zero
        nte4(6) = zero
        nte4(7) = un - x0 - y0 - z0
        nte4(8) = zero
        nte4(9) = zero
        nte4(10) = x0
        nte4(11) = zero
        nte4(12) = zero

        ff(1:12) = nte4(1:12)
        ff(13) = zero
        ff(14:24) = nte4(1:11)
        ff(25:26) = zero
        ff(27:36) = nte4(1:10)
!     ------------------------------------------------------------------
    else if (elrefe == 'T10') then

        x0 = x(1)
        y0 = x(2)
        z0 = x(3)
        nno = 10
        al = un - x0 - y0 - z0

        nte10(1) = (deux*y0-un)*y0
        nte10(2) = zero
        nte10(3) = zero
        nte10(4) = (deux*z0-un)*z0
        nte10(5) = zero
        nte10(6) = zero
        nte10(7) = (deux*al-un)*al
        nte10(8) = zero
        nte10(9) = zero
        nte10(10) = (deux*x0-un)*x0
        nte10(11) = zero
        nte10(12) = zero
        nte10(13) = quatre*z0*y0
        nte10(14) = zero
        nte10(15) = zero
        nte10(16) = quatre*z0*al
        nte10(17) = zero
        nte10(18) = zero
        nte10(19) = quatre*al*y0
        nte10(20) = zero
        nte10(21) = zero
        nte10(22) = quatre*x0*y0
        nte10(23) = zero
        nte10(24) = zero
        nte10(25) = quatre*x0*z0
        nte10(26) = zero
        nte10(27) = zero
        nte10(28) = quatre*x0*al
        nte10(29) = zero
        nte10(30) = zero

        ff(1:30) = nte10(1:30)
        ff(31) = zero
        ff(32:60) = nte10(1:29)
        ff(61:62) = zero
        ff(63:90) = nte10(1:28)
!     ------------------------------------------------------------------
    else
        ASSERT(.false.)
    end if

    ASSERT(dimf.ge.nno)

    call jedema()

end subroutine

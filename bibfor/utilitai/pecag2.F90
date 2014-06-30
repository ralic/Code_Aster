subroutine pecag2(ndim, nsymx, nsymy, np, xyp,&
                  vale, valpar)
    implicit none
#include "asterc/r8rddg.h"
#include "asterfort/jacobi.h"
#include "asterfort/orien2.h"
    integer :: ndim, np
    real(kind=8) :: vale(*), valpar(*), xyp(2)
    logical(kind=1) :: nsymx, nsymy
!     ------------------------------------------------------------------
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
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "CARA_GEOM"
!     ------------------------------------------------------------------
!
    integer :: i, nperm, nbvec, nitjac, itype, iordre
    real(kind=8) :: ar(6), br(6), vecpro(3, 3), valpro(3), angl(3)
    real(kind=8) :: epsi, tol, toldyn, jac(3), dx, dy
    real(kind=8) :: v1(3), v2(3), v3(3)
    real(kind=8) :: aire, cdgx, cdgy, cdgz, ixx, iyy, izz, ixy, ixz, iyz
    real(kind=8) :: ixprin, iyprin, izprin, alpha, beta, gamma
    real(kind=8) :: ixxp, iyyp, ixyp, xp, yp, xgp, ygp
    real(kind=8) :: ixr2, iyr2, ixpri2, iypri2, ixpr2p, iypr2p
!     ------------------------------------------------------------------
!
    epsi = 1.d-12
!
    aire = vale(1)
    cdgx = vale(2)
    cdgy = vale(3)
    cdgz = vale(4)
    ixx = vale(5)
    iyy = vale(6)
    izz = vale(7)
    ixy = vale(8)
    ixz = vale(9)
    iyz = vale(10)
    ixr2 = vale(26)
    iyr2 = vale(27)
    ixpri2 = vale(28)
    iypri2 = vale(29)
    if (ndim .eq. 2) then
        valpar(1) = aire
        valpar(2) = cdgx
        valpar(3) = cdgy
        valpar(4) = ixx
        valpar(5) = iyy
        valpar(6) = ixy
    else if (ndim .eq. 3) then
        do 10 i = 1, 10
            valpar(i) = vale(i)
10      continue
    endif
!
    if (nsymx .and. .not. nsymy) then
        cdgy = 0.d0
        dy = vale(3)
        ixx = 2*ixx + 2*aire*dy*dy
        iyy = 2*iyy
        izz = 2*izz + 2*aire*dy*dy
        aire = 2 * aire
    else if (nsymy .and. .not. nsymx) then
        cdgx = 0.d0
        dx = vale(2)
        ixx = 2*ixx
        iyy = 2*iyy + 2*aire*dx*dx
        izz = 2*izz + 2*aire*dx*dx
        aire = 2 * aire
    else if (nsymx .and. nsymy) then
        cdgx = 0.d0
        cdgy = 0.d0
        dx = vale(2)
        dy = vale(3)
        ixx = 4*ixx + 4*aire*dy*dy
        iyy = 4*iyy + 4*aire*dx*dx
        izz = 4*izz + 4*aire*(dx*dx + dy*dy)
        ixy = 0.d0
        aire = 4 * aire
    endif
    if (ndim .eq. 2) then
        valpar(12) = aire
        valpar(13) = cdgx
        valpar(14) = cdgy
        valpar(15) = ixx
        valpar(16) = iyy
        valpar(17) = ixy
        valpar(36) = ixr2
        valpar(37) = iyr2
    else if (ndim .eq. 3) then
        valpar(18) = aire
        valpar(19) = cdgx
        valpar(20) = cdgy
        valpar(21) = cdgz
        valpar(22) = ixx
        valpar(23) = iyy
        valpar(24) = izz
        valpar(25) = ixy
        valpar(26) = ixz
        valpar(27) = iyz
    endif
!
!     TRAITEMENT DE ORIG_INER
!
    if (ndim .eq. 2) then
        if (np .ne. 0) then
            xp = xyp(1)
            yp = xyp(2)
            xgp = xp - cdgx
            ygp = yp - cdgy
            ixxp = ixx + aire*ygp*ygp
            iyyp = iyy + aire*xgp*xgp
            ixyp = ixy + aire*xgp*ygp
            ixpr2p = ixr2 - ygp*(3.0d0*ixx+iyy) - aire*ygp*(xgp*xgp+ ygp*ygp) - 2.0d0*xgp*ixy
            iypr2p = iyr2 - xgp*(3.0d0*iyy+ixx) - aire*xgp*(xgp*xgp+ ygp*ygp) - 2.0d0*ygp*ixy
            valpar(21) = xp
            valpar(22) = yp
            valpar(23) = ixxp
            valpar(24) = iyyp
            valpar(25) = ixyp
        else
            ixxp = ixx
            iyyp = iyy
            ixyp = ixy
            ixpr2p = ixr2
            iypr2p = iyr2
            valpar(21) = cdgx
            valpar(22) = cdgy
            valpar(23) = ixxp
            valpar(24) = iyyp
            valpar(25) = ixyp
        endif
    endif
!
    if (abs(ixx) .lt. epsi .and. abs(iyy) .lt. epsi .and. abs(izz) .lt. epsi .and. abs(ixy)&
        .lt. epsi .and. abs(ixz) .lt. epsi .and. abs(iyz) .lt. epsi) then
!
        ixprin = 0.d0
        iyprin = 0.d0
        izprin = 0.d0
        alpha = 0.d0
        beta = 0.d0
        gamma = 0.d0
        ixpri2 = 0.d0
        iypri2 = 0.d0
    else
        ar(1) = ixx
        ar(2) = - ixy
        ar(3) = - ixz
        ar(4) = iyy
        ar(5) = - iyz
        ar(6) = izz
        br(1) = 1.d0
        br(2) = 0.d0
        br(3) = 0.d0
        br(4) = 1.d0
        br(5) = 0.d0
        br(6) = 1.d0
        nbvec = 3
        nperm = 12
        tol = 1.d-10
        toldyn = 1.d-2
        itype = 2
        iordre = 2
        call jacobi(nbvec, nperm, tol, toldyn, ar,&
                    br, vecpro, valpro, jac, nitjac,&
                    itype, iordre)
        if (valpro(1) .lt. valpro(2)) then
            v1(1) = 0.d0
            v1(2) = 0.d0
            v1(3) = 0.d0
            v2(1) = vecpro(1,1)
            v2(2) = vecpro(2,1)
            v2(3) = vecpro(3,1)
            v3(1) = vecpro(1,2)
            v3(2) = vecpro(2,2)
            v3(3) = vecpro(3,2)
            call orien2(v1, v2, v3, angl)
            ixprin = valpro(1)
            iyprin = valpro(2)
            izprin = valpro(3)
            alpha = angl(1) * r8rddg()
            beta = angl(2) * r8rddg()
            gamma = angl(3) * r8rddg()
        else
            v1(1) = 0.d0
            v1(2) = 0.d0
            v1(3) = 0.d0
            v2(1) = vecpro(1,2)
            v2(2) = vecpro(2,2)
            v2(3) = vecpro(3,2)
            v3(1) = vecpro(1,1)
            v3(2) = vecpro(2,1)
            v3(3) = vecpro(3,1)
            call orien2(v1, v2, v3, angl)
            ixprin = valpro(2)
            iyprin = valpro(1)
            izprin = valpro(3)
            alpha = angl(1) * r8rddg()
            beta = angl(2) * r8rddg()
            gamma = angl(3) * r8rddg()
            ixpri2 = -sin(angl(1))*iyr2 + cos(angl(1))*ixr2
            iypri2 = cos(angl(1))*iyr2 + sin(angl(1))*ixr2
        endif
    endif
    if (ndim .eq. 2) then
        valpar(18) = ixprin
        valpar(19) = iyprin
        valpar(20) = alpha
        valpar(38) = ixpri2
        valpar(39) = iypri2
        valpar(40) = ixpr2p
        valpar(41) = iypr2p
    else if (ndim .eq. 3) then
        valpar(26) = ixprin
        valpar(27) = iyprin
        valpar(28) = izprin
        valpar(29) = alpha
        valpar(30) = beta
        valpar(31) = gamma
    endif
!
end subroutine

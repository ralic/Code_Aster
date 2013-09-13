subroutine i3pdm2(epsi, n, s, nbs, p,&
                  dedans)
    implicit none
#include "asterfort/utmess.h"
    integer :: nbs
    real(kind=8) :: n(*), s(3, *), p(*), epsi
    logical :: dedans
!     ------------------------------------------------------------------
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
!     LE POINT P APPARTIENT-T-IL AU POLYGONE PLAN DE SOMMETS S
!     ------------------------------------------------------------------
! IN  N      : R : COMPOSANTE DE LA NORMALE AU PLAN CONTENANT LES S
! IN  P      : R : COMPOSANTE DU POINT
! IN  S      : R : TABLE(3,NBS) DES COORDONEES DES SOMMETS
! IN  NBS    : I : NOMBRE DE SOMMETS
! OUT DEDANS : L : REPONSE
!     ------------------------------------------------------------------
!
    integer :: i, j
    real(kind=8) :: xp, yp, zp, xd, yd, zd, xf, yf, zf, nx, ny, nz, vx, vy, vz
    real(kind=8) :: r11, r12, r13, r21, r22, r23, r31, r32, r33, ps, un
!
!======================================================================
!
    un = 1.0d0
    dedans = .true.
    r11 = n(1)*n(1)
    r12 = n(1)*n(2) - n(3)
    r13 = n(1)*n(3) + n(2)
    r21 = n(2)*n(1) + n(3)
    r22 = n(2)*n(2)
    r23 = n(2)*n(3) - n(1)
    r31 = n(3)*n(1) - n(2)
    r32 = n(3)*n(2) + n(1)
    r33 = n(3)*n(3)
    xp = p(1)
    yp = p(2)
    zp = p(3)
    i = 1
10  continue
    if (dedans .and. ( i .le. nbs )) then
        j = max(1,mod(i+1,nbs+1))
        xd = s(1,i)
        yd = s(2,i)
        zd = s(3,i)
        xf = s(1,j)
        yf = s(2,j)
        zf = s(3,j)
        nx = xf - xd
        ny = yf - yd
        nz = zf - zd
        ps = sqrt(nx*nx+ny*ny+nz*nz)
        if (ps .le. epsi*max(nx,ny,nz)) then
            call utmess('F', 'INTEMAIL_9')
        else
            ps = un/ps
            nx = nx*ps
            ny = ny*ps
            nz = nz*ps
        endif
        vx = r11*nx + r12*ny + r13*nz
        vy = r21*nx + r22*ny + r23*nz
        vz = r31*nx + r32*ny + r33*nz
        ps = vx*(xp-xd) + vy*(yp-yd) + vz*(zp-zd)
        dedans = ( (ps .ge. 0.0d0) .or. (abs(ps) .le. epsi) )
        i = i + 1
        goto 10
    endif
end subroutine

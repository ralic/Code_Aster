subroutine te0161(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/biline.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/provec.h"
#include "asterfort/pscvec.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "blas/ddot.h"
    character(len=16) :: option, nomte
! --- ------------------------------------------------------------------
!                          CALCUL FORCES REPARTIES
!     NOMTE :
!        MECABL2
!        MECA_POU_D_T_GD
!    OPTION :
!        CHAR_MECA_FR1D1D
!        CHAR_MECA_FF1D1D
!        CHAR_MECA_SR1D1D
!        CHAR_MECA_SF1D1D'
!
! --- ------------------------------------------------------------------
    integer :: icodre, jj
    integer :: nno, kp, i, ivectu, ipesa, nddl, npg, iyty, nordre, lsect
    integer :: ipoids, ivf, igeom, imate, iforc
    integer :: itemps, nbpar, idepla, ideplp, k, l, ic, neu, iret, neum1
    logical :: normal
    real(kind=8) :: r8min, r8bid, rho, a, coef
    real(kind=8) :: s, s2, s3, s4, s5, x(4), c1, c2(3), w(6), u(3), v(3), w2(3)
!
    integer :: ifcx, idfdk, jgano, ndim, nnos
    character(len=8) :: nompar(4), nompav(1)
    real(kind=8) :: valpav(1), fcx, vite2, vp(3)
    logical :: okvent, fozero
! --- ------------------------------------------------------------------
    data nompar/'X','Y','Z','INST'/
    data nompav/'VITE'/
! --- ------------------------------------------------------------------
    r8min = r8miem()
!
    if (nomte .eq. 'MECA_POU_D_T_GD') then
        nddl = 6
    else
        nddl = 3
    endif
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdk, jgano)
    call jevete('&INEL.CABPOU.YTY', 'L', iyty)
!
    nordre = 3*nno
! --- ------------------------------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ivectu)
! --- ------------------------------------------------------------------
    if (option .eq. 'CHAR_MECA_PESA_R') then
        call jevech('PMATERC', 'L', imate)
        call jevech('PPESANR', 'L', ipesa)
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', 'ELAS', 0, ' ', r8bid,&
                    1, 'RHO', rho, icodre, 1)
        if (nomte .eq. 'MECA_POU_D_T_GD') then
            call jevech('PCAGNPO', 'L', lsect)
        else
            call jevech('PCACABL', 'L', lsect)
        endif
        a = zr(lsect)
        c1 = a*rho*zr(ipesa)
        c2(1) = zr(ipesa+1)
        c2(2) = zr(ipesa+2)
        c2(3) = zr(ipesa+3)
    endif
!     PAR DEFAUT PAS DE VENT,  EFFORT DANS LE REPERE GLOBAL
    normal = .false.
    okvent = .false.
! --- ------------------------------------------------------------------
    if (option .eq. 'CHAR_MECA_FR1D1D' .or. option .eq. 'CHAR_MECA_SR1D1D') then
        c1 = 1.0d0
!        POUR LE CAS DU VENT
        call tecach('NNN', 'PVITER', 'L', 1, iforc,&
                    iret)
        if (iret .eq. 0) then
            normal = .true.
            okvent = .true.
        else
            call jevech('PFR1D1D', 'L', iforc)
            normal = abs(zr(iforc+6)) .gt. 1.001d0
        endif
    endif
!
! --- ------------------------------------------------------------------
    if (option .eq. 'CHAR_MECA_FF1D1D' .or. option .eq. 'CHAR_MECA_SF1D1D') then
        c1 = 1.0d0
        call jevech('PFF1D1D', 'L', iforc)
        normal = zk8(iforc+6) .eq. 'VENT'
        call tecach('NNN', 'PTEMPSR', 'L', 1, itemps,&
                    iret)
        if (iret .eq. 0) then
            x(4) = zr(itemps)
            nbpar = 4
        else
            nbpar = 3
        endif
    endif
!
! --- ------------------------------------------------------------------
    if (option .eq. 'CHAR_MECA_SR1D1D' .or. option .eq. 'CHAR_MECA_SF1D1D') then
        call jevech('PDEPLMR', 'L', idepla)
        call jevech('PDEPLPR', 'L', ideplp)
        do 10 i = 1, 3
            w(i) = zr(igeom+i-1) + zr(idepla-1+i) + zr(ideplp-1+i)
            w(i+3) = zr(igeom+i+2) + zr(idepla-1+i+nddl) + zr(ideplp- 1+i+nddl)
            w2(i) = w(i+3) - w(i)
10      continue
    else
        do 20 i = 1, 3
            w(i) = zr(igeom+i-1)
            w(i+3) = zr(igeom+i+2)
            w2(i) = w(i+3) - w(i)
20      continue
    endif
!
! --- ------------------------------------------------------------------
! --- FORCES REPARTIES PAR VALEURS REELLES
    if (option .eq. 'CHAR_MECA_FR1D1D') then
!        PAS DE MOMENT REPARTIS
        r8bid = sqrt(ddot(3,zr(iforc+3),1,zr(iforc+3),1))
        if (r8bid .gt. r8min) then
            call utmess('F', 'ELEMENTS3_1', sk=nomte)
        endif
!
        c2(1) = zr(iforc)
        c2(2) = zr(iforc+1)
        c2(3) = zr(iforc+2)
        if (normal) then
            s=ddot(3,c2,1,c2,1)
            s4 = sqrt(s)
            if (s4 .gt. r8min) then
                s=ddot(3,w2,1,w2,1)
                s2 = 1.d0/s
                call provec(w2, c2, u)
                s=ddot(3,u,1,u,1)
                s3 = sqrt(s)
                s5 = s3*sqrt(s2)/s4
                call provec(u, w2, v)
                call pscvec(3, s2, v, u)
                call pscvec(3, s5, u, c2)
            endif
        endif
    endif
!
! --- ------------------------------------------------------------------
    do 80 kp = 1, npg
        k = (kp-1)*nordre*nordre
        l = (kp-1)*nno
        if (option .eq. 'CHAR_MECA_FF1D1D' .or. option .eq. 'CHAR_MECA_SF1D1D') then
!           PAS DE MOMENT REPARTIS
            fozero = (zk8(iforc+3).ne.'&FOZERO') .or. (zk8(iforc+4) .ne.'&FOZERO') .or.&
                     (zk8(iforc+5).ne.'&FOZERO')
            if (fozero) then
                call utmess('F', 'ELEMENTS3_1', sk=nomte)
            endif
!
            do 40 ic = 1, 3
                x(ic) = 0.d0
                do 30 neu = 1, nno
                    x(ic) = x(ic) + w(3*neu+ic-3)*zr(ivf+l+neu-1)
30              continue
40          continue
            do 50 ic = 1, 3
                call fointe('FM', zk8(iforc+ic-1), nbpar, nompar, x,&
                            c2( ic), iret)
50          continue
            if (normal) then
                s=ddot(3,c2,1,c2,1)
                s4 = sqrt(s)
                if (s4 .gt. r8min) then
                    s=ddot(3,w2,1,w2,1)
                    s2 = 1.d0/s
                    call provec(w2, c2, u)
                    s=ddot(3,u,1,u,1)
                    s3 = sqrt(s)
                    s5 = s3*sqrt(s2)/s4
                    call provec(u, w2, v)
                    call pscvec(3, s2, v, u)
                    call pscvec(3, s5, u, c2)
                endif
            endif
        endif
        if (okvent) then
!           RECUPERATION DE LA VITESSE DE VENT RELATIVE AU NOEUD
            c2(1) = zr(iforc)
            c2(2) = zr(iforc+1)
            c2(3) = zr(iforc+2)
!           CALCUL DU VECTEUR VITESSE PERPENDICULAIRE
            s=ddot(3,c2,1,c2,1)
            s4 = sqrt(s)
            fcx = 0.0d0
            if (s4 .gt. r8min) then
                s=ddot(3,w2,1,w2,1)
                s2 = 1.d0/s
                call provec(w2, c2, u)
                call provec(u, w2, v)
                call pscvec(3, s2, v, vp)
!              NORME DE LA VITESSE PERPENDICULAIRE
                vite2=ddot(3,vp,1,vp,1)
                valpav(1) = sqrt(vite2)
                if (valpav(1) .gt. r8min) then
!                 RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
                    call tecach('ONN', 'PVENTCX', 'L', 1, ifcx,&
                                iret)
                    if (iret .ne. 0) then
                        call utmess('F', 'ELEMENTS3_39')
                    endif
                    if (zk8(ifcx)(1:1) .eq. '.') then
                        call utmess('F', 'ELEMENTS3_39')
                    endif
                    call fointe('FM', zk8(ifcx), 1, nompav, valpav,&
                                fcx, iret)
                    fcx = fcx/valpav(1)
                endif
            endif
            call pscvec(3, fcx, vp, c2)
        endif
!
        coef = zr(ipoids-1+kp)*c1*sqrt(biline(nordre,zr(igeom), zr(iyty+k),zr(igeom)))
        do 70 neu = 1, nno
            neum1 = neu - 1
            do 60 ic = 1, 3
                jj = ivectu+nddl*neum1+(ic-1)
                zr(jj) = zr(jj) + coef*c2(ic)*zr(ivf+l+neum1)
60          continue
70      continue
80  end do
end subroutine

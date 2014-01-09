subroutine ptforp(itype, option, nomte, a, a2,&
                  xl, rad, angs2, ist, nno,&
                  nc, pgl, pgl1, pgl2, fer,&
                  fei)
    implicit none
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/angvx.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/normev.h"
#include "asterfort/pmfitx.h"
#include "asterfort/provec.h"
#include "asterfort/pscvec.h"
#include "asterfort/ptfop1.h"
#include "asterfort/rcvala.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "blas/ddot.h"
    integer :: itype, ist, nno, nc
!
    character(len=*) :: option, nomte
    real(kind=8) :: pgl(3, 3), pgl1(3, 3), pgl2(3, 3), fer(*), fei(*)
    real(kind=8) :: a, a2, xl, rad, angs2
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
! TOLE
!
    real(kind=8) :: rho(1), coef1, coef2, s, s2, s4, s3, s5
    real(kind=8) :: zero, un, xxx, r8min, r8bid, g
    real(kind=8) :: u(3), v(3), w(8), w2(3), dw(12), tet1, tet2
    real(kind=8) :: q(12), qq(12), qqr(12), qqi(12), pta(3), dir(3)
    real(kind=8) :: dir1(3), dir2(3), d1, d2, omeg2, x1(3), x2(3), v1(3), v2(3)
    integer :: icodre(1)
    character(len=8) :: nompar(4)
    character(len=16) :: ch16
    logical :: global, normal
!
    integer :: ifcx, i, j, nnoc, ncc, lx, iorien, idepla, ideplp, lmate, lpesa
    integer :: lforc, itemps, nbpar, ier, iret, icoer, icoec, iretr, iretc
    integer :: lrota
    character(len=8) :: nompav(1)
    real(kind=8) :: valpav(1), fcx, vite2, vp(3), angle(3), casect(6)
    logical :: okvent
    real(kind=8) :: dimag
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: k
!-----------------------------------------------------------------------
    data         nompar/'X','Y','Z','INST'/
    data         nompav/'VITE'/
!     ------------------------------------------------------------------
!     --- INITIALISATION  ---
!
    r8min = r8miem()
    zero = 0.0d0
    un = 1.0d0
    do 1 i = 1, 12
        q(i) = zero
        qq(i) = zero
        fer(i) = zero
        fei(i) = zero
 1  end do
    do 2 i = 1, 3
        vp(i) = zero
 2  end do
    nnoc = 1
    ncc = 6
    global = .false.
!
!
! *********************************************************************
!
    call jevech('PGEOMER', 'L', lx)
    lx = lx-1
    call jevech('PCAORIE', 'L', iorien)
!
    if (option .eq. 'CHAR_MECA_SR1D1D' .or. option .eq. 'CHAR_MECA_SF1D1D') then
        if (nomte .eq. 'MECA_POU_C_T') goto 998
        call jevech('PDEPLMR', 'L', idepla)
        call jevech('PDEPLPR', 'L', ideplp)
        do 8 i = 1, 12
            dw(i) = zr(idepla-1+i) + zr(ideplp-1+i)
 8      continue
        do 10 i = 1, 3
            w(i) = zr(lx+i) + dw(i)
            w(i+4) = zr(lx+i+3) + dw(i+6)
            w2(i) = w(i+4) - w(i)
10      continue
        call angvx(w2, angle(1), angle(2))
        s=ddot(3,w2,1,w2,1)
        xl = sqrt(s)
        tet1=ddot(3,dw(4),1,w2,1)
        tet2=ddot(3,dw(10),1,w2,1)
        angle(3) = zr(iorien+2) + (tet1 + tet2)/xl/2.0d0
        call matrot(angle, pgl)
    else
        do 15 i = 1, 3
            w(i) = zr(lx+i)
            w(i+4) = zr(lx+i+3)
            w2(i) = w(i+4) - w(i)
15      continue
    endif
!
! *********************************************************************
!
    if (option .eq. 'CHAR_MECA_PESA_R' .or. option .eq. 'CHAR_MECA_ROTA_R') then
!
        if (option .eq. 'CHAR_MECA_PESA_R') then
            call jevech('PPESANR', 'L', lpesa)
            g=zr(lpesa-1+1)
!          -- DIR EST NORME :
            dir(1)=zr(lpesa-1+2)
            dir(2)=zr(lpesa-1+3)
            dir(3)=zr(lpesa-1+4)
        else if (option.eq.'CHAR_MECA_ROTA_R') then
            call jevech('PROTATR', 'L', lrota)
            omeg2=zr(lrota-1+1)**2
            do 778, k=1,3
            dir(k)=zr(lrota-1+1+k)
            pta(k)=zr(lrota-1+4+k)
            x1(k)=zr(lx+k) - pta(k)
            x2(k)=zr(lx+3+k) - pta(k)
778          continue
            call provec(dir, x1, v1)
            call provec(dir, x2, v2)
            call normev(v1, d1)
            call normev(v2, d2)
            call provec(v1, dir, dir1)
            call provec(v2, dir, dir2)
        endif
!
        call jevech('PMATERC', 'L', lmate)
        if (nomte.eq.'MECA_POU_D_EM' .or. nomte.eq.'MECA_POU_D_TGM') then
            if (ist .eq. 1) then
                call pmfitx(zi(lmate), 2, casect, r8bid)
            else
                call pmfitx(zi(lmate), 3, casect, r8bid)
            endif
            rho(1)=casect(1)
            coef1=1.d0
            coef2=1.d0
        else
            if (ist .eq. 1) then
                call rcvala(zi(lmate), ' ', 'ELAS', 0, ' ',&
                            [r8bid], 1, 'RHO', rho(1), icodre,&
                            1)
            else
                call rcvala(zi(lmate), ' ', 'ELAS', 0, ' ',&
                            [r8bid], 1, 'RHO', rho(1), icodre,&
                            0)
                if (icodre(1) .ne. 0) rho(1) = zero
            endif
!          ---A CAUSE DES CHARGEMENTS VARIABLE ---
            coef1 = a
            coef2 = a2
        endif
!
        do 20 i = 1, 3
            if (option .eq. 'CHAR_MECA_PESA_R') then
                q(i) = rho(1) * g * dir(i)
                q(i+6) = rho(1) * g * dir(i)
            else if (option.eq.'CHAR_MECA_ROTA_R') then
                q(i) = rho(1) * omeg2 * d1 * dir1(i)
                q(i+6) = rho(1) * omeg2 * d2 * dir2(i)
            endif
20      continue
!
!        ---UN CAS DE CHARGE DE PESANTEUR SE PASSE EN REPERE GLOBAL ---
!        --- PASSAGE REPERE LOCAL DU VECTEUR FORCE ---
        if (nomte.eq.'MECA_POU_C_T') then
            call utpvgl(nnoc, ncc, pgl1, q(1), qq(1))
            call utpvgl(nnoc, ncc, pgl2, q(7), qq(7))
        else
            call utpvgl(nno, nc, pgl, q(1), qq(1))
        endif
!
        global = .true.
!
        goto 777
    endif
!
! *********************************************************************
!
    okvent = .false.
    if (option .eq. 'CHAR_MECA_FR1D1D' .or. option .eq. 'CHAR_MECA_SR1D1D') then
!     --- FORCES REPARTIES PAR VALEURS REELLES---
!        POUR LE CAS DU VENT
        call tecach('NNN', 'PVITER', 'L', iret, iad=lforc)
        if (iret .eq. 0) then
            if (nomte .eq. 'MECA_POU_C_T') goto 997
            okvent = .true.
            normal = .false.
            global = .false.
            do 30 i = 1, 3
                q(i) = zr(lforc-1+i)
                q(i+6) = zr(lforc+2+i)
30          continue
        else
            call jevech('PFR1D1D', 'L', lforc)
            xxx = abs(zr(lforc+6))
            global = xxx .lt. 1.d-3
            normal = xxx .gt. 1.001d0
            do 40 i = 1, 6
                q(i) = zr(lforc-1+i)
                q(i+6) = q(i)
40          continue
        endif
!
        elseif ( option .eq. 'CHAR_MECA_FF1D1D' .or. option .eq.&
    'CHAR_MECA_SF1D1D' ) then
!     --- FORCES REPARTIES PAR FONCTIONS ---
        call tecach('NNN', 'PTEMPSR', 'L', iret, iad=itemps)
        if (iret .eq. 0) then
            w(4) = zr(itemps)
            w(8) = zr(itemps)
            nbpar = 4
        else
            nbpar = 3
        endif
        call jevech('PFF1D1D', 'L', lforc)
        normal = zk8(lforc+6) .eq. 'VENT'
        global = zk8(lforc+6) .eq. 'GLOBAL'
        do 50 i = 1, 6
            j = i + 6
            call fointe('FM', zk8(lforc+i-1), nbpar, nompar, w(1),&
                        q(i), ier)
            call fointe('FM', zk8(lforc+i-1), nbpar, nompar, w(5),&
                        q(j), ier)
50      continue
!
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
!
!     --- CONTROLE DE VALIDITE DE FORCES VARIANT LINEAIREMENT ---
    if (itype .ne. 0) then
        do 342 i = 1, 6
            if (qq(i) .ne. qq(i+6)) then
                if (itype .eq. 10) then
                    call utmess('F', 'ELEMENTS2_49')
                else
                    call utmess('F', 'ELEMENTS2_50')
                endif
            endif
342      continue
    endif
!
    if (okvent) then
        s=ddot(3,w2,1,w2,1)
        s2=1.d0/s
!
!        CALCUL DE LA NORME DU VECTEUR A PROJETTER
        s=ddot(3,q(1),1,q(1),1)
        s4 = sqrt(s)
!
!        CALCUL DU VECTEUR VITESSE PERPENDICULAIRE
        fcx = 0.0d0
        if (s4 .gt. r8min) then
            call provec(w2, q(1), u)
            call provec(u, w2, v)
            call pscvec(3, s2, v, vp)
!          NORME DE LA VITESSE PERPENDICULAIRE
            vite2=ddot(3,vp,1,vp,1)
            valpav(1) = sqrt( vite2 )
            if (valpav(1) .gt. r8min) then
!            RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
                call tecach('ONN', 'PVENTCX', 'L', iret, iad=ifcx)
                if (iret .ne. 0) goto 999
                if (zk8(ifcx)(1:1) .eq. '.') goto 999
                call fointe('FM', zk8(ifcx), 1, nompav, valpav,&
                            fcx, iret)
                fcx = fcx / valpav(1)
            endif
        endif
        call pscvec(3, fcx, vp, q(1))
!
!        CALCUL DE LA NORME DU VECTEUR A PROJETTER
        s=ddot(3,q(7),1,q(7),1)
        s4 = sqrt(s)
!
!        CALCUL DU VECTEUR VITESSE PERPENDICULAIRE
        fcx = 0.0d0
        if (s4 .gt. r8min) then
            call provec(w2, q(7), u)
            call provec(u, w2, v)
            call pscvec(3, s2, v, vp)
!          NORME DE LA VITESSE PERPENDICULAIRE
            vite2=ddot(3,vp,1,vp,1)
            valpav(1) = sqrt( vite2 )
            if (valpav(1) .gt. r8min) then
!            RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
                call tecach('ONN', 'PVENTCX', 'L', iret, iad=ifcx)
                if (iret .ne. 0) goto 999
                if (zk8(ifcx)(1:1) .eq. '.') goto 999
                call fointe('FM', zk8(ifcx), 1, nompav, valpav,&
                            fcx, iret)
                fcx = fcx / valpav(1)
            endif
        endif
        call pscvec(3, fcx, vp, q(7))
!
    else if (normal) then
        s=ddot(3,w2,1,w2,1)
        s2=1.d0/s
!
        s=ddot(3,q(1),1,q(1),1)
        s4 = sqrt(s)
        if (s4 .gt. r8min) then
            call provec(w2, q(1), u)
            s=ddot(3,u,1,u,1)
            s3 = sqrt(s)
            s5 = s3*sqrt(s2)/s4
            call provec(u, w2, v)
            call pscvec(3, s2, v, u)
            call pscvec(3, s5, u, q(1))
        endif
!
        s=ddot(3,q(7),1,q(7),1)
        s4 = sqrt(s)
        if (s4 .gt. r8min) then
            call provec(w2, q(7), u)
            s=ddot(3,u,1,u,1)
            s3 = sqrt(s)
            s5 = s3*sqrt(s2)/s4
            call provec(u, w2, v)
            call pscvec(3, s2, v, u)
            call pscvec(3, s5, u, q(7))
        endif
    endif
!     --- PASSAGE REPERE LOCAL DU VECTEUR FORCE  ---
    if (global .or. normal .or. okvent) then
        if (nomte.eq.'MECA_POU_C_T') then
            call utpvgl(nnoc, ncc, pgl1, q(1), qq(1))
            call utpvgl(nnoc, ncc, pgl2, q(7), qq(7))
        else
            call utpvgl(nno, nc, pgl, q(1), qq(1))
        endif
    else
        do 343 i = 1, 12
            qq(i) = q(i)
343      continue
    endif
!
!      ---A CAUSE DES CHARGEMENTS VARIABLES ---
    coef1 = un
    coef2 = un
!
777  continue
!
! *********************************************************************
!
!     --- RECUPERATION DU COEF_MULT ---
!
    call tecach('NNN', 'PCOEFFR', 'L', iretr, iad=icoer)
    call tecach('NNN', 'PCOEFFC', 'L', iretc, iad=icoec)
!
    if (iretr .eq. 0) then
        do 400 i = 1, 12
            qq(i) = qq(i) * zr(icoer)
400      continue
        call ptfop1(itype, coef1, coef2, xl, rad,&
                    angs2, global, qq, fer)
!
    else if (iretc .eq. 0) then
        do 410 i = 1, 12
            qqr(i) = qq(i) * dble( zc(icoec) )
            qqi(i) = qq(i) * dimag( zc(icoec) )
410      continue
        call ptfop1(itype, coef1, coef2, xl, rad,&
                    angs2, global, qqr, fer)
        call ptfop1(itype, coef1, coef2, xl, rad,&
                    angs2, global, qqi, fei)
!
    else
        call ptfop1(itype, coef1, coef2, xl, rad,&
                    angs2, global, qq, fer)
!
    endif
!
!
    goto 1000
!
997  continue
    call utmess('F', 'ELEMENTS2_51')
!
998  continue
    call utmess('F', 'ELEMENTS2_52')
!
999  continue
    call utmess('F', 'ELEMENTS2_53')
!
!
1000  continue
end subroutine

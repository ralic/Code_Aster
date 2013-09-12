subroutine pdadom(xm0, xm2, xm4, dom)
    implicit none
#include "jeveux.h"
#include "asterc/erfcam.h"
#include "asterc/r8pi.h"
#include "asterc/r8vide.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedetr.h"
#include "asterfort/limend.h"
#include "asterfort/rccome.h"
#include "asterfort/rcpare.h"
#include "asterfort/rcvale.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    real(kind=8) :: xm0, xm2, xm4, dom
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
! ======================================================================
!
!     CALCUL DOMMAGE EN FREQUENTIEL
!     ----------------------------------------------------------------
!
!
    integer :: icodwo, icodre(6)
    integer :: icodba, icodhs
    character(len=8) :: nommat, cara
    character(len=8) :: method, mecomp, nompar
    character(len=8) :: nomres(6), kcorre, kbid
    character(len=16) :: pheno, phenom
    real(kind=8) :: delta, rvke, alpha, pi, salt, x, val(6), re(1)
    real(kind=8) :: valmin, valmax, pas, xireg, rundf, nrupt(1)
    integer :: ibask, ifonc, ihosin, iawho2, nbval
    logical :: endur
    integer :: iarg
!
!     ----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iapics, ipoint, nbpar, nbpoin
    real(kind=8) :: rbid, x1, x2, xnpoin
    real(kind=8) :: xp, y, y1, yd1, yd2, ypic1, ypic2
!
!-----------------------------------------------------------------------
    rbid = 0.d0
    ifonc = 0
    ibask = 0
    ihosin = 0
    pi = r8pi()
    rundf = r8vide()
    call getvtx(' ', 'DOMMAGE', scal=method, nbret=nbval)
    call getvid(' ', 'MATER', scal=nommat, nbret=nbval)
    pheno = 'FATIGUE'
    call rccome(nommat, pheno, phenom, icodre(1))
    if (icodre(1) .eq. 1) call u2mess('F', 'FATIGUE1_24')
    cara = 'WOHLER'
    call rcpare(nommat, pheno, cara, icodwo)
    cara = 'A_BASQUI'
    call rcpare(nommat, pheno, cara, icodba)
    cara = 'A0'
    call rcpare(nommat, pheno, cara, icodhs)
    if (icodwo .eq. 0) then
        ifonc = 1
    else if (icodba.eq.0) then
        ibask = 1
    else if (icodhs.eq.0) then
        ihosin = 1
    else
        call u2mess('F', 'FATIGUE1_34')
    endif
!
!----  DEFINITION DES BORNES INTEGRATION
!
    call getvtx(' ', 'COMPTAGE', scal=mecomp, nbret=nbval)
    if (mecomp .eq. 'PIC     ' .and. xm4 .eq. rundf) then
        call u2mess('F', 'FATIGUE1_35')
    endif
    if (ihosin .ne. 0) then
        nomres(6) = 'SL'
        nbpar = 0
        nompar = ' '
        call rcvale(nommat, 'FATIGUE', nbpar, nompar, [rbid],&
                    1, nomres(6), val(6), icodre(6), 2)
        valmin = val(6)
        valmax = 10*sqrt(xm0)
    else
        valmin = 0.d0
        valmax = 10*sqrt(xm0)
    endif
    pas = (valmax-valmin)/300.d0
    if (pas .eq. 0.0d0) then
        call u2mess('F', 'FATIGUE1_36')
    endif
    xnpoin = (valmax-valmin)/pas
    nbpoin = int(xnpoin) + 1
!
!------- CALCUL DES POINTS INTEGRATION
!
    if (xm2 .eq. 0.d0) then
        call u2mess('F', 'FATIGUE1_37')
    endif
    if (mecomp .eq. 'PIC' .and. xm4 .eq. 0.d0) then
        call u2mess('F', 'FATIGUE1_38')
    endif
    call wkvect('&&PDADOM.DISPICS', 'V V R8', 2*nbpoin, iapics)
    if (mecomp .eq. 'PIC     ') xireg = sqrt( xm2*xm2/xm0/xm4)
    do 305 ipoint = 1, nbpoin
        x1 = valmin + (ipoint-1)*pas
        if (mecomp .eq. 'PIC     ') then
            alpha = xireg*x1/((sqrt(1.d0-xireg*xireg))*(sqrt(xm0)))
            alpha = (-1.d0/sqrt(2.d0))*alpha
            y1=sqrt(1-xireg*xireg) *exp(-x1*x1/(2.d0*xm0*(1.d0-xireg*&
            xireg)))
            xp=sqrt(pi/2.d0)*erfcam(alpha)
            y1=y1+((xireg*x1/sqrt(xm0))*exp(-x1*x1/(2.d0*xm0)))*(xp)
            y1=(sqrt(xm4)/(sqrt(xm2)*sqrt(xm0)))*y1
            y1=(1.d0/(2.d0*pi))*(1.d0/sqrt(2.d0*pi))*y1
        else if (mecomp.eq.'NIVEAU  ') then
            y1 = (1.d0/(2.d0*pi))*sqrt(xm2/(xm0*xm0*xm0))
            y1 = y1 *x1*exp(-x1*x1/(2.d0*xm0))
        endif
        zr(iapics-1+ipoint) = x1
        zr(iapics-1+nbpoin+ipoint) = y1
305  end do
!
!---------CORRECTION ELASTO-PLASTIQUE
!
    call getvtx(' ', 'CORR_KE', nbval=0, nbret=nbval)
    if (nbval .ne. 0) then
        call getvtx(' ', 'CORR_KE', scal=kcorre, nbret=nbval)
        call getvid(' ', 'MATER', scal=nommat, nbret=nbval)
        if (kcorre .eq. 'RCCM') then
            nomres(1) = 'N_KE'
            nomres(2) = 'M_KE'
            nomres(3) = 'SM'
            nbpar = 0
            nompar = ' '
            call rcvale(nommat, 'RCCM', nbpar, nompar, [rbid],&
                        3, nomres(1), val(1), icodre(1), 2)
            do 304 ipoint = 1, nbpoin
                delta = zr(iapics+ipoint-1)
                if (delta .le. 3.d0*val(3)) then
                    rvke = 1.d0
                    elseif(delta.gt.3.d0*val(3).and.delta.lt. 3.d0*val(2)*&
                val(3)) then
                    rvke = 1.d0 + (( 1-val(1))/(val(1)*(val(2)-1)))* ((delta/(3.d0*val(3)) )-1.d0&
                           )
                else if (delta.ge.3*val(2)*val(3)) then
                    rvke = 1.d0/val(1)
                endif
                zr(iapics+ipoint-1) = rvke * zr(iapics+ipoint-1)
304          continue
        endif
    endif
!
! ----- INTERPOLATION
!
    if (method .eq. 'WOHLER') then
!
! --- INTERPOLATION SUR LA COURBE DE WOHLER ---
!
        call wkvect('&&PDADOM.WOHLER2', 'V V R8', nbpoin, iawho2)
        if (ifonc .ne. 0) then
            nomres(1) = 'WOHLER'
            nbpar = 1
            pheno = 'FATIGUE'
            nompar = 'SIGM'
            do 307 ipoint = 1, nbpoin
                delta = zr(iapics-1+ipoint)
                call limend(nommat, delta, 'WOHLER', kbid, endur)
                if (endur) then
                    zr(iawho2+ipoint-1) = 0.d0
                else
                    call rcvale(nommat, pheno, nbpar, nompar, [delta],&
                                1, nomres(1), nrupt(1), icodre(1), 2)
                    zr(iawho2+ipoint-1) = 1.d0 / nrupt(1)
                endif
307          continue
        else if (ibask.ne.0) then
            nompar = ' '
            nbpar = 0
            nomres(1) = 'A_BASQUI'
            nomres(2) = 'BETA_BAS'
            call rcvale(nommat, 'FATIGUE', nbpar, nompar, [rbid],&
                        2, nomres, val, icodre, 2)
            do 308 ipoint = 1, nbpoin
                zr(iawho2+ipoint-1) = val(1)*zr(iapics+ipoint-1)**val( 2)
308          continue
        else if (ihosin.ne.0) then
            nomres(1) = 'E_REFE'
            nomres(2) = 'A0'
            nomres(3) = 'A1'
            nomres(4) = 'A2'
            nomres(5) = 'A3'
            nomres(6) = 'SL'
            nbpar = 0
            nompar = ' '
            call rcvale(nommat, 'FATIGUE', nbpar, nompar, [rbid],&
                        6, nomres, val, icodre, 2)
            nomres(1) = 'E'
            call rcvale(nommat, 'ELAS', nbpar, nompar, [rbid],&
                        1, nomres, re(1), icodre, 2)
            do 309 ipoint = 1, nbpoin
                salt = (val(1)/re(1))*zr(iapics+ipoint-1)
                if (salt .ge. val(6)) then
                    x = log10 (salt)
                    y = val(2) + val(3)*x + val(4)*(x**2) + val(5)*( x**3)
                    zr(iawho2+ipoint-1) = 1.d0 / (10.d0**y)
                else
                    zr(iawho2+ipoint-1) = 0.d0
                endif
309          continue
        endif
    endif
!
! -- CALCUL INTEGRALE
!
    dom = 0.d0
    do 310 ipoint = 2, nbpoin
        x2 = zr(iapics-1+ipoint)
        x1 = zr(iapics-1+ipoint-1)
        yd2 = zr(iawho2-1+ipoint)
        yd1 = zr(iawho2-1+ipoint-1)
        ypic2 = zr(iapics-1+nbpoin+ipoint)
        ypic1 = zr(iapics-1+nbpoin+ipoint-1)
        dom = dom + (yd2*ypic2+yd1*ypic1)* (x2-x1)/2.d0
310  end do
!
    call jedetr('&&PDADOM.DISPICS')
    call jedetr('&&PDADOM.WOHLER2')
!
end subroutine

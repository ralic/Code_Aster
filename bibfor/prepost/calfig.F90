subroutine calfig(guidag, resu, dimobs, dimtub, obsuse,&
                  tubuse)
! aslint: disable=
    implicit   none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterc/r8rddg.h"
#include "asterfort/assert.h"
#include "asterfort/fo0182.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/tbajli.h"
#include "asterfort/wkvect.h"
    integer :: no, dimobs, dimtub
    real(kind=8) :: obsuse(*), tubuse(*)
    character(len=8) :: guidag
    character(len=19) :: resu
!-----------------------------------------------------------------------
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
!   CALCULE LES FIGURES DE JEU A PARTIR DES TUBES ET OBSTACLES USES
! ----------------------------------------------------------------------
    integer :: i, j, k, n, p, q, r, k1, n1, contac, ndim, nv
    integer :: ibid, icomp1, icomp2, icomp3, lval, lpro
    integer :: ijeu, ijprm, itub, iobs, idray, idthe
    parameter  ( ndim = 3000 )
    complex(kind=8) :: c16b
    real(kind=8) :: r8b, c, d, e, f, ang2, angmin, angmax, rho2
    real(kind=8) :: x, y, xtube, ytube, rtube2
    real(kind=8) :: pas, pas1, pas2, pas3, thet, tetdeg, espace
    real(kind=8) :: ang2bi, rho2bi, xa, xabis, ya, yabis, a1, b1, game
    real(kind=8) :: rad, deg, pi
    integer :: nbval
    character(len=8) :: k8b
    character(len=16) :: nopara(2)
    character(len=19) :: nomfon
    character(len=24) :: tabk(2), nomf
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    espace = 0.d0
    rad = r8dgrd( )
    deg = r8rddg( )
    pi = r8pi( )
!
    call wkvect('&&CALFIG.JEU', 'V V R', 2*ndim, ijeu)
    call wkvect('&&CALFIG.JEUPRM', 'V V R', 2*ndim, ijprm)
    call wkvect('&&CALFIG.TUB', 'V V R', 2*ndim, itub)
    call wkvect('&&CALFIG.OBS', 'V V R', 2*ndim, iobs)
!
    nomf = resu(1:8)//'_INITIAL'
    call jeveuo(nomf(1:19)//'.VALE', 'L', idthe)
    call jelira(nomf(1:19)//'.VALE', 'LONMAX', nbval)
    no = nbval/2
    idray = idthe + no
!
    nopara(1) = 'LIEU'
    nopara(2) = 'FONCTION'
!
    call gcncon('_', k8b)
    nomfon = resu(1:8)//k8b
    call fo0182(nomfon, dimtub, tubuse)
    tabk(1) = 'TUBE_USE'
    tabk(2) = nomfon
    call tbajli(resu, 2, nopara, ibid, r8b,&
                c16b, tabk, 0)
!
    call gcncon('_', k8b)
    nomfon = resu(1:8)//k8b
    call fo0182(nomfon, dimobs, obsuse)
    tabk(1) = 'OBST_USE'
    tabk(2) = nomfon
    call tbajli(resu, 2, nopara, ibid, r8b,&
                c16b, tabk, 0)
!
! --- REMPLISSAGE DES TABLEAUX POUR LE CALCUL :
!     ---------------------------------------
!
    if (guidag .eq. 'CERCLE') then
        do 100 j = 1, no
            zr(ijeu+2*j-2) = zr(idthe+j-1) / rad
            zr(ijeu+2*j-1) = 6.5d-4
100      continue
    else
        do 101 j = 1, no
            zr(ijeu+2*j-2) = zr(idthe+j-1) / rad
            zr(ijeu+2*j-1) = zr(idray+j-1)
101      continue
    endif
!
    call gcncon('_', k8b)
    nomfon = resu(1:8)//k8b
    call fo0182(nomfon, no, zr(ijeu))
    tabk(1) = 'JEU_INIT'
    tabk(2) = nomfon
    call tbajli(resu, 2, nopara, ibid, r8b,&
                c16b, tabk, 0)
!
    n1 = no
    do 104 j = 1, (n1-1)
        zr(ijeu+2*(j+n1)-2) = zr(ijeu+2*j) + 360.d0
        zr(ijeu+2*(j+n1)-1) = zr(ijeu+2*j+1)
104  end do
    do 106 j = 1, dimobs
        zr(iobs+2*j-2) = obsuse(2*j-1)
        zr(iobs+2*j-1) = obsuse(2*j)
106  end do
    do 108 j = 1, (dimobs-1)
        zr(iobs+2*(j+dimobs)-2) = zr(iobs+2*j) + 360.d0
        zr(iobs+2*(j+dimobs)-1) = zr(iobs+2*j+1)
108  end do
    do 110 j = 1, dimtub
        zr(itub+2*j-2) = tubuse(2*j-1)
        zr(itub+2*j-1) = tubuse(2*j)
110  end do
    do 112 j = 1, (dimtub-1)
        zr(itub+2*(j+dimtub)-2) = zr(itub+2*j) + 360.d0
        zr(itub+2*(j+dimtub)-1) = zr(itub+2*j+1)
112  end do
!
! --- DETERMINATION DU PAS DE CALCUL :
!     ------------------------------
    pas1 = 0.100d-3
    pas2 = 0.010d-3
    pas3 = 0.001d-3
!
    pas = pas1
    n = 1
200  continue
    i = 1
    espace = zr(ijeu+2*n-1)
!
! ------ ANGLE DE BALAYAGE :
!        -----------------
!
    if (zr(ijeu+2*n-2) .gt. 90.d0) then
        if (zr(ijeu+2*n-2) .lt. 450.d0) then
            angmax = zr(ijeu+2*n-2)+90.d0
            angmin = zr(ijeu+2*n-2)-90.d0
        endif
    endif
!
    do 202 k1 = 1, (2*dimtub)
        if (zr(ijeu+2*n-2) .gt. 90.d0) then
            if (zr(ijeu+2*n-2) .lt. 450.d0) then
                if (zr(itub+2*k1-2) .gt. angmin) goto 204
            endif
        endif
202  continue
204  continue
    contac = 0
!
! ------ AVANCEE DU CRAYON, DETERMINATION DES COORDONNEES :
!        ------------------------------------------------
!
206  continue
    if (pas .eq. pas2) then
        icomp2 = icomp2 + 1
        icomp3 = 0
    else if (pas .eq. pas1) then
        icomp1 = i
        icomp2 = 0
        icomp3 = 0
    else if (pas .eq. pas3) then
        icomp3 = icomp3 + 1
    endif
    x = ( icomp1*pas1 + icomp2*pas2 + icomp3*pas3 + espace ) * cos( zr(ijeu+2*n-2)*rad )
    y = ( icomp1*pas1 + icomp2*pas2 + icomp3*pas3 + espace ) * sin( zr(ijeu+2*n-2)*rad )
!
! ------ BALAYAGE DE PART ET D'AUTRE DE L'ANGLE TRAITE :
!        ---------------------------------------------
!
    if (zr(ijeu+2*n-2) .gt. 90.d0) then
        if (zr(ijeu+2*n-2) .lt. 450.d0) then
            do 208 k = k1, (2*dimtub)
                if (zr(itub+2*k-2) .gt. angmax) goto 210
!
! --------------- AVANCEE DU CRAYON AU NIVEAU DE L'ANGLE BALAYE
!                 DANS LA DIRECTION DE L'ANGLE TRAITE :
!                 -----------------------------------
!
                xtube = zr(itub+2*k-1)*cos(zr(itub+2*k-2)*rad) + x
                ytube = zr(itub+2*k-1)*sin(zr(itub+2*k-2)*rad) + y
                rtube2 = xtube*xtube + ytube*ytube
                if (xtube .eq. 0.d0) then
                    thet = 0.d0
                else
                    if ((xtube.gt.0.d0) .and. (ytube.gt.0.d0)) then
                        thet = atan(ytube/xtube)
                        tetdeg = thet*deg
                    else if ((xtube.lt.0.d0).and.(ytube.gt.0.d0)) then
                        thet = pi-abs(atan(ytube/xtube))
                        tetdeg = thet*deg
                    else if ((xtube.lt.0.d0).and.(ytube.lt.0.d0)) then
                        thet = pi+abs(atan(ytube/xtube))
                        tetdeg = thet*deg
                    else if ((xtube.gt.0.d0).and.(ytube.lt.0.d0)) then
                        thet = 2.d0*pi-abs(atan(ytube/xtube))
                        tetdeg = thet*deg
                    endif
                endif
!
! --------------- DETERMINATION DE L ANGLE OBSTACLE CORRESPONDANT :
!                 -----------------------------------------------
!
                p = 0
1458              continue
                p = p + 1
                rho2 = zr(iobs+2*p-1)
                ang2 = zr(iobs+2*p-2)
                if (zr(iobs+2*p-2) .le. tetdeg) then
                    rho2bi = rho2
                    ang2bi = ang2
                    goto 1458
                endif
                xa = rho2*cos(ang2*rad)
                ya = rho2*sin(ang2*rad)
                xabis = rho2bi*cos(ang2bi*rad)
                yabis = rho2bi*sin(ang2bi*rad)
                if (abs(xa-xabis) .gt. r8prem( )) then
                    a1 = (ya-yabis)/(xa-xabis)
                    b1 = ya - a1*xa
                    rho2 = b1/(sin(tetdeg*rad)-a1*cos(tetdeg*rad))
                else
                    rho2 = xa/cos(tetdeg*rad)
                endif
!
! ------------------ TEST DE CONTACT :
!                    ---------------
!
                if (rtube2 .gt. (rho2*rho2)) then
                    if (pas .eq. pas1) then
                        i = i - 1
                        icomp1 = i
                        pas = pas2
                        icomp2 = 0
                        goto 206
                    else if (pas .eq. pas2) then
                        icomp2 = icomp2 - 1
                        pas = pas3
                        icomp3 = 0
                        goto 206
                    else if (pas .eq. pas3) then
                        contac = 1
                        icomp3 = icomp3 - 1
                    endif
                    goto 210
                endif
208          continue
210          continue
            if (contac .eq. 0) then
                i=i+1
                goto 206
            else
                if (contac .eq. 1) then
                    espace =espace+icomp1*pas1+icomp2*pas2+icomp3*&
                    pas3
                    pas = pas1
                    if (zr(ijeu+2*n-2) .eq. 360.d0) game = espace
                    if (zr(ijeu+2*n-2) .ge. 360.d0) then
                        zr(ijeu+2*n-2) = zr(ijeu+2*n-2)-360.d0
                    endif
                    zr(ijprm+2*n-2) = zr(ijeu+2*n-2)
                    zr(ijprm+2*n-1) = espace
                endif
            endif
        endif
    endif
    n = n + 1
    if (n .le. (2*n1-1)) goto 200
!
    do 300 q = 1, n
        c = zr(ijprm+2*q-2)
        d = zr(ijprm+2*q-1)
        do 302 r = (q+1), n
            if ((zr(ijprm+2*r-2).lt.c ) .and. (zr(ijprm+2*r-2) .gt.0.d0)) then
                e = c
                f = d
                c = zr(ijprm+2*r-2)
                d = zr(ijprm+2*r-1)
                zr(ijprm+2*r-2) = e
                zr(ijprm+2*r-1) = f
            endif
302      continue
        zr(ijprm+2*q-2) = c
        zr(ijprm+2*q-1) = d
300  end do
    nv = 2
    do 304 q = 1, n
        if (zr(ijprm+2*q-2) .ne. 0.d0) nv = nv +1
304  end do
!
!
    call gcncon('_', k8b)
    nomfon = resu(1:8)//k8b
!
    ASSERT(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', 'G V K24', 6, lpro)
    zk24(lpro) = 'FONCTION'
    zk24(lpro+1) = 'LIN LIN '
    zk24(lpro+2) = 'THETA   '
    zk24(lpro+3) = 'R       '
    zk24(lpro+4) = 'EE      '
    zk24(lpro+5) = nomfon
!
    call wkvect(nomfon//'.VALE', 'G V R8', 2*nv, lval)
!
    i = 1
    zr(lval+i-1) = 0.d0
    zr(lval+nv+i-1) = game
    do 306 q = 1, n
        if (zr(ijprm+2*q-2) .ne. 0.d0) then
            i = i + 1
            zr(lval+i-1) = zr(ijprm+2*q-2)
            zr(lval+nv+i-1) = zr(ijprm+2*q-1)
        endif
306  end do
    i = i + 1
    zr(lval+i-1) = 360.d0
    zr(lval+nv+i-1) = game
!
    tabk(1) = 'JEU_USE'
    tabk(2) = nomfon
    call tbajli(resu, 2, nopara, ibid, r8b,&
                c16b, tabk, 0)
!
    call jedetr('&&CALFIG.JEU')
    call jedetr('&&CALFIG.JEUPRM')
    call jedetr('&&CALFIG.TUB')
    call jedetr('&&CALFIG.OBS')
    call jedema()
end subroutine

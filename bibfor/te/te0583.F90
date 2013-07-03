subroutine te0583(option, nomte)
! aslint: disable=W1501
    implicit none
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
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/carcou.h"
#include "asterfort/elref5.h"
#include "asterfort/fointe.h"
#include "asterfort/jevecd.h"
#include "asterfort/jevech.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vlggl.h"
#include "asterfort/vlgglc.h"
    character(len=16) :: option, nomte
!    - FONCTION REALISEE:  CALCUL DU SECOND MEMBRE : TRAVAIL DE LA
!                          PRESSION ET FORCES LINEIQUES TUYAUX
!     OPTIONS :'CHAR_MECA_PESA_R' 'CHAR_MECA_FR1D1D''CHAR_MECA_PRES_R'
!              'CHAR_MECA_PRES_F'
! ......................................................................
    integer :: nbrddm
    parameter (nbrddm=156)
    real(kind=8) :: h, a, l, presno(4), prespg(4), rint
    real(kind=8) :: vpesan(6), fpesan(6), pesan, f(nbrddm)
    real(kind=8) :: pi, deuxpi, fi, pass(nbrddm, nbrddm)
    real(kind=8) :: fpesa1(6), fpesa2(6), fpesa3(6), vtemp(nbrddm)
    real(kind=8) :: vpesa1(6), vpesa2(6), vpesa3(6), vpesa4(6)
    real(kind=8) :: pgl(3, 3), pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), omega
    real(kind=8) :: hk, poids, rayon, theta, tk(4), ck, sk
    real(kind=8) :: cosfi, sinfi, te, pgl4(3, 3), fpesa4(6), xpg(4)
    real(kind=8) :: r8b, rext, sec, rho, r, time, valpar(4)
    integer :: codres, kpg, spt
    character(len=8) :: nompar(4), fami, poum
    character(len=16) :: phenom
    integer :: nbcou, nbsec, m, lorien, icoude
    integer :: ipoids, ivf, i, icou, ibloc, ino, nbpar, icompx, niter, iter
    integer :: icagep, igeom, lmater, jpesa, jout, lforc, iret
    integer :: igau, isect, ipres, k, ivect, nbrddl, indic0
    integer :: indic1, indic2, indic3, indic4, indic5, j
    integer :: jnbspi, nbsecm, nbcoum, itemps, ier
    integer :: ndim, nnos, nno, jcoopg, idfdk, jdfd2, jgano, npg
    parameter (nbsecm=32,nbcoum=10)
    real(kind=8) :: poicou(2*nbcoum+1), poisec(2*nbsecm+1)
    logical :: normal, global
    call elref5(' ', 'MASS', ndim, nno, nnos,&
                npg, ipoids, jcoopg, ivf, idfdk,&
                jdfd2, jgano)
!
    if (option .eq. 'CHAR_MECA_FC1D1D') then
        icompx = 1
        niter = 2
    else
        icompx = 0
        niter = 1
    endif
    pi = r8pi()
    deuxpi = 2.d0*pi
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou = zi(jnbspi-1+1)
    nbsec = zi(jnbspi-1+2)
!
!     -- CALCUL DES POIDS DES COUCHES ET DES SECTEURS:
    poicou(1) = 1.d0/3.d0
    do 10 i = 1, nbcou - 1
        poicou(2*i) = 4.d0/3.d0
        poicou(2*i+1) = 2.d0/3.d0
10  end do
    poicou(2*nbcou) = 4.d0/3.d0
    poicou(2*nbcou+1) = 1.d0/3.d0
    poisec(1) = 1.d0/3.d0
    do 20 i = 1, nbsec - 1
        poisec(2*i) = 4.d0/3.d0
        poisec(2*i+1) = 2.d0/3.d0
20  end do
    poisec(2*nbsec) = 4.d0/3.d0
    poisec(2*nbsec+1) = 1.d0/3.d0
    m = 3
    if (nomte .eq. 'MET6SEG3') m = 6
!
!
    do 30 i = 1, npg
        xpg(i) = zr(jcoopg-1+i)
30  end do
    nbrddl = nno* (6+3+6* (m-1))
    if (nbrddl .gt. nbrddm) then
        call u2mess('F', 'ELEMENTS4_40')
    endif
    if (nomte .eq. 'MET3SEG3') then
        if (nbrddl .ne. 63) then
            call u2mess('F', 'ELEMENTS4_41')
        endif
    else if (nomte.eq.'MET6SEG3') then
        if (nbrddl .ne. 117) then
            call u2mess('F', 'ELEMENTS4_41')
        endif
    else if (nomte.eq.'MET3SEG4') then
        if (nbrddl .ne. 84) then
            call u2mess('F', 'ELEMENTS4_41')
        endif
    else
        call u2mess('F', 'ELEMENTS4_42')
    endif
    call jevech('PCAORIE', 'L', lorien)
    call carcou(zr(lorien), l, pgl, rayon, theta,&
                pgl1, pgl2, pgl3, pgl4, nno,&
                omega, icoude)
    if (icoude .ge. 10) then
        icoude = icoude - 10
    endif
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCAGEPO', 'L', icagep)
    h = zr(icagep+1)
    a = zr(icagep) - h/2.d0
    rint = a - h/2.d0
    rext = a + h/2.d0
    sec = pi* (rext**2-rint**2)
! A= RMOY, H = EPAISSEUR RINT = RAYON INTERIEUR
    if (nno .eq. 3) then
        tk(1) = 0.d0
        tk(2) = theta
        tk(3) = theta/2.d0
    else if (nno.eq.4) then
        tk(1) = 0.d0
        tk(2) = theta
        tk(3) = theta/3.d0
        tk(4) = 2.d0*theta/3.d0
    endif
!  LA PRESSION NE TRAVAILLE QUE SUR LE TERME WO
    if (option(1:14) .eq. 'CHAR_MECA_PRES') then
        if (option(15:16) .eq. '_R') then
            call jevecd('PPRESSR', ipres, 0.d0)
            do 40 i = 1, nno
                presno(i) = zr(ipres-1+i)
40          continue
        else if (option(15:16).eq.'_F') then
            call jevech('PPRESSF', 'L', ipres)
            call jevech('PTEMPSR', 'L', itemps)
            valpar(4) = zr(itemps)
            nompar(4) = 'INST'
            nompar(1) = 'X'
            nompar(2) = 'Y'
            nompar(3) = 'Z'
            do 45 i = 1, nno
                valpar(1) = zr(igeom+3*(i-1) )
                valpar(2) = zr(igeom+3*(i-1)+1)
                valpar(3) = zr(igeom+3*(i-1)+2)
                call fointe('FM', zk8(ipres), 4, nompar, valpar,&
                            presno(i), ier)
45          continue
        endif
        do 60,igau = 1,npg
        prespg(igau) = 0.d0
        do 50,k = 1,nno
        hk = zr(ivf-1+nno* (igau-1)+k)
        prespg(igau) = hk*presno(k) + prespg(igau)
50      continue
60      continue
        call jevech('PVECTUR', 'E', ivect)
        do 90 k = 1, nno
!           TRAVAIL SUR UX
            indic0 = ivect - 1 + (6+6* (m-1)+3)* (k-1) + 1
!           TRAVAIL SUR UY
            indic1 = ivect - 1 + (6+6* (m-1)+3)* (k-1) + 2
!           TRAVAIL SUR UZ
            indic2 = ivect - 1 + (6+6* (m-1)+3)* (k-1) + 3
!           TRAVAIL SUR W0
            indic3 = ivect - 1 + (6+6* (m-1)+3)* (k-1) + 1 + 6 + 6* ( m-1)
!           TRAVAIL SUR WI1
            indic4 = ivect - 1 + (6+6* (m-1)+3)* (k-1) + 2 + 6 + 6* ( m-1)
!           TRAVAIL SUR W01
            indic5 = ivect - 1 + (6+6* (m-1)+3)* (k-1) + 3 + 6 + 6* ( m-1)
            do 80 igau = 1, npg
! BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
                hk = zr(ivf-1+nno* (igau-1)+k)
                if (icoude .eq. 1) then
                    ck = cos((1.d0+xpg(igau))*theta/2.d0-tk(k))
                    sk = sin((1.d0+xpg(igau))*theta/2.d0-tk(k))
                else
                    ck = 1.d0
                    sk = 0.d0
                endif
! BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
                do 70 isect = 1, 2*nbsec + 1
                    if (icoude .eq. 0) then
                        poids = zr(ipoids-1+igau)*poisec(isect)* (l/ 2.d0)* deuxpi/ (2.d0*nbsec)*&
                                &rint
                        zr(indic3) = zr(indic3) + hk*poids*prespg( igau)
                    else
                        fi = (isect-1)*deuxpi/ (2.d0*nbsec)
                        cosfi = cos(fi)
                        sinfi = sin(fi)
                        te = fi - omega
                        l = theta* (rayon+rint*sinfi)
                        poids = zr(ipoids-1+igau)*poisec(isect)* (l/ 2.d0)* deuxpi/ (2.d0*nbsec)*&
                                &rint
                        zr(indic0) = zr(indic0) + hk*poids*prespg( igau)*sinfi*sk
                        zr(indic1) = zr(indic1) - hk*poids*prespg( igau)*sinfi*ck
                        zr(indic2) = zr(indic2) - hk*poids*prespg( igau)*cosfi
                        zr(indic3) = zr(indic3) + hk*poids*prespg( igau)
                        zr(indic4) = zr(indic4) + hk*poids*prespg( igau)*cos(te)
                        zr(indic5) = zr(indic5) + hk*poids*prespg( igau)*sin(te)
                    endif
70              continue
80          continue
90      continue
        if (icoude .ne. 0) then
            call vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                        pgl4, zr(ivect), 'LG', pass, vtemp)
        endif
! CAS PESANTEUR ET FORCE LINEIQUE
        else if ((option.eq.'CHAR_MECA_PESA_R') .or. (&
    option.eq.'CHAR_MECA_FR1D1D') .or. (option.eq.'CHAR_MECA_FC1D1D'))&
    then
        do 250 iter = 1, niter
            if (option .eq. 'CHAR_MECA_PESA_R') then
                call jevech('PMATERC', 'L', lmater)
                call rccoma(zi(lmater), 'ELAS', 1, phenom, codres)
                if (phenom .eq. 'ELAS' .or. phenom .eq. 'ELAS_ISTR' .or. phenom .eq.&
                    'ELAS_ORTH') then
                    fami='FPG1'
                    kpg=1
                    spt=1
                    poum='+'
                    call rcvalb(fami, kpg, spt, poum, zi(lmater),&
                                ' ', phenom, 0, ' ', r8b,&
                                1, 'RHO', rho, codres, 1)
                else
                    call u2mess('F', 'ELEMENTS4_43')
                endif
                call jevech('PPESANR', 'L', jpesa)
                pesan = zr(jpesa)
                do 100 i = 1, 3
                    vpesan(i) = rho*pesan*zr(jpesa+i)
100              continue
                do 110 i = 4, 6
                    vpesan(i) = 0.d0
110              continue
            else
                if (icompx .eq. 0) then
                    call jevech('PFR1D1D', 'L', lforc)
                else
                    call jevech('PFC1D1D', 'L', lforc)
                endif
                if (icompx .eq. 1) then
                    if (iter .eq. 1) then
                        do 120 i = 1, 3
                            vpesan(i) = dble(zc(lforc-1+i))/sec
120                      continue
                    else
                        do 130 i = 1, 3
                            vpesan(i) = dimag(zc(lforc-1+i))/sec
130                      continue
                    endif
                else
                    do 140 i = 1, 3
                        vpesan(i) = zr(lforc-1+i)/sec
140                  continue
                endif
                do 150 i = 4, 6
                    vpesan(i) = 0.d0
150              continue
            endif
            do 160 i = 1, nbrddl
                f(i) = 0.d0
160          continue
            if (icoude .eq. 0) then
                call utpvgl(1, 6, pgl, vpesan(1), fpesan(1))
            else
                call utpvgl(1, 6, pgl1, vpesan(1), fpesa1(1))
                call utpvgl(1, 6, pgl2, vpesan(1), fpesa2(1))
                call utpvgl(1, 6, pgl3, vpesan(1), fpesa3(1))
                if (nno .eq. 4) then
                    call utpvgl(1, 6, pgl4, vpesan(1), fpesa4(1))
                endif
            endif
! BOUCLE SUR LES POINTS DE GAUSS DANS LA LONGUEUR
            do 210 igau = 1, npg
! BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
                do 200 icou = 1, 2*nbcou + 1
                    r = a + (icou-1)*h/ (2.d0*nbcou) - h/2.d0
! BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
                    do 190 isect = 1, 2*nbsec + 1
                        if (icoude .eq. 0) then
                            poids = zr(ipoids-1+igau)*poicou(icou)* poisec(isect)* (l/2.d0)*h*deu&
                                    &xpi/ (4.d0* nbcou*nbsec)*r
                            do 170 k = 1, nno
                                hk = zr(ivf-1+nno* (igau-1)+k)
                                ibloc = (9+6* (m-1))* (k-1)
                                f(ibloc+1) = f(ibloc+1) + poids*hk* fpesan(1)
                                f(ibloc+2) = f(ibloc+2) + poids*hk* fpesan(2)
                                f(ibloc+3) = f(ibloc+3) + poids*hk* fpesan(3)
170                          continue
                        else if (icoude.eq.1) then
                            fi = (isect-1)*deuxpi/ (2.d0*nbsec)
                            cosfi = cos(fi)
                            sinfi = sin(fi)
                            l = theta* (rayon+r*sinfi)
                            poids = zr(ipoids-1+igau)*poicou(icou)* poisec(isect)* (l/2.d0)*h*deu&
                                    &xpi/ (4.d0* nbcou*nbsec)*r
                            do 180 k = 1, 3
                                hk = zr(ivf-1+nno* (igau-1)+1)
                                ibloc = (9+6* (m-1))* (1-1)
                                f(ibloc+k) = f(ibloc+k) + poids*hk* fpesa1(k)
                                hk = zr(ivf-1+nno* (igau-1)+2)
                                ibloc = (9+6* (m-1))* (2-1)
                                f(ibloc+k) = f(ibloc+k) + poids*hk* fpesa2(k)
                                hk = zr(ivf-1+nno* (igau-1)+3)
                                ibloc = (9+6* (m-1))* (3-1)
                                f(ibloc+k) = f(ibloc+k) + poids*hk* fpesa3(k)
                                if (nno .eq. 4) then
                                    ibloc = (9+6* (m-1))* (4-1)
                                    f(ibloc+k) = f(ibloc+k) + poids* hk*fpesa4(k)
                                endif
180                          continue
                        endif
190                  continue
200              continue
210          continue
            if (icoude .eq. 0) then
                call vlggl(nno, nbrddl, pgl, f, 'LG',&
                           pass, vtemp)
            else
                call vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                            pgl4, f, 'LG', pass, vtemp)
            endif
            if (icompx .eq. 1) then
                call jevech('PVECTUC', 'E', jout)
                if (iter .eq. 1) then
                    do 220 j = 1, nbrddl
                        zc(jout-1+j) = f(j)
220                  continue
                else
                    do 230 j = 1, nbrddl
                        zc(jout-1+j) = dcmplx(dble(zc(jout-1+j)),dble( f(j)))
230                  continue
                endif
            else
                call jevech('PVECTUR', 'E', jout)
                do 240 i = 1, nbrddl
                    zr(jout-1+i) = f(i)
240              continue
            endif
250      continue
! CAS FORCE LINEIQUE FONCTION
    else if ((option.eq.'CHAR_MECA_FF1D1D')) then
        call jevech('PFF1D1D', 'L', lforc)
        normal = zk8(lforc+6) .eq. 'VENT'
        global = zk8(lforc+6) .eq. 'GLOBAL'
        if (normal) then
            call u2mesk('F', 'ELEMENTS4_44', 1, option)
        endif
        if (.not.global) then
            call u2mesk('F', 'ELEMENTS4_45', 1, option)
        endif
        nompar(4) = 'INST'
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        call tecach('NNN', 'PTEMPSR', 'L', 1, itemps,&
                    iret)
        if (itemps .ne. 0) then
            time = zr(itemps)
            valpar(4) = time
            nbpar = 4
        else
            nbpar = 3
        endif
!        NOEUDS 1 A 3
        ino = 1
        do 260 i = 1, 3
            valpar(i) = zr(igeom-1+3* (ino-1)+i)
260      continue
        do 270 i = 1, 3
            call fointe('FM', zk8(lforc+i-1), nbpar, nompar, valpar,&
                        vpesa1(i), ier)
            vpesa1(i) = vpesa1(i)/sec
270      continue
        ino = 2
        do 280 i = 1, 3
            valpar(i) = zr(igeom-1+3* (ino-1)+i)
280      continue
        do 290 i = 1, 3
            call fointe('FM', zk8(lforc+i-1), nbpar, nompar, valpar,&
                        vpesa2(i), ier)
            vpesa2(i) = vpesa2(i)/sec
290      continue
        ino = 3
        do 300 i = 1, 3
            valpar(i) = zr(igeom-1+3* (ino-1)+i)
300      continue
        do 310 i = 1, 3
            call fointe('FM', zk8(lforc+i-1), nbpar, nompar, valpar,&
                        vpesa3(i), ier)
            vpesa3(i) = vpesa3(i)/sec
310      continue
        if (nno .eq. 4) then
            ino = 4
            do 320 i = 1, 3
                valpar(i) = zr(igeom-1+3* (ino-1)+i)
320          continue
            do 330 i = 1, 3
                call fointe('FM', zk8(lforc+i-1), nbpar, nompar, valpar,&
                            vpesa4(i), ier)
                vpesa4(i) = vpesa4(i)/sec
330          continue
        endif
        do 340 i = 4, 6
            vpesa1(i) = 0.d0
            vpesa2(i) = 0.d0
            vpesa3(i) = 0.d0
            vpesa4(i) = 0.d0
340      continue
        do 350 i = 1, nbrddl
            f(i) = 0.d0
350      continue
        if (icoude .eq. 0) then
            call utpvgl(1, 6, pgl, vpesa1(1), fpesa1(1))
            call utpvgl(1, 6, pgl, vpesa2(1), fpesa2(1))
            call utpvgl(1, 6, pgl, vpesa3(1), fpesa3(1))
            if (nno .eq. 4) then
                call utpvgl(1, 6, pgl, vpesa4(1), fpesa4(1))
            endif
        else
            call utpvgl(1, 6, pgl1, vpesan(1), fpesa1(1))
            call utpvgl(1, 6, pgl2, vpesan(1), fpesa2(1))
            call utpvgl(1, 6, pgl3, vpesan(1), fpesa3(1))
            if (nno .eq. 4) then
                call utpvgl(1, 6, pgl4, vpesan(1), fpesa4(1))
            endif
        endif
! BOUCLE SUR LES POINTS DE GAUSS DANS LA LONGUEUR
        do 400 igau = 1, npg
! BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
            do 390 icou = 1, 2*nbcou + 1
                r = a + (icou-1)*h/ (2.d0*nbcou) - h/2.d0
! BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
                do 380 isect = 1, 2*nbsec + 1
                    if (icoude .eq. 0) then
                        poids = zr(ipoids-1+igau)*poicou(icou)*poisec( isect)* (l/2.d0)*h*deuxpi/&
                                & (4.d0*nbcou*nbsec)* r
                        do 360 k = 1, 3
                            hk = zr(ivf-1+nno* (igau-1)+1)
                            ibloc = (9+6* (m-1))* (1-1)
                            f(ibloc+k) = f(ibloc+k) + poids*hk*fpesa1( k)
                            hk = zr(ivf-1+nno* (igau-1)+2)
                            ibloc = (9+6* (m-1))* (2-1)
                            f(ibloc+k) = f(ibloc+k) + poids*hk*fpesa2( k)
                            hk = zr(ivf-1+nno* (igau-1)+3)
                            ibloc = (9+6* (m-1))* (3-1)
                            f(ibloc+k) = f(ibloc+k) + poids*hk*fpesa3( k)
                            if (nno .eq. 4) then
                                ibloc = (9+6* (m-1))* (4-1)
                                f(ibloc+k) = f(ibloc+k) + poids*hk* fpesa4(k)
                            endif
360                      continue
                    else if (icoude.eq.1) then
                        fi = (isect-1)*deuxpi/ (2.d0*nbsec)
                        cosfi = cos(fi)
                        sinfi = sin(fi)
                        l = theta* (rayon+r*sinfi)
                        poids = zr(ipoids-1+igau)*poicou(icou)*poisec( isect)* (l/2.d0)*h*deuxpi/&
                                & (4.d0*nbcou*nbsec)* r
                        do 370 k = 1, 3
                            hk = zr(ivf-1+nno* (igau-1)+1)
                            ibloc = (9+6* (m-1))* (1-1)
                            f(ibloc+k) = f(ibloc+k) + poids*hk*fpesa1( k)
                            hk = zr(ivf-1+nno* (igau-1)+2)
                            ibloc = (9+6* (m-1))* (2-1)
                            f(ibloc+k) = f(ibloc+k) + poids*hk*fpesa2( k)
                            hk = zr(ivf-1+nno* (igau-1)+3)
                            ibloc = (9+6* (m-1))* (3-1)
                            f(ibloc+k) = f(ibloc+k) + poids*hk*fpesa3( k)
                            if (nno .eq. 4) then
                                ibloc = (9+6* (m-1))* (4-1)
                                f(ibloc+k) = f(ibloc+k) + poids*hk* fpesa4(k)
                            endif
370                      continue
                    endif
380              continue
390          continue
400      continue
        if (icoude .eq. 0) then
            call vlggl(nno, nbrddl, pgl, f, 'LG',&
                       pass, vtemp)
        else
            call vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                        pgl4, f, 'LG', pass, vtemp)
        endif
        call jevech('PVECTUR', 'E', jout)
        do 410,i = 1,nbrddl
        zr(jout-1+i) = f(i)
410      continue
    endif
end subroutine

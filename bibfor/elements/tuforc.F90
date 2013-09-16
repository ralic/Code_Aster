subroutine tuforc(option, nomte, nbrddl, b, f,&
                  vin, vout, mat, pass, vtemp)
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/bcoudc.h"
#include "asterfort/bcoude.h"
#include "asterfort/carcou.h"
#include "asterfort/elref5.h"
#include "asterfort/jevech.h"
#include "asterfort/moytem.h"
#include "asterfort/ppgan2.h"
#include "asterfort/prmave.h"
#include "asterfort/promat.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/terefe.h"
#include "asterfort/utmess.h"
#include "asterfort/verifg.h"
#include "asterfort/vlggl.h"
#include "asterfort/vlgglc.h"
#include "blas/daxpy.h"
    character(len=16) :: nomte
    character(len=*) :: option
! ----------------------------------------------------------------------
! =====================================================================
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
!    - FONCTION REALISEE:  CALCUL DES OPTIONS FORC_NODA ET
!      EFGE_ELNO ELEMENT: MET3SEG3 MET6SEG3 MET3SEG4
    integer :: nbres, nbrddl, nbsecm, nbcoum, nval
    parameter (nbres=9)
    character(len=8) :: nomres(nbres), nompar
    integer :: icodre(nbres)
    real(kind=8) :: valres(nbres), valpar, h, a, l, e, nu
    parameter (nbsecm=32,nbcoum=10)
    real(kind=8) :: poicou(2*nbcoum+1), poisec(2*nbsecm+1)
    real(kind=8) :: pi, deuxpi, sig(4), fpg(4, 6)
    real(kind=8) :: b(4, nbrddl), c(4, 4), f(nbrddl), efg(6), fno(6)
    real(kind=8) :: pgl(3, 3), vin(nbrddl), vout(nbrddl), mat(nbrddl, 4)
    real(kind=8) :: vtemp(nbrddl), pass(nbrddl, nbrddl), cosfi, sinfi
    real(kind=8) :: vpg(4), sigth(2), hk(4, 4), vno(4)
    real(kind=8) :: beta, cisail, fi, g, poids, r, omega, xpg(4)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), rayon, theta
    real(kind=8) :: cp(2, 2), cv(2, 2), co(4, 4), si(4, 4), tk(4), pgl4(3, 3)
    integer :: nno, npg, nbcou, nbsec, m, nspg
    integer :: ipoids, ivf, icoude, icoud2
    integer :: imate, icagep, igeom, nbpar, i1, i2, ih, mmt
    integer :: igau, icou, isect, i, j, jin, jout, iret, ino, kpgs, itab(7)
    integer :: lorien, indice, k
    integer :: ip, ic, kp
    integer :: jnbspi, iret2, nbsp
    integer :: ndim, nnos, jcoopg, idfdk, jdfd2, jgano
    real(kind=8) :: epsthe, alphaf, betaf
    real(kind=8) :: alpham, betam, xa, xb, xc, xd
    real(kind=8) :: sigtmp(4), sigref
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, jcoopg, ivf, idfdk,&
                jdfd2, jgano)
!
    pi = r8pi()
    deuxpi = 2.d0*pi
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou = zi(jnbspi-1+1)
    nbsec = zi(jnbspi-1+2)
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
!
    m = 3
    if (nomte .eq. 'MET6SEG3') m = 6
!
!
    do 30 i = 1, npg
        xpg(i) = zr(jcoopg-1+i)
30  end do
    call jevech('PCAORIE', 'L', lorien)
    call carcou(zr(lorien), l, pgl, rayon, theta,&
                pgl1, pgl2, pgl3, pgl4, nno,&
                omega, icoud2)
    if (icoud2 .ge. 10) then
        icoude = icoud2 - 10
        mmt = 0
    else
        icoude = icoud2
        mmt = 1
    endif
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCAGEPO', 'L', icagep)
    h = zr(icagep+1)
    a = zr(icagep) - h/2.d0
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
    if (option .eq. 'FORC_NODA') then
        nspg=(2*nbsec + 1)*(2*nbcou + 1)
        call tecach('OOO', 'PCONTMR', 'L', 7, itab,&
                    iret)
        jin=itab(1)
        nbsp=itab(7)
        if (nbsp .ne. nspg) then
            call utmess('F', 'ELEMENTS_4')
        endif
        call jevech('PVECTUR', 'E', jout)
        do 40 i = 1, nbrddl
            f(i) = 0.d0
40      continue
        kpgs = 0
        do 100 igau = 1, npg
            do 90 icou = 1, 2*nbcou + 1
                if (mmt .eq. 0) then
                    r = a
                else
                    r = a + (icou-1)*h/ (2.d0*nbcou) - h/2.d0
                endif
                do 80 isect = 1, 2*nbsec + 1
                    kpgs = kpgs + 1
                    indice = jin - 1 + 6* (kpgs-1)
                    sig(1) = zr(indice+1)
                    sig(2) = zr(indice+2)
                    sig(3) = zr(indice+4)
                    sig(4) = zr(indice+5)
                    if (icoude .eq. 0) then
                        call bcoude(igau, icou, isect, l, h,&
                                    a, m, nno, nbcou, nbsec,&
                                    zr(ivf), zr(idfdk), zr(jdfd2), mmt, b)
                    else if (icoude.eq.1) then
                        fi = (isect-1)*deuxpi/ (2.d0*nbsec)
                        cosfi = cos(fi)
                        sinfi = sin(fi)
                        l = theta* (rayon+r*sinfi)
                        call bcoudc(igau, icou, isect, h, a,&
                                    m, omega, xpg, nno, nbcou,&
                                    nbsec, zr(ivf), zr(idfdk), zr(jdfd2), rayon,&
                                    theta, mmt, b)
                    endif
                    do 60 i = 1, 4
                        do 50 j = 1, nbrddl
                            mat(j,i) = b(i,j)
50                      continue
60                  continue
                    iret = 0
                    call prmave(0, mat, nbrddl, nbrddl, 4,&
                                sig, 4, vout, nbrddl, iret)
!  STOCKAGE DU VECTEUR VOUT DANS FI
                    poids = zr(ipoids-1+igau)*poicou(icou)*poisec( isect)* (l/2.d0)*h*deuxpi/ (4.&
                            &d0*nbcou*nbsec)*r
                    do 70 i = 1, nbrddl
                        f(i) = f(i) + vout(i)*poids
70                  continue
80              continue
90          continue
100      continue
! PASSAGE DU REPERE LOCAL AU REPERE GLOBAL
        if (icoude .eq. 0) then
            call vlggl(nno, nbrddl, pgl, f, 'LG',&
                       pass, vtemp)
        else
            call vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                        pgl4, f, 'LG', pass, vtemp)
        endif
        do 110,i = 1,nbrddl
        zr(jout-1+i) = f(i)
110      continue
    else if (option.eq.'REFE_FORC_NODA') then
        call r8inir(nbrddl, 0.d0, vtemp, 1)
        call terefe('SIGM_REFE', 'MECA_TUYAU', sigref)
        call jevech('PVECTUR', 'E', jout)
        do 120 i = 1, nbrddl
            f(i) = 0.d0
120      continue
        do 190 igau = 1, npg
            do 180 icou = 1, 2*nbcou + 1
                if (mmt .eq. 0) then
                    r = a
                else
                    r = a + (icou-1)*h/ (2.d0*nbcou) - h/2.d0
                endif
                do 170 isect = 1, 2*nbsec + 1
                    if (icoude .eq. 0) then
                        call bcoude(igau, icou, isect, l, h,&
                                    a, m, nno, nbcou, nbsec,&
                                    zr(ivf), zr(idfdk), zr(jdfd2), mmt, b)
                    else if (icoude.eq.1) then
                        fi = (isect-1)*deuxpi/ (2.d0*nbsec)
                        cosfi = cos(fi)
                        sinfi = sin(fi)
                        l = theta* (rayon+r*sinfi)
                        call bcoudc(igau, icou, isect, h, a,&
                                    m, omega, xpg, nno, nbcou,&
                                    nbsec, zr(ivf), zr(idfdk), zr(jdfd2), rayon,&
                                    theta, mmt, b)
                    endif
                    do 140 i = 1, 4
                        do 130 j = 1, nbrddl
                            mat(j,i) = b(i,j)
130                      continue
140                  continue
                    poids = zr(ipoids-1+igau)*poicou(icou)*poisec( isect)* (l/2.d0)*h*deuxpi/ (4.&
                            &d0*nbcou*nbsec)*r
                    iret = 0
!  POUR CHAQUE CMP DE SIGM_REFE, STOCKAGE DU VECTEUR VOUT DANS F
                    call r8inir(4, 0.d0, sigtmp, 1)
                    do 160 j = 1, 4
                        sigtmp(j) = sigref
                        call prmave(0, mat, nbrddl, nbrddl, 4,&
                                    sigtmp, 4, vout, nbrddl, iret)
                        sigtmp(j) = 0.d0
                        do 150 i = 1, nbrddl
                            vtemp(i) = vtemp(i) + abs(vout(i)*poids)
150                      continue
160                  continue
170              continue
180          continue
190      continue
!      ON PREND LA VALEUR MOYENNE DES FORCES NODALES DE REFERENCE
        nval = npg* (2*nbcou+1)* (2*nbsec+1)*4
        call daxpy(nbrddl, 1.d0/nval, vtemp, 1, f,&
                   1)
        call r8inir(nbrddl, 0.d0, vtemp, 1)
!
! PASSAGE DU REPERE LOCAL AU REPERE GLOBAL
        if (icoude .eq. 0) then
            call vlggl(nno, nbrddl, pgl, f, 'LG',&
                       pass, vtemp)
        else
            call vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                        pgl4, f, 'LG', pass, vtemp)
        endif
        do 200,i = 1,nbrddl
        zr(jout-1+i) = f(i)
200      continue
    else if (option.eq.'EFGE_ELNO') then
        call jevech('PMATERC', 'L', imate)
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nomres(3) = 'ALPHA'
        nspg=(2*nbsec + 1)*(2*nbcou + 1)
        iret2=0
        call moytem('RIGI', npg, nspg, '+', valpar,&
                    iret2)
        if (iret2 .ne. 0) valpar=0.d0
        nbpar = 1
        nompar = 'TEMP'
        call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                    ' ', 'ELAS', nbpar, nompar, [valpar],&
                    2, nomres, valres, icodre, 1)
        e = valres(1)
        nu = valres(2)
        beta = e/ (1.d0-nu**2)
        g = e/ (2.d0* (1.d0+nu))
        cisail = 1.d0
        c(1,1) = beta
        c(1,2) = nu*beta
        c(1,3) = 0.d0
        c(1,4) = 0.d0
        c(2,1) = nu*beta
        c(2,2) = beta
        c(2,3) = 0.d0
        c(2,4) = 0.d0
        c(3,1) = 0.d0
        c(3,2) = 0.d0
        c(3,3) = g
        c(3,4) = 0.d0
        c(4,1) = 0.d0
        c(4,2) = 0.d0
        c(4,3) = 0.d0
        c(4,4) = g*cisail
!  CONSTRUCTION DE LA MATRICE H(I,J) = MATRICE DES VALEURS DES
!  FONCTIONS DE FORMES AUX POINTS DE GAUSS
        do 260,k = 1,nno
        do 250,igau = 1,npg
        hk(k,igau) = zr(ivf-1+nno* (igau-1)+k)
250      continue
260      continue
        call jevech('PDEPLAR', 'L', jin)
        do 290 i = 1, nbrddl
            vin(i) = zr(jin-1+i)
290      continue
        if (icoude .eq. 0) then
            call vlggl(nno, nbrddl, pgl, vin, 'GL',&
                       pass, vtemp)
        else
            call vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                        pgl4, vin, 'GL', pass, vtemp)
        endif
        nspg=(2*nbsec + 1)*(2*nbcou + 1)
        do 340 igau = 1, npg
! ATTENTION IRET NON INITIALISE PAR VERIFG
            iret=0
            call verifg('RIGI', igau, nspg, '+', zi(imate),&
                        'ELAS', 1, epsthe, iret)
            if (iret .ne. 0) epsthe=0.d0
            sigth(1) = (c(1,1)+c(1,2))*epsthe
            sigth(2) = (c(2,1)+c(2,2))*epsthe
            do 300,i = 1,6
            efg(i) = 0.d0
300          continue
            do 320 icou = 1, 2*nbcou + 1
                if (mmt .eq. 0) then
                    r = a
                else
                    r = a + (icou-1)*h/ (2.d0*nbcou) - h/2.d0
                endif
                do 310 isect = 1, 2*nbsec + 1
                    fi = (isect-1)*deuxpi/ (2.d0*nbsec)
                    if (icoude .eq. 0) then
                        cosfi = cos(fi)
                        sinfi = sin(fi)
                        call bcoude(igau, icou, isect, l, h,&
                                    a, m, nno, nbcou, nbsec,&
                                    zr(ivf), zr(idfdk), zr(jdfd2), mmt, b)
                    else if (icoude.eq.1) then
!               FI = FI - OMEGA
                        cosfi = cos(fi)
                        sinfi = sin(fi)
                        l = theta* (rayon+r*sinfi)
                        call bcoudc(igau, icou, isect, h, a,&
                                    m, omega, xpg, nno, nbcou,&
                                    nbsec, zr(ivf), zr(idfdk), zr(jdfd2), rayon,&
                                    theta, mmt, b)
                    endif
                    call promat(c, 4, 4, 4, b,&
                                4, 4, nbrddl, mat)
                    iret = 0
                    call prmave(0, mat, 4, 4, nbrddl,&
                                vin, nbrddl, sig, 4, iret)
                    poids = poicou(icou)*poisec(isect)*h*deuxpi/ (4.d0*nbcou*nbsec)*r
                    efg(1) = efg(1) + poids* (sig(1)-sigth(1))
                    efg(2) = efg(2) - poids* (sinfi*sig(4)+cosfi*sig( 3))
                    efg(3) = efg(3) + poids* (sinfi*sig(3)-cosfi*sig( 4))
                    efg(4) = efg(4) - poids*sig(3)*r
                    efg(5) = efg(5) - poids* (sig(1)-sigth(1))*r* cosfi
                    efg(6) = efg(6) + poids* (sig(1)-sigth(1))*r* sinfi
310              continue
320          continue
            do 330,i = 1,6
            fpg(igau,i) = efg(i)
330          continue
340      continue
        if ((nno.eq.3) .and. (npg.eq.3)) then
!      POUR NE PAS SUPPRIMER LA SAVANTE PROGRAMMATION DE PATRICK
            do 360 igau = 1, npg
                do 350 ino = 1, nno
                    if (icoude .eq. 0) then
                        co(igau,ino) = 1.d0
                        si(igau,ino) = 0.d0
                    else
                        co(igau,ino) = cos( (1.d0+xpg(igau))*theta/ 2.d0- tk(ino))
                        si(igau,ino) = sin( (1.d0+xpg(igau))*theta/ 2.d0- tk(ino))
                    endif
350              continue
360          continue
            do 400,ino = 1,nno
            if (ino .eq. 1) then
                ih = 2
                ip = 1
                i1 = 1
                i2 = 3
            else if (ino.eq.2) then
                ih = 1
                ip = 2
                i1 = 3
                i2 = 1
            else
                do 370,i = 1,6
                fno(i) = fpg(2,i)
370              continue
                goto 380
            endif
            cp(1,1) = co(1,ih)*co(1,3) + si(1,ih)*si(1,3)
            cp(1,2) = -co(1,ih)*si(1,3) + si(1,ih)*co(1,3)
            cp(2,1) = -cp(1,2)
            cp(2,2) = cp(1,1)
            cv(1,1) = co(3,ih)*co(3,3) + si(3,ih)*si(3,3)
            cv(1,2) = -co(3,ih)*si(3,3) + si(3,ih)*co(3,3)
            cv(2,1) = -cp(1,2)
            cv(2,2) = cp(1,1)
            alphaf = hk(ih,3)* (co(1,ih)*fpg(1,1)+si(1,ih)*fpg(1, 2)) - hk(ih,3)*hk(3,1)* (cp(1,1&
                     &)*fpg(2,1)+ cp(1,2)* fpg(2,2)) - hk(ih,1)* (co(3,ih)*fpg(3,1)+si(3,ih)*fpg(&
                     & 3,2)) + hk(ih,1)*hk(3,3)* (cv(1,1)*fpg(2,1)+ cv(1,2)* fpg(2,2))
            betaf = hk(ih,3)* (-si(1,ih)*fpg(1,1)+co(1,ih)*fpg(1, 2)) - hk(ih,3)*hk(3,1)* (cp(2,1&
                    &)*fpg(2,1)+ cp(2,2)* fpg(2,2)) - hk(ih,1)* (-si(3,ih)*fpg(3,1)+co(3,ih)* fpg&
                    &(3,2)) + hk(ih,1)*hk(3,3)* (cv(2,1)*fpg(2,1)+ cv(2,2)*fpg(2,2))
            alpham = hk(ih,3)* (co(1,ih)*fpg(1,4)+si(1,ih)*fpg(1, 5)) - hk(ih,3)*hk(3,1)* (cp(1,1&
                     &)*fpg(2,4)+ cp(1,2)* fpg(2,5)) - hk(ih,1)* (co(3,ih)*fpg(3,4)+si(3,ih)*fpg(&
                     & 3,5)) + hk(ih,1)*hk(3,3)* (cv(1,1)*fpg(2,4)+ cv(1,2)* fpg(2,5))
            betam = hk(ih,3)* (-si(1,ih)*fpg(1,4)+co(1,ih)*fpg(1, 5)) - hk(ih,3)*hk(3,1)* (cp(2,1&
                    &)*fpg(2,4)+ cp(2,2)* fpg(2,5)) - hk(ih,1)* (-si(3,ih)*fpg(3,4)+co(3,ih)* fpg&
                    &(3,5)) + hk(ih,1)*hk(3,3)* (cv(2,1)*fpg(2,4)+ cv(2,2)*fpg(2,5))
            cp(1,1) = co(1,ih)*co(1,ip) + si(1,ih)*si(1,ip)
            cp(1,2) = -co(1,ih)*si(1,ip) + si(1,ih)*co(1,ip)
            cp(2,1) = -cp(1,2)
            cp(2,2) = cp(1,1)
            cv(1,1) = co(3,ih)*co(3,ip) + si(3,ih)*si(3,ip)
            cv(1,2) = -co(3,ih)*si(3,ip) + si(3,ih)*co(3,ip)
            cv(2,1) = -cp(1,2)
            cv(2,2) = cp(1,1)
            xa = hk(ip,1)*hk(ih,3)*cp(1,1) - hk(ip,3)*hk(ih,1)*cv( 1,1)
            xb = hk(ip,1)*hk(ih,3)*cp(1,2) - hk(ip,3)*hk(ih,1)*cv( 1,2)
            xc = hk(ip,1)*hk(ih,3)*cp(2,1) - hk(ip,3)*hk(ih,1)*cv( 2,1)
            xd = hk(ip,1)*hk(ih,3)*cp(2,2) - hk(ip,3)*hk(ih,1)*cv( 2,2)
            fno(1) = (xd*alphaf-xb*betaf)/ (xa*xd-xb*xc)
            fno(2) = (-xc*alphaf+xa*betaf)/ (xa*xd-xb*xc)
            fno(3) = (&
                     hk(ih,i2)*fpg(i1,3)-hk(ih,i1)*fpg(i2,3)- fpg(2,3)* (hk(3,i1)*hk(ih,i2)-hk(3,&
                     &i2)*hk(ih, i1)))/ ( hk(1,1)*hk(2,3)-hk(1,3)*hk(2,1)&
                     )
            fno(4) = (xd*alpham-xb*betam)/ (xa*xd-xb*xc)
            fno(5) = (-xc*alpham+xa*betam)/ (xa*xd-xb*xc)
            fno(6) = (&
                     hk(ih,i2)*fpg(i1,6)-hk(ih,i1)*fpg(i2,6)- fpg(2,6)* (hk(3,i1)*hk(ih,i2)-hk(3,&
                     &i2)*hk(ih, i1)))/ ( hk(1,1)*hk(2,3)-hk(1,3)*hk(2,1)&
                     )
380          continue
            do 390,i = 1,6
            vout(6* (ino-1)+i) = fno(i)
390          continue
400          continue
        else
            do 430 ic = 1, 6
                do 410 kp = 1, npg
                    vpg(kp) = fpg(kp,ic)
410              continue
                nnos = 2
                call ppgan2(jgano, 1, 1, vpg, vno)
                do 420 i = 1, nno
                    vout(6* (i-1)+ic) = vno(i)
420              continue
430          continue
        endif
        call jevech('PEFFORR', 'E', jout)
        do 460 j = 1, 6*nno
            zr(jout-1+j) = vout(j)
460      continue
    else
        call utmess('F', 'ELEMENTS4_49', sk=option)
    endif
!
end subroutine

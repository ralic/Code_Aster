subroutine tumass(nomte, nbrddl, mass)
! aslint: disable=W1306
    implicit none
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
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/carcou.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/klg.h"
#include "asterfort/klgcou.h"
#include "asterfort/moytem.h"
#include "asterfort/promat.h"
#include "asterfort/rcvala.h"
    character(len=16) :: nomte
    integer :: nbrddl
    real(kind=8) :: mass(nbrddl, nbrddl)
! ......................................................................
!
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          TUYAU
!                          OPTION :  MASS_MECA
!    - ARGUMENTS:
!        DONNEES:      NVEC, TNVEC, MASS1,MASS,K : MATRICES
!                      DIMENSIONNEES PAR LE TE0582 APPELANT
!
! ......................................................................
!
!
!
!     VARIABLES LOCALES
    integer :: nbres, icoude, nbsecm, nbcoum, nspg
    parameter (nbres=9)
    character(len=8) :: nomres(nbres), nompar
    integer :: icodre(nbres)
    parameter (nbsecm=32,nbcoum=10)
    real(kind=8) :: poicou(2*nbcoum+1), poisec(2*nbsecm+1)
    real(kind=8) :: valres(nbres), valpar, theta
    real(kind=8) :: rho, h, a, l
    real(kind=8) :: pi, deuxpi, fi, sinfi, cosfi, sinmfi, cosmfi, hk
    real(kind=8) :: poids, r, rayon, xpg(4)
    real(kind=8) :: pgl(3, 3), ck, sk, pgl4(3, 3)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), omega, tk(4)
    integer :: nno, npg, nbcou, nbsec, m, ino, i1, n
    integer :: ipoids, ivf, ibloc, icolon
    integer :: imate, icagep, igeom, nbpar, icoud2, mmt
    integer :: igau, icou, isect, i, j, lorien
    integer :: jcoopg, jnbspi, iret
    integer :: ndim, nnos, idfdk, jdfd2, jgano
    real(kind=8) :: nvec(6, nbrddl), tnvec(nbrddl, 6)
    real(kind=8) :: mass1(nbrddl, nbrddl)
! --------------------------------------------------------------------
    call elrefe_info(fami='MASS',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jcoopg=jcoopg,jvf=ivf,jdfde=idfdk,&
  jdfd2=jdfd2,jgano=jgano)
!
!
    pi = r8pi()
    deuxpi = 2.d0*pi
!
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
!
!
    m = 3
    if (nomte .eq. 'MET6SEG3') m = 6
!
!
!
    do 30 i = 1, npg
        xpg(i) = zr(jcoopg-1+i)
30  end do
!
! A= RMOY, H = EPAISSEUR
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCAGEPO', 'L', icagep)
    h = zr(icagep+1)
    a = zr(icagep) - h/2.d0
!
!     --- RECUPERATION DES ORIENTATIONS ---
!
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
!
    call jevech('PMATERC', 'L', imate)
!
!       -- CALCUL DES TEMPERATURES INF, SUP ET MOY
!          (MOYENNE DES NNO NOEUDS) ET DES COEF. DES POLY. DE DEGRE 2 :
!          ------------------------------------------------------------
    nspg=(2*nbsec + 1)*(2*nbcou + 1)
    iret=0
    call moytem('RIGI', npg, nspg, '+', valpar,&
                iret)
    if (iret .ne. 0) valpar=0.d0
    nbpar = 1
    nompar = 'TEMP'
!
! ======   OPTION MASS_MECA    =======
!
    nomres(1) = 'RHO'
    call rcvala(zi(imate), ' ', 'ELAS', nbpar, nompar,&
                [valpar], 1, nomres, valres, icodre,1)
    rho = valres(1)
    do 70 i = 1, nbrddl
        do 50 j = 1, nbrddl
            mass(i,j) = 0.d0
50      continue
        do 60 j = 1, 6
            nvec(j,i) = 0.d0
            tnvec(i,j) = 0.d0
60      continue
70  end do
!
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
!
! BOUCLE SUR LES POINTS DE GAUSS
!
    do 150 igau = 1, npg
!
! BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
!
        do 140 icou = 1, 2*nbcou + 1
! CALCUL DU RAYON DU POINT ICOU ( A= RMOY, H = EPAISSEUR)
            if (mmt .eq. 0) then
                r = a
            else
                r = a + (icou-1)*h/ (2.d0*nbcou) - h/2.d0
            endif
!
! BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
!
            do 130 isect = 1, 2*nbsec + 1
! CALCUL DE L'ANGLE FI DU POINT ISECT
                fi = (isect-1)*deuxpi/ (2.d0*nbsec)
!                        IF(ICOUDE.EQ.1) FI = FI - OMEGA
                cosfi = cos(fi)
                sinfi = sin(fi)
                do 100 ino = 1, nno
                    hk = zr(ivf-1+nno* (igau-1)+ino)
                    if (icoude .eq. 1) then
                        ck = cos((1.d0+xpg(igau))*theta/2.d0-tk(ino))
                        sk = sin((1.d0+xpg(igau))*theta/2.d0-tk(ino))
                    else
                        ck = 1.d0
                        sk = 0.d0
                    endif
!
                    ibloc = (9+6* (m-1))* (ino-1)
                    do 80 i1 = 1, 3
                        nvec(i1,ibloc+i1) = hk*ck
                        tnvec(ibloc+i1,i1) = hk*ck
80                  continue
                    nvec(1,ibloc+2) = hk*sk
                    nvec(1,ibloc+4) = hk*r*cosfi*sk
                    nvec(1,ibloc+5) = -hk*r*cosfi*ck
                    nvec(1,ibloc+6) = hk*r*sinfi
                    nvec(2,ibloc+1) = -hk*sk
                    nvec(2,ibloc+4) = hk*r*cosfi*ck
                    nvec(2,ibloc+5) = hk*r*cosfi*sk
                    nvec(3,ibloc+3) = hk
                    nvec(3,ibloc+4) = -hk*r*sinfi*ck
                    nvec(3,ibloc+5) = -hk*r*sinfi*sk
                    tnvec(ibloc+2,1) = hk*sk
                    tnvec(ibloc+4,1) = hk*r*cosfi*sk
                    tnvec(ibloc+5,1) = -hk*r*cosfi*ck
                    tnvec(ibloc+6,1) = hk*r*sinfi
                    tnvec(ibloc+1,2) = -hk*sk
                    tnvec(ibloc+4,2) = hk*r*cosfi*ck
                    tnvec(ibloc+5,2) = hk*r*cosfi*sk
                    tnvec(ibloc+3,3) = hk
                    tnvec(ibloc+4,3) = -hk*r*sinfi*ck
                    tnvec(ibloc+5,3) = -hk*r*sinfi*sk
!
                    do 90 n = 2, m
                        icolon = ibloc + 6 + 6* (n-2)
                        cosmfi = cos(n*fi)
                        sinmfi = sin(n*fi)
                        nvec(4,icolon+1) = hk*cosmfi
                        nvec(4,icolon+4) = hk*sinmfi
                        nvec(5,icolon+2) = hk*sinmfi
                        nvec(5,icolon+5) = hk*cosmfi
                        nvec(6,icolon+3) = hk*cosmfi
                        nvec(6,icolon+6) = hk*sinmfi
!
                        tnvec(icolon+1,4) = hk*cosmfi
                        tnvec(icolon+4,4) = hk*sinmfi
                        tnvec(icolon+2,5) = hk*sinmfi
                        tnvec(icolon+5,5) = hk*cosmfi
                        tnvec(icolon+3,6) = hk*cosmfi
                        tnvec(icolon+6,6) = hk*sinmfi
90                  continue
                    icolon = ibloc + 6* (m-1) + 6
                    nvec(5,icolon+2) = hk*sinfi
                    nvec(5,icolon+3) = -hk*cosfi
                    nvec(6,icolon+1) = hk
                    nvec(6,icolon+2) = hk*cosfi
                    nvec(6,icolon+3) = hk*sinfi
!
                    tnvec(icolon+2,5) = hk*sinfi
                    tnvec(icolon+3,5) = -hk*cosfi
                    tnvec(icolon+1,6) = hk
                    tnvec(icolon+2,6) = hk*cosfi
                    tnvec(icolon+3,6) = hk*sinfi
100              continue
                call promat(tnvec, nbrddl, nbrddl, 6, nvec,&
                            6, 6, nbrddl, mass1)
                if (icoude .eq. 1) l = theta* (rayon+r*sinfi)
                poids = zr(ipoids-1+igau)*poicou(icou)*poisec(isect)* (l/2.d0)*h*deuxpi/ (4.d0*nb&
                        &cou*nbsec)*r*rho
                do 120 i = 1, nbrddl
                    do 110 j = 1, i
                        mass(i,j) = mass(i,j) + poids*mass1(i,j)
                        mass(j,i) = mass(i,j)
110                  continue
120              continue
130          continue
140      continue
150  end do
!
    if (icoude .eq. 0) then
        call klg(nno, nbrddl, pgl, mass)
    else if (icoude.eq.1) then
        call klgcou(nno, nbrddl, pgl1, pgl2, pgl3,&
                    pgl4, mass)
    endif
!
end subroutine

subroutine tutemp(option, nomte, nbrddl, f, b,&
                  vout, pass, vtemp)
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
#include "asterfort/bcoudc.h"
#include "asterfort/bcoude.h"
#include "asterfort/carcou.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/moytem.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/verifg.h"
#include "asterfort/vlggl.h"
#include "asterfort/vlgglc.h"
    character(len=16) :: option
! ......................................................................
!
!    - FONCTION REALISEE:  CALCUL DU SECOND MEMBRE : TRAVAIL DE LA
!                          DILATATION THERMIQUE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
! ......................................................................
!
    integer :: nbrddl, nbsecm, nbcoum
    parameter (nbsecm=32,nbcoum=10)
    real(kind=8) :: h, a, l, valpar, beta, r
    real(kind=8) :: poicou(2*nbcoum+1), poisec(2*nbsecm+1)
    real(kind=8) :: f(nbrddl), b(4, nbrddl), vout(nbrddl), sig(4)
    real(kind=8) :: pi, deuxpi, fi, e, nu, valres(3)
    real(kind=8) :: pgl(3, 3), pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), omega
    real(kind=8) :: c(2, 2), coe1(1), pgl4(3, 3)
    real(kind=8) :: poids, rayon, theta, sinfi, xpg(4)
    real(kind=8) :: vtemp(nbrddl), pass(nbrddl, nbrddl)
    integer :: codres(3)
    character(len=8) :: nomres(3), nompar
    character(len=16) :: phenom, nomte
    integer :: nno, npg, nbcou, nbsec, m, lorien, icoude, i
    integer :: ipoids, ivf, icou, nbpar
    integer :: icagep, igeom, jout, imate, j
    integer :: igau, isect, icoud2, mmt, nspg
    integer :: jnbspi, iret, iret2
    integer :: ndim, nnos, jcoopg, idfdk, jdfd2, jgano
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jcoopg=jcoopg,jvf=ivf,jdfde=idfdk,&
  jdfd2=jdfd2,jgano=jgano)
!
    pi = r8pi()
    deuxpi = 2.d0*pi
!
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou = zi(jnbspi-1+1)
    nbsec = zi(jnbspi-1+2)
!
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCAGEPO', 'L', icagep)
    call jevech('PCAORIE', 'L', lorien)
    h = zr(icagep+1)
    a = zr(icagep) - h/2.d0
!
! A= RMOY, H = EPAISSEUR
! RINT = RAYON INTERIEUR
!
!
    m = 3
    if (nomte .eq. 'MET6SEG3') m = 6
!
!
    do 10 i = 1, npg
        xpg(i) = zr(jcoopg-1+i)
10  end do
!
!
!     LES POIDS POUR L'INTEGRATION DANS L'EPAISSEUR
!
    poicou(1) = 1.d0/3.d0
    do 20 i = 1, nbcou - 1
        poicou(2*i) = 4.d0/3.d0
        poicou(2*i+1) = 2.d0/3.d0
20  end do
    poicou(2*nbcou) = 4.d0/3.d0
    poicou(2*nbcou+1) = 1.d0/3.d0
!
!     LES POIDS POUR L'INTEGRATION SUR LA CIRCONFERENCE
!
    poisec(1) = 1.d0/3.d0
    do 30 i = 1, nbsec - 1
        poisec(2*i) = 4.d0/3.d0
        poisec(2*i+1) = 2.d0/3.d0
30  end do
    poisec(2*nbsec) = 4.d0/3.d0
    poisec(2*nbsec+1) = 1.d0/3.d0
!
!     FIN DES POIDS D'INTEGRATION
!
! CALCUL DE L = LONGUEUR DU COUDE
!
    call carcou(zr(lorien), l, pgl, rayon, theta,&
                pgl1, pgl2, pgl3, pgl4, nno,&
                omega, icoud2)
!
    if (icoud2 .ge. 10) then
        icoude = icoud2 - 10
        mmt = 0
    else
        icoude = icoud2
        mmt = 1
    endif
!
!---- RECUPERATION TEMPERATURE
!===============================================================
!          -- RECUPERATION DE LA TEMPERATURE :
!     -- SI LA TEMPERATURE N'EST PAS DONNEE:
    nspg=(2*nbsec + 1)*(2*nbcou + 1)
    iret2=0
    call moytem('RIGI', npg, nspg, '+', valpar,&
                iret2)
    nbpar = 1
    nompar = 'TEMP'
!===============================================================
!
!---- RECUPERATION DU COMPORTEMENT
!
    call jevech('PMATERC', 'L', imate)
    call rccoma(zi(imate), 'ELAS', 1, phenom, codres(1))
    nomres(1) = 'E'
    nomres(2) = 'NU'
!
    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                ' ', phenom, nbpar, nompar, [valpar],&
                2, nomres, valres, codres, 1)
    e = valres(1)
    nu = valres(2)
!
!
! DEFINITION DE LA MATRICE DE COMPORTEMENT C
! POUR LA DILATATION
!
    beta = 1.d0/ (1.d0-nu**2)
!
    c(1,1) = e*beta
    c(1,2) = e*nu*beta
!
    c(2,1) = e*nu*beta
    c(2,2) = e*beta
!
!
!     FIN DE LA CONSTRUCTION DE LA MATRICE DE COMPORTEMENT C
!
    if (option .eq. 'CHAR_MECA_TEMP_R') then
!
!----- CAS DILATATION THERMIQUE
!
!===============================================================
!
!
!
        do 80,i = 1,nbrddl
        f(i) = 0.d0
80      continue
!
!     DEBUT CONSTRUCTION DE B
!
! BOUCLE SUR LES POINTS DE GAUSS
        nspg=(2*nbsec + 1)*(2*nbcou + 1)
        do 130 igau = 1, npg
! ATTENTION IRET NON INITIALISE PAR VERIFG
            iret=0
            call verifg('RIGI', igau, nspg, '+', zi(imate),&
                        'ELAS', 1, coe1, iret)
            if (iret .ne. 0) coe1(1)=0.d0
            sig(1) = (c(1,1)+c(1,2))*coe1(1)
            sig(2) = (c(2,1)+c(2,2))*coe1(1)
            sig(3) = 0.d0
            sig(4) = 0.d0
!
! BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
!
            do 120 icou = 1, 2*nbcou + 1
                if (mmt .eq. 0) then
                    r = a
                else
                    r = a + (icou-1)*h/ (2.d0*nbcou) - h/2.d0
                endif
!
!
! BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
!
                do 110 isect = 1, 2*nbsec + 1
                    if (icoude .eq. 0) then
                        call bcoude(igau, icou, isect, l, h,&
                                    a, m, nno, nbcou, nbsec,&
                                    zr(ivf), zr(idfdk), zr(jdfd2), mmt, b)
                    else if (icoude.eq.1) then
                        fi = (isect-1)*deuxpi/ (2.d0*nbsec)
!
                        sinfi = sin(fi)
                        l = theta* (rayon+r*sinfi)
                        call bcoudc(igau, icou, isect, h, a,&
                                    m, omega, xpg, nno, nbcou,&
                                    nbsec, zr(ivf), zr(idfdk), zr(jdfd2), rayon,&
                                    theta, mmt, b)
                    endif
!
                    do 90 j = 1, nbrddl
                        vout(j) = b(1,j)*sig(1) + b(2,j)*sig(2)
90                  continue
!
!
!  STOCKAGE DU VECTEUR VOUT DANS FI
!
                    poids = zr(ipoids-1+igau)*poicou(icou)*poisec( isect)* (l/2.d0)*h*deuxpi/ (4.&
                            &d0*nbcou*nbsec)*r
!
                    do 100 i = 1, nbrddl
                        f(i) = f(i) + vout(i)*poids
100                  continue
!
!  FIN STOCKAGE
!
110              continue
120          continue
130      continue
        if (icoude .eq. 0) then
            call vlggl(nno, nbrddl, pgl, f, 'LG',&
                       pass, vtemp)
        else
            call vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                        pgl4, f, 'LG', pass, vtemp)
        endif
        call jevech('PVECTUR', 'E', jout)
        do 140,i = 1,nbrddl
        zr(jout-1+i) = f(i)
140      continue
    endif
end subroutine

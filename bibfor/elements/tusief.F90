subroutine tusief(option, nomte, nbrddl, b, vin,&
                  mat, pass, vtemp)
    implicit none
! ----------------------------------------------------------------------
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
#include "asterfort/u2mesk.h"
#include "asterfort/verifg.h"
#include "asterfort/vlggl.h"
#include "asterfort/vlgglc.h"
    character(len=16) :: option, nomte
! ......................................................................
!
!   FONCTION REALISEE:  CALCUL DES OPTIONS EPSI_ELGA,
!                                          DEGE_ELGA,
!                                          DEGE_ELNO,
!                                          SIEF_ELGA POUR UN TUYAU
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nbres, nbrddl
    parameter (nbres=9)
    character(len=8) :: nomres(nbres), nompar
    integer :: icodre(nbres)
    real(kind=8) :: valres(nbres), valpar, degg(24)
    real(kind=8) :: h, a, l, e, nu, beta, cisail, g, omega, dhk
    real(kind=8) :: sinfi, fi, deuxpi, r, at1, at2, vpg(4), vno(4)
    real(kind=8) :: b(4, nbrddl), c(4, 4), epsthe, hk, sigth(2), xpg(4)
    real(kind=8) :: pgl(3, 3), vin(nbrddl), vout(4), mat(4, nbrddl)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), rayon, theta
    real(kind=8) :: vtemp(nbrddl), pass(nbrddl, nbrddl), pgl4(3, 3)
    integer :: nno, npg, nbcou, nbsec, icoude, ndim, jcoopg, nspg
    integer :: ipoids, ivf, kpgs, k, nnos
    integer :: imate, icagep, nbpar
    integer :: igau, icou, isect, i, j, jin, jout, iret, indice
    integer :: lorien, icoud2, mmt, jnbspi
    integer :: nddl, m, idfdk, jdfd2, jgano
!
!
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, jcoopg, ivf, idfdk,&
                jdfd2, jgano)
!
    deuxpi = 2.d0*r8pi()
!
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou = zi(jnbspi-1+1)
    nbsec = zi(jnbspi-1+2)
!
    call jevech('PCAGEPO', 'L', icagep)
!     H = EPAISSEUR, A= RMOY
    h = zr(icagep+1)
    a = zr(icagep) - h/2.d0
!
    m = 3
    if (nomte .eq. 'MET6SEG3') m = 6
!
    do 10 i = 1, npg
        xpg(i) = zr(jcoopg-1+i)
10  end do
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
    if (option .eq. 'SIEF_ELGA') then
!
        call jevech('PMATERC', 'L', imate)
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nomres(3) = 'ALPHA'
        nspg=(2*nbsec + 1)*(2*nbcou + 1)
        iret=0
        call moytem('RIGI', npg, nspg, '+', valpar,&
                    iret)
        if (iret .ne. 0) valpar=0.d0
        nbpar = 1
        nompar = 'TEMP'
        call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                    ' ', 'ELAS', nbpar, nompar, valpar,&
                    2, nomres, valres, icodre, 1)
        e = valres(1)
        nu = valres(2)
!
!        DEFINITION DE LA MATRICE DE COMPORTEMENT C
!
        beta = e/ (1.d0-nu**2)
        g = e/ (2.d0* (1.d0+nu))
        cisail = 1.d0
!
        c(1,1) = beta
        c(1,2) = nu*beta
        c(1,3) = 0.d0
        c(1,4) = 0.d0
!
        c(2,1) = nu*beta
        c(2,2) = beta
        c(2,3) = 0.d0
        c(2,4) = 0.d0
!
        c(3,1) = 0.d0
        c(3,2) = 0.d0
        c(3,3) = g
        c(3,4) = 0.d0
!
        c(4,1) = 0.d0
        c(4,2) = 0.d0
        c(4,3) = 0.d0
        c(4,4) = g*cisail
!
!        FIN DE LA CONSTRUCTION DE LA MATRICE DE COMPORTEMENT C
    endif
!
    call jevech('PDEPLAR', 'L', jin)
    do 20 i = 1, nbrddl
        vin(i) = zr(jin-1+i)
20  end do
    if (icoude .eq. 0) then
        call vlggl(nno, nbrddl, pgl, vin, 'GL',&
                   pass, vtemp)
    else
        call vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                    pgl4, vin, 'GL', pass, vtemp)
    endif
!
    if (option .eq. 'EPSI_ELGA') then
        call jevech('PDEFOPG', 'E', jout)
!
    else if (option.eq.'DEGE_ELNO'.or. option.eq.'DEGE_ELGA') then
!
        if (option .eq. 'DEGE_ELGA') call jevech('PDEFOPG', 'E', jout)
        if (option .eq. 'DEGE_ELNO') call jevech('PDEFOGR', 'E', jout)
        call r8inir(24, 0.d0, degg, 1)
        nbrddl = nno* (6+3+6* (m-1))
        nddl = (6+3+6* (m-1))
!
    else if (option.eq.'SIEF_ELGA') then
        call jevech('PCONTRR', 'E', jout)
!
    else
        call u2mesk('F', 'ELEMENTS4_49', 1, option)
!
    endif
!
    kpgs = 0
    sigth(1) = 0.d0
    sigth(2) = 0.d0
    nspg=(2*nbsec + 1)*(2*nbcou + 1)
!
! --- BOUCLE SUR LES POINTS DE GAUSS
! ---- BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
    do 120 igau = 1, npg
        if (option .eq. 'SIEF_ELGA') then
!         ATTENTION IRET NON INITIALISE PAR VERIFG
            iret=0
            call verifg('RIGI', igau, nspg, '+', zi(imate),&
                        'ELAS', 1, epsthe, iret)
            if (iret .ne. 0) epsthe=0.d0
            at1 = (c(1,1)+c(1,2))*epsthe
            at2 = (c(2,1)+c(2,2))*epsthe
        endif
!
        if ((option.eq.'SIEF_ELGA') .or. (option.eq.'EPSI_ELGA')) then
! --- BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
            do 100 icou = 1, 2*nbcou + 1
!
                if (mmt .eq. 0) then
                    r = a
                else
                    r = a + (icou-1)*h/ (2.d0*nbcou) - h/2.d0
                endif
!
                do 90 isect = 1, 2*nbsec + 1
                    kpgs = kpgs + 1
!
                    if (icoude .eq. 0) then
                        call bcoude(igau, icou, isect, l, h,&
                                    a, m, nno, nbcou, nbsec,&
                                    zr(ivf), zr(idfdk), zr(jdfd2), mmt, b)
                    else if (icoude.eq.1) then
                        fi = (isect-1)*deuxpi/ (2.d0*nbsec)
!                  FI = FI - OMEGA
                        sinfi = sin(fi)
                        l = theta* (rayon+r*sinfi)
                        call bcoudc(igau, icou, isect, h, a,&
                                    m, omega, xpg, nno, nbcou,&
                                    nbsec, zr(ivf), zr(idfdk), zr(jdfd2), rayon,&
                                    theta, mmt, b)
                    endif
!
                    if (option .eq. 'EPSI_ELGA') then
                        do 80 i = 1, 4
                            do 70 j = 1, nbrddl
                                mat(i,j) = b(i,j)
70                          continue
80                      continue
                    else if (option.eq.'SIEF_ELGA') then
                        sigth(1) = at1
                        sigth(2) = at2
                        call promat(c, 4, 4, 4, b,&
                                    4, 4, nbrddl, mat)
                    endif
                    iret = 0
                    call prmave(0, mat, 4, 4, nbrddl,&
                                vin, nbrddl, vout, 4, iret)
!
!  STOCKAGE DU VECTEUR VOUT
!
                    indice = jout - 1 + 6* (kpgs-1)
                    zr(indice+1) = vout(1) - sigth(1)
                    zr(indice+2) = vout(2) - sigth(2)
                    zr(indice+3) = 0.d0
                    zr(indice+4) = vout(3)
                    zr(indice+5) = vout(4)
                    zr(indice+6) = 0.d0
!
90              continue
100          continue
!
        else if (option.eq.'DEGE_ELNO'.or.option.eq.'DEGE_ELGA') then
!            DEFORMATIONS GENERALISEES DE POUTRE
!
            do 110 k = 1, nno
                if (icoude .eq. 1) then
                    l = theta*rayon
                endif
                hk = zr(ivf-1+nno* (igau-1)+k)
                dhk = zr(ivf-1+nno*npg+nno* (igau-1)+k)* (2.d0/l)
                dhk = zr(idfdk-1+nno* (igau-1)+k)* (2.d0/l)
!
!           EPXX=DU/DX
                degg(6* (igau-1)+1) = degg( 6* (igau-1)+1) + dhk*vin( nddl* (k-1)+1)
!              GAXY=DV/DX -DRZ
                degg(6* (igau-1)+2) = degg(&
                                      6* (igau-1)+2) + dhk*vin( nddl* (k-1)+2) - hk*vin(nddl* (k-&
                                      &1)+6&
                                      )
!              GAXZ=DW/DX +DRY
                degg(6* (igau-1)+3) = degg(&
                                      6* (igau-1)+3) + dhk*vin( nddl* (k-1)+3) + hk*vin(nddl* (k-&
                                      &1)+5&
                                      )
!              GAT=D(DRX)/DX
                degg(6* (igau-1)+4) = degg( 6* (igau-1)+4) + dhk*vin( nddl* (k-1)+4)
!              KY=D(DRY)/DX
                degg(6* (igau-1)+5) = degg( 6* (igau-1)+5) + dhk*vin( nddl* (k-1)+5)
!              KZ=D(DRZ)/DX
                degg(6* (igau-1)+6) = degg( 6* (igau-1)+6) + dhk*vin( nddl* (k-1)+6)
110          continue
!
        endif
!
120  end do
!
!
!
    if (option .eq. 'DEGE_ELNO' .or. option .eq. 'DEGE_ELGA') then
        do 150 i = 1, 6
            do 130 igau = 1, npg
                vpg(igau) = degg(6* (igau-1)+i)
                if (option .eq. 'DEGE_ELGA') zr(jout+6*(igau-1)+i-1)=vpg( igau)
130          continue
            if (option .eq. 'DEGE_ELNO') then
                call ppgan2(jgano, 1, 1, vpg, vno)
                do 140 j = 1, nno
                    zr(jout+6* (j-1)+i-1) = vno(j)
140              continue
            endif
150      continue
    endif
!
end subroutine

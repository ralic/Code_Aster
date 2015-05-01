subroutine dkqrge(nomte, xyzl, pgl, rig)
    implicit  none
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/coqrep.h"
#include "asterfort/cosiro.h"
#include "asterfort/dkqbnl.h"
#include "asterfort/dxefro.h"
#include "asterfort/dxqloc.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gquad4.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
#include "asterfort/matmul.h"
#include "asterfort/prmama.h"
#include "asterc/r8dgrd.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
    real(kind=8) :: xyzl(3, *), pgl(*), rig(*)
    character(len=16) ::  nomte
! ======================================================================
!
!     matrice de rigidite geometrique de l'element de plaque dkq
!     ------------------------------------------------------------------
!     in  nomte  : nom du type element
!     in  xyzl   : coordonnees locales des quatre noeuds
!     in  option : option rigi_meca_ge
!     in  pgl    : matrice de passage global/local
!     out rig    : matrice de rigidite geometrique
!     ------------------------------------------------------------------
!
    integer :: nbsig
    parameter (nbsig=6)
    integer :: nbcon
    parameter (nbcon=8)
!
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: j2, jtab(7), nbcou, iret, jsigm, nbsp, npgh
    integer :: i, ipg, jcoqu, icou, idec, j, ier
    real(kind=8) :: poids
    real(kind=8) :: bnl(2, 12), bnli(12, 2)
    real(kind=8) :: h, epi
    real(kind=8) :: hb, hh, hm, cb, ch, cm
    real(kind=8) :: caraq4(25), jacob(5), qsi, eta
    real(kind=8) :: ctor
    real(kind=8) :: flex(12, 12), memb(64), mefl(96)
    real(kind=8) :: flexi(12, 12)
    real(kind=8) :: effint(32), effgt(32), alpha, beta
    real(kind=8) :: t2ev(4), t2ve(4), c, s
!
! --- contraintes
    real(kind=8) :: sixxb, siyyb, sixyb
    real(kind=8) :: sixxm, siyym, sixym
    real(kind=8) :: sixxh, siyyh, sixyh
    real(kind=8) :: nxx, nxy, nyy, normal(2, 2)
!
! deb ------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jcoopg=icoopg,jvf=ivf,jdfde=idfdx,&
  jdfd2=idfd2,jgano=jgano)
!
!     ----- mise a zero des matrices : flex ,memb et mefl :
!
    call r8inir(144, 0.d0, flex, 1)
    call r8inir(64, 0.d0, memb, 1)
    call r8inir(96, 0.d0, mefl, 1)
!
! ------- epaisseur
!
    call jevech('PCACOQU', 'L', jcoqu)
    h = zr(jcoqu)
    alpha = zr(jcoqu+1) * r8dgrd()
    beta = zr(jcoqu+2) * r8dgrd()
    ctor = zr(jcoqu+3)
!
! ------- nombre des couches
!
    if (nomte .eq. 'MEDKQU4') then
        call jevech('PNBSP_I', 'L', j2)
        nbcou=zi(j2)
    endif
!
!     -- contraintes dans les couches :
!     ----------------------------------
    call tecach('OOO', 'PCONTRR', 'L', iret, nval=7,&
                    itab=jtab)
    jsigm = jtab(1)
    npg = jtab(3)
    nbsp = jtab(7)
    npgh = 3
!
    if (nomte .eq. 'MEDKQU4') then
        ASSERT(nbsp.eq.nbcou*npgh)
        ASSERT(jtab(2).eq.nbsig*npg)
    endif
!
! ---     passage des contraintes dans le repere intrinseque :
!
    if (nomte .eq. 'MEDKQU4') then
        call cosiro(nomte, 'PCONTRR', 'L', 'UI', 'G',&
                    jsigm, 'S')
    else if (nomte.eq.'MEDKQG4') then
        call tecach('OON', 'PCONTRR', 'L', iret, nval=7,&
                    itab=jtab)
        jsigm=jtab(1)
        do 25 i = 1, nbcon*npg
            effgt(i)=zr(jsigm-1+i)
25      continue
        call coqrep(pgl, alpha, beta, t2ev, t2ve,&
                    c, s)
        call dxefro(npg, t2ev, effgt, effint)
    endif
!
!     ----- calcul des grandeurs geometriques sur le quadrangle --------
!
    call gquad4(xyzl, caraq4)
!
    do 10 ipg = 1, npg
        qsi = zr(icoopg-1+ndim*(ipg-1)+1)
        eta = zr(icoopg-1+ndim*(ipg-1)+2)
!
!        ----- calcul du jacobien sur le quadrangle --------------------
!
        call jquad4(xyzl, qsi, eta, jacob)
        poids = zr(ipoids+ipg-1)*jacob(1)
!
! ------- calcul des efforts ed membrane
!
        nxx=0.d0
        nyy=0.d0
        nxy=0.d0
!
        if (nomte .eq. 'MEDKQU4') then
!
!       -- boucle sur les couches :
            hb=-h/2
            do 20,icou=1,nbcou
            idec=((ipg-1)*nbcou+(icou-1))*npgh*nbsig
            epi=h/nbcou
            hm=hb+epi/2.d0
            hh=hm+epi/2.d0
!         -- sixxb, siyyb, ... : contraintes au bas de la couche
            sixxb=zr(jsigm-1+idec+1)
            siyyb=zr(jsigm-1+idec+2)
            sixyb=zr(jsigm-1+idec+4)
!         -- sixxm, siyym, ... : contraintes au milieu de la couche
            sixxm=zr(jsigm-1+idec+1+nbsig)
            siyym=zr(jsigm-1+idec+2+nbsig)
            sixym=zr(jsigm-1+idec+4+nbsig)
!         -- sixxh, siyyh, ... : contraintes en haut de la couche
            sixxh=zr(jsigm-1+idec+1+2*nbsig)
            siyyh=zr(jsigm-1+idec+2+2*nbsig)
            sixyh=zr(jsigm-1+idec+4+2*nbsig)
!         -- on integre dans l'epaisseur de chaque couche
!            avec une forrmule de newton-cotes a 3 points
!            les coefficients sont 1/6, 4/6 et 1/6
            cb=epi/6
            cm=4.d0*epi/6
            ch=epi/6
!         -- nxx, nyy, nxy = somme de sixx, siyy, sixy :
            nxx=nxx+cb*sixxb+cm*sixxm+ch*sixxh
            nyy=nyy+cb*siyyb+cm*siyym+ch*siyyh
            nxy=nxy+cb*sixyb+cm*sixym+ch*sixyh
!         -- mise a jour de hb pour la couche suivante :
            hb=hb+epi
!
! --- fin de la boucle sur les couches
!
20          continue
!
        else if (nomte.eq.'MEDKQG4') then
            nxx = effint((ipg-1)*nbcon+1)
            nyy = effint((ipg-1)*nbcon+2)
            nxy = effint((ipg-1)*nbcon+3)
        endif
!
        normal(1,1)=nxx * poids
        normal(2,2)=nyy * poids
        normal(1,2)=nxy * poids
        normal(2,1)=normal(1,2)
!
! --- calcul de la matrice bnl deformations non-lineaires de membrane
!
        call dkqbnl(qsi, eta, jacob(2), bnl)
!
        call prmama(3, bnl, 2, 2, 12,&
                    normal, 2, 2, 2, bnli,&
                    12, 12, 2, ier)
        call matmul(bnli, bnl, 12, 2, 12,&
                    flexi)
!
! --- calcul de la matrice de rigidite geometrique
!
        do 30 i = 1, 12
            do 35 j = 1, 12
                flex(i,j)=flex(i,j)+flexi(i,j)
35          continue
30      continue
!
! --- fin de la boucle sur le points d'integration
!
10  continue
!
    call dxqloc(flex, memb, mefl, ctor, rig)
!
end

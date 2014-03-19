subroutine te0478(option, nomte)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/ppga1d.h"
#include "asterfort/tecach.h"
#include "asterfort/utpvlg.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!
!     CALCUL DES COORDONNEES DES POINTS DE GAUSS + POIDS
!     POUR LES ELEMENTS 0D ET 1D (POI ET SEG)
!
!     TRAITEMENT SPECIFIQUE POUR LES ELEMENTS A SOUS POINTS
!     (PMF, TUYAU, COQUE(2D))
!
! ----------------------------------------------------------------------
    integer :: ndim, nno, nnos, npg, jgano, icopg, idfde, ipoids, ivf, igeom
    integer :: tab(2), iret, ndim1, igepo
    integer :: inbf, nbfib, jacf, iorien, nbsp, nbcou, nbsec
    integer :: ncarfi, isec, icou, isp, icoq
    integer :: ig, ifi, k, i
    real(kind=8) :: copg(4, 4), copg2(3, 4), pgl(3, 3), gm1(3), gm2(3), airefb
    real(kind=8) :: epcou, alpha, rayon, ep, y, z, hh, r
    real(kind=8) :: dfdx(3), cour, jacp, cosa, sina, spoid
! ----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim1,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    ASSERT(npg.le.4)
!
!     NDIM1 EST LA DIMENSION TOPOLOGIQUE. IL FAUT CALCULER LA
!     DIMENSION DE L'ESPACE NDIM (2 OU 3) :
    call tecach('OOO', 'PGEOMER', 'L', iret, nval=2,&
                itab=tab)
    ndim = tab(2)/nno
    igeom = tab(1)
!
!     ZR(ICOPG) : COORDONNEES POINTS DE GAUSS + POIDS
    call jevech('PCOORPG', 'E', icopg)
!
! == POUTRES MULTIFIBRES ==
!
    if (nomte .eq. 'MECA_POU_D_EM' .or. nomte .eq. 'MECA_POU_D_TGM') then
        call jevech('PNBSP_I', 'L', inbf)
        nbfib = zi(inbf)
        call jevech('PFIBRES', 'L', jacf)
        ncarfi = 3
        call jevech('PCAORIE', 'L', iorien)
        call matrot(zr(iorien), pgl)
!
!       POSITION ET POIDS DES POINTS DE GAUSS
        call ppga1d(ndim, nno, npg, zr(ipoids), zr(ivf),&
                    zr(idfde), zr( igeom), copg)
!
        gm1(1)=0.d0
!       BOUCLE SUR LES FIBRES (4 VALEURS PAR FIBRE, X,Y,Z,W)
        do 100 ifi = 1, nbfib
            gm1(2)=zr(jacf+(ifi-1)*ncarfi)
            gm1(3)=zr(jacf+(ifi-1)*ncarfi+1)
            call utpvlg(1, 3, pgl, gm1, gm2)
            airefb=zr(jacf+(ifi-1)*ncarfi+2)
!
            do 110 ig = 1, npg
                zr(icopg-1+4*nbfib*(ig-1)+4*(ifi-1)+1)=copg(1,ig)+gm2(&
                1)
                zr(icopg-1+4*nbfib*(ig-1)+4*(ifi-1)+2)=copg(2,ig)+gm2(&
                2)
                zr(icopg-1+4*nbfib*(ig-1)+4*(ifi-1)+3)=copg(3,ig)+gm2(&
                3)
!           POUR LE POIDS, ON MULTIPLIE PAR L'AIRE DES FIBRES
                zr(icopg-1+4*nbfib*(ig-1)+4*(ifi-1)+4)=copg(4,ig)*&
                airefb
110          continue
100      continue
!
! == TUYAUX ==
!
        elseif((nomte.eq.'MET3SEG3').or. (nomte.eq.'MET3SEG4')&
    .or.(nomte.eq.'MET6SEG3')) then
!       NOMBRE DE COUCHES ET NOMBRE DE SECTIONS
        call jevech('PNBSP_I', 'L', inbf)
        nbcou = zi(inbf )
        nbsec = 2*zi(inbf+1)+1
!       NOMBRE DE SOUS POINTS PAR POINT DE GAUSS
        nbsp= nbsec*(2*nbcou+1)
!       RAYON ET EPAISSEUR DU TUYAUX
        call jevech('PCAGEPO', 'L', igepo)
        rayon = zr(igepo)
        ep = zr(igepo+1)
        epcou = ep/nbcou
        call jevech('PCAORIE', 'L', iorien)
!
!       POSITION ET POIDS DES POINTS DE GAUSS
        call ppga1d(ndim, nno, npg, zr(ipoids), zr(ivf),&
                    zr(idfde), zr( igeom), copg)
!
        gm1(1)=0.d0
        do 20 ig = 1, npg
            call matrot(zr(iorien+3*(ig-1)), pgl)
!         CALCUL DES COORDONNEES ET STOCKAGE
!         LES SOUS POINTS SONT STOCKES NIVEAU PAR NIVEAU
!         (IL Y A PLUSIEURS NIVEAUX PAR COUCHE)
!         EN COMMENCANT PAR LA SECTION Z LOCAL = 0 ET Y >0
            do 30 isec = 0, nbsec-1
                alpha=2.d0*r8pi()/(nbsec-1)
                y=cos(-isec*alpha)
                z=sin(-isec*alpha)
                do 40 icou = 1, 2*nbcou+1
                    gm1(2)=(rayon-ep+(icou-1)*epcou/2)*y
                    gm1(3)=(rayon-ep+(icou-1)*epcou/2)*z
                    call utpvlg(1, 3, pgl, gm1, gm2)
!
                    zr(icopg+4*(ig-1)*nbsp+4*(icou-1)*nbsec+isec*4+0)=&
                    copg(1,ig)+gm2(1)
                    zr(icopg+4*(ig-1)*nbsp+4*(icou-1)*nbsec+isec*4+1)=&
                    copg(2,ig)+gm2(2)
                    zr(icopg+4*(ig-1)*nbsp+4*(icou-1)*nbsec+isec*4+2)=&
                    copg(3,ig)+gm2(3)
!              ON LAISSE LE POIDS A 0
                    zr(icopg+4*(ig-1)*nbsp+4*(icou-1)*nbsec+isec*4+3)=&
                    0.d0
!
40              continue
30          continue
20      continue
!
! == COQUE(2D) ==
!
        elseif((nomte.eq.'METCSE3').or. (nomte.eq.'MECXSE3')&
    .or.(nomte.eq.'METDSE3')) then
!
        ASSERT(ndim.eq.2)
!
        call jevech('PNBSP_I', 'L', inbf)
        nbcou=zi(inbf)
        call jevech('PCACOQU', 'L', icoq)
        ep=zr(icoq)
        epcou=ep/nbcou
!
        call ppga1d(ndim, nno, npg, zr(ipoids), zr(ivf),&
                    zr(idfde), zr( igeom), copg2)
!
        do 50 ig = 1, npg
!       CALCUL DU VECTEUR NORMAL UNITAIRE AU POINT DE GAUSS
            k = (ig-1)*nno
            call dfdm1d(nno, zr(ipoids+ig-1), zr(idfde+k), zr(igeom), dfdx,&
                        cour, jacp, cosa, sina)
            if (nomte .eq. 'MECXSE3') then
                r = 0.d0
                do 10 i = 1, nno
                    r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
10              continue
                jacp = jacp*r
            endif
            gm2(1)=cosa
            gm2(2)=sina
!
            do 60 icou = 1, nbcou
                do 70 isp = 1, 3
                    hh=-ep/2+(icou-1+0.5d0*(isp-1))*epcou
                    zr(icopg+(ig-1)*9*nbcou+(icou-1)*9+(isp-1)*3+0)=&
                    copg2(1,ig)+hh*gm2(1)
                    zr(icopg+(ig-1)*9*nbcou+(icou-1)*9+(isp-1)*3+1)=&
                    copg2(2,ig)+hh*gm2(2)
                    if (isp .eq. 2) then
                        spoid=2.0d0/3
                    else
                        spoid=1.0d0/6
                    endif
!             POUR LE POIDS, ON MULTIPLIE PAR L'EPAISSEUR PAR COUCHE
                    zr(icopg+(ig-1)*9*nbcou+(icou-1)*9+(isp-1)*3+2)=&
                    jacp*spoid*epcou
70              continue
60          continue
50      end do
!
! ==  AUTRES ELEMENTS
!
    else
        call ppga1d(ndim, nno, npg, zr(ipoids), zr(ivf),&
                    zr(idfde), zr( igeom), zr(icopg))
    endif
!
end subroutine

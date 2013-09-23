subroutine dsqmas(xyzl, option, pgl, mas, ener)
    implicit none
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/dialum.h"
#include "asterfort/dsqcis.h"
#include "asterfort/dsqdi2.h"
#include "asterfort/dsqdis.h"
#include "asterfort/dsqnib.h"
#include "asterfort/dsqniw.h"
#include "asterfort/dsxhft.h"
#include "asterfort/dxhmft.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxqloc.h"
#include "asterfort/dxqloe.h"
#include "asterfort/dxqnim.h"
#include "asterfort/dxroep.h"
#include "asterfort/elref5.h"
#include "asterfort/gquad4.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
    real(kind=8) :: xyzl(3, *), pgl(*), mas(*), ener(*)
    character(len=16) :: option
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
!     MATRICE MASSE DE L'ELEMENT DE PLAQUE DSQ
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
!     OUT MAS    : MATRICE DE RIGIDITE
!     OUT ENER   : TERMES POUR ENER_CIN (ECIN_ELEM)
!     ------------------------------------------------------------------
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: i, j, k, i1, i2, int, ii(8), jj(8), ll(16)
    integer :: multic, p, jdepg, jcoqu, j1, j2, jvitg, iret
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2), dfc(3, 2)
    real(kind=8) :: hft2(2, 6), hmft2(2, 6), flex(12, 12)
    real(kind=8) :: bcb(2, 12), bca(2, 4), an(4, 12), am(4, 8), bcm(2, 8)
    real(kind=8) :: nfx(12), nfy(12), nmx(8), nmy(8), nmi(4)
    real(kind=8) :: memb(8, 8), mefl(8, 12), amemb(64)
    real(kind=8) :: wsq(12), wmesq(8), depl(24), vite(24)
    real(kind=8) :: masloc(300), masglo(300), rof, wgtmf
    real(kind=8) :: zero, unquar, undemi, un, neuf, douze, excent, xinert
    real(kind=8) :: coefm, wgtf, wgtm, detj, wgt, roe, rho, epais
    real(kind=8) :: qsi, eta, jacob(5), caraq4(25), t2iu(4), t2ui(4), t1ve(9)
    character(len=3) :: stopz
    logical :: coupmf, exce, iner
!     ------------------------------------------------------------------
    real(kind=8) :: ctor
    data (ii(k),k=1,8)/1,10,19,28,37,46,55,64/
    data (jj(k),k=1,8)/5,14,23,32,33,42,51,60/
    data (ll(k),k=1,16)/3,7,12,16,17,21,26,30,35,39,44,48,49,53,58,62/
!     ------------------------------------------------------------------
!
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, icoopg, ivf, idfdx,&
                idfd2, jgano)
!
    zero = 0.0d0
    unquar = 0.25d0
    undemi = 0.5d0
    un = 1.0d0
    neuf = 9.0d0
    douze = 12.0d0
!
    excent = zero
!
    call dxroep(rho, epais)
    roe = rho*epais
    rof = rho*epais*epais*epais/douze
!
    call jevech('PCACOQU', 'L', jcoqu)
    ctor = zr(jcoqu+3)
    excent = zr(jcoqu+4)
    xinert = zr(jcoqu+5)
!
    exce = .false.
    iner = .false.
    if (abs(excent) .gt. un/r8gaem()) exce = .true.
    if (abs(xinert) .gt. un/r8gaem()) iner = .true.
    if (.not. iner) rof = 0.0d0
!
! --- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE :
!     ---------------------------------------------------
    call gquad4(xyzl, caraq4)
!
! --- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
! --- MEMBRANE ET CISAILLEMENT INVERSEE :
!     ---------------------------------
    call dxmate('RIGI', df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2iu, t2ui, t1ve)
!
! --- INITIALISATIONS :
!     ---------------
    call r8inir(144, zero, flex, 1)
    call r8inir(96, zero, mefl, 1)
    call r8inir(32, zero, am, 1)
!
! --- CAS AVEC EXCENTREMENT
!     ---------------------
! --- CALCUL DES MATRICES AN ET AM QUI SONT TELLES QUE
! --- ALPHA = AN*UN + AM*UM
! ---  UN DESIGNANT LES DEPLACEMENTS DE FLEXION  UN = (W,BETA_X,BETA_Y)
! ---  UM DESIGNANT LES DEPLACEMENTS DE MEMBRANE UM = (U,V)
!
! --- CAS SANS EXCENTREMENT
!     ---------------------
! --- ON CALCULE SEULEMENT AN QUI EST TEL QUE ALPHA = AN*UN :
!      ----------------------------------------------------
    if (exce) then
        call dsqdi2(xyzl, df, dci, dmf, dfc,&
                    dmc, an, am)
    else
        call dsqdis(xyzl, caraq4, df, dci, an)
    endif
!
!===========================================================
! ---  CALCUL DE LA PARTIE MEMBRANE DE LA MATRICE DE MASSE =
!===========================================================
!
! --- PRISE EN COMPTE DES TERMES DE MEMBRANE CLASSIQUES
! --- EN U*U ET V*V :
!     -------------
    coefm = caraq4(21)*roe/neuf
    do 30 k = 1, 64
        amemb(k) = zero
30  end do
    do 40 k = 1, 8
        amemb(ii(k)) = un
        amemb(jj(k)) = unquar
40  end do
    do 50 k = 1, 16
        amemb(ll(k)) = undemi
50  end do
    do 60 k = 1, 64
        memb(k,1) = coefm*amemb(k)
60  end do
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     ===================================
    do 70 int = 1, npg
        qsi = zr(icoopg-1+ndim*(int-1)+1)
        eta = zr(icoopg-1+ndim*(int-1)+2)
!
! ---   CALCUL DU JACOBIEN SUR LE QUADRANGLE :
!       ------------------------------------
        call jquad4(xyzl, qsi, eta, jacob)
!
! ---   CALCUL DU PRODUIT HF.T2 :
!       -----------------------
        call dsxhft(df, jacob(2), hft2)
!
! ---   CALCUL DU PRODUIT HMF.T2 :
!       ------------------------
        call dxhmft(dmf, jacob(2), hmft2)
!
! ---   CALCUL DES MATRICES BCB, BCA ET BCM :
!       -----------------------------------
        call dsqcis(qsi, eta, caraq4, hmft2, hft2,&
                    bcm, bcb, bca)
!
! ---   CALCUL DES FONCTIONS D'INTERPOLATION DE LA FLECHE :
!       -------------------------------------------------
        call dsqniw(qsi, eta, caraq4, dci, bcm,&
                    bcb, bca, an, am, wsq,&
                    wmesq)
!
! ---   CALCUL DES FONCTIONS D'INTERPOLATION DES ROTATIONS :
!       --------------------------------------------------
        call dsqnib(qsi, eta, caraq4, an, am,&
                    nfx, nfy, nmx, nmy)
!
! ---   CALCUL DES FONCTIONS DE FORME DE MEMBRANE :
!       -----------------------------------------
        call dxqnim(qsi, eta, nmi)
!
!==========================================================
! ---  CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE =
!==========================================================
!
        detj = jacob(1)
!
! ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION W
! ---   EST EGALE A RHO_F = RHO*EPAIS :
!       -----------------------------
        wgt = zr(ipoids+int-1)*detj*roe
!
! ---   CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE
! ---   DUE AUX SEULS TERMES DE LA FLECHE W :
!       -----------------------------------
        do 80 i = 1, 12
            do 90 j = 1, 12
                flex(i,j) = flex(i,j) + wsq(i)*wsq(j)*wgt
90          continue
80      continue
!
! ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION BETA
! ---   EST EGALE A RHO_F = RHO*EPAIS**3/12 + D**2*EPAIS*RHO :
!       ----------------------------------------------------
        wgtf = zr(ipoids+int-1)*detj*(rof+excent*excent*roe)
!
! ---   PRISE EN COMPTE DES TERMES DE FLEXION DUS AUX ROTATIONS :
!       -------------------------------------------------------
        do 100 i = 1, 12
            do 110 j = 1, 12
                flex(i,j) = flex(i,j)+(nfx(i)*nfx(j)+nfy(i)*nfy(j))* wgtf
110          continue
100      continue
!==============================================================
! ---   CAS D'UN ELEMENT EXCENTRE : IL APPARAIT DE TERMES DE  =
! ---   COUPLAGE MEMBRANE-FLEXION ET DE NOUVEAUX TERMES POUR  =
! ---   PARTIE MEMBRANE DE LA MATRICE DE MASSE :              =
!==============================================================
!
        if (exce) then
!
!===================================================================
! ---  CALCUL DE LA PARTIE MEMBRANE-FLEXION DE LA MATRICE DE MASSE =
!===================================================================
!
! ---     POUR LE COUPLAGE MEMBRANE-FLEXION, ON DOIT TENIR COMPTE
! ---     DE 3 MASSES VOLUMIQUES
! ---     RHO_M  = EPAIS*RHO
! ---     RHO_MF = D*EPAIS*RHO
! ---     RHO_F  = RHO*EPAIS**3/12 + D**2*EPAIS*RHO
!         -------------------------------------------------
            wgtm = zr(ipoids+int-1)*detj*roe
            wgtmf = zr(ipoids+int-1)*detj*excent*roe
            wgtf = zr(ipoids+int-1)*detj*(rof+excent*excent*roe)
!
! ---     PRISE EN COMPTE DES TERMES DE COUPLAGE MEMBRANE-FLEXION
! ---     ON A 3 TYPES DE TERMES DONT IL FAUT TENIR COMPTE
! ---     1) LES TERMES U*BETA     -> NMI*NFX ET NMI*NFY (RHO_MF)
! ---     2) LES TERMES W*W        -> WMESQ*WST          (RHO_M)
! ---     3) LES TERMES BETA*BETA  -> NMX*NFX            (RHO_F)
!         ------------------------------------------------------
!
! ---      1) TERMES DE COUPLAGE MEMBRANE-FLEXION U*BETA :
!             ------------------------------------------
            do 120 k = 1, 4
                i1 = 2*(k-1)+1
                i2 = i1 +1
                do 130 j = 1, 12
                    mefl(i1,j) = mefl(i1,j)+nmi(k)*nfx(j)*wgtmf
                    mefl(i2,j) = mefl(i2,j)+nmi(k)*nfy(j)*wgtmf
130              continue
120          continue
! ---      2) TERMES DE COUPLAGE MEMBRANE-FLEXION W*W ET BETA*BETA :
!             ----------------------------------------------------
            do 140 i = 1, 8
                do 150 j = 1, 12
                    mefl(i,j) = mefl(i,j) + wmesq(i)*wsq(j)*wgtm + (nmx(i)*nfx(j) + nmy(i)*nfy(j)&
                                &)*wgtf
150              continue
140          continue
!
!===========================================================
! ---  AJOUT DE NOUVEAUX TERMES A LA PARTIE MEMBRANE       =
! ---  DE LA MATRICE DE MASSE DUS A L'EXCENTREMENT         =
!===========================================================
!
! ---     PRISE EN COMPTE DES TERMES DE MEMBRANE
! ---     ON A 3 TYPES DE TERMES DONT IL FAUT TENIR COMPTE
! ---     1) LES TERMES U*BETA     -> NMI*NMX ET NMI*NMY (RHO_MF)
! ---     2) LES TERMES W*W        -> WMESQ*WMESQ        (RHO_M)
! ---     3) LES TERMES BETA*BETA  -> NMX*NMX            (RHO_F)
!         ------------------------------------------------------
!
! ---      1) TERMES DE MEMBRANE U*BETA :
!             -------------------------
            do 160 k = 1, 4
                i1 = 2*(k-1)+1
                i2 = i1 +1
                do 170 p = 1, 4
                    j1 = 2*(p-1)+1
                    j2 = j1 +1
                    memb(i1,j1) = memb(i1,j1)+ (nmi(k)*nmx(j1)+nmi(p)* nmx(i1))*wgtmf
                    memb(i1,j2) = memb(i1,j2)+ (nmi(k)*nmx(j2)+nmi(p)* nmy(i1))*wgtmf
                    memb(i2,j1) = memb(i2,j1)+ (nmi(k)*nmy(j1)+nmi(p)* nmx(i2))*wgtmf
                    memb(i2,j2) = memb(i2,j2)+ (nmi(k)*nmy(j2)+nmi(p)* nmy(i2))*wgtmf
170              continue
160          continue
! ---      2) TERMES DE MEMBRANE WMESQ*WMESQ ET BETA*BETA :
!           -------------------------------------------
            do 180 i = 1, 8
                do 190 j = 1, 8
                    memb(i,j) = memb(i,j) + wmesq(i)*wmesq(j)*wgtm + (nmx(i)*nmx(j) + nmy(i)*nmy(&
                                &j))*wgtf
190              continue
180          continue
!
        endif
! ---   FIN DU TRAITEMENT DU CAS D'UN ELEMENT EXCENTRE
!       ----------------------------------------------
70  end do
! --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
!     ---------------------------------------------
!
! --- INSERTION DES DIFFERENTES PARTIES CALCULEES DE LA MATRICE
! --- DE MASSE A LA MATRICE ELLE MEME :
!     ===============================
    if (( option .eq. 'MASS_MECA' ) .or. (option.eq.'M_GAMMA')) then
        call dxqloc(flex, memb, mefl, ctor, mas)
!
        else if (option.eq.'MASS_MECA_DIAG' .or.&
     &         option.eq.'MASS_MECA_EXPLI' ) then
        call dxqloc(flex, memb, mefl, ctor, masloc)
        wgt = caraq4(21)*roe
        call utpslg(4, 6, pgl, masloc, masglo)
        call dialum(4, 6, 24, wgt, masglo,&
                    mas)
!
    else if (option.eq.'ECIN_ELEM') then
        stopz='ONO'
! IRET NE PEUT VALOIR QUE 0 (TOUT VA BIEN) OU 2 (CHAMP NON FOURNI)
        call tecach(stopz, 'PVITESR', 'L', iret, iad=jvitg)
        if (iret .eq. 0) then
            call utpvgl(4, 6, pgl, zr(jvitg), vite)
            call dxqloe(flex, memb, mefl, ctor, .false.,&
                        vite, ener)
        else
            call tecach(stopz, 'PDEPLAR', 'L', iret, iad=jdepg)
            if (iret .eq. 0) then
                call utpvgl(4, 6, pgl, zr(jdepg), depl)
                call dxqloe(flex, memb, mefl, ctor, .false.,&
                            depl, ener)
            else
                call utmess('F', 'ELEMENTS2_1', sk=option)
            endif
        endif
    endif
!
end subroutine

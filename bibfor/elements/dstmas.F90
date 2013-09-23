subroutine dstmas(xyzl, option, pgl, mas, ener)
    implicit none
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/dialum.h"
#include "asterfort/dstci2.h"
#include "asterfort/dstcis.h"
#include "asterfort/dstnib.h"
#include "asterfort/dstniw.h"
#include "asterfort/dsxhft.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxroep.h"
#include "asterfort/dxtloc.h"
#include "asterfort/dxtloe.h"
#include "asterfort/dxtnim.h"
#include "asterfort/elref5.h"
#include "asterfort/gtria3.h"
#include "asterfort/jevech.h"
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
!     MATRICE MASSE DE L'ELEMENT DE PLAQUE DST
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
!     OUT MAS    : MATRICE DE RIGIDITE
!     OUT ENER   : TERMES POUR ENER_CIN (ECIN_ELEM)
!     ------------------------------------------------------------------
    integer :: i, j, k, i1, i2, j1, j2, int, p, multic, jcoqu, jdepg
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: jvitg, iret
    real(kind=8) :: nfx(9), nfy(9), nmx(6), nmy(6), nmi(3)
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2), dfc(3, 2)
    real(kind=8) :: hft2(2, 6), bca(2, 3), an(3, 9), am(3, 6)
    real(kind=8) :: flex(9, 9), memb(6, 6), mefl(6, 9)
    real(kind=8) :: wst(9), wmest(6), depl(18), vite(18)
    real(kind=8) :: masloc(171), masglo(171)
    real(kind=8) :: rho, epais, roe, rof, ctor, excent, detj, wgt
    real(kind=8) :: zero, un, six, douze, wgtf, wgtm, wgtmf
    real(kind=8) :: qsi, eta, carat3(21), t2iu(4), t2ui(4), t1ve(9)
    character(len=3) :: stopz
    logical :: coupmf, exce
!     ------------------------------------------------------------------
!
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, icoopg, ivf, idfdx,&
                idfd2, jgano)
!
    zero = 0.0d0
    un = 1.0d0
    six = 6.0d0
    douze = 12.0d0
!
    exce = .false.
    excent = zero
!
    do 10 k = 1, 54
        mefl(k,1) = zero
10  end do
    do 20 k = 1, 81
        flex(k,1) = zero
20  end do
    do 30 k = 1, 36
        memb(k,1) = zero
30  end do
!
    call r8inir(18, zero, am, 1)
!
    call dxroep(rho, epais)
    roe = rho*epais
    rof = rho*epais*epais*epais/douze
!
    call jevech('PCACOQU', 'L', jcoqu)
    ctor = zr(jcoqu+3)
    excent = zr(jcoqu+4)
    if (abs(excent) .gt. un/r8gaem()) exce = .true.
!
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
    call gtria3(xyzl, carat3)
!
!     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
!           MEMBRANE ET CISAILLEMENT INVERSEE --------------------------
    call dxmate('RIGI', df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2iu, t2ui, t1ve)
!
!     -------- CALCUL DU PRODUIT HF.T2 ---------------------------------
    call dsxhft(df, carat3(9), hft2)
!
    if (exce) then
! ---   CALCUL DES MATRICES BCA ,AN ET AM DANS LE CAS DE L'EXCENTREMENT:
!       ---------------------------------------------------------------
        call dstci2(dci, carat3, hft2, dfc, dmc,&
                    bca, an, am)
    else
! ---   CALCUL DES MATRICES BCA ET AN DANS LE CAS NON EXCENTRE :
!       ------------------------------------------------------
        call dstcis(dci, carat3, hft2, bca, an)
    endif
!
    detj = carat3(7)
!
!===========================================================
! ---  CALCUL DE LA PARTIE MEMBRANE DE LA MATRICE DE MASSE =
!===========================================================
!
! --- PRISE EN COMPTE DES TERMES DE MEMBRANE CLASSIQUES
! --- EN U*U ET V*V :
!     -------------
    memb(1,1) = carat3(8)*roe/six
    memb(1,3) = carat3(8)*roe/douze
    memb(1,5) = memb(1,3)
    memb(2,2) = memb(1,1)
    memb(2,4) = memb(1,3)
    memb(2,6) = memb(1,3)
    memb(3,1) = memb(1,3)
    memb(3,3) = memb(1,1)
    memb(3,5) = memb(1,3)
    memb(4,2) = memb(1,3)
    memb(4,4) = memb(1,1)
    memb(4,6) = memb(1,3)
    memb(5,1) = memb(1,3)
    memb(5,3) = memb(1,3)
    memb(5,5) = memb(1,1)
    memb(6,2) = memb(1,3)
    memb(6,4) = memb(1,3)
    memb(6,6) = memb(1,1)
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     ===================================
!
    do 40 int = 1, npg
        qsi = zr(icoopg-1+ndim*(int-1)+1)
        eta = zr(icoopg-1+ndim*(int-1)+2)
!
! ---   CALCUL DES FONCTIONS D'INTERPOLATION DE LA FLECHE :
!       -------------------------------------------------
        call dstniw(qsi, eta, carat3, dci, bca,&
                    an, am, wst, wmest)
!
! ---   CALCUL DES FONCTIONS D'INTERPOLATION DES ROTATIONS :
!       --------------------------------------------------
        call dstnib(qsi, eta, carat3, an, am,&
                    nfx, nfy, nmx, nmy)
!
! ---   CALCUL DES FONCTIONS DE FORME DE MEMBRANE :
!       -----------------------------------------
        call dxtnim(qsi, eta, nmi)
!
! ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION W
! ---   EST EGALE A RHO_F = RHO*EPAIS :
!       -----------------------------
        wgt = zr(ipoids+int-1)*detj*roe
!
!==========================================================
! ---  CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE =
!==========================================================
!
! ---   CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE
! ---   DUE AUX SEULS TERMES DE LA FLECHE W :
!       -----------------------------------
        do 50 i = 1, 9
            do 60 j = 1, 9
                flex(i,j) = flex(i,j) + wst(i)*wst(j)*wgt
60          continue
50      continue
!
! ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION BETA
! ---   EST EGALE A RHO_F = RHO*EPAIS**3/12 + D**2*EPAIS*RHO :
!       ----------------------------------------------------
        wgtf = zr(ipoids+int-1)*detj*(rof+excent*excent*roe)
!
! ---   PRISE EN COMPTE DES TERMES DE FLEXION DUS AUX ROTATIONS :
!       -------------------------------------------------------
        do 70 i = 1, 9
            do 80 j = 1, 9
                flex(i,j) = flex(i,j)+(nfx(i)*nfx(j)+nfy(i)*nfy(j))* wgtf
80          continue
70      continue
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
! ---     2) LES TERMES W*W        -> WMEST*WST          (RHO_M)
! ---     3) LES TERMES BETA*BETA  -> NMX*NFX            (RHO_F)
!         ------------------------------------------------------
!
! ---      1) TERMES DE COUPLAGE MEMBRANE-FLEXION U*BETA :
!             ------------------------------------------
            do 90 k = 1, 3
                do 100 j = 1, 9
                    i1 = 2*(k-1)+1
                    i2 = i1 +1
                    mefl(i1,j) = mefl(i1,j)+nmi(k)*nfx(j)*wgtmf
                    mefl(i2,j) = mefl(i2,j)+nmi(k)*nfy(j)*wgtmf
100              continue
90          continue
! ---      2) TERMES DE COUPLAGE MEMBRANE-FLEXION W*W ET BETA*BETA :
!             ----------------------------------------------------
            do 110 i = 1, 6
                do 120 j = 1, 9
                    mefl(i,j) = mefl(i,j) + wmest(i)*wst(j)*wgtm + (nmx(i)*nfx(j) + nmy(i)*nfy(j)&
                                &)*wgtf
120              continue
110          continue
!
!===========================================================
! ---  AJOUT DE NOUVEAUX TERMES A LA PARTIE MEMBRANE       =
! ---  DE LA MATRICE DE MASSE DUS A L'EXCENTREMENT         =
!===========================================================
!
! ---     PRISE EN COMPTE DES TERMES DE MEMBRANE
! ---     ON A 3 TYPES DE TERMES DONT IL FAUT TENIR COMPTE
! ---     1) LES TERMES U*BETA     -> NMI*NMX ET NMI*NMY (RHO_MF)
! ---     2) LES TERMES W*W        -> WMEST*WMEST        (RHO_M)
! ---     3) LES TERMES BETA*BETA  -> NMX*NMX            (RHO_F)
!         ------------------------------------------------------
!
! ---      1) TERMES DE MEMBRANE U*BETA :
!             -------------------------
            do 130 k = 1, 3
                i1 = 2*(k-1)+1
                i2 = i1 +1
                do 140 p = 1, 3
                    j1 = 2*(p-1)+1
                    j2 = j1 +1
                    memb(i1,j1) = memb(i1,j1)+ (nmi(k)*nmx(j1)+nmi(p)* nmx(i1))*wgtmf
                    memb(i1,j2) = memb(i1,j2)+ (nmi(k)*nmx(j2)+nmi(p)* nmy(i1))*wgtmf
                    memb(i2,j1) = memb(i2,j1)+ (nmi(k)*nmy(j1)+nmi(p)* nmx(i2))*wgtmf
                    memb(i2,j2) = memb(i2,j2)+ (nmi(k)*nmy(j2)+nmi(p)* nmy(i2))*wgtmf
140              continue
130          continue
! ---      2) TERMES DE MEMBRANE WMEST*WMEST ET BETA*BETA :
!             -------------------------------------------
            do 150 i = 1, 6
                do 160 j = 1, 6
                    memb(i,j) = memb(i,j) + wmest(i)*wmest(j)*wgtm + (nmx(i)*nmx(j) + nmy(i)*nmy(&
                                &j))*wgtf
160              continue
150          continue
!
        endif
! ---   FIN DU TRAITEMENT DU CAS D'UN ELEMENT EXCENTRE
!       ----------------------------------------------
40  end do
! --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
!     ---------------------------------------------
!
! --- INSERTION DES DIFFERENTES PARTIES CALCULEES DE LA MATRICE
! --- DE MASSE A LA MATRICE ELLE MEME :
!     ===============================
    if (( option .eq. 'MASS_MECA' ) .or. (option.eq.'M_GAMMA')) then
        call dxtloc(flex, memb, mefl, ctor, mas)
!
        else if (option.eq.'MASS_MECA_DIAG' .or.&
     &         option.eq.'MASS_MECA_EXPLI' ) then
        call dxtloc(flex, memb, mefl, ctor, masloc)
        wgt = carat3(8)*roe
        call utpslg(3, 6, pgl, masloc, masglo)
        call dialum(3, 6, 18, wgt, masglo,&
                    mas)
!
    else if (option.eq.'ECIN_ELEM') then
        stopz='ONO'
! IRET NE PEUT VALOIR QUE 0 (TOUT VA BIEN) OU 2 (CHAMP NON FOURNI)
        call tecach(stopz, 'PVITESR', 'L', iret, iad=jvitg)
        if (iret .eq. 0) then
            call utpvgl(3, 6, pgl, zr(jvitg), vite)
            call dxtloe(flex, memb, mefl, ctor, .false.,&
                        vite, ener)
        else
            call tecach(stopz, 'PDEPLAR', 'L', iret, iad=jdepg)
            if (iret .eq. 0) then
                call utpvgl(3, 6, pgl, zr(jdepg), depl)
                call dxtloe(flex, memb, mefl, ctor, .false.,&
                            depl, ener)
            else
                call utmess('F', 'ELEMENTS2_1', sk=option)
            endif
        endif
    endif
!
end subroutine

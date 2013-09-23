subroutine dkqmas(xyzl, option, pgl, mas, ener)
    implicit none
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/dialum.h"
#include "asterfort/dkqnib.h"
#include "asterfort/dkqniw.h"
#include "asterfort/dxqloc.h"
#include "asterfort/dxqloe.h"
#include "asterfort/dxqnim.h"
#include "asterfort/dxroep.h"
#include "asterfort/elref5.h"
#include "asterfort/gquad4.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
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
!     MATRICE MASSE DE L'ELEMENT DE PLAQUE DKQ
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
!     OUT MAS    : MATRICE DE RIGIDITE
!     OUT ENER   : TERMES POUR ENER_CIN (ECIN_ELEM)
!     ------------------------------------------------------------------
    integer :: i, j, k, i1, i2, int, ii(8), jj(8), ll(16)
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: jdepg, jcoqu, jvitg, iret
    real(kind=8) :: roe, rho, epais, rof, zero, douze, qsi, eta
    real(kind=8) :: detj, wgt, nfx(12), nfy(12), nmi(4), vite(24)
    real(kind=8) :: wkq(12), depl(24), masloc(300), masglo(300)
    real(kind=8) :: flex(12, 12), memb(8, 8), mefl(8, 12), amemb(64)
    real(kind=8) :: unquar, undemi, un, neuf, excent, xinert
    real(kind=8) :: coefm, wgtf, wgtmf, caraq4(25), jacob(5)
    character(len=3) :: stopz
    logical :: exce, iner
!     ------------------------------------------------------------------
    real(kind=8) :: ctor
    data (ii(k),k=1,8)&
     &   / 1, 10, 19, 28, 37, 46, 55, 64 /
    data (jj(k),k=1,8)&
     &   / 5, 14, 23, 32, 33, 42, 51, 60 /
    data (ll(k),k=1,16)&
     &   / 3, 7, 12, 16, 17, 21, 26, 30, 35, 39, 44, 48, 49, 53, 58, 62/
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
    call dxroep(rho, epais)
    roe = rho * epais
    rof = rho*epais*epais*epais/douze
    excent = zero
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
! --- INITIALISATIONS :
!     ---------------
    do 10 k = 1, 96
        mefl(k,1) = zero
10  end do
    do 20 k = 1, 144
        flex(k,1) = zero
20  end do
!
!======================================
! ---  CALCUL DE LA MATRICE DE MASSE  =
!======================================
!=====================================================================
! ---  CALCUL DE LA PARTIE MEMBRANE CLASSIQUE DE LA MATRICE DE MASSE =
! ---  LES TERMES SONT EN NK*NP                                      =
!=====================================================================
!
    coefm = caraq4(21) * roe / neuf
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
        memb(k,1) = coefm * amemb(k)
60  end do
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     ===================================
    do 70 int = 1, npg
        qsi = zr(icoopg-1+ndim*(int-1)+1)
        eta = zr(icoopg-1+ndim*(int-1)+2)
!
!
! ---    CALCUL DU JACOBIEN SUR LE QUADRANGLE :
!        ------------------------------------
        call jquad4(xyzl, qsi, eta, jacob)
!
!===========================================================
! ---  CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE  =
!===========================================================
!
! ---    CALCUL DES FONCTIONS D'INTERPOLATION DE LA FLECHE :
!        -------------------------------------------------
        call dkqniw(qsi, eta, caraq4, wkq)
!
        detj = jacob(1)
!
! ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION W
! ---   EST EGALE A RHO_E = RHO*EPAIS :
!       -----------------------------
        wgt = zr(ipoids+int-1) * detj * roe
!
! ---   CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE
! ---   DUE AUX SEULS TERMES DE LA FLECHE W :
!       -----------------------------------
        do 80 i = 1, 12
            do 90 j = 1, 12
                flex(i,j) = flex(i,j) + wkq(i) * wkq(j) * wgt
90          continue
80      continue
!
! ---   CALCUL DES FONCTIONS D'INTERPOLATION DES ROTATIONS :
!       --------------------------------------------------
        call dkqnib(qsi, eta, caraq4, nfx, nfy)
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
!
!====================================================================
! ---  CAS OU L'ELEMENT EST EXCENTRE                                =
!====================================================================
!
        if (exce) then
!
! ---     FONCTIONS D'INTERPOLATION MEMBRANE :
!         ----------------------------------
            call dxqnim(qsi, eta, nmi)
!
!====================================================================
! ---  CALCUL DE LA PARTIE MEMBRANE-FLEXION DE LA MATRICE DE MASSE  =
!====================================================================
!
! ---     POUR LE COUPLAGE MEMBRANE-FLEXION, ON DOIT TENIR COMPTE
! ---     DE LA MASSE VOLUMIQUE
! ---     RHO_MF = D*EPAIS*RHO  :
!         --------------------
            wgtmf = zr(ipoids+int-1)*detj*excent*roe
!
! ---     TERMES DE COUPLAGE MEMBRANE-FLEXION U*BETA :
!         ------------------------------------------
            do 120 k = 1, 4
                i1 = 2*(k-1)+1
                i2 = i1 +1
                do 130 j = 1, 12
                    mefl(i1,j) = mefl(i1,j)+nmi(k)*nfx(j)*wgtmf
                    mefl(i2,j) = mefl(i2,j)+nmi(k)*nfy(j)*wgtmf
130              continue
120          continue
!
        endif
! ---   FIN DU TRAITEMENT DU CAS D'UN ELEMENT EXCENTRE
!       ----------------------------------------------
70  end do
! --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
!     ---------------------------------------------
!
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
        wgt = caraq4(21) * roe
        call utpslg(4, 6, pgl, masloc, masglo)
        call dialum(4, 6, 24, wgt, masglo,&
                    mas)
!
    else if (option .eq. 'ECIN_ELEM') then
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

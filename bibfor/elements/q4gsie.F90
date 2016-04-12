subroutine q4gsie(option, fami, xyzl, pgl, depl,&
                  nbcou, cdl)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dsqbfb.h"
#include "asterfort/dsxhlt.h"
#include "asterfort/dxdmul.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxqbm.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gquad4.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
#include "asterfort/q4gbc.h"
#include "asterfort/q4glxy.h"
    character(len=4) :: fami
    character(len=16) :: option
    real(kind=8) :: xyzl(3, *), pgl(3, *), depl(*), cdl(*)
    integer :: nbcou
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     RELATION ELAS_COQUE/ELAS_COQMU
!     CONTRAINTES DE L'ELEMENT DE PLAQUE Q4G (SIEF_ELGA)
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
!     IN  DEPL   : DEPLACEMENTS
!     OUT CDL    : CONTRAINTES AUX POINTS DE GAUSS DANS LE REPERE LOCAL
!                  LE CALCUL EST FAIT SUR UNE SEULE COUCHE (ELAS_COQUE)
!                  SUR 3 NIVEAUX : 3 PTS D INTEGRATION DANS L EPAISSEUR
!                  CORRESPONDANT AUX NIVEAUX INF, MOY, SUP
    integer :: nnomai
    parameter  (nnomai=4)
    integer :: nddlme
    parameter  (nddlme=2)
    integer :: nddlfl
    parameter  (nddlfl=3)
!
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: jcaco, i, j, k, ie, icpg, ig, icou, iniv, multic
    real(kind=8) :: zic, epais, excen
    real(kind=8) :: depf(nddlfl*nnomai), depm(nddlme*nnomai)
    real(kind=8) :: vt(2), lambda(4)
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2), dfc(3, 2)
    real(kind=8) :: h(3, 3), d1i(2, 2), d2i(2, 4)
    real(kind=8) :: bf(3, nddlfl*nnomai), bm(3, nddlme*nnomai)
    real(kind=8) :: sm(3), sf(3), hlt2(4, 6)
    real(kind=8) :: eps(3), sig(3), cist(2)
    real(kind=8) :: qsi, eta, caraq4(25), t2iu(4), t2ui(4), t1ve(9)
    real(kind=8) :: bc(2, 12)
    real(kind=8) :: bcdf(2)
    real(kind=8) :: jacob(5), hicou, zmin, zmax, quotient, a, b, c
    aster_logical :: coupmf, lcalct
!     ------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jcoopg=icoopg, jvf=ivf, jdfde=idfdx, jdfd2=idfd2,&
                     jgano=jgano)
!
!     ----- RAPPEL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
!           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
    call gquad4(xyzl, caraq4)
!
!     ----- CARACTERISTIQUES DES MATERIAUX --------
    call dxmate(fami, df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2iu, t2ui, t1ve)
!
!     -------- CALCUL DE LA MATRICE DE HOOKE EN MEMBRANE ---------------
    if (multic .eq. 0) then
        call jevech('PCACOQU', 'L', jcaco)
        epais = zr(jcaco)
        hicou = epais/nbcou
        excen = zr(jcaco-1+5)
        do k = 1, 9
            h(k,1) = dm(k,1)/epais
        end do
    endif
!
!     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
    do j = 1, nnomai
        do i = 1, nddlme
            depm(i+2* (j-1)) = depl(i+6* (j-1))
        end do
        depf(1+3* (j-1)) = depl(1+2+6* (j-1))
        depf(2+3* (j-1)) = depl(3+2+6* (j-1))
        depf(3+3* (j-1)) = -depl(2+2+6* (j-1))
    end do
!              ---------------------
!
!  BOUCLE SUR LES POINTS D INTEGRATION
!
    if (option .eq. 'EPSI_ELGA') then
        lcalct=.false.
    else
        lcalct=.true.
    endif
!
    do ie = 1, npg
        qsi = zr(icoopg-1+ndim*(ie-1)+1)
        eta = zr(icoopg-1+ndim*(ie-1)+2)
!         ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE ----------------
        call jquad4(xyzl, qsi, eta, jacob)
!         ------- CALCUL DU PRODUIT HL.T2 ---------------------------
        call dsxhlt(df, jacob(2), hlt2)
!         ----- CALCUL DE LA MATRICE BM -----------------------------
        call dxqbm(qsi, eta, jacob(2), bm)
!         ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
        call dsqbfb(qsi, eta, jacob(2), bf)
!         ---- CALCUL DE LA MATRICE BC AU POINT QSI ETA -------------
        call q4gbc(qsi, eta, jacob(2), caraq4, bc)
!         ------ SM = BM.DEPM ----------------------------------------
        do i = 1, 3
            sm(i) = 0.d0
        end do
        do i = 1, 3
            do j = 1, nddlme*nnomai
                sm(i) = sm(i) + bm(i,j)*depm(j)
            end do
        end do
!         ------ SF = BF.DEPF ---------------------------------------
        do i = 1, 3
            sf(i) = 0.d0
        end do
        do i = 1, 3
            do j = 1, nddlfl*nnomai
                sf(i) = sf(i) + bf(i,j)*depf(j)
            end do
        end do

!   coefficients pour calcul de sixz et siyz
    if (multic .eq. 0) then
        zmin = excen - epais/2.d0
        zmax = excen + epais/2.d0
        quotient = 1.d0*zmax**3-3*zmax**2*zmin + 3*zmax*zmin**2-1.d0*zmin**3
        a = -6.d0/quotient
        b = 6.d0*(zmin+zmax)/quotient
        c = -6.d0*zmax*zmin/quotient
    endif
!
!  BOUCLE SUR LES COUCHES
!
        do icou = 1, nbcou
!
!  BOUCLE SUR LES POINTS D'INTEGRATION DANS L'EPAISSEUR DE LA COUCHE
!
            do ig = 1, 3
!
!           INDICE DANS LE CHAMP DE CONTRAINTES A ECRIRE
                icpg = 6*3*nbcou*(ie-1) + 6*3*(icou-1) + 6*(ig-1)
!
                if (multic .eq. 0) then
!             -- MONOCOUCHE
!             -- COTE DES POINTS D'INTEGRATION
!             --------------------------------
                    zic = excen - epais/2.d0 + (icou-1)*hicou
                    if (ig .eq. 1) then
                        zic = zic
                    else if (ig.eq.2) then
                        zic = zic + hicou/2.d0
                    else
                        zic = zic + hicou
                    endif
                    d1i(1,1) = a*zic*zic + b*zic + c
                    d1i(2,2) = d1i(1,1)
                    d1i(1,2) = 0.d0
                    d1i(2,1) = 0.d0
                else
!             -- EN MULTICOUCHES
!             -- ON CALCULE TOUT D'UN COUP
                    iniv = ig - 2
                    call dxdmul(lcalct, icou, iniv, t1ve, t2ui,&
                                h, d1i, d2i, zic, hicou)
                endif
!
                do i = 1, 3
                    eps(i) = sm(i) + zic*sf(i)
                    sig(i) = 0.d0
                end do
!         ------ VT = DC.BC.DEPF -------------------------------------
                vt(1) = 0.d0
                vt(2) = 0.d0
                bcdf(1) = 0.d0
                bcdf(2) = 0.d0
                do j = 1, 12
                    bcdf(1) = bcdf(1) + bc(1,j)*depf(j)
                    bcdf(2) = bcdf(2) + bc(2,j)*depf(j)
                end do
                vt(1) = dc(1,1)*bcdf(1) + dc(1,2)*bcdf(2)
                vt(2) = dc(2,1)*bcdf(1) + dc(2,2)*bcdf(2)
!
                if (option .eq. 'EPSI_ELGA') then
!           ------ DCIS = DCI.VT --------------------------------------
                    cdl(icpg+1) = eps(1)
                    cdl(icpg+2) = eps(2)
                    cdl(icpg+3) = 0.d0
!           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
                    cdl(icpg+4) = eps(3)/2.d0
                    cdl(icpg+5) = bcdf(1)/2.d0
                    cdl(icpg+6) = bcdf(2)/2.d0
!
                else
!             SIEF_ELGA
!
                    do i = 1, 3
                        do j = 1, 3
                            sig(i) = sig(i) + h(i,j)*eps(j)
                        end do
                    end do
!
!         ------ CIST = D1I.VT ( + D2I.LAMBDA SI MULTICOUCHES ) ------
                    cist(1) = d1i(1,1)*vt(1) + d1i(1,2)*vt(2)
                    cist(2) = d1i(2,1)*vt(1) + d1i(2,2)*vt(2)
                    if (multic .gt. 0) then
                        call q4glxy(hlt2, depf, lambda)
                        do j = 1, 4
                            cist(1) = cist(1) + d2i(1,j)*lambda(j)
                            cist(2) = cist(2) + d2i(2,j)*lambda(j)
                        end do
                    endif
!
                    cdl(icpg+1) = sig(1)
                    cdl(icpg+2) = sig(2)
                    cdl(icpg+3) = 0.d0
                    cdl(icpg+4) = sig(3)
                    cdl(icpg+5) = cist(1)
                    cdl(icpg+6) = cist(2)
                endif
            end do
        end do
    end do
!
end subroutine

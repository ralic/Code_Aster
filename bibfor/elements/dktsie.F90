subroutine dktsie(option, fami, xyzl, pgl, depl,&
                  nbcou, cdl)
    implicit  none
    include 'jeveux.h'
    include 'asterfort/dktbf.h'
    include 'asterfort/dktlxy.h'
    include 'asterfort/dkttxy.h'
    include 'asterfort/dsxhft.h'
    include 'asterfort/dsxhlt.h'
    include 'asterfort/dxdmul.h'
    include 'asterfort/dxmate.h'
    include 'asterfort/dxtbm.h'
    include 'asterfort/elref5.h'
    include 'asterfort/gtria3.h'
    include 'asterfort/jevech.h'
    character(len=4) :: fami
    character(len=16) :: option
    real(kind=8) :: xyzl(3, *), pgl(3, *), depl(*), cdl(*)
    integer :: nbcou
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     RELATION ELAS_COQUE
!     CONTRAINTES DE L'ELEMENT DE PLAQUE DKT (SIEF_ELGA)
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
!     IN  DEPL   : DEPLACEMENTS
!     OUT CDL    : CONTRAINTES AUX POINTS DE GAUSS DANS LE REPERE LOCAL
!                  LE CALCUL EST FAIT SUR UNE SEULE COUCHE (ELAS_COQUE)
!                  SUR 3 NIVEAUX : 3 PTS D INTEGRATION DANS L EPAISSEUR
!                  CORRESPONDANT AUX NIVEAUX INF, MOY, SUP
    integer :: nnomai
    parameter  (nnomai=3)
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
    real(kind=8) :: sm(3), sf(3), hft2(2, 6), hlt2(4, 6)
    real(kind=8) :: eps(3), sig(3), cist(2), dcis(2)
    real(kind=8) :: qsi, eta, carat3(21), t2ev(4), t2ve(4), t1ve(9)
    real(kind=8) :: hicou
    logical :: coupmf, lcalct
!     ------------------------------------------------------------------
!
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, icoopg, ivf, idfdx,&
                idfd2, jgano)
!
!     ----- RAPPEL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
!           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
    call gtria3(xyzl, carat3)
!
!     ----- CARACTERISTIQUES DES MATERIAUX --------
    call dxmate(fami, df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2ev, t2ve, t1ve)
!
!     -------- CALCUL DE LA MATRICE DE HOOKE EN MEMBRANE ---------------
    if (multic .eq. 0) then
        call jevech('PCACOQU', 'L', jcaco)
        epais = zr(jcaco)
        hicou = epais/nbcou
        excen = zr(jcaco-1+5)
        do 10 k = 1, 9
            h(k,1) = dm(k,1)/epais
10      continue
    endif
!
!     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
    do 30 j = 1, nnomai
        do 20 i = 1, nddlme
            depm(i+2* (j-1)) = depl(i+6* (j-1))
20      continue
        depf(1+3* (j-1)) = depl(1+2+6* (j-1))
        depf(2+3* (j-1)) = depl(3+2+6* (j-1))
        depf(3+3* (j-1)) = -depl(2+2+6* (j-1))
30  end do
!     ------ CALCUL DE LA MATRICE BM -----------------------------------
    call dxtbm(carat3(9), bm)
!     ------ SM = BM.DEPM ----------------------------------------------
    do 40 i = 1, 3
        sm(i) = 0.d0
40  end do
    do 60 i = 1, 3
        do 50 j = 1, nddlme*nnomai
            sm(i) = sm(i) + bm(i,j)*depm(j)
50      continue
60  end do
!
!     ------- CALCUL DU PRODUIT HF.T2 ----------------------------------
    call dsxhft(df, carat3(9), hft2)
!     ------ VT = HFT2.TKT.DEPF ---------------------------------------
    call dkttxy(carat3(16), carat3(13), hft2, depf, vt)
!              ---------------------
!
    if (multic .gt. 0) then
!       ------- CALCUL DU PRODUIT HL.T2 ---------------------------
        call dsxhlt(df, carat3(9), hlt2)
!       ------ LAMBDA = HLT2.TKT.DEPF -----------------------------
        call dktlxy(carat3(16), carat3(13), hlt2, depf, lambda)
    endif
!
!
!  BOUCLE SUR LES POINTS D INTEGRATION
!
    if (option .eq. 'EPSI_ELGA') then
        lcalct=.false.
    else
        lcalct=.true.
    endif
    do 300 ie = 1, npg
        qsi = zr(icoopg-1+ndim*(ie-1)+1)
        eta = zr(icoopg-1+ndim*(ie-1)+2)
!         ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
        call dktbf(qsi, eta, carat3, bf)
!         ------ SF = BF.DEPF ---------------------------------------
        do 340 i = 1, 3
            sf(i) = 0.d0
340      continue
        do 360 i = 1, 3
            do 350 j = 1, nddlfl*nnomai
                sf(i) = sf(i) + bf(i,j)*depf(j)
350          continue
360      continue
!
!  BOUCLE SUR LES COUCHES
!
        do 400 icou = 1, nbcou
!
!  BOUCLE SUR LES POINTS D'INTEGRATION DANS L'EPAISSEUR DE LA COUCHE
!
            do 500, ig = 1 , 3
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
                d1i(1,1) = 3.d0/ (2.d0*epais) - zic*zic*6.d0/ ( epais*epais*epais)
                d1i(2,2) = d1i(1,1)
                d1i(1,2) = 0.d0
                d1i(2,1) = 0.d0
            else
!             -- EN MULTICOUCHES
!             -- ON CALCULE TOUT D'UN COUP
                iniv = ig - 2
                call dxdmul(lcalct, icou, iniv, t1ve, t2ve,&
                            h, d1i, d2i, zic, hicou)
            endif
!
            do 370 i = 1, 3
                eps(i) = sm(i) + zic*sf(i)
                sig(i) = 0.d0
370          continue
!
            if (option .eq. 'EPSI_ELGA') then
!           ------ DCIS = DCI.VT --------------------------------------
                dcis(1) = dci(1,1)*vt(1) + dci(1,2)*vt(2)
                dcis(2) = dci(2,1)*vt(1) + dci(2,2)*vt(2)
                cdl(icpg+1) = eps(1)
                cdl(icpg+2) = eps(2)
                cdl(icpg+3) = 0.d0
!          --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
                cdl(icpg+4) = eps(3)/2.d0
                cdl(icpg+5) = dcis(1)/2.d0
                cdl(icpg+6) = dcis(2)/2.d0
!
            else
!             SIEF_ELGA
                do 390 i = 1, 3
                    do 380 j = 1, 3
                        sig(i) = sig(i) + h(i,j)*eps(j)
380                  continue
390              continue
!             ---- CIST = D1I.VT ( + D2I.LAMBDA SI MULTICOUCHES ) -----
                cist(1) = d1i(1,1)*vt(1) + d1i(1,2)*vt(2)
                cist(2) = d1i(2,1)*vt(1) + d1i(2,2)*vt(2)
                if (multic .gt. 0) then
                    do 395 j = 1, 4
                        cist(1) = cist(1) + d2i(1,j)*lambda(j)
                        cist(2) = cist(2) + d2i(2,j)*lambda(j)
395                  continue
                endif
!
                cdl(icpg+1) = sig(1)
                cdl(icpg+2) = sig(2)
                cdl(icpg+3) = 0.d0
                cdl(icpg+4) = sig(3)
                cdl(icpg+5) = cist(1)
                cdl(icpg+6) = cist(2)
            endif
500          continue
400      continue
300  end do
!
end subroutine

subroutine dkqedg(xyzl, option, pgl, depl, edgl)
    implicit  none
    include 'jeveux.h'
    include 'asterfort/dkqbf.h'
    include 'asterfort/dkqtxy.h'
    include 'asterfort/dsxhft.h'
    include 'asterfort/dxmate.h'
    include 'asterfort/dxqbm.h'
    include 'asterfort/elref5.h'
    include 'asterfort/gquad4.h'
    include 'asterfort/jquad4.h'
    real(kind=8) :: xyzl(3, *), pgl(3, *), depl(*), edgl(*)
    character(len=16) :: option
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     ------------------------------------------------------------------
!     EFFORTS ET DEFORMATIONS GENERALISES DE L'ELEMENT DE PLAQUE DKQ
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  OPTION : NOM DE L'OPTION DE CALCUL
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
!     IN  DEPL   : DEPLACEMENTS
!     OUT EDGL   : EFFORTS OU DEFORMATIONS GENERALISES AUX NOEUDS DANS
!                  LE REPERE INTRINSEQUE A L'ELEMENT
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: multic, ne, k, j, i, ie
    real(kind=8) :: depf(12), depm(8)
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2), dfc(3, 2)
    real(kind=8) :: hft2(2, 6)
    real(kind=8) :: bf(3, 12), bm(3, 8)
    real(kind=8) :: bdf(3), bdm(3), dcis(2)
    real(kind=8) :: vf(3), vm(3), vt(2), qsi, eta, caraq4(25), jacob(5)
    real(kind=8) :: vfm(3), vmf(3), t2ev(4), t2ve(4), t1ve(9)
    logical :: coupmf
    character(len=4) :: fami
!     ------------------------------------------------------------------
!
    if (option(6:9) .eq. 'ELGA') then
        call elref5(' ', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, icoopg, ivf, idfdx,&
                    idfd2, jgano)
        ne = npg
        fami='RIGI'
    else if (option(6:9).eq.'ELNO') then
        call elref5(' ', 'NOEU', ndim, nno, nnos,&
                    npg, ipoids, icoopg, ivf, idfdx,&
                    idfd2, jgano)
        ne = nno
        fami='NOEU'
    endif
!
!     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
!           MEMBRANE ET CISAILLEMENT INVERSEES ------------------------
!
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
    call gquad4(xyzl, caraq4)
!     ----- CARACTERISTIQUES DES MATERIAUX --------
    call dxmate(fami, df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2ev, t2ve, t1ve)
!     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
    do 20 j = 1, 4
        do 10 i = 1, 2
            depm(i+2* (j-1)) = depl(i+6* (j-1))
10      continue
        depf(1+3* (j-1)) = depl(1+2+6* (j-1))
        depf(2+3* (j-1)) = depl(3+2+6* (j-1))
        depf(3+3* (j-1)) = -depl(2+2+6* (j-1))
20  end do
!
    if (option(1:4) .eq. 'DEGE') then
        do 80 ie = 1, ne
            qsi = zr(icoopg-1+ndim*(ie-1)+1)
            eta = zr(icoopg-1+ndim*(ie-1)+2)
!           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
            call jquad4(xyzl, qsi, eta, jacob)
!           ------- CALCUL DU PRODUIT HF.T2 ---------------------------
            call dsxhft(df, jacob(2), hft2)
!           ----- CALCUL DES MATRICES BM ET BF AU POINT QSI ETA -------
            call dxqbm(qsi, eta, jacob(2), bm)
            call dkqbf(qsi, eta, jacob(2), caraq4, bf)
            do 30 k = 1, 3
                bdf(k) = 0.d0
                bdm(k) = 0.d0
30          continue
            do 60 i = 1, 3
                do 40 j = 1, 12
                    bdf(i) = bdf(i) + bf(i,j)*depf(j)
40              continue
                do 50 j = 1, 8
                    bdm(i) = bdm(i) + bm(i,j)*depm(j)
50              continue
60          continue
!           ------ VT = HFT2.TKQ.DEPF ---------------------------------
            call dkqtxy(qsi, eta, hft2, depf, caraq4(13),&
                        caraq4(9), vt)
!           ------ DCIS = DCI.VT --------------------------------------
            dcis(1) = dci(1,1)*vt(1) + dci(1,2)*vt(2)
            dcis(2) = dci(2,1)*vt(1) + dci(2,2)*vt(2)
            do 70 i = 1, 3
                edgl(i+8* (ie-1)) = bdm(i)
                edgl(i+3+8* (ie-1)) = bdf(i)
70          continue
!           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
            edgl(3+8* (ie-1)) = edgl(3+8* (ie-1))/2.d0
            edgl(6+8* (ie-1)) = edgl(6+8* (ie-1))/2.d0
            edgl(7+8* (ie-1)) = dcis(1)/2.d0
            edgl(8+8* (ie-1)) = dcis(2)/2.d0
80      continue
    else
        do 160 ie = 1, ne
            qsi = zr(icoopg-1+ndim*(ie-1)+1)
            eta = zr(icoopg-1+ndim*(ie-1)+2)
!           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
            call jquad4(xyzl, qsi, eta, jacob)
!           ------- CALCUL DU PRODUIT HF.T2 ----------------------------
            call dsxhft(df, jacob(2), hft2)
!           ----- CALCUL DES MATRICES BM ET BF AU POINT QSI ETA --------
            call dxqbm(qsi, eta, jacob(2), bm)
            call dkqbf(qsi, eta, jacob(2), caraq4, bf)
            do 90 k = 1, 3
                bdf(k) = 0.d0
                bdm(k) = 0.d0
                vf(k) = 0.d0
                vm(k) = 0.d0
                vfm(k) = 0.d0
                vmf(k) = 0.d0
90          continue
!           ------ VF = DF.BF.DEPF , VFM = DMF.BM.DEPM ----------------
!           ------ VM = DM.BM.DEPM , VMF = DMF.BF.DEPF ----------------
            do 120 i = 1, 3
                do 100 j = 1, 12
                    bdf(i) = bdf(i) + bf(i,j)*depf(j)
100              continue
                do 110 j = 1, 8
                    bdm(i) = bdm(i) + bm(i,j)*depm(j)
110              continue
120          continue
            do 140 i = 1, 3
                do 130 j = 1, 3
                    vf(i) = vf(i) + df(i,j)*bdf(j)
                    vfm(i) = vfm(i) + dmf(i,j)*bdm(j)
                    vm(i) = vm(i) + dm(i,j)*bdm(j)
                    vmf(i) = vmf(i) + dmf(i,j)*bdf(j)
130              continue
140          continue
!           ------ VT = HFT2.TKQ.DEPF ---------------------------------
            call dkqtxy(qsi, eta, hft2, depf, caraq4(13),&
                        caraq4(9), vt)
            do 150 i = 1, 3
                edgl(i+8* (ie-1)) = vm(i) + vmf(i)
                edgl(i+3+8* (ie-1)) = vf(i) + vfm(i)
150          continue
            edgl(7+8* (ie-1)) = vt(1)
            edgl(8+8* (ie-1)) = vt(2)
160      continue
    endif
!
end subroutine

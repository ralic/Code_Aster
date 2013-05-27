subroutine dktedg(xyzl, option, pgl, depl, edgl,&
                  multic)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/dktbf.h'
    include 'asterfort/dkttxy.h'
    include 'asterfort/dsxhft.h'
    include 'asterfort/dxmate.h'
    include 'asterfort/dxtbm.h'
    include 'asterfort/elref5.h'
    include 'asterfort/gtria3.h'
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
!     EFFORTS ET DEFORMATIONS GENERALISES DE L'ELEMENT DE PLAQUE DKT
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
!     IN  OPTION : NOM DE L'OPTION DE CALCUL
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
!     IN  DEPL   : DEPLACEMENTS
!     OUT EDGL   : EFFORTS OU DEFORMATIONS GENERALISES AUX NOEUDS DANS
!                  LE REPERE INTRINSEQUE A L'ELEMENT
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: multic, ne, k, j, i, ie
    real(kind=8) :: depf(9), depm(6)
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2)
    real(kind=8) :: hft2(2, 6), dfc(3, 2), bf(3, 9), bm(3, 6)
    real(kind=8) :: bdm(3), bdf(3), dcis(2), vf(3), vm(3), vt(2)
    real(kind=8) :: vfm(3), vmf(3), distn
    real(kind=8) :: qsi, eta, carat3(21), t2ev(4), t2ve(4), t1ve(9)
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
    distn = 0.d0
!
!     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
!           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
!
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
    call gtria3(xyzl, carat3)
!     ----- CARACTERISTIQUES DES MATERIAUX --------
    call dxmate(fami, df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2ev, t2ve, t1ve)
!     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
    do 20 j = 1, 3
        do 10 i = 1, 2
            depm(i+2* (j-1)) = depl(i+6*(j-1))
10      continue
        depf(1+3* (j-1)) = depl(1+2+6*(j-1))
        depf(2+3* (j-1)) = depl(3+2+6*(j-1))
        depf(3+3* (j-1)) = -depl(2+2+6*(j-1))
20  end do
!     ------ CALCUL DE LA MATRICE BM -----------------------------------
    call dxtbm(carat3(9), bm)
    do 30 k = 1, 3
        bdm(k) = 0.d0
30  end do
    do 50 i = 1, 3
        do 40 j = 1, 6
            bdm(i) = bdm(i) + bm(i,j)*depm(j)
40      continue
50  end do
!     ------- CALCUL DU PRODUIT HF.T2 ----------------------------------
    call dsxhft(df, carat3(9), hft2)
!     ------ VT = HFT2.TKT.DEPF ---------------------------------------
    call dkttxy(carat3(16), carat3(13), hft2, depf, vt)
    if (option(1:4) .eq. 'DEGE') then
        do 110 ie = 1, ne
            qsi = zr(icoopg-1+ndim*(ie-1)+1)
            eta = zr(icoopg-1+ndim*(ie-1)+2)
!           ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
            call dktbf(qsi, eta, carat3, bf)
            do 70 k = 1, 3
                bdf(k) = 0.d0
70          continue
            do 90 i = 1, 3
                do 80 j = 1, 9
                    bdf(i) = bdf(i) + bf(i,j)*depf(j)
80              continue
90          continue
!           ------ DCIS = DCI.VT --------------------------------------
            dcis(1) = dci(1,1)*vt(1) + dci(1,2)*vt(2)
            dcis(2) = dci(2,1)*vt(1) + dci(2,2)*vt(2)
            do 100 i = 1, 3
                edgl(i+8* (ie-1)) = bdm(i) + distn*bdf(i)
                edgl(i+3+8* (ie-1)) = bdf(i)
100          continue
!           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
            edgl(3+8* (ie-1)) = edgl(3+8* (ie-1))/2.d0
            edgl(6+8* (ie-1)) = edgl(6+8* (ie-1))/2.d0
            edgl(7+8* (ie-1)) = dcis(1)/2.d0
            edgl(8+8* (ie-1)) = dcis(2)/2.d0
110      continue
    else
        do 120 k = 1, 3
            vm(k) = 0.d0
            vfm(k) = 0.d0
120      continue
        do 140 i = 1, 3
            do 130 j = 1, 3
                vm(i) = vm(i) + dm(i,j)*bdm(j)
                vfm(i) = vfm(i) + dmf(i,j)*bdm(j)
130          continue
140      continue
        do 250 ie = 1, ne
            qsi = zr(icoopg-1+ndim*(ie-1)+1)
            eta = zr(icoopg-1+ndim*(ie-1)+2)
!           ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
            call dktbf(qsi, eta, carat3, bf)
            do 150 k = 1, 3
                bdf(k) = 0.d0
                vf(k) = 0.d0
                vmf(k) = 0.d0
150          continue
!           ------ VF = DF.BF.DEPF , VMF = DMF.BF.DEPF ----------------
            do 170 i = 1, 3
                do 160 j = 1, 9
                    bdf(i) = bdf(i) + bf(i,j)*depf(j)
160              continue
170          continue
            do 230 i = 1, 3
                do 220 j = 1, 3
                    vf(i) = vf(i) + df(i,j)*bdf(j)
                    vmf(i) = vmf(i) + dmf(i,j)*bdf(j)
220              continue
230          continue
            do 240 i = 1, 3
                edgl(i+ 8*(ie-1)) = vm(i) + vmf(i)
                edgl(i+3+8*(ie-1)) = vf(i) + vfm(i)
240          continue
            edgl(7+8*(ie-1)) = vt(1)
            edgl(8+8*(ie-1)) = vt(2)
250      continue
    endif
!
end subroutine

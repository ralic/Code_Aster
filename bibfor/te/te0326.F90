subroutine te0326(option, nomte)
    implicit none
!
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
!...................................................................
!
! BUT: CALCUL DES VECTEURS ELEMENTAIRES POUR CALCULER LE 2 IEME MEMBRE
!                          DE LA
! FORMULATION VARIATIONNELLE PERMETTANT D'OBTENIR LE POTENTIEL  PHI2
!          DANS LA DECOMPOSITION DU POTENTIEL FLUCTUANT
!          ELEMENTS ISOPARAMETRIQUES 2D
!
!          OPTION : CHAR_THER_PHID_R
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!..................................................................
!
    include 'jeveux.h'
!
    include 'asterfort/divgra.h'
    include 'asterfort/e1e2nn.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/subacv.h'
    include 'asterfort/sumetr.h'
    integer :: icodre
    character(len=8) :: fami, poum
    character(len=16) :: nomte, option
    real(kind=8) :: jac(9), nx(9), ny(9), nz(9)
    real(kind=8) :: sx(9, 9), sy(9, 9), sz(9, 9)
    real(kind=8) :: norm(3, 9), r8b, rho
    real(kind=8) :: acloc(3, 9), acc(3, 9), flufn(9)
    real(kind=8) :: vibar(2, 9), e1(3, 9), e2(3, 9)
    real(kind=8) :: divsig(9), xin(9), cova(3, 3), metr(2, 2), a(2, 2)
    real(kind=8) :: jc, cnva(3, 3), e1n(3, 9), e2n(3, 9)
    real(kind=8) :: san(9), can(9), dxn(2)
    real(kind=8) :: dxinde(9), dxindk(9)
    real(kind=8) :: dfde(9, 9), dfdk(9, 9), nxn(9), nyn(9), nzn(9)
    real(kind=8) :: normn(3, 9), j1n(9), j2n(9), vibarn(2, 9), gphgxn(9)
    integer :: ipoids, ivf, idfdx, idfdy, igeom
    integer :: ndim, nno, ipg, npg1, ivectt, imate
    integer :: idec, jdec, kdec, ldec, kpg, spt
    integer :: i, j, k, iacce, idim, ii, ino, itemp, jno, nnos, jgano
!-----------------------------------------------------------------------
!
!     CALCUL DES DERIVEES PREMIERES DES FONCTIONS DE FORME
!     POUR LES ELEMENTS QUAD4 ET QUAD8
!
    call elref4(' ', 'NOEU', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
    do 111 ii = 1, nno
        kdec=(ii-1)*nno*ndim
        do 211 j = 1, nno
            idec=(j-1)*ndim
            dfde(j,ii) = zr(idfdx+kdec+idec)
            dfdk(j,ii) = zr(idfdy+kdec+idec)
211      continue
111  end do
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PVECTTR', 'E', ivectt)
    call jevech('PACCELR', 'L', iacce)
    call jevech('PTEMPER', 'L', itemp)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THER', 0, ' ', r8b,&
                1, 'RHO_CP', rho, icodre, 1)
!
    do 1200 i = 1, nno
        acloc(1,i)=0.0d0
        acloc(2,i)=0.0d0
        acloc(3,i)=0.0d0
1200  end do
    k=0
    do 1201 i = 1, nno
        do 20 idim = 1, 3
            k=k+1
            acloc(idim,i) = zr(iacce+k-1)
20      continue
1201  end do
    do 1052 ipg = 1, npg1
        acc(1,ipg)=0.0d0
        acc(2,ipg)=0.0d0
        acc(3,ipg)=0.0d0
1052  end do
    do 1051 ipg = 1, npg1
        ldec=(ipg-1)*nno
        do 105 i = 1, nno
            acc(1,ipg) = acc(1,ipg) + acloc(1,i)*zr(ivf+ldec+i-1)
            acc(2,ipg) = acc(2,ipg) + acloc(2,i)*zr(ivf+ldec+i-1)
            acc(3,ipg) = acc(3,ipg) + acloc(3,i)*zr(ivf+ldec+i-1)
105      continue
1051  end do
!
    do 11 i = 1, nno
        zr(ivectt+i-1) = 0.d0
11  end do
!
! --- CALCUL DES VECTEURS E1, E2 TANGENTS A L'ELEMENT NON NORMALISES
!     ET DES VECTEURS UNITAIRES NORMES
!
    do 90 ipg = 1, npg1
        kdec=(ipg-1)*nno*ndim
!
        e1(1,ipg)=0.0d0
        e1(2,ipg)=0.0d0
        e1(3,ipg)=0.0d0
!
        e2(1,ipg)=0.0d0
        e2(2,ipg)=0.0d0
        e2(3,ipg)=0.0d0
!
        do 91 j = 1, nno
            idec=(j-1)*ndim
!
            e1(1,ipg)= e1(1,ipg)+zr(igeom + 3*(j-1) -1+1) *zr(idfdx+&
            kdec+idec)
            e1(2,ipg)= e1(2,ipg)+zr( igeom + 3*(j-1) -1+2) *zr(idfdx+&
            kdec+idec)
            e1(3,ipg)= e1(3,ipg)+zr( igeom + 3*(j-1) -1+3) *zr(idfdx+&
            kdec+idec)
!
            e2(1,ipg)= e2(1,ipg)+zr( igeom + 3*(j-1) -1+1) *zr(idfdy+&
            kdec+idec)
            e2(2,ipg)=e2(2,ipg)+zr( igeom + 3*(j-1) -1+2) *zr(idfdy+&
            kdec+idec)
            e2(3,ipg)=e2(3,ipg)+zr( igeom + 3*(j-1) -1+3) *zr(idfdy+&
            kdec+idec)
91      continue
90  end do
!
! --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ
!
    do 21 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 22 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
22      continue
21  end do
!
! --- BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 ipg = 1, npg1
!
        kdec=(ipg-1)*nno*ndim
        ldec=(ipg-1)*nno
!
        nx(ipg) = 0.0d0
        ny(ipg) = 0.0d0
        nz(ipg) = 0.0d0
!
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 104 j = 1, nno
                jdec = (j-1)*ndim
!
                nx(ipg) = nx(ipg) + zr(idfdx+kdec+idec) * zr(idfdy+ kdec+jdec) * sx(i,j)
                ny(ipg) = ny(ipg) + zr(idfdx+kdec+idec) * zr(idfdy+ kdec+jdec) * sy(i,j)
                nz(ipg) = nz(ipg) + zr(idfdx+kdec+idec) * zr(idfdy+ kdec+jdec) * sz(i,j)
104          continue
102      continue
!
! ------ CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
        jac(ipg) = sqrt (nx(ipg)*nx(ipg) + ny(ipg)*ny(ipg) + nz(ipg)* nz(ipg))
!
! ------ CALCUL DE LA NORMALE UNITAIRE
!
        norm(1,ipg) = nx(ipg)/jac(ipg)
        norm(2,ipg) = ny(ipg)/jac(ipg)
        norm(3,ipg) = nz(ipg)/jac(ipg)
!
101  end do
!
! --- CALCUL DU PRODUIT (XI.N) AUX NOEUDS
!
    call e1e2nn(nno, dfde, dfdk, e1n, e2n,&
                nxn, nyn, nzn, normn, j1n,&
                j2n, san, can)
!
    do 200 i = 1, nno
        xin(i)=acloc(1,i)*normn(1,i) + acloc(2,i)*normn(2,i) + acloc(&
        3,i)*normn(3,i)
200  end do
!
    do 202 ipg = 1, npg1
!
! CALCUL DES DERIVEES DES (XIN) (GRADSIGMA(XI.N)) AUX POINTS DE GAUSS
!
        dxinde(ipg)=0.0d0
        dxindk(ipg)=0.0d0
!
        kdec=(ipg-1)*nno*ndim
        do 203 i = 1, nno
            idec=(i-1)*ndim
            dxinde(ipg)=dxinde(ipg)+zr(idfdx+kdec+idec)*xin(i)
            dxindk(ipg)=dxindk(ipg)+zr(idfdy+kdec+idec)*xin(i)
203      continue
!
        dxn(1) = dxinde(ipg)
        dxn(2) = dxindk(ipg)
!
        vibar(1,ipg) = 0.0d0
        vibar(2,ipg) = 0.0d0
!
! ------ CALCUL DE GRAD(PHIBARRE):VITESSE FLUIDE PERMANENTE
!        VIBAR  AUX POINTS DE GAUSS
!
        do 108 i = 1, nno
            idec=(i-1)*ndim
            vibar(1,ipg)=vibar(1,ipg)+zr(itemp+i-1)*zr(idfdx+kdec+&
            idec)
            vibar(2,ipg)=vibar(2,ipg)+zr(itemp+i-1)*zr(idfdy+kdec+&
            idec)
108      continue
!
! ------ CONSTITUTION DE LA BASE DES DEUX VECTEURS COVARIANTS
!        AUX POINTS DE GAUSS
!
        do 204 i = 1, 3
            cova(i,1)=e1(i,ipg)
            cova(i,2)=e2(i,ipg)
204      continue
!
! ------ ON CALCULE LE TENSEUR METRIQUE
!
        call sumetr(cova, metr, jc)
!
! ------ CALCUL DE LA BASE CONTRAVARIANTE
!
        call subacv(cova, metr, jc, cnva, a)
!
! ------ CALCUL DU PRODUIT SCALAIRE GRAD(POTENTIEL PERMANENT)*
!        GRAD(DEPLACEMENT NORMAL) RELATIVEMENT A LA BASE CONTRAVARIANTE
!
        gphgxn(ipg) = 0.d0
        do 205 i = 1, 2
            do 206 j = 1, 2
                gphgxn(ipg) = gphgxn(ipg) + a(i,j)*vibar(i,ipg)*dxn(j)
206          continue
205      continue
!
202  end do
!
!
! --- CALCUL DE LA DIVSIGMA(GRAD(PHIBAR)) AUX POINTS DE GAUSS
!
    call divgra(e1, e2, dfde, dfdk, vibarn,&
                divsig)
!
! --- CALCUL DU FLUX FLUIDE NORMAL AUX POINTS DE GAUSS
!
    do 1070 ipg = 1, npg1
        flufn(ipg) = 0.0d0
        flufn(ipg) = acc(1,ipg)*norm(1,ipg)+acc(2,ipg)*norm(2,ipg) +acc(3,ipg)*norm(3,ipg)
1070  end do
!
! --- CALCUL DU VECTEUR ASSEMBLE SECOND MEMBRE
!     POUR LE CALCUL DU SECOND POTENTIEL INSTATIONNAIRE PHI2
!
    do 61 ipg = 1, npg1
        ldec=(ipg-1)*nno
        do 103 i = 1, nno
            zr(ivectt+i-1) = zr(ivectt+i-1) + rho*jac(ipg)*zr(ipoids+ ipg-1) *zr(ivf+ldec+i-1) *(&
                             &flufn(ipg)*divsig(ipg) + gphgxn(ipg))
!
103      continue
61  end do
!
end subroutine

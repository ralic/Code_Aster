subroutine cabrp1(kpi, ipoids, ipoid2, ivf, ivf2,&
                  idfde, idfde2, geom, dimdef, dimuel,&
                  ndim, nddls, nddlm, nno, nnos,&
                  nnom, axi, regula, b, poids,&
                  poids2)
! ======================================================================
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
! person_in_charge: romeo.fernandes at edf.fr
! TOLE CRP_21 CRS_1404
! ======================================================================
    implicit  none
    include 'jeveux.h'
!
    include 'asterfort/dfdm2d.h'
    include 'asterfort/dfdm3d.h'
    logical :: axi
    integer :: kpi, ipoids, ipoid2, idfde, idfde2, ndim, regula(6), dimdef, ivf
    integer :: ivf2, nno, nnos, nnom, nddls, nddlm, dimuel
    real(kind=8) :: geom(ndim, *), poids, poids2, b(dimdef, dimuel)
! ======================================================================
! --- BUT : CALCUL DE L'OPERATEUR B ------------------------------------
! ======================================================================
! --- IN ---------------------------------------------------------------
! --- NBDDL  : VECTEUR DIMENSION DU NOMBRE DE DDLS ---------------------
! --- NBNO   : VECTEUR DIMENSION DU NOMBRE DE NOEUDS -------------------
! --- KPI    : INDICE DU POINT D'INTEGRATION ---------------------------
! --- IPOIDS : ADRESSE DES FONCTIONS POIDS D'ORDRE 2 -------------------
! --- IPOID2 : ADRESSE DES FONCTIONS POIDS D'ORDRE 1 -------------------
! --- IVF2   : ADRESSE DES FONCTIONS DE FORME D'ORDRE 1 ----------------
! --- IDFDE  : ADRESSE DES DERIVEES DES FONCTIONS DE FORME D'ORDRE 2 ---
! --- IDFDE2 : ADRESSE DES DERIVEES DES FONCTIONS DE FORME D'ORDRE 1 ---
! --- GEOM   : CARACTERISTIQUES GEOMETRIQUES DE L'ELEMENT REEL ---------
! --- DIMDEF : DIMENSION DU VECTEUR DES DEFORMATIONS GENERALISEES ------
! --- NDIM   : DIMENSION DU PROBLEME -----------------------------------
! --- OUT --------------------------------------------------------------
! --- B      : OPERATEUR B DEFINI TEL QUE E=B.U ------------------------
! --- POIDS  : POIDS ASSOCIE AUX FONCTIONS DE FORME D'ORDRE 2 ----------
! --- POIDS2 : POIDS ASSOCIE AUX FONCTIONS DE FORME D'ORDRE 1 ----------
! ======================================================================
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, j, n, adder1, adder2, adder3
    real(kind=8) :: dfdi(nno, 3), dfdi2(nnos, 3)
! ======================================================================
! --- INITIALISATION DE LA MATRICE B -----------------------------------
! ======================================================================
    do 100 i = 1, dimuel
        do 200 j = 1, dimdef
            b(j,i)=0.d0
200      continue
100  end do
    adder1 = regula(1)
    adder2 = regula(2)
    adder3 = regula(3)
! ======================================================================
! --- CAS 2D -----------------------------------------------------------
! ======================================================================
    if (ndim .eq. 2) then
! ======================================================================
! --- CAS QUADRATIQUES -------------------------------------------------
! ======================================================================
        call dfdm2d(nno, kpi, ipoids, idfde, geom,&
                    dfdi(1, 1), dfdi(1, 2), poids)
! ======================================================================
! --- CAS LINEAIRES ----------------------------------------------------
! ======================================================================
        call dfdm2d(nnos, kpi, ipoid2, idfde2, geom,&
                    dfdi2(1, 1), dfdi2(1, 2), poids2)
    else if (ndim.eq.3) then
! ======================================================================
! --- CAS QUADRATIQUES -------------------------------------------------
! ======================================================================
        call dfdm3d(nno, kpi, ipoids, idfde, geom,&
                    dfdi(1, 1), dfdi(1, 2), dfdi(1, 3), poids)
! ======================================================================
! --- CAS LINEAIRES ----------------------------------------------------
! ======================================================================
        call dfdm3d(nnos, kpi, ipoid2, idfde2, geom,&
                    dfdi2(1, 1), dfdi2(1, 2), dfdi2(1, 3), poids2)
    endif
! ======================================================================
! --- REMPLISSAGE DE L OPERATEUR B -------------------------------------
! ======================================================================
! --- SUR LES NOEUDS SOMMETS -------------------------------------------
! ======================================================================
    do 10 n = 1, nnos
        do 20 i = 1, ndim
            b(adder1,(n-1)*nddls+i) = b(adder1,(n-1)*nddls+i)-dfdi(n, i)
20      continue
        b(adder1,(n-1)*nddls+ndim+2) = b(adder1, (n-1)*nddls+ndim+2) + zr(ivf2+n+(kpi-1)*nnos-1)
10  end do
! ======================================================================
! --- SUR LES NOEUDS MILIEUX -------------------------------------------
! ======================================================================
    do 30 n = 1, nnom
        do 40 i = 1, ndim
            b(adder1,nnos*nddls+(n-1)*nddlm+i)= b(adder1,nnos*nddls+(&
            n-1)*nddlm+i)-dfdi(n+nnos,i)
40      continue
30  end do
! ======================================================================
! --- POUR LES GRADIENTS DE VARIATIONS VOLUMIQUE ET LES VAR VOL --------
! --- ON UTILISE LES FONCTIONS DE FORME D'ORDRE 1 ----------------------
! ======================================================================
! --- SUR LES NOEUDS SOMMETS -------------------------------------------
! ======================================================================
    do 50 n = 1, nnos
        do 60 i = 1, ndim
            b(adder2-1+i,(n-1)*nddls+ndim+2)= b(adder2-1+i,(n-1)*&
            nddls+ndim+2)+dfdi2(n,i)
60      continue
50  end do
! ======================================================================
! --- POUR LE MULTIPLICATEUR DE LAGRANGE -------------------------------
! --- (PRES) -----------------------------------------------------------
! ======================================================================
    do 300 n = 1, nnos
        b(adder3,(n-1)*nddls+ndim+1)=b(adder3,(n-1)*nddls+ndim+1)+&
        zr(ivf2+n+(kpi-1)*nnos-1)
300  end do
! ======================================================================
end subroutine

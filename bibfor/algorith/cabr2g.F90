subroutine cabr2g(kpi, ipoids, ipoid2, ivf, ivf2,&
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
! ======================================================================
! aslint: disable=W1306,W1504
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/matini.h"
#include "asterfort/utmess.h"
!
    logical(kind=1) :: axi
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
    integer :: i, j, k, n, adder1, adder2, adder3
    real(kind=8) :: dfdi(nno, 3), dfdi2(nnos, 3)
! ======================================================================
! --- INITIALISATION DE LA MATRICE B -----------------------------------
! ======================================================================
    call matini(dimdef, dimuel, 0.d0, b)
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
                    poids, dfdi(1, 1), dfdi(1, 2))
! ======================================================================
! --- CAS LINEAIRES ----------------------------------------------------
! ======================================================================
        call dfdm2d(nnos, kpi, ipoid2, idfde2, geom,&
                    poids2, dfdi2(1, 1), dfdi2(1, 2))
    else if (ndim.eq.3) then
! ======================================================================
! --- CAS QUADRATIQUES -------------------------------------------------
! ======================================================================
        call dfdm3d(nno, kpi, ipoids, idfde, geom,&
                    poids, dfdi(1, 1), dfdi(1, 2), dfdi(1, 3))
! ======================================================================
! --- CAS LINEAIRES ----------------------------------------------------
! ======================================================================
        call dfdm3d(nnos, kpi, ipoid2, idfde2, geom,&
                    poids2, dfdi2(1, 1), dfdi2(1, 2), dfdi2(1, 3))
    else
        call utmess('F', 'ALGORITH6_13')
    endif
! ======================================================================
! --- REMPLISSAGE DE L OPERATEUR B -------------------------------------
! ======================================================================
! --- TERMES -(DUJ/DXI-VJI) --------------------------------------------
! ======================================================================
! --- SUR LES NOEUDS SOMMETS -------------------------------------------
! ======================================================================
    do 10 n = 1, nnos
        do 20 j = 1, ndim
            do 30 i = 1, ndim
                b(adder1-1+(j-1)*ndim+i,(n-1)*nddls+j) = b(&
                                                         adder1-1+(j-1)*ndim+i,&
                                                         (n-1)*nddls+j ) - dfdi(n, i&
                                                         )
                b(adder1-1+(j-1)*ndim+i,(n-1)*nddls+ndim+(j-1)*ndim+i)&
                = b(adder1-1+(j-1)*ndim+i,(n-1)*nddls+ndim+(j-1)*ndim+&
                i) + zr(ivf2+n+(kpi-1)*nnos-1)
30          continue
20      continue
10  end do
    do 40 n = 1, nnom
        do 50 j = 1, ndim
            do 60 i = 1, ndim
                b(adder1-1+(j-1)*ndim+i,nnos*nddls+(n-1)*nddlm+j) =&
                b(adder1-1+(j-1)*ndim+i,nnos*nddls+(n-1)*nddlm+j) -&
                dfdi(n+nnos,i)
60          continue
50      continue
40  end do
! ======================================================================
! --- POUR LES GRADIENTS DE VARIATIONS VOLUMIQUE -----------------------
! --- ON UTILISE LES FONCTIONS DE FORME D'ORDRE 1 ----------------------
! ======================================================================
! --- SUR LES NOEUDS SOMMETS -------------------------------------------
! ======================================================================
    do 120 n = 1, nnos
        do 130 k = 1, ndim
            do 140 j = 1, ndim
                do 150 i = 1, ndim
                    b(adder2-1+(k-1)*ndim*ndim+(j-1)*ndim+i, (n-1)*&
                    nddls+ndim+(k-1)*ndim+j)= b(adder2-1+(k-1)*ndim*&
                    ndim+(j-1)*ndim+i, (n-1)*nddls+ndim+(k-1)*ndim+j)+&
                    dfdi2(n,i)
150              continue
140          continue
130      continue
120  end do
! ======================================================================
! --- POUR LE MULTIPLICATEUR DE LAGRANGE -------------------------------
! --- (PRES) -----------------------------------------------------------
! --- ON UTILISE LES FONCTIONS DE FORME D'ORDRE 0 ----------------------
! ======================================================================
! --- SUR LES NOEUDS CENTRAUX ------------------------------------------
! ======================================================================
    do 190 i = 1, ndim
        do 210 j = 1, ndim
            b(adder3-1+(i-1)*ndim+j,nnos*nddls+nnom*nddlm+(i-1)*ndim+&
            j)= 1.0d0
210      continue
190  end do
! ======================================================================
end subroutine

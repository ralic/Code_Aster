subroutine caeihm(nomte, axi, perman, mecani, press1,&
                  press2, tempe, dimdef, dimcon, ndim,&
                  nno1, nno2, npi, npg, dimuel,&
                  iw, ivf1, idf1, ivf2, idf2,&
                  jgano1, iu, ip, ipf, iq,&
                  modint)
!
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
!
!
! ======================================================================
! --- BUT : PREPARATION DU CALCUL SUR UN ELEMENT DE JOINT HM -----------
! ======================================================================
!
! IN NOMTE   : NOM DU TYPE D'ELEMENT
! IN AXI     : AXI ?
! OUT PERMAN : MODELISATION HM PERMAMENTE ?
! OUT MECANI : TABLEAU INFO SUR MECANIQUE
! OUT PRESS1 : TABLEAU INFO SUR HYDRAULIQUE CONSTITUANT 1
! OUT PRESS2 : TABLEAU INFO SUR HYDRAULIQUE CONSTITUANT 2
! OUT TEMPE  : TABLEAU INFO SUR THERMIQUE
! OUT DIMDEF : DIMENSION DES DEFORMATIONS GENERALISEES
! OUT DIMCON : DIMENSION DES CONTRAINTES GENERALISEES
! OUT NDIM   : DIMENSION DU PROBLEME (2 OU 3)
! OUT NNO1   : NOMBRE DE NOEUDS DES BORDS INF ET DUP DE L'ELEMENT
! OUT NNO2   : NOMBRE DE NOEUDS DU SEGMENT CENTRAL
! OUT NPI    : NOMBRE DE POINTS D'INTEGRATION DE L'ELEMENT
! OUT NPG    : NOMBRE DE POINTS DE GAUSS
! OUT DIMUEL : NOMBRE DE DDL TOTAL DE L'ELEMENT
! OUT IW     : ADRESSE DU TABLEAU POIDS POUR FONCTION DE FORME P2
! OUT IVF1   : ADRESSE DU TABLEAU DES FONCTIONS DE FORME P2
! OUT IDF1   : ADRESSE DU TABLEAU DES DERIVESS DES FONCTIONS DE FORME P2
! OUT IVF2   : ADRESSE DU TABLEAU DES FONCTIONS DE FORME P1
! OUT IDF2 : ADRESSE DU TABLEAU DES DERIVESS DES FONCTIONS DE FORME P1
! OUT JGANO1  : ADRESSE DANS ZR DE LA MATRICE DE PASSAGE
!              GAUSS -> NOEUDS
! OUT IU     : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
! OUT IP     : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION MILIEU
! OUT IPF    : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION FACES
! OUT IQ     : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE HYDRO
! OUT MODINT : MODE D'INTEGRATION
!
! CORPS DU PROGRAMME
! aslint: disable=W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "asterfort/elref2.h"
#include "asterfort/elref4.h"
#include "asterfort/greihm.h"
#include "asterfort/lteatt.h"
#include "asterfort/modthm.h"
    logical :: axi, perman
    integer :: mecani(8), press1(9), press2(9), tempe(5), dimuel
    integer :: ndim, nnos, nno1, nno2, ntrou
    integer :: dimdef, dimcon
    integer :: npg, npi, n, i
    integer :: ivf1, idf1, ivf2, idf2, jgano1, jgano2, iw
    integer :: iu(3, 18), ip(2, 9), ipf(2, 2, 9), iq(2, 2, 9)
    integer :: f1q8(6), f2q8(2), f3q8(2), f4q8(2)
    character(len=3) :: modint
    character(len=8) :: lielrf(10)
    character(len=16) :: nomte
!
! --- INITIALISATIONS --------------------------------------------------
!
    perman= .false.
    ndim=2
    axi=.false.
! ======================================================================
! --- INITIALISATION DES GRANDEURS GENERALISEES SELON MODELISATION -----
! ======================================================================
    call greihm(nomte, perman, ndim, mecani, press1,&
                press2, tempe, dimdef, dimcon)
!
!
    call modthm(nomte, modint)
!
    if (lteatt(' ','AXIS','OUI')) then
        axi = .true.
    endif
! ======================================================================
! --- ADAPTATION AU MODE D'INTEGRATION ---------------------------------
! --- DEFINITION DE L'ELEMENT (NOEUDS, SOMMETS, POINTS DE GAUSS) -------
! ======================================================================
    call elref2(nomte, 2, lielrf, ntrou)
    call elref4(lielrf(1), 'RIGI', ndim, nno1, nnos,&
                npi, iw, ivf1, idf1, jgano1)
    call elref4(lielrf(2), 'RIGI', ndim, nno2, nnos,&
                npi, iw, ivf2, idf2, jgano2)
!
    if (modint .eq. 'RED') then
        npg= npi-nnos
    endif
    if (modint .eq. 'CLA') then
        npg= npi
    endif
!
    ndim = ndim + 1
!
! ======================================================================
! --- DETERMINATION DES DECALAGES D'INDICE POUR ACCEDER AUX DDL --------
! ======================================================================
!
    data f1q8  /1,2,5,4,3,7/
    data f2q8 /8,6/
    data f3q8 /1,2/
    data f4q8 /4,3/
!
    if ((nomte(1:9).eq.'HM_J_DPQ8') .or. (nomte(1:9).eq.'HM_J_AXQ8')) then
        dimuel = 2*nno1*ndim+nno2*3*(press1(1)+press2(1))+2
        do 10 n = 1, 5
            do 11 i = 1, 2
                iu(i,n) = i + (f1q8(n)-1)*3
11          continue
10      continue
        do 12 i = 1, 2
            iu(i,6) = iu(i,3) + 4
12      continue
!
        do 20 n = 1, 2
            ip(1,n) = 16 + (f2q8(n)-6)*2
20      continue
!
        do 30 n = 1, 2
            ipf(1,1,n) = 3+(f4q8(n)-1)*3
30      continue
!
        do 40 n = 1, 2
            ipf(1,2,n) = 3+(f3q8(n)-1)*3
40      continue
        iq(1,1,1)=iu(2,6)+1
        iq(1,2,1)=iu(2,3)+1
    endif
!
! ======================================================================
end subroutine

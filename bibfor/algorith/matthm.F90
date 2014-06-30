subroutine matthm(ndim, axi, nno1, nno2, dimuel,&
                  dimdef, iu, ip, ipf, iq,&
                  yap1, yap2, yate, addep1, addep2,&
                  addlh1, vff1, vff2, dffr2, wref,&
                  geom, ang, wi, q)
!
!
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
! ======================================================================
!
! =====================================================================
!.......................................................................
!
!     BUT:  CALCUL DE LA MATRICE DE PASSAGE DES DDL A LA DEFORMATION
!           GENERALISEES
!.......................................................................
! =====================================================================
! IN NDIM    : DIMENSION DE L'ESPACE
! IN AXI     : .TRUE. SI AXISYMETRIE
! IN NNO1    : NOMBRE DE NOEUDS DE LA FAMILLE 1
! IN NNO2    : NOMBRE DE NOEUDS DE LA FAMILLE 2
! IN DIMUEL  : NOMBRE DE DDL
! IN DIMDEF  : DIMENSION DU VECTEUR DEFORMATIONS GENERALISEES
! IN IU      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
! IN IP      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION MILIEU
! IN IPF     : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION FACES
! IN IQ      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE HYDRO
! IN YAP1    : SI=1 : EQUATION SUR LA PRESSION 1
! IN YAP2    : SI=1 : EQUATION SUR LA PRESSION 2
! IN YATE    : SI=1 : EQUATION SUR LA TEMPERATURE
! IN ADDEP1  : ADRESSE DES DEFORMATIONS PRESSION 1
! IN ADDEP2  : ADRESSE DES DEFORMATIONS PRESSION 2
! IN ADDLH1  : ADRESSE DES DEFORMATIONS LAGRANGE PRESSION 1
! IN VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE 1)
! IN VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE 2)
! IN DFFR2   : DERIVEES DES FONCTIONS DE FORME (FAMILLE 2)
! IN WREF    : POIDS DE REFERENCE DU POINT D'INTEGRATION
! IN GEOM    : COORDONNEES DES NOEUDS (FAMILLE 1)
! IN ANG     : ANGLES D'EULER NODAUX (FAMILLE 1)
! =====================================================================
! OUT WI     : POIDS REEL DU POINT D'INTEGRATION
! OUT Q      : MATRICE DE PASSAGE DDL -> DEFORMATIONS GENERALISEES
!......................................................................
!
!
! aslint: disable=W1306,W1504
    implicit none
!
! - VARIABLES ENTREE
#include "asterfort/dfdm1d.h"
#include "asterfort/eicine.h"
    integer :: ndim, nno1, nno2, dimuel, dimdef, yap1, yap2, yate
    integer :: iu(3, 18), ip(2, 9), ipf(2, 2, 9), iq(2, 2, 9)
    integer :: addep1, addep2, addlh1
    real(kind=8) :: vff1(nno1), vff2(nno2), dffr2(ndim-1, nno2)
    real(kind=8) :: wref, geom(ndim, nno2), ang(24)
    logical(kind=1) :: axi
!
! - VARIABLES SORTIE
!
    real(kind=8) :: q(dimdef, dimuel), wi
!
! - VARIABLES LOCALES
    integer :: i, j, n, kj, f
    real(kind=8) :: b(3, 3, 2*nno1), cour, jacp, sina, cosa, dfdx(nno2)
!
! ======================================================================
! --- INITIALISATION ----------------------------------------------
! ======================================================================
    do 108 i = 1, dimdef
        do 109 j = 1, dimuel
            q(i,j)=0.d0
109      continue
108  continue
!
! ======================================================================
! --- CALCUL DE Q ET WI ----------------------------------------------
! ======================================================================
!
! - CALCUL DES DERIVEES DES FONCTIONS DE FORME / ABSCISSE CURVILIGNE
!
    call dfdm1d(nno2, wi, dffr2, geom, dfdx,&
                cour, jacp, cosa, sina)
!
! - CALCUL DE LA MATRICE DE PASSAGE U GLOBAL -> SAUT DE U LOCAL
!
    call eicine(ndim, axi, nno1, nno2, vff1,&
                vff2, wref, dffr2, geom, ang,&
                wi, b)
!
!
    do 10 i = 1, ndim
        do 11 j = 1, ndim
            do 12 n = 1, 2*nno1
                kj=iu(j,n)
                q(i,kj) = b(i,j,n)
12          continue
11      continue
10  end do
!
!
!
! - LIGNES PRESS1
!
    if (yap1 .eq. 1) then
        do 30 n = 1, nno2
            q(addep1,ip(1,n)) = vff2(n)
            do 31 i = 1, ndim-1
                q(addep1+i,ip(1,n)) = dfdx(n)
31          continue
            do 32 f = 1, 2
                q(addlh1+f-1,ipf(1,f,n)) = vff2(n)
                q(addlh1+f+1,iq(1,f,1)) = 1
32          continue
30      continue
    endif
!
!
end subroutine

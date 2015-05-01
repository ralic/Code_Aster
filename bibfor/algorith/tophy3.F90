subroutine tophy3(icho, ia, dplmod, nbchoc, nbmode,&
                  xgene, ux, uy, uz, nbexci,&
                  psidel, coef)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!    CONVERTIT EN BASE PHYSIQUE POUR DES DDLS GENERALISES DONNES
!
!    IN  : ICHO      :   INDICE CARACTERISANT LA NON-LINEARITE
!    IN  : IA        :   INDICE = 0 => TRAITEMENT DU NOEUD_1
!                               = 3 => TRAITEMENT DU NOEUD_2
!    IN  : DPLMOD    :   VECTEUR DES DEPL MODAUX AUX NOEUDS DE CHOC
!    IN  : NBCHOC    :   NOMBRE DE CHOCS
!    IN  : NBMODE    :   NB DE MODES NORMAUX CONSIDERES
!    IN  : XGENE     :   VECTEUR DES COORDONNEES GENERALISEES
!    OUT : UX,Y,Z    :   VALEURS DES DDLS CORRESPONDANTS
!
!    IN  : TEMPS     :   INSTANT DE CALCUL DES DEPL_IMPO
!    IN  : NBEXCI    :   NOMBRE D'ACCELERO DIFFERENTS
!    IN  : PSIDEL    :   VALEUR DU VECTEUR PSI*DELTA
!    IN  : COEF      :   INTENSITE DE L'EXCITATION A L'INSTANT T
!-----------------------------------------------------------------------
!
#include "jeveux.h"
    integer :: icho, ia, nbmode, nbchoc
    real(kind=8) :: xgene(nbmode), dplmod(nbchoc, nbmode, *)
    real(kind=8) :: psidel(nbchoc, nbexci, *), ux, uy, uz, coef(nbexci)
!
! ----------------------------------------------------------------------
!
!
!
!
!
!-----------------------------------------------------------------------
    integer :: i, iex, nbexci
!-----------------------------------------------------------------------
    ux=0.0d0
    uy=0.0d0
    uz=0.0d0
!
!
    do 10 i = 1, nbmode
        ux = ux + dplmod(icho,i,1+ia)*xgene(i)
10  end do
    do 11 iex = 1, nbexci
        ux = ux + psidel(icho,iex,1+ia)*coef(iex)
11  end do
!
    do 20 i = 1, nbmode
        uy = uy + dplmod(icho,i,2+ia)*xgene(i)
20  end do
    do 21 iex = 1, nbexci
        uy = uy + psidel(icho,iex,2+ia)*coef(iex)
21  end do
!
    do 30 i = 1, nbmode
        uz = uz + dplmod(icho,i,3+ia)*xgene(i)
30  end do
    do 31 iex = 1, nbexci
        uz = uz + psidel(icho,iex,3+ia)*coef(iex)
31  end do
!
end subroutine

subroutine tophys(icho, ia, dplmod, nbchoc, nbmode,&
                  xgene, ux, uy, uz)
    implicit none
! ----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!    CONVERTIT EN BASE PHYSIQUE POUR DES DDLS GENERALISES DONNES
!
!    ICHO      IN    INDICE CARACTERISANT LA NON-LINEARITE
!    IA        IN    INDICE = 0 => TRAITEMENT DU NOEUD_1
!                           = 3 => TRAITEMENT DU NOEUD_2
!    DPLMOD    IN    VECTEUR DES DEPL MODAUX AUX NOEUDS DE CHOC
!    NBCHOC    IN    NOMBRE DE CHOCS
!    NBMODE    IN    NB DE MODES NORMAUX CONSIDERES
!    XGENE     IN    VECTEUR DES COORDONNEES GENERALISEES
!    UX,Y,Z    OUT   VALEURS DES DDLS CORRESPONDANTS
!-----------------------------------------------------------------------
!
    integer :: icho, ia, nbmode, nbchoc
    real(kind=8) :: xgene(nbmode), dplmod(nbchoc, nbmode, *), ux, uy, uz
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    ux=0.0d0
    uy=0.0d0
    uz=0.0d0
!
    do 10 i = 1, nbmode
        ux = ux + dplmod(icho,i,1+ia)*xgene(i)
        uy = uy + dplmod(icho,i,2+ia)*xgene(i)
        uz = uz + dplmod(icho,i,3+ia)*xgene(i)
10  end do
!
!
end subroutine

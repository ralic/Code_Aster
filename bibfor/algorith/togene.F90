subroutine togene(icho, ia, dplmod, nbchoc, nbmode,&
                  fx, fy, fz, fgene)
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
!    CONVERTIT EN BASE MODALE POUR DES DDLS PHYSIQUES DONNES
!    ATTENTION FGENE N'EST PAS REINITIALISE
!-----------------------------------------------------------------------
!
!    ICHO      IN        INDICE CARACTERISANT LA NON-LINEARITE
!    IA        IN        INDICE = 0 => TRAITEMENT DU NOEUD_1
!                               = 3 => TRAITEMENT DU NOEUD_2
!    DPLMOD    IN        VECTEUR DES DEPL MODAUX AUX NOEUDS DE CHOC
!    NBCHOC    IN        NOMBRE DE CHOCS
!    NBMODE    IN        NB DE MODES NORMAUX CONSIDERES
!    FX,Y,Z    IN        VALEURS DES DDLS CORRESPONDANTS
!    FGENE     IN/OUT    VECTEUR DES COORDONNEES GENERALISEES
!-----------------------------------------------------------------------
!
    integer :: icho, ia, nbmode, nbchoc
    real(kind=8) :: fgene(nbmode), dplmod(nbchoc, nbmode, *), fx, fy, fz
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    do 10 i = 1, nbmode
        fgene(i)=fgene(i)+dplmod(icho,i,1+ia)*fx +dplmod(icho,i,2+ia)*&
        fy +dplmod(icho,i,3+ia)*fz
10  end do
!
!
end subroutine

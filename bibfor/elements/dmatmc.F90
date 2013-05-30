subroutine dmatmc(fami, modeli, mater, instan, poum,&
                  igau, isgau, repere, xyzgau, nbsig,&
                  d)
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
!.======================================================================
    implicit none
!
!      DMATMC :   CALCUL DE LA MATRICE DE HOOKE POUR LES ELEMENTS
!                 ISOPARAMETRIQUES POUR DES MATERIAUX ISOTROPE,
!                 ORTHOTROPE ET ISOTROPE TRANSVERSE
!
!   ARGUMENT        E/S  TYPE         ROLE
!    FAMI           IN     K4       FAMILLE DU POINT DE GAUSS
!    MODELI         IN     K2       MODELISATION
!    MATER          IN     I        MATERIAU
!    IGAU           IN     I        POINT DE GAUSS
!    ISGAU          IN     I        SOUS-POINT DE GAUSS
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    POUM           IN     K1       + OU -
!    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
!                                   D'ORTHOTROPIE
!    XYZGAU(3)      IN     R        COORDONNEES DU POINT D'INTEGRATION
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE A
!                                   L'ELEMENT
!    D(NBSIG,1)     OUT    R        MATRICE DE HOOKE
!
!
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    include 'asterfort/assert.h'
    include 'asterfort/dmat3d.h'
    include 'asterfort/dmatcp.h'
    include 'asterfort/dmatdp.h'
    include 'asterfort/lteatt.h'
    character(len=*) :: fami, poum
    character(len=2) :: modeli
    integer :: mater, nbsig, igau, isgau
    real(kind=8) :: repere(7), xyzgau(1), d(nbsig, 1), instan
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!       ------------------------
! ----  CAS MASSIF 3D ET FOURIER
!       ------------------------
    if (lteatt(' ','DIM_TOPO_MAILLE','3') .or. lteatt(' ','FOURIER','OUI')) then
!
        call dmat3d(fami, mater, instan, poum, igau,&
                    isgau, repere, xyzgau, d)
!
!       ----------------------------------------
! ----  CAS DEFORMATIONS PLANES ET AXISYMETRIQUE
!       ----------------------------------------
        elseif (lteatt(' ','D_PLAN','OUI').or. lteatt(' ','AXIS','OUI')&
    .or.modeli.eq.'DP') then
!
        call dmatdp(fami, mater, instan, poum, igau,&
                    isgau, repere, d)
!
!       ----------------------
! ----  CAS CONTRAINTES PLANES
!       ----------------------
    else if (lteatt(' ','C_PLAN','OUI')) then
!
        call dmatcp(fami, mater, instan, poum, igau,&
                    isgau, repere, d)
!
    else
        call assert(.false.)
    endif
!.============================ FIN DE LA ROUTINE ======================
end subroutine

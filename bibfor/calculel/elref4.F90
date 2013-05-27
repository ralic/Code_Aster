subroutine elref4(elrz, famil, ndim, nno, nnos,&
                  npg, ipoids, ivf, idfde, jgano)
    implicit none
    include 'asterfort/elref5.h'
    character(len=*) :: elrz, famil
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
! ----------------------------------------------------------------------
! BUT: RECUPERER DANS UNE ROUTINE TE00IJ LES ADRESSES DANS ZR
!      - DES POIDS DES POINTS DE GAUSS  : IPOIDS
!      - DES VALEURS DES FONCTIONS DE FORME : IVF
!      - DES VALEURS DES DERIVEES 1ERES DES FONCTIONS DE FORME : IDFDE
!      - DE LA MATRICE DE PASSAGE GAUSS -> NOEUDS : JGANO
! ----------------------------------------------------------------------
!   IN   ELRZ : NOM DE L'ELREFA (K8) OU ' '
!                 SI ELRZ=' ' : ON PREND L'ELREFA "PRINCIPAL"
!        FAMIL  : NOM (LOCAL) DE LA FAMILLE DE POINTS DE GAUSS :
!                 'STD','RICH',...
!   OUT  NDIM   : DIMENSION DE L'ESPACE (=NB COORDONNEES)
!        NNO    : NOMBRE DE NOEUDS DU TYPE_MAILLE
!        NNOS   : NOMBRE DE NOEUDS SOMMETS DU TYPE_MAILLE
!        NPG    : NOMBRE DE POINTS DE GAUSS
!        IPOIDS : ADRESSE DANS ZR DU TABLEAU POIDS(IPG)
!        IVF    : ADRESSE DANS ZR DU TABLEAU FF(INO,IPG)
!        IDFDE  : ADRESSE DANS ZR DU TABLEAU DFF(IDIM,INO,IPG)
!        JGANO  : ADRESSE DANS ZR DE LA MATRICE DE PASSAGE
!                      GAUSS -> NOEUDS (DIM= 2+NNO*NPG)
!                 ATTENTION : LES 2 1ERS TERMES SONT LES
!                             DIMMENSIONS DE LA MATRICE: NNO ET NPG
!   -------------------------------------------------------------------
    integer :: jcoopg, jdfd2
!
!
    call elref5(elrz, famil, ndim, nno, nnos,&
                npg, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
!
!
end subroutine

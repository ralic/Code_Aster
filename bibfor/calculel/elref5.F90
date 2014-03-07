subroutine elref5(elrz, famil, ndim, nno, nnos,&
                  npg, ipoids, jcoopg, ivf, idfde,&
                  jdfd2, jgano)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elref6.h"
    character(len=*) :: elrz, famil
    integer :: ndim, nno, nnos, npg, ipoids, jcoopg, ivf, idfde, jdfd2, jgano
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
!      - DES COORDONNEES DES POINTS DE GAUSS  : JCOOPG
!      - DES VALEURS DES FONCTIONS DE FORME : IVF
!      - DES VALEURS DES DERIVEES 1ERES DES FONCTIONS DE FORME : IDFDE
!      - DES VALEURS DES DERIVEES 2EMES DES FONCTIONS DE FORME : JDFD2
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
!        JCOOPG : ADRESSE DANS ZR DU TABLEAU COOPG(IDIM,IPG)
!        IVF    : ADRESSE DANS ZR DU TABLEAU FF(INO,IPG)
!        IDFDE  : ADRESSE DANS ZR DU TABLEAU DFF(IDIM,INO,IPG)
!        JDFD2  : ADRESSE DANS ZR DU TABLEAU DFF2(IDIM,JDIM,INO,IPG)
!        JGANO  : ADRESSE DANS ZR DE LA MATRICE DE PASSAGE
!                      GAUSS -> NOEUDS (DIM= 2+NNO*NPG)
!                 ATTENTION : LES 2 1ERS TERMES SONT LES
!                             DIMMENSIONS DE LA MATRICE: NNO ET NPG
!
!   -------------------------------------------------------------------
    character(len=16) :: option, nomte, nomtm
    common /cakk01/option,nomte,nomtm
    integer :: nute, jnbelr, jnoelr, iactif, jpnlfp, jnolfp, nblfpg
    common /caii11/nute,jnbelr,jnoelr,iactif,jpnlfp,jnolfp,nblfpg
!
    character(len=8) :: elrf
! DEB ------------------------------------------------------------------
!
    if (elrz .eq. ' ') then
        call elref1(elrf)
        ASSERT(elrf.ne.'XXXXXXXX')
    else
        elrf = elrz
    endif
!
    call elref6(elrf, nomte, famil, ndim, nno,&
                nnos, npg, ipoids, jcoopg, ivf,&
                idfde, jdfd2, jgano)
!
end subroutine

subroutine mmreac(nbdm  ,ndim  ,nne   ,nnm   ,jgeom , &
                  jdepm ,jdepde,ppe,geomae,geomam)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"

    real(kind=8) :: ppe
    integer :: nbdm, ndim, nne, nnm
    integer :: jgeom, jdepm,jdepde
    real(kind=8) :: geomae(9, 3), geomam(9, 3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - TE)
!
! MISE A JOUR DE LA GEOMETRIE
!
! ----------------------------------------------------------------------
!
! GEOM = GEOM_INIT + DEPMOI
!
! IN  NBDM   : NB DE DDL DE LA MAILLE ESCLAVE
! IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
! IN  NNE    : NOMBRE DE NOEUDS ESCLAVES
! IN  NNM    : NOMBRE DE NOEUDS MAITRES
! IN  JGEOM  : POINTEUR JEVEUX SUR GEOMETRIE INITIALE (MAILLAGE)
! IN  JDEPM  : POINTEUR JEVEUX SUR CHAMP DE DEPLACEMENT A L'INSTANT
!              PRECEDENT
! IN PPE     : LA REACTUALISATION DES NORMALES N'EST PAS LA MEME EN NEWTON GENE
!                   PPE = 0 --> POINT_FIXE
!                    PPE = 1 --> NEWTON_GENE (FULL)
!            0<PPE<1 --> NEWTON_GENE (AVEC NORMALE INEXACTE)
! OUT GEOMAE : GEOMETRIE ACTUALISEE SUR NOEUDS ESCLAVES
! OUT GEOMAM : GEOMETRIE ACTUALISEE SUR NOEUDS MAITRES
!
    integer :: inoe, inom, idim
!
! ----------------------------------------------------------------------
!
!
    geomae = 0.d0
    geomam = 0.d0
!
! --- NOEUDS ESCLAVES
!
    do  inoe = 1, nne
        do 110 idim = 1, ndim
            geomae(inoe,idim) = zr( jgeom+(inoe-1)*ndim+idim-1) + zr(jdepm+(inoe-1)*nbdm+idim-1)&
                                             +ppe* zr(jdepde+(inoe-1)*nbdm+idim-1)
110      continue
  end do
!
! --- NOEUDS MAITRES
!
    do  inom = 1, nnm
        do 122 idim = 1, ndim
            geomam(inom,idim) = zr(&
                                jgeom+nne*ndim+(inom-1)*ndim+idim- 1)+ zr(jdepm+nne*nbdm+(inom-1)&
                                &*ndim+idim-1&
                                ) + ppe*zr(jdepde+nne*nbdm+(inom-1)&
                                &*ndim+idim-1&
                                )
122      continue
  end do
!
!
end subroutine

subroutine mmdepm(nbdm  ,ndim  ,nne   ,nnm   ,jdepm , &
                  jdepde, ffe  ,ffm   ,ddeple,ddeplm, &
                  deplme, deplmm)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbdm, ndim, nne, nnm
    integer :: jdepde, jdepm
    real(kind=8) :: ffe(9), ffm(9)
    real(kind=8) :: ddeple(3), deplme(3)
    real(kind=8) :: ddeplm(3), deplmm(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DES INCREMENTS - DEPLACEMENTS
!
! ----------------------------------------------------------------------
!
!
! DEPDEL - INCREMENT DE DEPLACEMENT DEPUIS DEBUT DU PAS DE TEMPS
! DEPMOI - DEPLACEMENT DEBUT DU PAS DE TEMPS
!
! IN  NBDM   : NB DE DDL DE LA MAILLE ESCLAVE
!                NDIM = 2 -> NBDM = DX/DY/LAGR_C/LAGR_F1
!                NDIM = 3 -> NBDM = DX/DY/DZ/LAGR_C/LAGR_F1/LAGR_F2
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  JDEPDE : ADRESSE JEVEUX POUR DEPDEL
! IN  JDEPM  : ADRESSE JEVEUX POUR DEPMOI
! IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
! IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
! OUT DDEPLE : INCREMENT DEPDEL DU DEPL. DU POINT DE CONTACT
! OUT DDEPLM : INCREMENT DEPDEL DU DEPL. DU PROJETE DU POINT DE CONTACT
! OUT DEPLME : DEPLACEMENTS DEPMOI DU POINT DE CONTACT
! OUT DEPLMM : DEPLACEMENTS DEPMOI DU PROJETE DU POINT DE CONTACTT
!
!
!
!
    integer :: idim, inoe
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    do  idim = 1, 3
        ddeplm(idim) = 0.d0
        deplmm(idim) = 0.d0
        ddeple(idim) = 0.d0
        deplme(idim) = 0.d0
   end do
!
! --- DEPLACEMENT PAS DE TEMPS MOINS POUR LES NOEUDS ESCLAVES
!
    do  idim = 1, ndim
        do 32 inoe = 1, nne
            deplme(idim) = deplme(idim) + ffe(inoe)* zr(jdepm+(inoe-1) *nbdm+idim-1)
32      continue
  end do
!
! --- INCREMENT DEPDEL DU DEPL. POUR LES NOEUDS ESCLAVES
!
    do  idim = 1, ndim
        do 132 inoe = 1, nne
            ddeple(idim) = ddeple(idim) + ffe(inoe)* zr(jdepde+( inoe-1)*nbdm+idim-1)
132      continue
  end do
!
! --- DEPLACEMENT PAS DE TEMPS MOINS POUR LES NOEUDS MAITRES
!
    do  idim = 1, ndim
        do 42 inoe = 1, nnm
            deplmm(idim) = deplmm(idim) + ffm(inoe)* zr(jdepm+nne* nbdm+(inoe-1)*ndim+idim-1)
42      continue
  end do
!
! --- INCREMENT DEPDEL DU DEPL. POUR LES NOEUDS MAITRES
!
    do  idim = 1, ndim
        do 142 inoe = 1, nnm
            ddeplm(idim) = ddeplm(idim) + ffm(inoe)* zr(jdepde+nne* nbdm+(inoe-1)*ndim+idim-1)
142      continue
  end do
!
    call jedema()
!
end subroutine

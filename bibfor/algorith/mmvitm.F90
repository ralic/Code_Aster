subroutine mmvitm(nbdm, ndim, nne, nnm, ffe,&
                  ffm, jvitm, jaccm, jvitp, vitme,&
                  vitmm, vitpe, vitpm, accme, accmm)
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbdm, ndim, nne, nnm
    real(kind=8) :: ffe(9), ffm(9)
    integer :: jvitm, jvitp, jaccm
    real(kind=8) :: accme(3), vitme(3), accmm(3), vitmm(3)
    real(kind=8) :: vitpe(3), vitpm(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DES VITESSES/ACCELERATIONS
!
! ----------------------------------------------------------------------
!
!
! DEPDEL - INCREMENT DE DEPLACEMENT DEPUIS DEBUT DU PAS DE TEMPS
! DEPMOI - DEPLACEMENT DEBUT DU PAS DE TEMPS
! GEOMAx - GEOMETRIE ACTUALISEE GEOM_INIT + DEPMOI
!
!
!
! IN  NBDM   : NB DE DDL DE LA MAILLE ESCLAVE
!                NDIM = 2 -> NBDM = DX/DY/LAGR_C/LAGR_F1
!                NDIM = 3 -> NBDM = DX/DY/DZ/LAGR_C/LAGR_F1/LAGR_F2
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
! IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
! IN  JVITM  : ADRESSE JEVEUX POUR CHAMP DE VITESSES A L'INSTANT
!              PRECEDENT
! IN  JVITP  : ADRESSE JEVEUX POUR CHAMP DE VITESSES A L'INSTANT
!              COURANT
! IN  JACCM  : ADRESSE JEVEUX POUR CHAMP D'ACCELERATIONS A L'INSTANT
!               PRECEDENT
! OUT VITME  : VITESSE PRECEDENTE DU POINT DE CONTACT
! OUT ACCME  : ACCELERATION PRECEDENTE DU POINT DU CONTACT
! OUT VITMM  : VITESSE PRECEDENTE DU PROJETE DU POINT DE CONTACT
! OUT ACCMM  : ACCELERATION PRECEDENTE  DU PROJETE DU POINT DU CONTACT
! OUT VITPE  : VITESSE COURANTE DU POINT DE CONTACT
! OUT VITPM  : VITESSE COURANTE DU PROJETE DU POINT DU CONTACT
!
!
!
!
    integer :: idim, inoe, inom
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    do 10 idim = 1, 3
        vitpm(idim) = 0.d0
        vitpe(idim) = 0.d0
        vitme(idim) = 0.d0
        accme(idim) = 0.d0
        vitmm(idim) = 0.d0
        accmm(idim) = 0.d0
        vitpe(idim) = 0.d0
10  end do
!
! --- POUR LES NOEUDS ESCLAVES
!
    do 31 idim = 1, ndim
        do 32 inoe = 1, nne
            vitpe(idim) = vitpe(idim) + ffe(inoe)* zr(jvitp+(inoe-1)* nbdm+idim-1)
            vitme(idim) = vitme(idim) + ffe(inoe)* zr(jvitm+(inoe-1)* nbdm+idim-1)
            accme(idim) = accme(idim) + ffe(inoe)* zr(jaccm+(inoe-1)* nbdm+idim-1)
!
32      continue
31  end do
!
! --- POUR LES NOEUDS MAITRES
!
    do 41 idim = 1, ndim
        do 42 inom = 1, nnm
            vitpm(idim) = vitpm(idim) + ffm(inom)* zr(jvitp+nne*nbdm+( inom-1)*ndim+idim-1)
            vitmm(idim) = vitmm(idim) + ffm(inom)* zr(jvitm+nne*nbdm+( inom-1)*ndim+idim-1)
            accmm(idim) = accmm(idim) + ffm(inom)* zr(jaccm+nne*nbdm+( inom-1)*ndim+idim-1)
!
42      continue
41  end do
!
    call jedema()
!
end subroutine

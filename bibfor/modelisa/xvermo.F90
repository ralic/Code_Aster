subroutine xvermo(nfiss, fiss, mai)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    integer :: nfiss
    character(len=8) :: fiss(nfiss), mai
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (VERIFICATION DES SD)
!
! VERIFICATION QUE LES FISSURES SONT TOUTES DEFINIES A PARTIR DU
! MAILLAGE UTILISE POUR CONSTRUIRE LE MODELE SAIN EN ENTREE DE
! MODI_MODELE_XFEM  (MC MODELE_IN)
! ----------------------------------------------------------------------
!
! IN  NFISS  : NOMBRE DE FISSURES
! IN  FISS   : LISTE DES NOMS DES FISSURES
! IN  MAI    : NOM DU MAILLAGE UTILISE POUR CONSTRUIRE LE MODELE SAIN
!              EN ENTREE DE MODI_MODELE_XFEM  (MC MODELE_IN)
!
!
!
!
    integer :: ifiss
    character(len=8) :: maif, valk(3)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    do ifiss = 1, nfiss
!
!       RECUPERATION DU MAILLAGE ASSOCIE A LA FISSURE COURANTE
        call dismoi('NOM_MAILLA', fiss(ifiss), 'FISS_XFEM', repk=maif)
!
!       VERIFICATION DE LA COHERENCE
        if (mai .ne. maif) then
            valk(1)=fiss(ifiss)
            valk(2)=maif
            valk(3)=mai
            call utmess('F', 'XFEM_39', nk=3, valk=valk)
        endif
!
    end do
!
    call jedema()
end subroutine

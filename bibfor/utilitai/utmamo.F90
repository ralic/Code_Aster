subroutine utmamo(modele, nbtrou, litrou)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/juveca.h'
    include 'asterfort/utmam2.h'
    include 'asterfort/wkvect.h'
    integer :: nbtrou
    character(len=8) :: modele
    character(len=*) :: litrou
! ----------------------------------------------------------------------
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
!
! person_in_charge: josselin.delmas at edf.fr
!
!     BUT:
!       FILTRER LES MAILLES AFFECTEES PAR LE MODELE
!                   **                       **
!       IDEM QUE UTMAM2 MAIS AVEC UN VECTEUR JEVEUX
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   MODELE    : NOM DU MODELE
!
!      SORTIE :
!-------------
! OUT  NBTROU    : NOMBRE DE MAILLE TROUVEES
! OUT  LITROU    : LISTE DES MAILLES TROUVEES (OBJET JEVEUX)
!                  SI NBTROU = 0, L'OBJET JEVEUX N'EST PAS CREE
!
!.......................................................................
!
    integer :: nbmail
    integer :: itrma
!
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call jelira(modele//'.MAILLE', 'LONMAX', nbmail, k8bid)
!
    call wkvect(litrou, 'V V I', nbmail, itrma)
!
    call utmam2(modele, nbmail, nbtrou, zi(itrma))
!
    if (nbtrou .gt. 0) then
        call juveca(litrou, nbtrou)
    else
        call jedetr(litrou)
    endif
!
    call jedema()
!
end subroutine

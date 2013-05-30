subroutine rsutro(nomsd, iordg, iordr, ierr)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: iordg, iordr, ierr
    character(len=*) :: nomsd
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
! person_in_charge: jacques.pellet at edf.fr
!      CORRESPONDANCE NUMERO D'ORDRE UTILISATEUR (IORDR) AVEC LE
!      NUMERO DE RANGEMENT (IORDG).
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
! IN  : IORDG  : NUMERO DE RANGEMENT.
! OUT : IORDR  : NUMERO D'ORDRE UTILISATEUR.
! OUT : IERR   : CODE RETOUR
!                = 0  , LA RELATION EXISTE .
!                = 10 , LE .ORDR N'EST PAS REMPLI (SD RESULTAT VIDE)
!                = 20 , LE NUMERO DE RANGEMENT EST SUPERIEUR AU MAX
!                       AUTORISE
! ----------------------------------------------------------------------
    character(len=19) :: nomd2
    character(len=1) :: k1bid
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: jordr, nbordr
!-----------------------------------------------------------------------
    call jemarq()
    ierr = 0
    nomd2 = nomsd
!
!     --- RECUPERATION DU .ORDR ---
    call jelira(nomd2//'.ORDR', 'LONUTI', nbordr, k1bid)
    if (nbordr .eq. 0) then
        ierr = 10
    else if (iordg.gt.nbordr) then
        ierr = 20
    else
        call jeveuo(nomd2//'.ORDR', 'L', jordr)
        iordr = zi(jordr+iordg-1)
    endif
!
    call jedema()
end subroutine

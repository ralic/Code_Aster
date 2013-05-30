subroutine nmextd(nomcha, sdieto, champ)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmetnc.h'
    include 'asterfort/nmetob.h'
    character(len=24) :: nomcha, sdieto
    character(len=19) :: champ
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
!
! RETOURNE LE CHAMP POUR L'EXTRACTION
!
! ----------------------------------------------------------------------
!
!
! IN  NOMCHA : NOM DU CHAMP
! IN  SDIETO : SD GESTION IN ET OUT
! OUT CHAMP  : NOM DU CHAMP
!
!
!
!
    integer :: icham
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INDICE DU CHAMP
!
    call nmetob(sdieto, nomcha, icham)
!
! --- NOM DU CHAMP DANS L'OPERATEUR
!
    call nmetnc(sdieto, icham, champ)
!
    call jedema()
!
end subroutine

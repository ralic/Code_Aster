subroutine nmlost(sdtime)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmrtim.h'
    include 'asterfort/nmtimr.h'
    character(len=24) :: sdtime
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! TEMPS PERDU (A CAUSE DECOUPE PAS DE TEMPS)
!
! ----------------------------------------------------------------------
!
!
! IN  SDTIME : SD TIMER
!
!
!
!
    real(kind=8) :: time
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- TEMPS PASSE DANS LE PAS
!
    call nmtimr(sdtime, 'TEMPS_PHASE', 'P', time)
!
! --- SAUVEGARDE TEMPS GASPILLE
!
    call nmrtim(sdtime, 'PAS_LOST', time)
!
    call jedema()
end subroutine

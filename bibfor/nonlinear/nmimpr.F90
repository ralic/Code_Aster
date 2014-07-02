subroutine nmimpr(sdimpr)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "asterfort/impsdl.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/obgetb.h"
#include "asterfort/obgeti.h"
#include "asterfort/obgeto.h"
    character(len=24) :: sdimpr
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (AFFICHAGE)
!
! IMPRESSION D'UNE LIGNE DU TABLEAU DE CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: sdtabc
    aster_logical :: lcsv, lprint
    integer :: larlig, unimes, unicsv
    character(len=1) :: sepcol
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- TABLEAU DE CONVERGENCE ET OPTIONS
!
    call obgeto(sdimpr, 'TABLEAU_CONV', sdtabc)
    call obgetb(sdtabc, 'SORTIE_CSV', lcsv)
    call obgeti(sdtabc, 'UNITE_CSV', unicsv)
    call obgeti(sdtabc, 'LARGEUR_LIGNE', larlig)
    unimes = iunifi('MESSAGE')
!
! --- AFFICHAGE POUR CE PAS ?
!
    call obgetb(sdimpr, 'PRINT', lprint)
!
! --- IMPRESSION LIGNE DANS LE FICHIER MESSAGE
!
    sepcol = '|'
    if (lprint) call impsdl(sdtabc, sepcol, unimes)
!
! --- IMPRESSION LIGNE DANS LE FICHIER CSV
!
    if (lcsv) then
        sepcol = ','
        call impsdl(sdtabc, sepcol, unicsv)
    endif
!
    call jedema()
end subroutine

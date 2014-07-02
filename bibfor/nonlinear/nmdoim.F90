subroutine nmdoim(sdimpr)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/obsetb.h"
#include "asterfort/obseti.h"
    character(len=24) :: sdimpr
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (STRUCTURES DE DONNEES)
!
! LECTURE DES DONNNES IMPRESSION
!
! ----------------------------------------------------------------------
!
!
! OUT SDIMPR : SD IMPRESSION
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=16) :: motfac, repk
    aster_logical :: ltcvfi, linfre, linftp
    integer :: noc, utcvfi, pasaff
!
! ----------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... LECTURE INFO AFFICHAGE'
    endif
!
! --- INITIALISATIONS
!
    motfac = 'AFFICHAGE'
    linfre = .false.
    linftp = .false.
    utcvfi = 0
    pasaff = 1
    ltcvfi = .false.
!
! --- INFO SUR LES RESIDUS
!
    call getvtx(motfac, 'INFO_RESIDU', iocc=1, scal=repk, nbret=noc)
    if (noc .eq. 0) then
        linfre = .false.
    else
        if (repk .eq. 'OUI') then
            linfre = .true.
        else if (repk.eq.'NON') then
            linfre = .false.
        else
            ASSERT(.false.)
        endif
    endif
!
! --- INFO SUR LE TEMPS
!
    call getvtx(motfac, 'INFO_TEMPS', iocc=1, scal=repk, nbret=noc)
    if (noc .eq. 0) then
        linftp = .false.
    else
        if (repk .eq. 'OUI') then
            linftp = .true.
        else if (repk.eq.'NON') then
            linftp = .false.
        else
            ASSERT(.false.)
        endif
    endif
!
! --- SORTIE TABLEAU CONVERGENCE DANS FICHIER CSV ?
!
    call getvis(motfac, 'UNITE', iocc=1, scal=utcvfi, nbret=noc)
    if (noc .eq. 0) then
        ltcvfi = .false.
    else
        if (utcvfi .eq. 0) then
            ltcvfi = .false.
        else
            ltcvfi = .true.
        endif
    endif
!
! --- FREQUENCE DE REACTUALISATION DE L'AFFICHAGE
!
    call getvis(motfac, 'PAS', iocc=1, scal=pasaff, nbret=noc)
    if (noc .eq. 0) then
        pasaff = 1
    endif
!
! --- SAUVEGARDE DONNEES
!
    call obsetb(sdimpr, 'INFO_RESIDU', linfre)
    call obsetb(sdimpr, 'INFO_TEMPS ', linftp)
    call obsetb(sdimpr, 'TABL_CONV_CSV', ltcvfi)
    call obseti(sdimpr, 'UNIT_CONV_CSV', utcvfi)
    call obseti(sdimpr, 'REAC_AFFICHAGE', pasaff)
!
end subroutine

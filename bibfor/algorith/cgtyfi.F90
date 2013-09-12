subroutine cgtyfi(typfis, nomfis)
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=8) :: typfis, nomfis
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : DETERMINATION DU TYPE ET DU NOM DE LA SD DECRIVANT LE
!           FOND DE FISSURE
!
! OUT :
!   TYPFIS : TYPE DE LA SD DECRIVANT LE FOND DE FISSURE
!            ('THETA' OU 'FONDIFSS' OU 'FISSURE')
!   NOMFIS : NOM DE LA SD DECRIVANT LE FOND DE FISSURE
! ======================================================================
!
    integer :: iarg, ithet, ifond, ifiss
!
    call jemarq()
!
    call getvid('THETA', 'THETA', iocc=1, scal=nomfis, nbret=ithet)
    call getvid('THETA', 'FOND_FISS', iocc=1, scal=nomfis, nbret=ifond)
    call getvid('THETA', 'FISSURE', iocc=1, scal=nomfis, nbret=ifiss)
!
    ASSERT(ithet.eq.0.or.ithet.eq.1)
    ASSERT(ifond.eq.0.or.ifond.eq.1)
    ASSERT(ifiss.eq.0.or.ifiss.eq.1)
!
!     NORMALEMENT, CETTE REGLE D'EXCLUSION EST VERIFIEE DANS LE CAPY
    ASSERT(ithet+ifond+ifiss.eq.1)
!
    if (ithet .eq. 1) then
!
        typfis='THETA'
!
    else if (ifond.eq.1) then
!
        typfis='FONDFISS'
!
    else if (ifiss.eq.1) then
!
        typfis='FISSURE'
!
    endif
!
    call jedema()
!
end subroutine

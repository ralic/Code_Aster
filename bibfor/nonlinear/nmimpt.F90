subroutine nmimpt(numins, sddisc, sdimpr)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/diinst.h"
#include "asterfort/dinins.h"
#include "asterfort/nmimcr.h"
#include "asterfort/nmimen.h"
#include "asterfort/nmimr0.h"
#include "asterfort/obgetb.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesr.h"
#include "asterfort/utdidt.h"
    integer :: numins
    character(len=24) :: sdimpr
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (AFFICHAGE)
!
! AFFICHAGE ENTETE
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO INSTANT COURANT
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: r8bid
    real(kind=8) :: instan
    integer :: lenivo
    character(len=16) :: metlis
    integer :: ibid
    logical :: lprint
!
! ----------------------------------------------------------------------
!
!
! --- METHODE DE GESTION DE LA LISTE D'INSTANTS
!
    call utdidt('L', sddisc, 'LIST', ibid, 'METHODE',&
                r8bid, ibid, metlis)
    if (metlis .eq. 'MANUEL') then
        lenivo = dinins(sddisc,numins)
    else if (metlis.eq.'AUTO') then
        lenivo = 0
    else
        ASSERT(.false.)
    endif
!
! --- INSTANT COURANT
!
    instan = diinst(sddisc,numins)
    call nmimcr(sdimpr, 'INCR_INST', instan, .true.)
    call nmimr0(sdimpr, 'INST')
!
! --- AFFICHAGE ACTIF ?
!
    call obgetb(sdimpr, 'PRINT', lprint)
!
! --- AFFICHAGE ENTETE
!
    if (lprint) then
        if (lenivo .eq. 0) then
            call u2mesr('I', 'MECANONLINE6_6', 1, instan)
        else
            call u2mesg('I', 'MECANONLINE6_1', 0, ' ', 1,&
                        lenivo, 1, instan)
        endif
    endif
!
! --- AFFICHAGE TITRE DES COLONNES
!
    call nmimen(sdimpr)
!
end subroutine

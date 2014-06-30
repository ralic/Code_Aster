subroutine nmobsv(noma, sddisc, sdieto, sdobse, numins)
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
#include "asterfort/diinst.h"
#include "asterfort/lobs.h"
#include "asterfort/nmobse.h"
    integer :: numins
    character(len=8) :: noma
    character(len=24) :: sdieto
    character(len=19) :: sddisc, sdobse
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! REALISER UNE OBSERVATION
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  SDIETO : SD GESTION IN ET OUT
! IN  SDOBSE : SD OBSERVATION
! IN  NUMINS : NUMERO DE L'INSTANT COURANT
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: inst
    logical(kind=1) :: lobsv
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    lobsv = .false.
!
! --- TEMPS COURANT
!
    inst = diinst(sddisc,numins)
!
! --- DOIT-ON FAIRE UNE OBSERVATION  ?
!
    call lobs(sdobse, numins, inst, lobsv)
!
! --- AU MOINS UNE OBSERVATION
!
    if (lobsv) then
        call nmobse(noma, sdieto, sdobse, inst)
    endif
!
end subroutine

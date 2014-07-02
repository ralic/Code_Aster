subroutine nmfinp(sddisc, numins, lstop)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/didern.h"
#include "asterfort/diinst.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmjalo.h"
#include "asterfort/utdidt.h"
    character(len=19) :: sddisc
    integer :: numins
    aster_logical :: lstop
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! FIN DU TRANSITOIRE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  LSTOP  : .TRUE. SI DERNIER INSTANT DE LA LISTE
!
!
!
!
    integer :: ibid
    real(kind=8) :: r8bid, prec, jalon
    real(kind=8) :: inst
    character(len=16) :: metlis
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lstop = .false.
    inst = diinst(sddisc,numins)
!
! --- PRECISION SUR LES INSTANTS
! --- (LIEE A CELLE DE VAL_MIN DE PAS_MINI DANS DEFI_LIST_INST.CAPY)
!
    prec = 1.d-12
!
! --- METHODE DE GESTION DE LA LISTE D'INSTANTS
!
    call utdidt('L', sddisc, 'LIST', ibid, 'METHODE',&
                r8bid, ibid, metlis)
!
! --- CONVERGENCE DU CALCUL: DERNIER PAS !
!
    if (didern(sddisc,numins)) lstop = .true.
!
! --- CONVERGENCE DU CALCUL: CAS LISTE AUTOMATIQUE
!
    if (metlis .eq. 'AUTO') then
        call nmjalo(sddisc, inst, prec, jalon)
        if (jalon .eq. r8vide()) lstop = .true.
    endif
!
    call jedema()
end subroutine

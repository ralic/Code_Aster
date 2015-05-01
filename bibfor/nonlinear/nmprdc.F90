subroutine nmprdc(method, numedd, depmoi, sddisc, numins,&
                  incest, depest)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsinch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcopy.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    character(len=16) :: method(*)
    character(len=19) :: depmoi, depest
    character(len=24) :: numedd
    character(len=19) :: sddisc, incest
    integer :: numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PREDICTION)
!
! PREDICTION PAR DEPLACEMENT CALCULE
!
! ----------------------------------------------------------------------
!
!
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION
! IN  NUMEDD : NUME_DDL
! IN  NUMINS : NUMERO INSTANT COURANT
! IN  SDDISC : SD DISC_INST
! IN  DEPMOI : DEPL. EN T-
! OUT INCEST : INCREMENT DE DEPLACEMENT EN PREDICTION
! OUT DEPEST : DEPLACEMENT ESTIME
!
!
!
!
    integer :: ifm, niv
    integer ::    neq
    integer :: iret, ibid
    real(kind=8) :: instan
    character(len=19) :: deplu
    real(kind=8), pointer :: depes(:) => null()
    real(kind=8), pointer :: depm(:) => null()
    real(kind=8), pointer :: inces(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... PAR DEPL. CALCULE'
    endif
!
! --- INITIALISATIONS
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    instan = diinst(sddisc,numins )
!
! --- INITIALISATIONS
!
    deplu = '&&NMPRDC.DEPEST'
!
! --- LECTURE DANS LE CONCEPT EVOL_NOLI
!
    call rsinch(method(6)(1:8), 'DEPL', 'INST', instan, deplu,&
                'EXCLU', 'EXCLU', 0, 'V', iret)
    if (iret .gt. 0) then
        call utmess('F', 'MECANONLINE2_27', sk=method(6)(1:8), si=ibid, sr=instan)
    endif
!
! --- COPIE DU DEPLACEMENT ESTIME
!
    if (numins .eq. 1) then
        call vtcopy(deplu, depest, 'F', iret)
    else
        call copisd('CHAMP_GD', 'V', deplu, depest)
    endif
!
    call jeveuo(depest(1:19)//'.VALE', 'L', vr=depes)
    call jeveuo(depmoi(1:19)//'.VALE', 'L', vr=depm)
!
! --- INITIALISATION DE L'INCREMENT: INCEST = DEPEST - DEPMOI
!
    call jeveuo(incest(1:19)// '.VALE', 'E', vr=inces)
    call dcopy(neq, depes, 1, inces, 1)
    call daxpy(neq, -1.d0, depm, 1, inces,&
               1)
!
    call jedema()
end subroutine

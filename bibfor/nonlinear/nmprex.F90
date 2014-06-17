subroutine nmprex(numedd, depmoi, solalg, sddisc, numins,&
                  incest, depest)
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
#include "asterfort/nmchex.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
#include "blas/daxpy.h"
    character(len=24) :: numedd
    character(len=19) :: sddisc, incest
    character(len=19) :: solalg(*), depmoi, depest
    integer :: numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PREDICTION)
!
! PREDICTION PAR EXTRAPOLATION
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUME_DDL
! IN  NUMINS : NUMERO INSTANT COURANT
! IN  SDDISC : SD DISC_INST
! IN  DEPMOI : DEPL. EN T-
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! OUT INCEST : INCREMENT DE DEPLACEMENT EN PREDICTION
! OUT DEPEST : DEPLACEMENT ESTIME
!
!
!
!
    integer :: neq
    character(len=19) :: depold
    integer :: ifm, niv
    real(kind=8) :: instam, instap, instaa, coef
    real(kind=8), pointer :: depes(:) => null()
    real(kind=8), pointer :: inces(:) => null()
    real(kind=8), pointer :: old(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... PAR EXTRAPOLATION'
    endif
!
! --- INITIALISATIONS
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    instam = diinst(sddisc,numins-1)
    instap = diinst(sddisc,numins )
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(solalg, 'SOLALG', 'DEPOLD', depold)
!
! --- INITIALISATION DU DEPLACEMENT ESTIME
!
    call copisd('CHAMP_GD', 'V', depmoi, depest)
    call jeveuo(depest(1:19)//'.VALE', 'E', vr=depes)
!
! --- INITIALISATION DE L'INCREMENT
!
    call jeveuo(incest(1:19)//'.VALE', 'E', vr=inces)
    call r8inir(neq, 0.d0, inces, 1)
!
! --- EXTRAPOLATION DES DEPLACEMENTS S'IL EXISTE UN PAS PRECEDENT
!
    if (numins .ge. 2) then
        instaa = diinst(sddisc,numins-2)
        if (instaa .eq. instam) then
            call utmess('F', 'ALGORITH8_28')
        endif
        coef = (instap-instam) / (instam-instaa)
        call jeveuo(depold(1:19)// '.VALE', 'L', vr=old)
        call daxpy(neq, coef, old, 1, depes,&
                   1)
        call daxpy(neq, coef, old, 1, inces,&
                   1)
    endif
!
    call jedema()
end subroutine

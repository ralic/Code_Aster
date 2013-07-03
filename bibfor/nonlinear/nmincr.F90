subroutine nmincr(sddyna, ddincr, coedep, coevit, coeacc,&
                  dddepl, ddvite, ddacce)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynin.h"
#include "asterfort/ndynlo.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
    character(len=19) :: sddyna
    real(kind=8) :: coedep, coevit, coeacc
    character(len=19) :: ddincr
    character(len=19) :: dddepl, ddvite, ddacce
!
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL)
!
! CONVERSION INCREMENT dINCR VENANT DE K.dINCR = F EN
! dU,dV,dA  SUIVANT SCHEMAS
!
! ----------------------------------------------------------------------
!
! dU = COEDEP.DINCR
! dV = COEVIT.DINCR
! dA = COEACC.DINCR
!
! EN EXPLICITE PURE, CE NE SONT PAS DES INCREMENTS MAIS DIRECTEMENT
! LA SOLUTION EN N+1
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  DDINCR : INCREMENT SOLUTION DE K.DDINCR = F
! IN  COEDEP : COEF. POUR INCREMENT DEPLACEMENT
! IN  COEVIT : COEF. POUR INCREMENT VITESSE
! IN  COEACC : COEF. POUR INCREMENT ACCELERATION
! OUT DDDEPL : INCREMENT DEPLACEMENT
! OUT DDVITE : INCREMENT VITESSE
! OUT DDACCE : INCREMENT ACCELERATION
!
!
!
!
    logical :: lstat, ldyna
    logical :: ldepl, lvite, lacce
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVEES
!
    lstat = ndynlo(sddyna,'STATIQUE')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
!
! --- TYPE DE FORMULATION SCHEMA DYNAMIQUE GENERAL
!
    if (lstat) then
        ldepl = .true.
        lvite = .false.
        lacce = .false.
    else if (ldyna) then
        ldepl = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.1
        lvite = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.2
        lacce = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.3
    else
        call assert(.false.)
    endif
!
! --- CALCUL DES INCREMENTS
!
    if (ldepl) then
        call copisd('CHAMP_GD', 'V', ddincr, dddepl)
        if (coevit .ne. 0.d0) then
            call vtzero(ddvite)
            call vtaxpy(coevit, ddincr, ddvite)
        endif
        if (coeacc .ne. 0.d0) then
            call vtzero(ddacce)
            call vtaxpy(coeacc, ddincr, ddacce)
        endif
    else if (lvite) then
        call copisd('CHAMP_GD', 'V', ddincr, ddvite)
        call vtzero(dddepl)
        call vtzero(ddacce)
        call vtaxpy(coedep, ddincr, dddepl)
        call vtaxpy(coeacc, ddincr, ddacce)
!
    else if (lacce) then
        call copisd('CHAMP_GD', 'V', ddincr, ddacce)
        call vtzero(dddepl)
        call vtzero(ddvite)
        call vtaxpy(coedep, ddincr, dddepl)
        call vtaxpy(coevit, ddincr, ddvite)
!
    else
        call assert(.false.)
    endif
!
!
    call jedema()
end subroutine

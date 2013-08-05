subroutine nmamod(phase, numedd, sddyna, vitplu, vitkm1,&
                  cnamom)
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
#include "asterfort/dismoi.h"
#include "asterfort/fmodam.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynkk.h"
#include "asterfort/ndynlo.h"
    character(len=4) :: phase
    character(len=19) :: vitplu, vitkm1
    character(len=19) :: sddyna
    character(len=24) :: numedd, cnamom
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE)
!
! CALCUL DES FORCES D'AMORTISSEMENT MODAL
!
! ----------------------------------------------------------------------
!
!
! IN  PHASE  : PHASE DE CALCUL (PREDICTION OU CORRECTION)
! IN  VITPLU : VITESSE COURANTE
! IN  VITKM1 : VITESSE ITERATION NEWTON PRECEDENTE
! IN  NUMEDD : NUME_DDL
! IN  SDDYNA : SD DYNAMIQUE
! OUT CNAMOM : VECT_ASSE FORCES AMORTISSEMENT MODAL
!
!
!
!
    integer :: jvitp, jvitkm, jmoda
    character(len=24) :: valmod, basmod
    character(len=19) :: sdammo
    logical :: nreavi
    integer :: neq, iret
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
    call ndynkk(sddyna, 'SDAMMO', sdammo)
    valmod = sdammo(1:19)//'.VALM'
    basmod = sdammo(1:19)//'.BASM'
    nreavi = ndynlo(sddyna,'NREAVI')
!
! --- ACCES OBJETS JEVEUX
!
    call jeveuo(vitplu(1:19)//'.VALE', 'L', jvitp)
    call jeveuo(vitkm1(1:19)//'.VALE', 'L', jvitkm)
    call jeveuo(cnamom(1:19)//'.VALE', 'E', jmoda)
!
! --- CALCUL FORCES MODALES
!
    if (phase .eq. 'PRED') then
        call fmodam(neq, zr(jvitkm), valmod, basmod, zr(jmoda))
    else if (phase.eq.'CORR') then
        call fmodam(neq, zr(jvitp), valmod, basmod, zr(jmoda))
        if (nreavi) then
            call fmodam(neq, zr(jvitp), valmod, basmod, zr(jmoda))
        endif
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine

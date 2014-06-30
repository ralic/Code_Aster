subroutine nmhyst(amort, vitplu, cnhyst)
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
    character(len=19) :: amort
    character(len=19) :: vitplu, cnhyst
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE)
!
! CALCUL DES FORCES D'AMORTISSEMENT HYSTERETIQUE
!
! ----------------------------------------------------------------------
!
!
! IN  VITPLU : VITESSE COURANTE
! IN  AMORT  : MATR_ASSE AMORTISSEMENT
! OUT CNHYST : VECT_ASSE FORCES AMORTISSEMENT
!
!
!
!
    integer ::  jamor
    real(kind=8), pointer :: hyst(:) => null()
    real(kind=8), pointer :: vitp(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES OBJETS JEVEUX
!
    call jeveuo(vitplu(1:19)//'.VALE', 'L', vr=vitp)
    call jeveuo(amort(1:19) //'.&INT', 'L', jamor)
    call jeveuo(cnhyst(1:19)//'.VALE', 'E', vr=hyst)
!
! --- CALCUL FORCES AMORTISSEMENT
!
    call mrmult('ZERO', jamor, vitp, hyst, 1,&
                .true._1)
!
    call jedema()
end subroutine

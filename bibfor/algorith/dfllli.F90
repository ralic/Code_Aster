subroutine dfllli(lisins, dtmin, nbinst)
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
#include "asterc/r8maem.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=19) :: lisins
    real(kind=8) :: dtmin
    integer :: nbinst
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION LISTE INSTANTS
!
! VERIFICATIONS LISTE D'INSTANTS
!
! ----------------------------------------------------------------------
!
!
! IN  LISINS : NOM DE LA SD LISTE
! OUT DTMIN  : PAS DE TEMPS MINIMUM DE LA LISTE
! OUT NBINST : NOMBRE D'INSTANTS
!
!
!
!
    integer :: i
    real(kind=8) :: deltat
    real(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    dtmin = r8maem()
!
! --- RECUPERATION DE LA LISTE D'INSTANTS FOURNIE
!
    call jeveuo(lisins//'.VALE', 'L', vr=vale)
    call jelira(lisins//'.VALE', 'LONMAX', nbinst)
!
! --- VERIFICATION IL Y A AU MOINS UN INSTANT DE CALCUL
!
    if (nbinst .lt. 2) then
        call utmess('F', 'DISCRETISATION_86')
    endif
!
! --- INTERVALLE DE TEMPS MINIMAL : DTMIN
!
    do 30 i = 1, nbinst-1
        deltat = vale(1+i) - vale(i)
        dtmin = min(deltat,dtmin)
30  end do
!
! --- VERIFICATION DE LA STRICTE CROISSANCE DE LA LISTE D'INSTANTS
!
    if (dtmin .le. 0.d0) then
        call utmess('F', 'DISCRETISATION_87')
    endif
!
    call jedema()
end subroutine

function didern(sddisc, numins)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
    aster_logical :: didern
    integer :: numins
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE - DISCRETISATION)
!
! RETOURNE VRAI SI ON SORT DE LA LISTE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! IN  NUMINS : NUMERO DE L'INSTANT
! OUT DIINST : VRAI SI ON SORT DE LA LISTE D'INSTANT
!
! SI LE CALCUL N'EST PAS UN TRANSITOIRE -> TPSDIT N'EXISTE PAS ET ON
! SORT DE LA LISTE TOUT DE SUITE (LE CALCUL EST TERMINE)
!
!
!
!
    integer :: nbtemp, iret
    character(len=24) :: tpsdit
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD LISTE D'INSTANTS
!
    tpsdit = sddisc(1:19)//'.DITR'
    call jeexin(tpsdit, iret)
    if (iret .eq. 0) then
        didern = .true.
    else
        call jelira(tpsdit, 'LONMAX', nbtemp)
        nbtemp = nbtemp - 1
        didern = numins .eq. nbtemp
    endif
!
    call jedema()
end function

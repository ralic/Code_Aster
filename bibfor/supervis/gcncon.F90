subroutine gcncon(type, result)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=1) :: type
    character(len=*) :: result
!     ------------------------------------------------------------------
!     ATTRIBUTION D'UN NOM DE CONCEPT UNIQUE MEME EN POURSUITE
!     ------------------------------------------------------------------
! IN  TYPE   : K1 : TYPE DE CONCEPT PARMI
!                   '.' : LE CONCEPT EST DETRUIT A LA FIN DE L'EXECUTION
!                   '_' : LE CONCEPT EST CONSERVE POUR UNE POURSUITE
! OUT RESULT : K8 : NOM UNIQUE = TYPE//NUMERO_UNIQUE
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     --- VARIABLES LOCALES --------------------------------------------
    integer :: ipos
    integer :: ier
    character(len=24) :: numuni
    character(len=8) :: nomuni
!     ------------------------------------------------------------------
    call jemarq()
    numuni='&&_NUM_CONCEPT_UNIQUE'
!     ------------------------------------------------------------------
    if ((type.eq.'.') .or. (type.eq.'_') .or. (type.eq.'S')) then
        call jeexin(numuni, ier)
        if (ier .eq. 0) then
!           INITIALISATION D'UN NUM POUR CREER UN NOM DE CONCEPT UNIQUE
            call wkvect(numuni, 'G V I', 1, ipos)
            zi(ipos)=0
        endif
!        RECUPERATION, FORMATTAGE ET INCREMENTATION
        call jeveuo(numuni, 'E', ipos)
        write (nomuni,'(A,I7.7)') type,zi(ipos)
        result=nomuni
        zi(ipos)=zi(ipos)+1
        call assert(zi(ipos) .lt. 10000000)
    else
        call u2mesk('F', 'SUPERVIS_8', 1, type)
    endif
    call jedema()
end subroutine

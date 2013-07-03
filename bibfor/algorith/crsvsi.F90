subroutine crsvsi(solveu)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
    character(len=19) :: solveu
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES)
!
! ACTIVATION DE STOP_SINGULIER = 'NON' EN CAS DE DECOUPE DU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  SOLVEU  : NOM SD SOLVEUR
!
!
!
!
    integer :: islvi, islvk
    integer :: nprec
    character(len=24) :: nomslv
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call jeveuo(solveu//'.SLVK', 'L', islvk)
    call jeveuo(solveu//'.SLVI', 'E', islvi)
    nomslv = zk24(islvk-1+1)
    nprec = zi(islvi-1+1)
!
    if ((nomslv.eq.'MUMPS' ) .or. (nomslv.eq.'MULT_FRONT') .or. (nomslv.eq.'LDLT' )) then
        if (nprec .gt. 0) then
            zi(islvi-1+3) = 2
        else
            call u2mesk('I', 'DISCRETISATION_40', 1, nomslv)
        endif
    else
        call u2mesk('I', 'DISCRETISATION_40', 1, nomslv)
    endif
!
    call jedema()
end subroutine

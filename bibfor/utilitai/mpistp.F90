subroutine mpistp(imode)
! person_in_charge: mathieu.courtois at edf.fr
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
!     ARGUMENT IN
#include "asterf_config.h"
#include "asterfort/assert.h"
#include "asterfort/gtstat.h"
#include "asterfort/jefini.h"
#include "asterfort/onerrf.h"
#include "asterfort/ststat.h"
#include "asterfort/u2mess.h"
    integer :: imode
!-----------------------------------------------------------------------
!     FONCTION REALISEE : MPI COMM STOP
!     UN PROC DEMANDE AUX AUTRES PROCESSEURS D'INTERROMPRE.
!
!     - IMODE = 1, UN DES PROCESSEURS N'A PAS REPONDU DANS LE DELAI,
!       L'EXECUTION EST INTERROMPUE PAR MPI_ABORT
!
!     - IMODE = 2, TOUS LES PROCS SE SYNCHRONISENT ET TERMINENT AVEC
!       LE MEME UTMESS('S').
!       NE DEVRAIT PAS ETRE APPELE EN SEQUENTIEL.
!
!     L'APPELANT NE DEVRAIT RIEN EXECUTER APRES L'APPEL A MPISTP.
!-----------------------------------------------------------------------
!
#include "aster_constant.h"
    integer :: lout, imod2
    character(len=16) :: compex
    logical :: labort
!
    call ststat(ST_ER_OTH)
    labort = .not. gtstat(ST_EXCEPT)
!
!     S'IL FAUT FAIRE UN ABORT, ON FORCE IMODE=1
    imod2 = imode
    if (labort) then
        call onerrf(' ', compex, lout)
        if (compex(1:lout) .eq. 'ABORT') then
            imod2 = 1
        endif
    endif
!
    if (imod2 .eq. 1) then
#ifdef _USE_MPI
        call u2mess('D', 'APPELMPI_99')
#endif
        call jefini('ERREUR')
!
    else if (imod2 .eq. 2) then
        if (labort) then
            call u2mess('M', 'APPELMPI_95')
        endif
!
    else
        call assert(.false.)
    endif
!
end subroutine

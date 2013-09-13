subroutine nmobno(sdobse, motfac, nbocc)
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
#include "asterfort/getvtx.h"
#include "asterfort/impfoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    integer :: nbocc
    character(len=19) :: sdobse
    character(len=16) :: motfac
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - OBSERVATION)
!
! LECTURE NOM DES OBSERVATION
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-FACTEUR POUR LIRE
! IN  SDOBSE : NOM DE LA SD POUR OBSERVATION
! IN  NBOCC  : NOMBRE D'OCCURRENCES DE MOTFAC
!
! ----------------------------------------------------------------------
!
    integer :: iocc, nbtit, ibid
    character(len=24) :: obsnom
    integer :: jobsno
    character(len=80) :: titobs
    character(len=1) :: chaine
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- SD POUR SAUVER LES TITRES
!
    obsnom = sdobse(1:14)//'     .TITR'
    call wkvect(obsnom, 'V V K80', nbocc, jobsno)
!
    do 10 iocc = 1, nbocc
        call impfoi(0, 1, iocc, chaine)
        titobs = 'OBSERVATION_'//chaine
        call getvtx(motfac, 'TITRE', iocc=iocc, nbval=0, nbret=nbtit)
        nbtit = - nbtit
        ASSERT(nbtit.le.1)
        if (nbtit .ne. 0) then
            call getvtx(motfac, 'TITRE', iocc=iocc, nbval=nbtit, vect=titobs,&
                        nbret=ibid)
        endif
        zk80(jobsno+iocc-1) = titobs
10  end do
!
    call jedema()
end subroutine

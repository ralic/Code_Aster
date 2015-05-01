subroutine nmarex(motfac, sdarch)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=19) :: sdarch
    character(len=16) :: motfac
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (ARCHIVAGE)
!
! CONSTRUCTION CHAMPS EXCLUS DE L'ARCHIVAGE
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-FACTEUR POUR LIRE <CHAM_EXCL>
! IN  SDARCH : NOM DE LA SD ARCHIVAGE
!
!
!
!
    integer :: ibid
    integer :: nb
    character(len=24) :: arcexc
    integer :: jarexc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOM DE LA SD
!
    arcexc = sdarch(1:19)//'.AEXC'
!
! --- CONSTRUCTION CHAMPS EXCLUS DE L'ARCHIVAGE
!
    call getvtx(motfac, 'CHAM_EXCLU', iocc=1, nbval=0, nbret=nb)
    nb = -nb
    if (nb .ne. 0) then
        call wkvect(arcexc, 'V V K16', nb, jarexc)
        call getvtx(motfac, 'CHAM_EXCLU', iocc=1, nbval=nb, vect=zk16(jarexc),&
                    nbret=ibid)
    endif
!
    call jedema()
!
end subroutine

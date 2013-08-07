subroutine cfverl(defico, resoco)
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
    implicit      none
#include "jeveux.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesi.h"
    character(len=24) :: resoco, defico
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES)
!
! VERIFICATION FACETTISATION
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
!
!
!
    character(len=8) :: nomnoe
    integer :: nnoeu, ier
    integer :: ino, nbno
    real(kind=8) :: angle
    character(len=19) :: sdappa
    character(len=24) :: apverk, apvera
    integer :: jlistn, jlista
    logical :: lliss
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    sdappa = resoco(1:14)//'.APPA'
    call jeexin(sdappa(1:19)//'.VERK', ier)
    lliss = cfdisl(defico,'LISSAGE')
!
! --- SD VERIFICATION FACETTISATION
!
    apverk = sdappa(1:19)//'.VERK'
    apvera = sdappa(1:19)//'.VERA'
    call jeveuo(apverk, 'L', jlistn)
    call jeveuo(apvera, 'L', jlista)
    call jelira(apverk, 'LONUTI', ival=nnoeu)
    call jelira(apverk, 'LONMAX', ival=nbno)
    if ((nnoeu.eq.0) .or. (lliss)) goto 999
!
    call u2mesi('I', 'CONTACT3_19', 1, nnoeu)
!
    do 10 ino = 1, nbno
        nomnoe = zk8(jlistn+ino-1)
        angle = zr(jlista+ino-1)
        if (nomnoe .ne. ' ') then
            write(6,1000) nomnoe,angle
        endif
10  end do
!
    1000 format (a8,3x,f8.2)
!
999  continue
!
    call jedema()
end subroutine

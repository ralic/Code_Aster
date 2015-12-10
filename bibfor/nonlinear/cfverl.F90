subroutine cfverl(ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES)
!
! VERIFICATION FACETTISATION
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
!
! ----------------------------------------------------------------------
!
    character(len=8) :: nomnoe
    integer :: nnoeu, ier
    integer :: ino, nbno
    real(kind=8) :: angle
    character(len=19) :: sdappa
    character(len=24) :: apverk, apvera
    integer :: jlistn, jlista
    aster_logical :: lliss
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
    call jeexin(sdappa(1:19)//'.VERK', ier)
    lliss = cfdisl(ds_contact%sdcont_defi,'LISSAGE')
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
    call utmess('I', 'CONTACT3_19', si=nnoeu)
!
    do ino = 1, nbno
        nomnoe = zk8(jlistn+ino-1)
        angle = zr(jlista+ino-1)
        if (nomnoe .ne. ' ') then
            write(6,100) nomnoe,angle
        endif
    end do
!
100 format (a8,3x,f8.2)
!
999 continue
!
    call jedema()
end subroutine

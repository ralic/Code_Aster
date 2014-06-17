subroutine nmiret(codret, tabret)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/sdmpic.h"
#include "asterfort/utmess.h"
    logical :: tabret(0:10)
    character(len=19) :: codret
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! RESUME LES CODES RETOURS DES TE
!
! ----------------------------------------------------------------------
!
!
! IN  CODRET  : CHAM_ELEM ISSU DES TE
! OUT TABRET  : TABRET(0) = .TRUE. UN CODE RETOUR NON NUL EXISTE
!                TABRET(I) = .TRUE. CODE RETOUR I RENCONTRE
!                             SINON .FALSE.
!                I VALANT DE 1 A 10
!
!
!
!
    integer :: iret,  jcesd,  jcesl, nbmail, icmp
    integer :: ima, iad, vali
    character(len=8) :: nomgd
    character(len=19) :: chamns
    integer, pointer :: cesv(:) => null()
    character(len=8), pointer :: cesk(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    do 10 iret = 0, 10
        tabret(iret) = .false.
10  end do
!
! --- ON TRANSFORME LE "CHAM_ELEM" EN UN "CHAM_ELEM_S"
!
    chamns = '&&NMIRET.CHAMNS'
!
!     -- EN ATTENDANT DE FAIRE MIEUX, POUR PERMETTRE MUMPS/DISTRIBUE :
    call sdmpic('CHAM_ELEM', codret)
!
    call celces(codret, 'V', chamns)
!
! --- ACCES AU CHAM_ELEM_S
!
    call jeveuo(chamns//'.CESK', 'L', vk8=cesk)
    call jeveuo(chamns//'.CESD', 'L', jcesd)
    call jeveuo(chamns//'.CESV', 'L', vi=cesv)
    call jeveuo(chamns//'.CESL', 'L', jcesl)
!
!     CHAM_ELEM/ELGA MAIS EN FAIT : 1 POINT ET 1 SOUS_POINT PAR ELEMENT
    if ((zi(jcesd-1+3).ne.1) .or. (zi(jcesd-1+4).ne.1)) then
        ASSERT(.false.)
    endif
!
    nomgd = cesk(2)
    if (nomgd .ne. 'CODE_I') then
        ASSERT(.false.)
    endif
!
    nbmail = zi(jcesd-1+1)
    icmp = zi(jcesd-1+2)
    if (icmp .ne. 1) then
        ASSERT(.false.)
    endif
!
    do 20 ima = 1, nbmail
!
        call cesexi('C', jcesd, jcesl, ima, 1,&
                    1, icmp, iad)
        if (iad .le. 0) goto 20
!
        iret = cesv(iad)
        if (iret .eq. 0) then
        else if (iret .lt. 11 .and. iret .gt. 0) then
            tabret(iret) = .true.
        else
            vali = iret
            call utmess('A', 'MECANONLINE2_67', si=vali)
        endif
!
20  end do
!
    do 30 iret = 1, 10
        if (tabret(iret)) tabret(0) = .true.
30  end do
!
    call detrsd('CHAM_ELEM_S', chamns)
!
    call jedema()
end subroutine

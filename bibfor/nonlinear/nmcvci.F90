subroutine nmcvci(charge, infoch, fomult, numedd, depmoi,&
                  instap, cncine)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
!
!
! BUT : CALCULER LE CHAM_NO CNCINE QUI CONTIENT  L'INCREMENT DE
!       DEPLACEMENT IMPOSE PAR LES CHARGES CINEMATIQUES.
!       POUR CELA, ON FAIT LA DIFFERENCE ENTRE LES INSTANTS "+" ET "-"
!       MAIS POUR L'INSTANT "-", IL FAUT PARTIR DU "VRAI" CHAMP
!       DE DEPLACEMENT.
!----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/ascavc.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/vtcmbl.h"
#include "asterfort/vtcreb.h"
    character(len=24) :: charge, infoch, fomult, numedd, cncine
    character(len=19) :: depmoi
    character(len=24) :: l2cnci(2), cncinm, cncinp
    character(len=8) :: char1
    real(kind=8) :: instap, coefr(2)
    integer ::  neq, ieq, neq2,  iret, j1, jinfc, ichar
    integer :: nbchar, iexi, jlchar
    character(len=1) :: typch(2)
    logical :: lvcine
    integer, pointer :: dlci(:) => null()
    real(kind=8), pointer :: cncim(:) => null()
!----------------------------------------------------------------------
!
    call jemarq()
!
!     -- CREATION DE CNCINE = 0. PARTOUT :
!     --------------------------------------
    call exisd('CHAMP_GD', cncine, iret)
    if (iret .eq. 0) call vtcreb(cncine, numedd, 'V', 'R', neq)
    call jelira(cncine(1:19)//'.VALE', 'LONMAX', ival=neq)
    call jelira(depmoi(1:19)//'.VALE', 'LONMAX', ival=neq2)
    ASSERT(neq.eq.neq2)
    call jeveuo(cncine(1:19)//'.VALE', 'E', j1)
    do 2, ieq=1,neq
    zr(j1-1+ieq)=0.d0
    2 end do
!
!
!     -- Y-A-T-IL DES CHARGES CINEMATIQUES ?
!     -----------------------------------------------------------------
    lvcine=.false.
    call jeveuo(infoch, 'L', jinfc)
    do 10 ichar = 1, zi(jinfc)
        if (zi(jinfc+ichar) .lt. 0) lvcine=.true.
10  end do
!
!     -- Y-A-T-IL DES CHARGES CONTENANT DES CHARGES CINEMATIQUES ?
!     -----------------------------------------------------------------
    call jeveuo(charge, 'L', jlchar)
    call jelira(charge, 'LONMAX', ival=nbchar)
    do 11 ichar = 1, nbchar
        char1=zk24(jlchar-1+ichar)(1:8)
        call jeexin(char1//'.ELIM      .AFCK', iexi)
        if (iexi .gt. 0) lvcine=.true.
11  end do
!
!     -- S'IL N'Y A PAS DE CHARGES CINEMATIQUES, IL N'Y A RIEN A FAIRE:
!     -----------------------------------------------------------------
    if (.not.lvcine) goto 9999
!
!
!     -- S'IL Y A DES CHARGES CINEMATIQUES :
!     -----------------------------------------------------------------
    cncinm='&&NMCHAR.CNCIMM'
    cncinp='&&NMCHAR.CNCIMP'
!
!
!     CALCUL DE UIMP+ :
!     ---------------------
    call ascavc(charge, infoch, fomult, numedd, instap,&
                cncinp)
    call jeveuo(cncinp(1:19)//'.DLCI', 'L', vi=dlci)
!
!
!     CALCUL DE UIMP- : C'EST U- LA OU ON IMPOSE LE DEPLACEMENT
!                       ET 0. AILLEURS
!     ---------------------------------------------------------
    call copisd('CHAMP_GD', 'V', depmoi, cncinm)
    call jeveuo(cncinm(1:19)//'.VALE', 'E', vr=cncim)
    do 1, ieq=1,neq
    if (dlci(ieq) .eq. 0) then
        cncim(ieq)=0.d0
    endif
    1 end do
!
!     DIFFERENCE UIMP+ - UIMP- :
!     ---------------------------
    coefr(1)=-1.d0
    coefr(2)=+1.d0
    l2cnci(1)=cncinm
    l2cnci(2)=cncinp
    typch(1)='R'
    typch(2)='R'
    call vtcmbl(2, typch, coefr, typch, l2cnci,&
                typch(1), cncine)
!
!     MENAGE :
!     ---------
    call detrsd('CHAM_NO', cncinm)
    call detrsd('CHAM_NO', cncinp)
!
9999  continue
    call jedema()
!
end subroutine

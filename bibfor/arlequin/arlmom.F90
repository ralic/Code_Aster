subroutine arlmom(mailar,modarl)

! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================

    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/jexatr.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/typele.h"
#include "asterfort/assert.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedema.h"

!     ARGUMENTS:
!     ----------
    character(len=8) :: mailar,modarl

! ----------------------------------------------------------------------

! ROUTINE ARLEQUIN

! CREATION DU PSEUDO-MODELE

! ----------------------------------------------------------------------


! IN  MAILAR : NOM DU PSEUDO-MAILLAGE
! IN  MODARL : NOM DU PSEUDO-MODELE

    character(len=24) :: modmai
    integer :: jmoma
    integer :: jdime
    integer :: nbma
    character(len=19) :: ligarl
    integer :: igrel,iel,ima,nute,nbelgr
    integer :: ialiel,illiel,iaux1

! ----------------------------------------------------------------------

    call jemarq()

! --- INFO SUR LE MAILLAGE

    call jeveuo(mailar(1:8)//'.DIME','L',jdime)
    nbma = zi(jdime - 1 + 3)

! --- CREATION DES SDs DE BASE DE MODELE

    modmai = modarl(1:8)//'.MAILLE    '
    call wkvect(modmai,'V V I',nbma,jmoma)

! --- ACCES AU LIGREL

    ligarl = modarl(1:8)//'.MODELE'
    call jeveuo(ligarl//'.LIEL','L',ialiel)
    call jeveuo(jexatr(ligarl//'.LIEL','LONCUM'),'L',illiel)

! --- REMPLISSAGE DE LA SD MODELE//'.MAILLE'

    do 10 igrel = 1,nbgrel(ligarl)
        nute   = typele(ligarl,igrel)
        nbelgr = nbelem(ligarl,igrel)
        iaux1  = ialiel-1+zi(illiel-1+igrel)-1
        do 20 iel = 1,nbelgr
            ima    =  zi(iaux1+iel)
            if (ima > nbma) then
                ASSERT(.false.)
            else
                zi(jmoma+ima-1) = nute
            endif
        20 end do
    10 end do

    call jedetr(modmai)
    call jedema()

end subroutine

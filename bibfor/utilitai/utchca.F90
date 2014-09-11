subroutine utchca(cartez, maz, nomaiz, nocmp, typrez,&
                  valr, vali, valc, ier)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
!
    integer :: vali, ier
    real(kind=8) :: valr
    complex(kind=8) :: valc
    character(len=*) :: cartez, maz, nomaiz, nocmp, typrez
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
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
! ======================================================================
!     EXTRAIRE UNE VALEUR (R/I/C) DANS UNE CARTE
! ----------------------------------------------------------------------
! IN  : CARTEZ : NOM DE LA CARTE DONT ON DESIRE EXTRAIRE 1 COMPOSANTE
! IN  : MAZ  : NOM DU MAILLAGE
! IN  : NOMAIZ : NOM DE LA MAILLE A EXTRAIRE
! IN  : NOCMP  : NOM DU DDL A EXTRAIRE
! IN  : TYPREZ : TYPE DU CHAMP ET DU RESULTAT (R/I/C).
! OUT : VALR   : VALEUR REELLE EXTRAITE
! OUT : VALI   : VALEUR ENTIERE EXTRAITE
! OUT : VALC   : VALEUR COMPLEXE EXTRAITE
! OUT : IER    : CODE RETOUR.
!
! ATTENTION : CETTE ROUTINE EST UN MARTEAU PILON POUR ECRASER UNE MOUCHE
!             ELLE NE DOIT ETRE APPELEE QUE DANS TEST_RESU
! ----------------------------------------------------------------------
!
    integer :: iret, numa, iad1,  jcesd, jcesl
    integer :: jcesv, kcmp, nbcmp
    character(len=1) :: typsca
    character(len=4) :: type
    character(len=19) :: cart19, ces
    character(len=8) :: ma, nomail
    character(len=8), pointer :: cesc(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    ier = 0
    cart19=cartez
    nomail=nomaiz
    ma=maz
    typsca = typrez
!
    call jelira(cart19//'.VALE', 'TYPE', cval=type)
    if (type .ne. 'R' .and. type .ne. 'I' .and. type .ne. 'C') then
        call utmess('E', 'UTILITAI5_29', sk=type)
    endif
    ASSERT(type.eq.typsca)
!
    call jenonu(jexnom(ma//'.NOMMAI', nomail), numa)
    ASSERT(numa.gt.0)
!
!
    ces='&&UTCHCA.CES'
    call carces(cart19, 'ELEM', ' ', 'V', ces,&
                ' ', iret)
    ASSERT(iret.eq.0)
!
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESC', 'L', vk8=cesc)
    call jeveuo(ces//'.CESV', 'L', jcesv)
    call jeveuo(ces//'.CESL', 'L', jcesl)
!
    call jelira(ces//'.CESC', 'LONMAX', nbcmp)
    kcmp = indik8(cesc,nocmp,1,nbcmp)
    if (kcmp .eq. 0) then
        call utmess('F', 'CHAMPS_3', sk=nocmp)
    endif
!
    call cesexi('C', jcesd, jcesl, numa, 1,&
                1, kcmp, iad1)
    if (iad1 .le. 0) then
        call utmess('F', 'CALCULEL3_6', sk=nocmp)
    endif
!
    if (typsca .eq. 'R') then
        valr=zr(jcesv-1+iad1)
    else if (typsca.eq.'C') then
        valc=zc(jcesv-1+iad1)
    else if (typsca.eq.'I') then
        vali=zi(jcesv-1+iad1)
    endif
!
!
    call jedema()
end subroutine

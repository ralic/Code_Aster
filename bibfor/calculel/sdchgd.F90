subroutine sdchgd(champ, tysca)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: champ, tysca
! ----------------------------------------------------------------------
!     BUT: CHANGER LA GRANDEUR ASSOCIEE A UN CHAM_NO/_ELEM
!          EN FAIT CHANGER DANS LE .DESC (OU .CELD)
!          DU CHAMP LE TYPE_SCALAIRE
!          DE LA GRANDEUR:
!         'DEPLA_R' --> 'DEPLA_C' , ...
!
!   ON CHERCHE LE NOM DE LA GRANDEUR ASSOCIEE AU CHAM_NO/_ELEM
!   ON SUPPOSE QU'IL EST DE LA FORME : XXXX_R OU XXXX_C OU XXXX_F
!   ON MODIFIE ALORS LE NUMERO DE LA GRANDEUR POUR SURCHARGER
!   LE SUFFIXE _R _C _F .
!
!     IN:
!       CHAMP (K19): NOM D'UN CHAM_NO/_ELEM
!       TYSCA  (K1) : 'R', 'C' OU 'F'
!
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
    character(len=19) :: nocham
    character(len=8) :: nomgd, nomgd2
    character(len=1) :: tysca2
!
! DEB-------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadesc, ibid, igd, igd2
!-----------------------------------------------------------------------
    call jemarq()
    nocham= champ
    tysca2= tysca
    call jeexin(nocham//'.DESC', ibid)
    if (ibid .gt. 0) then
        call jeveuo(nocham//'.DESC', 'E', iadesc)
    else
        call jeveuo(nocham//'.CELD', 'E', iadesc)
    endif
!
    igd = zi(iadesc-1+1)
    call jenuno(jexnum('&CATA.GD.NOMGD', igd), nomgd)
    nomgd2= nomgd(1:5)//tysca2(1:1)
    call jenonu(jexnom('&CATA.GD.NOMGD', nomgd2), igd2)
    if (igd2 .eq. 0) call u2mesk('F', 'CALCULEL4_79', 1, nomgd2)
    zi(iadesc-1+1)= igd2
!
!
    call jedema()
end subroutine

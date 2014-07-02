subroutine tranma(geomi, d, bidim)
    implicit none
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
!
!     BUT : TRANSLATION D'UN MAILLAGE
!
!     IN :
!            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE A TRANSLATER
!            DIR    : VECTEUR DE TRANSLATION
!     OUT:
!            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE ACTUALISE
!
!
! ----------------------------------------------------------------------
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: n1, i, iadcoo
    aster_logical :: bidim
    character(len=19) :: geomi
    character(len=24) :: coorjv
    real(kind=8) :: d(3)
!
    call jemarq()
    coorjv=geomi(1:19)//'.VALE'
    call jeveuo(coorjv, 'E', iadcoo)
    call jelira(coorjv, 'LONMAX', n1)
    n1=n1/3
    iadcoo=iadcoo-1
    if (bidim) then
        do 10 i = 1, n1
            zr(iadcoo+3*(i-1)+1)=zr(iadcoo+3*(i-1)+1)+d(1)
            zr(iadcoo+3*(i-1)+2)=zr(iadcoo+3*(i-1)+2)+d(2)
 10     continue
    else
        do 20 i = 1, n1
            zr(iadcoo+3*(i-1)+1)=zr(iadcoo+3*(i-1)+1)+d(1)
            zr(iadcoo+3*(i-1)+2)=zr(iadcoo+3*(i-1)+2)+d(2)
            zr(iadcoo+3*(i-1)+3)=zr(iadcoo+3*(i-1)+3)+d(3)
 20     continue
    endif
    call jedema()
end subroutine

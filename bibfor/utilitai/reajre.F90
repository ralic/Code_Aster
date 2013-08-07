subroutine reajre(matelz, resuez, basez)
    implicit none
#include "jeveux.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
#include "asterfort/wkvect.h"
    character(len=*) :: matelz, resuez, basez
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
!
!      BUT: AJOUTER DANS LE MATR_ELEM (OU VECT_ELEM) MATELZ LE
!           RESUELEM RESUEZ
!           SI RESUEZ=' ', ON CREE UN MATR_ELEM (OU VECT_ELEM) VIERGE
!
!     IN  : MATELZ = NOM DE LA SD MATR_ELEM OU VECT_ELEM
!           RESUEZ = NOM DE LE SD RESU_ELEM
!           BASEZ  = BASE DE CREATION ('V' OU 'G')
!
!
!
    integer :: iret, nlmax, nluti, ndim, jrelr, iret1, iret2
    parameter(ndim=10)
    character(len=1) :: base
    character(len=19) :: matele, resuel
!
    call jemarq()
!
    resuel=resuez
    matele=matelz
    base=basez
!
    call jeexin(matele//'.RELR', iret)
!
!     ALLOCATION
    if (iret .eq. 0) then
        call wkvect(matele//'.RELR', base//' V K24', ndim, jrelr)
        call jeecra(matele//'.RELR', 'LONUTI', 0)
    endif
!
!     EXISTENCE DU RESU_ELEM ?
    if (resuel .eq. ' ') goto 9999
!
!     ATTENTION : PARFOIS, RESUEL N'EST PAS UN RESUELEM MAIS UN CHAM_NO
    call exisd('RESUELEM', resuel, iret1)
    call exisd('CHAM_NO', resuel, iret2)
    if ((iret1.eq.0) .and. (iret2.eq.0)) goto 9999
!
!
!     REDIMENSIONNEMENT DE .RELR SI NECESSAIRE :
!     -------------------------------------------
    call jelira(matele//'.RELR', 'LONMAX', nlmax)
    call jelira(matele//'.RELR', 'LONUTI', nluti)
    if (nlmax .eq. nluti) call juveca(matele//'.RELR', nlmax+ndim)
!
!     STOCKAGE :
!     -----------
    call jeveuo(matele//'.RELR', 'E', jrelr)
    zk24(jrelr+nluti) = resuel
    call jeecra(matele//'.RELR', 'LONUTI', nluti+1)
!
9999  continue
!
    call jedema()
!
end subroutine

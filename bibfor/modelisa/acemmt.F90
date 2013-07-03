subroutine acemmt(noma, nmmt)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
    character(len=8) :: noma
    integer :: nmmt(*)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     LECTURE DE MODI_METRIQUE POUR LES TUYAUX
!     REMPLISSAGE DU TABLEAU NMMT CONTENANT POUR CHAQUE MAILLE
!        0 : SI MODI_METRIQUE : NON
!        1 : SI MODI_METRIQUE : OUI
! ----------------------------------------------------------------------
    integer :: nbocpo, iocc, ibid, immt, nbma, jma, i, ima
    character(len=8) :: mmt, typmcl(2)
    character(len=16) :: motfac, motcls(2)
    character(len=24) :: mesmai
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
!
    motfac = 'POUTRE'
    call getfac(motfac, nbocpo)
    if (nbocpo .eq. 0) goto 9999
!
    mesmai = '&&ACEMMT.MES_MAILLES'
    motcls(1) = 'GROUP_MA'
    motcls(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
    do 10 iocc = 1, nbocpo
!
        call getvtx(motfac, 'MODI_METRIQUE', iocc, iarg, 1,&
                    mmt, ibid)
        if (mmt .eq. 'NON') then
            immt = 0
        else if (mmt.eq.'OUI') then
            immt = 1
        endif
!
        call reliem(' ', noma, 'NU_MAILLE', motfac, iocc,&
                    2, motcls, typmcl, mesmai, nbma)
        if (nbma .ne. 0) then
            call jeveuo(mesmai, 'L', jma)
            do 12 i = 1, nbma
                ima = zi(jma+i-1)
                nmmt(ima) = immt
12          continue
            call jedetr(mesmai)
        endif
!
10  end do
!
9999  continue
    call jedema()
end subroutine

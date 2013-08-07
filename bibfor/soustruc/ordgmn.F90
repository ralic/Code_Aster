subroutine ordgmn(noma, nomgrp)
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
!
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mess.h"
    character(len=8) :: noma, nomgrp
! ----------------------------------------------------------------------
!     BUT: ORDONNER LA LISTE DE NOEUDS SELON LA LISTE DE MAILLES
!          DE MEME NOM (LES MAILLES DOIVENT ETRE DES POI1)
!
!     IN: NOMA   : NOM DU MAILLAGE
!         NOMGRP : NOM DES GROUPES (MAILLES ET NOEUDS)
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    character(len=24) :: grpma, grpno
    integer :: nbma, nbno
    integer :: iret, ialino, ialima, i, ima, numno, iacnex, nbnoma
    call jemarq()
    grpma = noma//'.GROUPEMA       '
    grpno = noma//'.GROUPENO       '
!
    call jeexin(jexnom(grpma, nomgrp), iret)
    if (iret .eq. 0) goto 9999
    call jelira(jexnom(grpma, nomgrp), 'LONUTI', nbma)
    call jeveuo(jexnom(grpma, nomgrp), 'L', ialima)
!
    call jeexin(jexnom(grpno, nomgrp), iret)
    if (iret .eq. 0) then
        call u2mess('F', 'SOUSTRUC_17')
    endif
    call jelira(jexnom(grpno, nomgrp), 'LONUTI', nbno)
    call jeveuo(jexnom(grpno, nomgrp), 'E', ialino)
!
    if (nbma .ne. nbno) then
        call u2mess('F', 'SOUSTRUC_18')
    endif
!
    do 10 i = 1, nbma
        ima=zi(ialima+i-1)
        call jeveuo(jexnum(noma//'.CONNEX', ima), 'L', iacnex)
        call jelira(jexnum(noma//'.CONNEX', ima), 'LONMAX', nbnoma)
        if (nbnoma .ne. 1) then
            call u2mess('F', 'SOUSTRUC_19')
        endif
        numno=zi(iacnex)
        zi(ialino+i-1)=numno
10  end do
!
9999  continue
    call jedema()
end subroutine

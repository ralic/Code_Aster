subroutine ssdeu2(nval, iliste, nvalap)
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
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: nval, iliste(nval), nvalap
! ----------------------------------------------------------------------
!     BUT:
!        - ENLEVER LES DOUBLONS D'UNE LISTE D'ENTIERS.
!          LES ENTIERS SONT RETASSES VERS LE DEBUT DE LA LISTE.
!        - ENLEVER LES "ZERO".
!
!     IN:
!        NVAL  :  NOMBRE D'ENTIERS A PRENDRE EN COMPTE.
!     IN/OUT:
!        ILISTE:  LISTE DES ENTIERS.
!                 EN SORTIE, ELLE EST RETASSEE. LA FIN DE LA LISTE EST
!                 MISE A ZERO.
!     OUT:
!        NVALAP:  NOMBRE D'ENTIERS DIFFERENTS TROUVES DANS LA LISTE.
!
! ----------------------------------------------------------------------
    character(len=8) :: kbid
!
!
!     -- L'OBJET DE TRAVAIL "&&SSDEU2.WK1" CONTIENDRA DES "1" AU NIVEAU
!        DES ENTIERS A ELIMINER.
!     ---------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iawk1, idecal, iret, j, ndim
!-----------------------------------------------------------------------
    call jemarq()
    call jeexin('&&SSDEU2.WK1', iret)
    if (iret .eq. 0) then
        ndim=max(1000,2*nval)
        call wkvect('&&SSDEU2.WK1', 'V V I', ndim, iawk1)
    else
        call jelira('&&SSDEU2.WK1', 'LONMAX', ndim, kbid)
        if (ndim .lt. nval) then
            call jedetr('&&SSDEU2.WK1')
            call wkvect('&&SSDEU2.WK1', 'V V I', 2*nval, iawk1)
        else
            call jeveuo('&&SSDEU2.WK1', 'E', iawk1)
        endif
    endif
!
!     -- MISE A ZERO DE "&&SSDEU2.WK1":
!     ---------------------------------
    do 10, i=1,nval
    zi(iawk1-1+i)=0
    10 end do
!
!     -- MISE A "1" PARTIELLE DE  "&&SSDEU2.WK1":
!     -------------------------------------------
    nvalap= nval
    do 1 , i=1,nval
    if (iliste(i) .eq. 0) then
        zi(iawk1-1+i)=1
        nvalap= nvalap-1
        goto 1
    endif
    do 2 , j=1,i-1
    if (iliste(j) .eq. iliste(i)) then
        zi(iawk1-1+i)=1
        nvalap= nvalap-1
        goto 1
    endif
 2  continue
    1 end do
!
!
!     -- RETASSAGE DE LA LISTE:
!     -------------------------
    idecal=0
    do 3 , i=1,nval
    if (zi(iawk1-1+i) .eq. 1) then
        idecal= idecal+1
    else
        iliste(i-idecal)=iliste(i)
    endif
    3 end do
!
!     -- ON COMPLETE PAR DES ZERO:
!     ----------------------------
    do 4 , i=nvalap+1,nval
    iliste(i)=0
    4 end do
!
!
    call jedema()
end subroutine

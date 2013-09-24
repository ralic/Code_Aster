subroutine utimco(unit, obin, nivo, lattr, lcont)
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
#include "asterfort/jeexin.h"
#include "asterfort/jeimpa.h"
#include "asterfort/jeimpo.h"
#include "asterfort/jelira.h"
#include "asterfort/jeprat.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: obin
    integer :: nivo, unit
    logical :: lattr, lcont
! ----------------------------------------------------------------------
!     IN:
!       UNIT   : UNITE LOGIQUE D'IMPRESSION
!       OBIN   : NOM D'UNE COLLECTION  JEVEUX (K24) A IMPRIMER
!       NIVO   : NIVEAU D'IMPRESSION
!      LATTR   : VRAI : ON IMPRIME LES ATTRIBUTS
!              : FAUX : ON N'IMPRIME PAS LES ATTRIBUTS
!      LCONT   : VRAI : ON IMPRIME LE CONTENU DES OBJETS
!              : FAUX : ON N'IMPRIME PAS LE CONTENU DES OBJETS
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=24) :: ob1
    character(len=50) :: acces
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ioc, iret, nmaxoc
!-----------------------------------------------------------------------
    ob1 = obin
!
    call jeexin(ob1, iret)
    if (iret .le. 0) then
        call utmess('A', 'UTILITAI_99', sk=ob1)
        goto 999
    endif
!
    call jelira(ob1, 'NMAXOC', nmaxoc)
    call jelira(ob1, 'ACCES', cval=acces)
!
    write(unit,*)'IMPRESSION DE LA COLLECTION : ',ob1
!
    if (lattr) call jeimpa(unit, ob1, ' ')
    if ((lcont) .and. (acces(1:2).eq.'NO')) then
        call jeprat(unit, ob1, '$$NOM','REPERTOIRE DE NOMS DE LA COLLECTION :'//ob1)
    endif
!
!     -- BOUCLE SUR LES ELEMENTS DE LA COLLECTION :
!     ---------------------------------------------
    do ioc = 1, nmaxoc
        if ((nivo.eq.1) .and. (ioc.gt.10)) goto 999
        call jeexin(jexnum(ob1, ioc), iret)
        if (iret .eq. 0) goto 1
        if (lattr) then
            call jeimpa(unit, jexnum(ob1, ioc), ' ')
        endif
        if (lcont) then
            call jeimpo(unit, jexnum(ob1, ioc), ' ')
        endif
 1      continue
    end do
!
999  continue
end subroutine

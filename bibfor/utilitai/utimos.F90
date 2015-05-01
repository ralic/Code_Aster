subroutine utimos(unit, obin, lattr, lcont)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     --
!     ARGUMENTS:
!     ----------
#include "asterf_types.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeimpa.h"
#include "asterfort/jeimpo.h"
#include "asterfort/utmess.h"
    character(len=*) :: obin
    integer :: unit
    aster_logical :: lattr, lcont
! ----------------------------------------------------------------------
!     IN:
!       UNIT   : UNITE LOGIQUE D'IMPRESSION
!       OBIN   : NOM D'UN OBJET SIMPLE JEVEUX (K24) A IMPRIMER
!      LATTR   : VRAI : ON IMPRIME LES ATTRIBUTS
!              : FAUX : ON N'IMPRIME PAS LES ATTRIBUTS
!      LCONT   : VRAI : ON IMPRIME LE CONTENU DES OBJETS
!              : FAUX : ON N'IMPRIME PAS LE CONTENU DES OBJETS
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=24) :: ob1
! DEB-------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    integer :: iret
!-----------------------------------------------------------------------
    ob1 = obin
!
    call jeexin(ob1, iret)
    if (iret .le. 0) then
        call utmess('A', 'UTILITAI_99', sk=ob1)
        goto 9999
    endif
!
    if (lattr) then
        call jeimpa(unit, ob1, ' ')
    endif
!
    if (lcont) then
        call jeimpo(unit, ob1, ' ')
    endif
!
9999 continue
end subroutine

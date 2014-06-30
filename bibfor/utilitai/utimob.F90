subroutine utimob(unit, obin, nivo, lattr, lcont,&
                  xous)
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
!     --
!     ARGUMENTS:
!     ----------
#include "asterfort/utimco.h"
#include "asterfort/utimos.h"
#include "asterfort/utmess.h"
    character(len=*) :: obin, xous
    integer :: nivo, unit
    logical(kind=1) :: lattr, lcont
! ----------------------------------------------------------------------
!     IN:
!       UNIT   : UNITE LOGIQUE D'IMPRESSION
!       OBIN   : NOM D'UN OBJET JEVEUX (K24) A IMPRIMER
!       NIVO   : NIVEAU D'IMPRESSION
!      LATTR   : VRAI : ON IMPRIME LES ATTRIBUTS
!              : FAUX : ON N'IMPRIME PAS LES ATTRIBUTS
!      LCONT   : VRAI : ON IMPRIME LE CONTENU DES OBJETS
!              : FAUX : ON N'IMPRIME PAS LE CONTENU DES OBJETS
!       XOUS   : 'X' : COLLECTION ; 'S' : OBJET SIMPLE
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=1) :: xous2
    character(len=24) :: ob1
    character(len=40) :: lb
! DEB-------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    ob1 = obin
    xous2=xous
    lb='----------------------------------------'
    write(unit,'(A40,A40)') lb,lb
!
    if (xous2 .eq. 'X') then
        call utimco(unit, ob1, nivo, lattr, lcont)
    else if (xous2 .eq.'S') then
        call utimos(unit, ob1, lattr, lcont)
    else
!
        call utmess('F', 'UTILITAI5_41', sk=xous2)
    endif
!
end subroutine

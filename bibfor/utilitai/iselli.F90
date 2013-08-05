function iselli(elrefz)
    implicit  none
#include "asterfort/assert.h"
    logical :: iselli
    character(len=*) :: elrefz
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
! person_in_charge: samuel.geniaut at edf.fr
! ----------------------------------------------------------------------
!
! FONCTION VALANT TRUE SI L'ELEMENT DE REFERENCE EST LINEAIRE
!        (AUTANT DE NOEUDS QUE DE NOEUDS SOMMET)
!
! ----------------------------------------------------------------------
!
! IN   ELREFE : ELEMENT DE REFERENCE (ELREFE)
! OUT  ISELLI : TRUE SI L'ELEMENT DE REFERENCE EST LINEAIRE
!
    character(len=3) :: elrefe
!
    elrefe = elrefz
!
    if (elrefe .eq. 'PO1' .or. elrefe .eq. 'SE2' .or. elrefe .eq. 'TR3' .or. elrefe .eq.&
        'QU4' .or. elrefe .eq. 'TE4' .or. elrefe .eq. 'PY5' .or. elrefe .eq. 'PE6' .or.&
        elrefe .eq. 'HE8') then
!
        iselli=.true.
!
        elseif (elrefe.eq.'SE3'.or. elrefe.eq.'TR6'.or. elrefe.eq.'TR7'&
    .or. elrefe.eq.'QU8'.or. elrefe.eq.'QU9'.or. elrefe.eq.'T10'.or.&
    elrefe.eq.'P13'.or. elrefe.eq.'P15'.or. elrefe.eq.'P18'.or.&
    elrefe.eq.'H20'.or. elrefe.eq.'H27') then
!
        iselli=.false.
!
    else
!
        ASSERT(.false.)
!
    endif
!
end function

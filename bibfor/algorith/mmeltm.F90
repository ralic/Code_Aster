function mmeltm(ityp)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    character(len=16) :: mmeltm
#include "asterfort/assert.h"
    integer :: ityp
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! DONNE LE TYPE_MAILLE DE L'ELEMENT DE CONTACT ITYP
! REPERE DANS LE CATALOGUE &CATA.TM
!
! ----------------------------------------------------------------------
!
!
! IN  ITYP   : NUMERO DU TYPE
! OUT MMELTM : NOM DU TYPE ELEMENT
!
!
! ----------------------------------------------------------------------
!
    integer :: nbtyp
    parameter   (nbtyp=40)
    integer :: k
    character(len=16) :: nomtm(nbtyp)
!
    data (nomtm(k),k=1,nbtyp) /&
     &      'SEG22' ,'SEG33' ,'SEG23' ,'SEG32' ,'TRIA33',&
     &      'TR3TR6','TR6TR3','TRIA66','QUAD44','QU4QU8',&
     &      'QU8QU4','QUAD88','QU4TR3','TR3QU4','TR6QU4',&
     &      'QU4TR6','TR6QU8','QU8TR6','TR6QU9','QU9TR6',&
     &      'QU8TR3','TR3QU8','QU8QU9','QU9QU8','QU9QU4',&
     &      'QU4QU9','QU9TR3','TR3QU9','QUAD99','SEG22' ,&
     &      'SE2TR3','SE2TR6','SE2QU4','SE2QU8','SE2QU9',&
     &      'SE3TR3','SE3TR6','SE3QU4','SE3QU8','SE3QU9'/
!
! ----------------------------------------------------------------------
!
    mmeltm = '                '
    if ((ityp.le.0) .or. (ityp.gt.nbtyp)) then
        ASSERT(.false.)
    else
        mmeltm = nomtm(ityp)
    endif
!
end function

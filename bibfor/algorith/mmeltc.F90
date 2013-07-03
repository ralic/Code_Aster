function mmeltc(ityp)
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
    character(len=16) :: mmeltc
#include "asterfort/assert.h"
    integer :: ityp
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! DONNE LE TYPE_ELEM DE L'ELEMENT DE CONTACT ITYP
!
! ----------------------------------------------------------------------
!
!
! IN  ITYP   : NUMERO DU TYPE
! OUT MMELTC : NOM DU TYPE ELEMENT
!
!
! ----------------------------------------------------------------------
!
    integer :: nbtyp
    parameter   (nbtyp=40)
    integer :: k
    character(len=16) :: nomtc(nbtyp)
!
    data (nomtc(k),k=1,nbtyp) /&
     &      'COS2S2','COS3S3','COS2S3','COS3S2','COT3T3',&
     &      'COT3T6','COT6T3','COT6T6','COQ4Q4','COQ4Q8',&
     &      'COQ8Q4','COQ8Q8','COQ4T3','COT3Q4','COT6Q4',&
     &      'COQ4T6','COT6Q8','COQ8T6','COT6Q9','COQ9T6',&
     &      'COQ8T3','COT3Q8','COQ8Q9','COQ9Q8','COQ9Q4',&
     &      'COQ4Q9','COQ9T3','COT3Q9','COQ9Q9','COP2P2',&
     &      'COS2T3','COS2T6','COS2Q4','COS2Q8','COS2Q9',&
     &      'COS3T3','COS3T6','COS3Q4','COS3Q8','COS3Q9'/
!
! ----------------------------------------------------------------------
!
    mmeltc = '                '
    if ((ityp.le.0) .or. (ityp.gt.nbtyp)) then
        call assert(.false.)
    else
        mmeltc = nomtc(ityp)
    endif
!
end function

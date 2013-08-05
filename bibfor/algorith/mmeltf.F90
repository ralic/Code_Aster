function mmeltf(ityp)
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
    character(len=16) :: mmeltf
#include "asterfort/assert.h"
    integer :: ityp
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! DONNE LE TYPE_ELEM DE L'ELEMENT DE FROTTEMENT ITYP
!
! ----------------------------------------------------------------------
!
!
! IN  ITYP   : NUMERO DU TYPE
! OUT MMELTF : NOM DU TYPE ELEMENT
!
!
! ----------------------------------------------------------------------
!
    integer :: nbtyp
    parameter   (nbtyp=40)
    integer :: k
    character(len=16) :: nomtf(nbtyp)
!
    data (nomtf(k),k=1,nbtyp) /&
     &      'CFS2S2','CFS3S3','CFS2S3','CFS3S2','CFT3T3',&
     &      'CFT3T6','CFT6T3','CFT6T6','CFQ4Q4','CFQ4Q8',&
     &      'CFQ8Q4','CFQ8Q8','CFQ4T3','CFT3Q4','CFT6Q4',&
     &      'CFQ4T6','CFT6Q8','CFQ8T6','CFT6Q9','CFQ9T6',&
     &      'CFQ8T3','CFT3Q8','CFQ8Q9','CFQ9Q8','CFQ9Q4',&
     &      'CFQ4Q9','CFQ9T3','CFT3Q9','CFQ9Q9','CFP2P2',&
     &      'CFS2T3','CFS2T6','CFS2Q4','CFS2Q8','CFS2Q9',&
     &      'CFS3T3','CFS3T6','CFS3Q4','CFS3Q8','CFS3Q9'/
!
! ----------------------------------------------------------------------
!
    mmeltf = '                '
    if ((ityp.le.0) .or. (ityp.gt.nbtyp)) then
        ASSERT(.false.)
    else
        mmeltf = nomtf(ityp)
    endif
!
end function

function mmeltn(ityp)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    integer :: mmeltn
#include "asterfort/assert.h"
    integer :: ityp
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! DONNE LE NOMBRE DE NOEUDS DE L'ELEMENT DE CONTACT ITYP
!
! ----------------------------------------------------------------------
!
!
! IN  ITYP   : NUMERO DU TYPE
! OUT MMELTN : NOMBRE DE NOEUDS DE L'ELEMENT DE CONTACT
!
!
! ----------------------------------------------------------------------
!
    integer :: nbtyp
    parameter   (nbtyp=40)
    integer :: k
    integer :: npl(nbtyp)
!
    data (npl(k),k=1,nbtyp) /&
     &      4 ,6 ,5 ,5 ,6 ,&
     &      9 ,9 ,12,8 ,12,&
     &      12,16,7 ,7 ,10,&
     &      10,14,14,15,15,&
     &      11,11,17,17,13,&
     &      13,12,12,18,4 ,&
     &      5 ,8 ,6 ,10,11,&
     &      6 ,9 ,7 ,11,12/
!
! ----------------------------------------------------------------------
!
    mmeltn = 0
    if ((ityp.le.0) .or. (ityp.gt.nbtyp)) then
        ASSERT(.false.)
    else
        mmeltn = npl(ityp)
    endif
!
end function

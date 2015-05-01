subroutine nuelrf(elrefe, nujni)
    implicit  none
#include "asterfort/assert.h"
    character(len=8) :: elrefe
    integer :: nujni
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! BUT :  DONNER LE NUMERO DE LA ROUTINE JNI00I ASSOCIEE
!        A UN ELREFE
! IN  ELREFE : NOM DE L'ELREFE
! OUT NUJNI  : NUMERO DE LA ROUTINE JNI00I
!.......................................................................
!
!     D'ABORD LES ELREFA :
!     --------------------
    if (elrefe .eq. 'HE8' .or. elrefe .eq. 'H20' .or. elrefe .eq. 'H27' .or. elrefe .eq.&
        'PE6' .or. elrefe .eq. 'P15' .or. elrefe .eq. 'TE4' .or. elrefe .eq. 'T10' .or.&
        elrefe .eq. 'PY5' .or. elrefe .eq. 'P13' .or. elrefe .eq. 'QU4' .or. elrefe .eq.&
        'QU8' .or. elrefe .eq. 'QU9' .or. elrefe .eq. 'TR3' .or. elrefe .eq. 'TR6' .or.&
        elrefe .eq. 'TR7' .or. elrefe .eq. 'SE2' .or. elrefe .eq. 'SE3' .or. elrefe .eq.&
        'SE4' .or. elrefe .eq. 'PO1' .or. elrefe .eq. 'P18') then
        nujni = 2
!
    else if (elrefe.eq.'CABPOU') then
        nujni = 92
    else if (elrefe.eq.'THCOSE2') then
        nujni = 91
    else if (elrefe.eq.'THCOSE3') then
        nujni = 91
    else if (elrefe(1:4).eq.'POHO') then
        nujni = 15
    else if (elrefe.eq.'MEC3QU9H') then
        nujni = 80
    else if (elrefe.eq.'MEC3TR7H') then
        nujni = 80
!
!     -- POUR LES ELREFE VIDES :
    else if (elrefe(1:2).eq.'V_') then
        nujni = 1
!
    else
        ASSERT(.false.)
    endif
!
end subroutine

!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine mmmcri(criter    , noma  , depmoi, depgeo, depplu,&
                     ds_contact, epsmax, cvgnoe, cvgval, mmconv)
        use NonLin_Datastructure_type
        character(len=4) :: criter
        character(len=8) :: noma
        character(len=19) :: depmoi
        character(len=19) :: depgeo
        character(len=19) :: depplu
        type(NL_DS_Contact), intent(in) :: ds_contact
        real(kind=8) :: epsmax
        character(len=16) :: cvgnoe
        real(kind=8) :: cvgval
        aster_logical :: mmconv
    end subroutine mmmcri
end interface

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
    subroutine cfaddm(ds_contact, lctfd, lctf3d, posnoe, iliai,&
                      ndimg, nbnom, posnsm, coefno, tau1,&
                      tau2, norm, jeu, coornp)
        use NonLin_Datastructure_type
        type(NL_DS_Contact), intent(in) :: ds_contact
        aster_logical :: lctfd
        aster_logical :: lctf3d
        integer :: posnoe
        integer :: iliai
        integer :: ndimg
        integer :: nbnom
        integer :: posnsm(*)
        real(kind=8) :: coefno(*)
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: norm(3)
        real(kind=8) :: jeu
        real(kind=8) :: coornp(3)
    end subroutine cfaddm
end interface

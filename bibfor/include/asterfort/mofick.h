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
    subroutine mofick(fa, fav, cont, tange, maxfa,&
                      nface, nfacev, nfacem, fluxk, flux1k,&
                      flux2k, fluxl, flux1l, flux2l, moyfl,&
                      moyfl1, moyfl2)
        integer :: nfacem
        integer :: nfacev
        integer :: nface
        integer :: maxfa
        integer :: fa
        integer :: fav
        aster_logical :: cont
        aster_logical :: tange
        real(kind=8) :: fluxk(nface)
        real(kind=8) :: flux1k(1:maxfa+1, nface)
        real(kind=8) :: flux2k(1:maxfa+1, nface)
        real(kind=8) :: fluxl(nfacev)
        real(kind=8) :: flux1l(1:maxfa+1, nfacev)
        real(kind=8) :: flux2l(1:maxfa+1, nfacev)
        real(kind=8) :: moyfl(nfacem)
        real(kind=8) :: moyfl1(1:maxfa, 1:maxfa+1, 0:1)
        real(kind=8) :: moyfl2(1:maxfa, 1:maxfa+1, 0:1)
    end subroutine mofick
end interface

!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine calvol(np1, nbm, icoupl, indic, kmod00,&
                      cmod00, amor00, puls00, pulsi, amori,&
                      masgi, tpfl, veci1, vecr1, vecr2,&
                      vecr5, vecr3, vgap, vecr4, locfl0,&
                      amflu0, xsi0)
        integer :: np1
        integer :: nbm
        integer :: icoupl
        integer :: indic
        real(kind=8) :: kmod00(np1, *)
        real(kind=8) :: cmod00(np1, *)
        real(kind=8) :: amor00(*)
        real(kind=8) :: puls00(*)
        real(kind=8) :: pulsi(*)
        real(kind=8) :: amori(*)
        real(kind=8) :: masgi(*)
        character(len=8) :: tpfl
        integer :: veci1(*)
        real(kind=8) :: vecr1(*)
        real(kind=8) :: vecr2(*)
        real(kind=8) :: vecr5(*)
        real(kind=8) :: vecr3(*)
        real(kind=8) :: vgap
        real(kind=8) :: vecr4(*)
        aster_logical :: locfl0(*)
        real(kind=8) :: amflu0(np1, *)
        real(kind=8) :: xsi0(*)
    end subroutine calvol
end interface

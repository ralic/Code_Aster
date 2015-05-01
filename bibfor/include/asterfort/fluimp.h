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
    subroutine fluimp(itypfl, nivpar, nivdef, melflu, typflu,&
                      nuor, freq, freqi, nbm, vite,&
                      npv, carac, calcul, amoc)
        integer :: npv
        integer :: nbm
        integer :: itypfl
        integer :: nivpar
        integer :: nivdef
        character(len=19) :: melflu
        character(len=8) :: typflu
        integer :: nuor(nbm)
        real(kind=8) :: freq(2*nbm*npv)
        real(kind=8) :: freqi(*)
        real(kind=8) :: vite(npv)
        real(kind=8) :: carac(2)
        aster_logical :: calcul(2)
        real(kind=8) :: amoc(*)
    end subroutine fluimp
end interface

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
    subroutine avplcr(nbvec, vectn, vectu, vectv, nbordr,&
                      kwork, somnow, vwork, tdisp, tspaq,&
                      i, nomcri, nomfor, grdvie, forvie,&
                      fordef, fatsoc, proaxe, nommat, vala,&
                      coefpa, post, cudomx, nxm, nym,&
                      nzm)
        integer :: tdisp
        integer :: nbordr
        integer :: nbvec
        real(kind=8) :: vectn(3*nbvec)
        real(kind=8) :: vectu(3*nbvec)
        real(kind=8) :: vectv(3*nbvec)
        integer :: kwork
        integer :: somnow
        real(kind=8) :: vwork(tdisp)
        integer :: tspaq
        integer :: i
        character(len=16) :: nomcri
        character(len=16) :: nomfor
        character(len=8) :: grdvie
        character(len=16) :: forvie
        aster_logical :: fordef
        real(kind=8) :: fatsoc
        character(len=16) :: proaxe
        character(len=8) :: nommat
        real(kind=8) :: vala
        real(kind=8) :: coefpa
        aster_logical :: post
        real(kind=8) :: cudomx
        real(kind=8) :: nxm(2)
        real(kind=8) :: nym(2)
        real(kind=8) :: nzm(2)
    end subroutine avplcr
end interface

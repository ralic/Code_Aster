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
    subroutine asexc2(motfac, nbocc, nbmode, momec, amort,&
                      corfre, noma, ndir, nomsup, nomspe,&
                      dirspe, echspe, nature, nbsupm, nsupp,&
                      knoeu, kvspe, kaspe, nopara, nordr)
        integer :: nbmode
        character(len=*) :: motfac
        integer :: nbocc
        character(len=*) :: momec
        real(kind=8) :: amort(*)
        aster_logical :: corfre
        character(len=8) :: noma
        integer :: ndir(*)
        character(len=8) :: nomsup(3, *)
        character(len=8) :: nomspe(3, *)
        real(kind=8) :: dirspe(3, *)
        real(kind=8) :: echspe(3, *)
        integer :: nature(3, *)
        integer :: nbsupm
        integer :: nsupp(*)
        character(len=*) :: knoeu
        character(len=*) :: kvspe
        character(len=*) :: kaspe
        character(len=24) :: nopara(*)
        integer :: nordr(*)
    end subroutine asexc2
end interface

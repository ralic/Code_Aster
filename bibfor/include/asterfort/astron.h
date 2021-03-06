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
    subroutine astron(nomsy, psmo, monoap, muapde, nbsup,&
                      nsupp, neq, nbmode, id, vecmod,&
                      momec, gamma0, nomsup, reasup, recmor,&
                      recmop, nopara, nordr)
        integer :: nbmode
        integer :: neq
        integer :: nbsup
        character(len=16) :: nomsy
        character(len=*) :: psmo
        aster_logical :: monoap
        aster_logical :: muapde
        integer :: nsupp(*)
        integer :: id
        real(kind=8) :: vecmod(neq, *)
        character(len=*) :: momec
        real(kind=8) :: gamma0(*)
        character(len=*) :: nomsup(nbsup, *)
        real(kind=8) :: reasup(nbsup, nbmode, *)
        real(kind=8) :: recmor(nbsup, neq, *)
        real(kind=8) :: recmop(nbsup, neq, *)
        character(len=16) :: nopara(*)
        integer :: nordr(*)
    end subroutine astron
end interface

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
interface
    subroutine ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                      nbmode, vecmod, parmod, id, reasup,&
                      spectr, repmod, corfre, amort, muapde,&
                      tcosup, im, nbdis)
        integer :: nbmode
        integer :: neq
        integer :: nbsup
        character(len=16) :: nomsy
        logical :: monoap
        integer :: nsupp(*)
        real(kind=8) :: vecmod(neq, *)
        real(kind=8) :: parmod(nbmode, *)
        integer :: id
        real(kind=8) :: reasup(nbsup, nbmode, *)
        real(kind=8) :: spectr(*)
        real(kind=8) :: repmod(nbsup, neq, *)
        logical :: corfre
        real(kind=8) :: amort(*)
        logical :: muapde
        integer :: tcosup(nbsup, *)
        integer :: im
        integer :: nbdis(*)
    end subroutine ascarm
end interface

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
    subroutine nmvmpo(fami, npg, option, nomte, nc,&
                      xl, icodma, sect, carcri, compor,&
                      u, du, contm, hoel, hota,&
                      d1b, work, rg0, contp, fl,&
                      klv)
        integer :: nc
        character(len=*) :: fami
        integer :: npg
        character(len=*) :: option
        character(len=*) :: nomte
        real(kind=8) :: xl
        integer :: icodma
        real(kind=8) :: sect(*)
        real(kind=8) :: carcri(*)
        character(len=16) :: compor(*)
        real(kind=8) :: u(2*nc)
        real(kind=8) :: du(2*nc)
        real(kind=8) :: contm(3*nc)
        real(kind=8) :: hoel(nc, nc)
        real(kind=8) :: hota(nc, nc)
        real(kind=8) :: d1b(nc, 2*nc)
        real(kind=8) :: work(nc, 2*nc)
        real(kind=8) :: rg0(2*nc, 2*nc)
        real(kind=8) :: contp(3*nc)
        real(kind=8) :: fl(2*nc)
        real(kind=8) :: klv(*)
    end subroutine nmvmpo
end interface

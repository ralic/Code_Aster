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
interface
    subroutine nmvmpo(fami, npg, nno, option, nc, &
                      xl, wgauss, icodma, sect, u, &
                      du, contm, contp, fl, klv)
        character(len=*) :: fami
        integer :: npg
        integer :: nno
        character(len=*) :: option
        integer :: nc
        real(kind=8) :: xl
        real(kind=8) :: wgauss(npg)
        integer :: icodma
        real(kind=8) :: sect(*)
        real(kind=8) :: u(nno*nc)
        real(kind=8) :: du(nno*nc)
        real(kind=8) :: contm(npg*nc)
        real(kind=8) :: contp(npg*nc)
        real(kind=8) :: fl(nno*nc)
        real(kind=8) :: klv(*)
    end subroutine nmvmpo
end interface

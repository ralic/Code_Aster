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
    subroutine b3d_bwpw(biot0, vw0, xsat, poro0, epsvt,&
                        epsvpw, epsvpg, vg, pw1, bw1,&
                        xnsat, mfr1, pw0, dpw, vw1)
        real(kind=8) :: biot0
        real(kind=8) :: vw0
        real(kind=8) :: xsat
        real(kind=8) :: poro0
        real(kind=8) :: epsvt
        real(kind=8) :: epsvpw
        real(kind=8) :: epsvpg
        real(kind=8) :: vg
        real(kind=8) :: pw1
        real(kind=8) :: bw1
        real(kind=8) :: xnsat
        integer :: mfr1
        real(kind=8) :: pw0
        real(kind=8) :: dpw
        real(kind=8) :: vw1
    end subroutine b3d_bwpw
end interface 

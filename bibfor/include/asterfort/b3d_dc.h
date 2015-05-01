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
    subroutine b3d_dc(sigec03, long3, dcf, dc0, e,&
                      delta, gf, rc, epic, beta,&
                      gama, reg, fr2, suc1, ifour,&
                      aleas0, bw1, pw1)
        real(kind=8) :: sigec03(3)
        real(kind=8) :: long3(3)
        real(kind=8) :: dcf
        real(kind=8) :: dc0
        real(kind=8) :: e
        real(kind=8) :: delta
        real(kind=8) :: gf
        real(kind=8) :: rc
        real(kind=8) :: epic
        real(kind=8) :: beta
        real(kind=8) :: gama
        real(kind=8) :: reg
        real(kind=8) :: fr2
        real(kind=8) :: suc1
        integer :: ifour
        real(kind=8) :: aleas0
        real(kind=8) :: bw1
        real(kind=8) :: pw1
    end subroutine b3d_dc
end interface 

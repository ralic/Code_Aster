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
    subroutine dsqniw(qsi, eta, caraq4, dci, bcm,&
                      bcb, bca, an, am, wsq,&
                      wmesq)
        real(kind=8) :: qsi
        real(kind=8) :: eta
        real(kind=8) :: caraq4(*)
        real(kind=8) :: dci(2, 2)
        real(kind=8) :: bcm(2, 8)
        real(kind=8) :: bcb(2, 12)
        real(kind=8) :: bca(2, 4)
        real(kind=8) :: an(4, 12)
        real(kind=8) :: am(4, 8)
        real(kind=8) :: wsq(12)
        real(kind=8) :: wmesq(8)
    end subroutine dsqniw
end interface

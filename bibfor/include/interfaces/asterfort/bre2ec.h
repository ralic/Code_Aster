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
    subroutine bre2ec(k0, k1, k2, eta1, eta2,&
                      e1i, e2i, a, t, b,&
                      e2p, pw, e2f)
        real(kind=8) :: k0
        real(kind=8) :: k1
        real(kind=8) :: k2
        real(kind=8) :: eta1
        real(kind=8) :: eta2
        real(kind=8) :: e1i
        real(kind=8) :: e2i
        real(kind=8) :: a
        real(kind=8) :: t
        real(kind=8) :: b
        real(kind=8) :: e2p
        real(kind=8) :: pw
        real(kind=8) :: e2f
    end subroutine bre2ec
end interface

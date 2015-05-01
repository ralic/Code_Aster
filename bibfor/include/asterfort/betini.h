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
    subroutine betini(materf, nmat, sig, sigeq, sigh,&
                      coefa, coefb, coefar, coefbr, coneco,&
                      conetr)
        integer :: nmat
        real(kind=8) :: materf(nmat, 2)
        real(kind=8) :: sig(6)
        real(kind=8) :: sigeq
        real(kind=8) :: sigh
        real(kind=8) :: coefa(2, 2)
        real(kind=8) :: coefb(2)
        real(kind=8) :: coefar(2, 2)
        real(kind=8) :: coefbr(2)
        real(kind=8) :: coneco
        real(kind=8) :: conetr
    end subroutine betini
end interface

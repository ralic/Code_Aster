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
    subroutine b3d_sigd(bg1, pg1, bw1, pw1, sfld,&
                        ssg6, e1, rt2, ept1, mfr,&
                        erreur, dg3, dw3, xnu0, sigaf6,&
                        dflu0, sigef6, xmg, sigat6, vplg33,&
                        vplg33t, vssw33, vssw33t, ssw6, dth0)
        real(kind=8) :: bg1
        real(kind=8) :: pg1
        real(kind=8) :: bw1
        real(kind=8) :: pw1
        real(kind=8) :: sfld
        real(kind=8) :: ssg6(6)
        real(kind=8) :: e1
        real(kind=8) :: rt2
        real(kind=8) :: ept1
        integer :: mfr
        integer :: erreur
        real(kind=8) :: dg3(3)
        real(kind=8) :: dw3(3)
        real(kind=8) :: xnu0
        real(kind=8) :: sigaf6(6)
        real(kind=8) :: dflu0
        real(kind=8) :: sigef6(6)
        real(kind=8) :: xmg
        real(kind=8) :: sigat6(6)
        real(kind=8) :: vplg33(3, 3)
        real(kind=8) :: vplg33t(3, 3)
        real(kind=8) :: vssw33(3, 3)
        real(kind=8) :: vssw33t(3, 3)
        real(kind=8) :: ssw6(6)
        real(kind=8) :: dth0
    end subroutine b3d_sigd
end interface 

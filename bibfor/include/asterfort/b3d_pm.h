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
    subroutine b3d_pm(vg, vp0, dv0, depst6, ssg6,&
                      dg3, epspg6, epspfg6, etag, bg,&
                      xmg, pg, dt, epg0, vw0,&
                      poro0, biot0, xwsat, pw1, bw1,&
                      epsvpw, vplg33, vplg33t, e0, rt0,&
                      ept0, erreur, xwns, mfr, pw0,&
                      dpw, vw1)
        real(kind=8) :: vg
        real(kind=8) :: vp0
        real(kind=8) :: dv0
        real(kind=8) :: depst6(6)
        real(kind=8) :: ssg6(6)
        real(kind=8) :: dg3(3)
        real(kind=8) :: epspg6(6)
        real(kind=8) :: epspfg6(6)
        real(kind=8) :: etag
        real(kind=8) :: bg
        real(kind=8) :: xmg
        real(kind=8) :: pg
        real(kind=8) :: dt
        real(kind=8) :: epg0
        real(kind=8) :: vw0
        real(kind=8) :: poro0
        real(kind=8) :: biot0
        real(kind=8) :: xwsat
        real(kind=8) :: pw1
        real(kind=8) :: bw1
        real(kind=8) :: epsvpw
        real(kind=8) :: vplg33(3, 3)
        real(kind=8) :: vplg33t(3, 3)
        real(kind=8) :: e0
        real(kind=8) :: rt0
        real(kind=8) :: ept0
        integer :: erreur
        real(kind=8) :: xwns
        integer :: mfr
        real(kind=8) :: pw0
        real(kind=8) :: dpw
        real(kind=8) :: vw1
    end subroutine b3d_pm
end interface 

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
    subroutine b3d_viscoplast(pg, etag, dt, epg0, dff3,&
                              xmg, bg, epsvpg, epspg6, epspfg6,&
                              vssd33, vssd33t, vplg33, vplg33t, dg3)
        real(kind=8) :: pg
        real(kind=8) :: etag
        real(kind=8) :: dt
        real(kind=8) :: epg0
        real(kind=8) :: dff3(3)
        real(kind=8) :: xmg
        real(kind=8) :: bg
        real(kind=8) :: epsvpg
        real(kind=8) :: epspg6(6)
        real(kind=8) :: epspfg6(6)
        real(kind=8) :: vssd33(3, 3)
        real(kind=8) :: vssd33t(3, 3)
        real(kind=8) :: vplg33(3, 3)
        real(kind=8) :: vplg33t(3, 3)
        real(kind=8) :: dg3(3)
    end subroutine b3d_viscoplast
end interface 

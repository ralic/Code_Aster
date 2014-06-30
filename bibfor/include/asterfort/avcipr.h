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
    subroutine avcipr(nbvec, vectn, vectu, vectv, nbordr,&
                      kwork, sommw, vwork, tdisp, tspaq,&
                      ipgn, nomcri, nomfor, fordef, fatsoc,&
                      proaxe, pseuil, method, ncycl, jvmin,&
                      jvmax, jomin, jomax)
        integer :: tdisp
        integer :: nbordr
        integer :: nbvec
        real(kind=8) :: vectn(3*nbvec)
        real(kind=8) :: vectu(3*nbvec)
        real(kind=8) :: vectv(3*nbvec)
        integer :: kwork
        integer :: sommw
        real(kind=8) :: vwork(tdisp)
        integer :: tspaq
        integer :: ipgn
        character(len=16) :: nomcri
        character(len=16) :: nomfor
        logical(kind=1) :: fordef
        real(kind=8) :: fatsoc
        character(len=16) :: proaxe
        real(kind=8) :: pseuil
        character(len=8) :: method
        integer :: ncycl(nbvec)
        integer :: jvmin
        integer :: jvmax
        integer :: jomin
        integer :: jomax
    end subroutine avcipr
end interface

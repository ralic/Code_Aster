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
    subroutine thmrcp(etape, imate, thmc, meca, hydr,&
                      ther, t0, p10, p20, phi0,&
                      pvp0, t, p1, p1m, p2,&
                      phi, endo, pvp, rgaz, rhod,&
                      cpd, biot, satm, satur, dsatur,&
                      pesa, permfh, permli, dperml, permgz,&
                      dperms, dpermp, fick, dfickt, dfickg,&
                      lambp, dlambp, rhol, unsurk, alpha,&
                      cpl, lambs, dlambs, viscl, dviscl,&
                      mamolg, cpg, lambt, dlambt, viscg,&
                      dviscg, mamolv, cpvg, viscvg, dvisvg,&
                      fickad, dfadt, cpad, kh, pad,&
                      em, lambct, isot, dficks, instap,&
                      retcom)
        character(len=8) :: etape
        integer :: imate
        character(len=16) :: thmc
        character(len=16) :: meca
        character(len=16) :: hydr
        character(len=16) :: ther
        real(kind=8) :: t0
        real(kind=8) :: p10
        real(kind=8) :: p20
        real(kind=8) :: phi0
        real(kind=8) :: pvp0
        real(kind=8) :: t
        real(kind=8) :: p1
        real(kind=8) :: p1m
        real(kind=8) :: p2
        real(kind=8) :: phi
        real(kind=8) :: endo
        real(kind=8) :: pvp
        real(kind=8) :: rgaz
        real(kind=8) :: rhod
        real(kind=8) :: cpd
        real(kind=8) :: biot
        real(kind=8) :: satm
        real(kind=8) :: satur
        real(kind=8) :: dsatur
        real(kind=8) :: pesa(3)
        real(kind=8) :: permfh
        real(kind=8) :: permli
        real(kind=8) :: dperml
        real(kind=8) :: permgz
        real(kind=8) :: dperms
        real(kind=8) :: dpermp
        real(kind=8) :: fick
        real(kind=8) :: dfickt
        real(kind=8) :: dfickg
        real(kind=8) :: lambp
        real(kind=8) :: dlambp
        real(kind=8) :: rhol
        real(kind=8) :: unsurk
        real(kind=8) :: alpha
        real(kind=8) :: cpl
        real(kind=8) :: lambs
        real(kind=8) :: dlambs
        real(kind=8) :: viscl
        real(kind=8) :: dviscl
        real(kind=8) :: mamolg
        real(kind=8) :: cpg
        real(kind=8) :: lambt
        real(kind=8) :: dlambt
        real(kind=8) :: viscg
        real(kind=8) :: dviscg
        real(kind=8) :: mamolv
        real(kind=8) :: cpvg
        real(kind=8) :: viscvg
        real(kind=8) :: dvisvg
        real(kind=8) :: fickad
        real(kind=8) :: dfadt
        real(kind=8) :: cpad
        real(kind=8) :: kh
        real(kind=8) :: pad
        real(kind=8) :: em
        real(kind=8) :: lambct
        real(kind=8) :: isot(6)
        real(kind=8) :: dficks
        real(kind=8) :: instap
        integer :: retcom
    end subroutine thmrcp
end interface

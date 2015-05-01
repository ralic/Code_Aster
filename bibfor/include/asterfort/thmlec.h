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
    subroutine thmlec(imate, thmc, meca, hydr, ther,&
                      t, p1, p2, phi, end,&
                      pvp, pad, rgaz, tbiot, satur,&
                      dsatur, pesa, tperm, permli, dperml,&
                      permgz, dperms, dpermp, fick, dfickt,&
                      dfickg, lambp, dlambp, unsurk, alpha,&
                      lambs, dlambs, viscl, dviscl, mamolg,&
                      tlambt, tdlamt, viscg, dviscg, mamolv,&
                      fickad, dfadt, tlamct, instap,&
                      angmas, aniso, ndim)
        integer :: ndim
        integer :: imate
        character(len=16) :: thmc
        character(len=16) :: meca
        character(len=16) :: hydr
        character(len=16) :: ther
        real(kind=8) :: t
        real(kind=8) :: p1
        real(kind=8) :: p2
        real(kind=8) :: phi
        real(kind=8) :: end
        real(kind=8) :: pvp
        real(kind=8) :: pad
        real(kind=8) :: rgaz
        real(kind=8) :: tbiot(6)
        real(kind=8) :: satur
        real(kind=8) :: dsatur
        real(kind=8) :: pesa(3)
        real(kind=8) :: tperm(ndim, ndim)
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
        real(kind=8) :: unsurk
        real(kind=8) :: alpha
        real(kind=8) :: lambs
        real(kind=8) :: dlambs
        real(kind=8) :: viscl
        real(kind=8) :: dviscl
        real(kind=8) :: mamolg
        real(kind=8) :: tlambt(ndim, ndim)
        real(kind=8) :: tdlamt(ndim, ndim)
        real(kind=8) :: viscg
        real(kind=8) :: dviscg
        real(kind=8) :: mamolv
        real(kind=8) :: fickad
        real(kind=8) :: dfadt
        real(kind=8) :: tlamct(ndim, ndim)
        real(kind=8) :: instap
        real(kind=8) :: angmas(3)
        integer :: aniso
    end subroutine thmlec
end interface 

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
    subroutine fetfiv(nbsd, nbi, vd1, vd2, vdo,&
                      matas, vsdf, vddl, infofe, irex,&
                      ifiv, nbproc, rang, k24irz, sdfeti)
        integer :: nbi
        integer :: nbsd
        real(kind=8) :: vd1(nbi)
        real(kind=8) :: vd2(nbi)
        real(kind=8) :: vdo(nbi)
        character(len=19) :: matas
        integer :: vsdf(nbsd)
        integer :: vddl(nbsd)
        character(len=24) :: infofe
        integer :: irex
        integer :: ifiv
        integer :: nbproc
        integer :: rang
        character(len=24) :: k24irz
        character(len=19) :: sdfeti
    end subroutine fetfiv
end interface

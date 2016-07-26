!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine xmasel(nnop, nfh, nfe, ddlc, igeom,&
                      imate, pintt, cnset, heavt, lonch,&
                      basloc, lsn, lst, matuu, heavn, jpmilt,&
                      jstno, nnops, ddlm)
        integer :: nnop
        integer :: nfh
        integer :: nfe
        integer :: ddlc
        integer :: igeom
        integer :: imate
        integer :: jpmilt
        integer :: jstno
        integer :: ddlm
        integer :: nnops
        real(kind=8) :: pintt(33)
        integer :: cnset(128)
        integer :: heavt(36)
        integer :: lonch(10)
        real(kind=8) :: basloc(*)
        real(kind=8) :: lsn(nnop)
        real(kind=8) :: lst(nnop)
        real(kind=8) :: matuu(*)
        integer :: heavn(27,5)
    end subroutine xmasel
end interface

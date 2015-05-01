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
    subroutine mdrecf(nexci, nexcir, idescf, nomfon, coefm,&
                      iadvec, inumor, fondep, fonvit, fonacc,&
                      neq, typbas, basemo, nbmode, riggen,&
                      nommot, nomres)
        integer :: nbmode
        integer :: nexci
        integer :: nexcir
        integer :: idescf(*)
        character(len=8) :: nomfon(2*nexci)
        real(kind=8) :: coefm(*)
        integer :: iadvec(*)
        integer :: inumor(*)
        character(len=8) :: fondep(2*nexci)
        character(len=8) :: fonvit(2*nexci)
        character(len=8) :: fonacc(2*nexci)
        integer :: neq
        character(len=16) :: typbas
        character(len=8) :: basemo
        real(kind=8) :: riggen(nbmode)
        character(len=8) :: nommot
        character(len=8) :: nomres
    end subroutine mdrecf
end interface

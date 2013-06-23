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
    subroutine vpecst(ifm, typres, omgmin, omgmax, nbfre1,&
                      nbfre2, nbfreq, nblagr, typep, typcon,&
                      dimc1, zimc1)
        integer :: ifm
        character(len=16) :: typres
        real(kind=8) :: omgmin
        real(kind=8) :: omgmax
        integer :: nbfre1
        integer :: nbfre2
        integer :: nbfreq
        integer :: nblagr
        character(len=1) :: typep
        character(len=8) :: typcon
        real(kind=8) :: dimc1
        complex(kind=8) :: zimc1
    end subroutine vpecst
end interface

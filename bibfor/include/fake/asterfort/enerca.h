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
    subroutine enerca(valinc, dep0, vit0, depl1, vite1,&
                      masse, amort, rigid, fexte, famor,&
                      fliai, fnoda, fcine, lamort, ldyna,&
                      lexpl, sdener, schema)
        character(len=19) :: valinc(*)
        real(kind=8) :: dep0(*)
        real(kind=8) :: vit0(*)
        real(kind=8) :: depl1(*)
        real(kind=8) :: vite1(*)
        character(len=19) :: masse
        character(len=19) :: amort
        character(len=19) :: rigid
        real(kind=8) :: fexte(*)
        real(kind=8) :: famor(*)
        real(kind=8) :: fliai(*)
        real(kind=8) :: fnoda(*)
        real(kind=8) :: fcine(*)
        logical :: lamort
        logical :: ldyna
        logical :: lexpl
        character(len=19) :: sdener
        character(len=8) :: schema
    end subroutine enerca
end interface

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
    subroutine dltcrr(result, neq, nbordr, iarchi, texte,&
                      ifm, t0, lcrea, typres, masse,&
                      rigid, amort, dep0, vit0, acc0,&
                      fexte, famor, fliai, numedd, nume,&
                      nbtyar, typear)
        integer :: nbtyar
        integer :: neq
        character(len=8) :: result
        integer :: nbordr
        integer :: iarchi
        character(len=*) :: texte
        integer :: ifm
        real(kind=8) :: t0
        logical(kind=1) :: lcrea
        character(len=16) :: typres
        character(len=8) :: masse
        character(len=8) :: rigid
        character(len=8) :: amort
        real(kind=8) :: dep0(neq)
        real(kind=8) :: vit0(neq)
        real(kind=8) :: acc0(neq)
        real(kind=8) :: fexte(2*neq)
        real(kind=8) :: famor(2*neq)
        real(kind=8) :: fliai(2*neq)
        character(len=24) :: numedd
        integer :: nume
        character(len=16) :: typear(nbtyar)
    end subroutine dltcrr
end interface

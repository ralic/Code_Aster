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
    subroutine tbliva(nomta, npacri, lipacr, vi, vr,&
                      vc, vk, crit, prec, para,&
                      ctype, vali, valr, valc, valk,&
                      ier)
        character(len=*) :: nomta
        integer :: npacri
        character(len=*) :: lipacr(*)
        integer :: vi(*)
        real(kind=8) :: vr(*)
        complex(kind=8) :: vc(*)
        character(len=*) :: vk(*)
        character(len=*) :: crit(*)
        real(kind=8) :: prec(*)
        character(len=*) :: para
        character(len=*) :: ctype
        integer :: vali
        real(kind=8) :: valr
        complex(kind=8) :: valc
        character(len=*) :: valk
        integer :: ier
    end subroutine tbliva
end interface

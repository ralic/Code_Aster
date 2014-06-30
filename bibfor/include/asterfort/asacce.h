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
    subroutine asacce(nomsy, monoap, muapde, nbsup, neq,&
                      nbmode, id, moncha, vecmod, parmod,&
                      gamma0, recmor, recmod, nbdis)
        integer :: nbmode
        integer :: neq
        integer :: nbsup
        character(len=16) :: nomsy
        logical(kind=1) :: monoap
        logical(kind=1) :: muapde
        integer :: id
        character(len=*) :: moncha
        real(kind=8) :: vecmod(neq, *)
        real(kind=8) :: parmod(nbmode, *)
        real(kind=8) :: gamma0(*)
        real(kind=8) :: recmor(nbsup, neq, *)
        real(kind=8) :: recmod(nbsup, neq, *)
        integer :: nbdis(nbsup)
    end subroutine asacce
end interface

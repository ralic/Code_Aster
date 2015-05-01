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
    subroutine xmelet(nomte, typmai, elrees, elrema, elreco,&
                      ndim, nddl, jnne, jnnm, nnc,&
                      jddle, jddlm, nconta, ndeple, nsinge,&
                      nsingm, nfhe, nfhm)
        character(len=16) :: nomte
        character(len=8) :: typmai
        character(len=8) :: elrees
        character(len=8) :: elrema
        character(len=8) :: elreco
        integer :: ndim
        integer :: nddl
        integer :: jnne(3)
        integer :: jnnm(3)
        integer :: nnc
        integer :: jddle(2)
        integer :: jddlm(2)
        integer :: nconta
        integer :: ndeple
        integer :: nsinge
        integer :: nsingm
        integer :: nfhe
        integer :: nfhm
    end subroutine xmelet
end interface

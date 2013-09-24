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
    subroutine remnbn(basmod, nbmod, nbddr, nbdax, flexdr,&
                      flexga, flexax, tetgd, tetax, cmode,&
                      vecmod, neq, beta)
        integer :: neq
        integer :: nbdax
        integer :: nbddr
        integer :: nbmod
        character(len=8) :: basmod
        character(len=24) :: flexdr
        character(len=24) :: flexga
        character(len=24) :: flexax
        character(len=24) :: tetgd
        character(len=24) :: tetax
        complex(kind=8) :: cmode(nbmod+nbddr+nbdax)
        complex(kind=8) :: vecmod(neq)
        real(kind=8) :: beta
    end subroutine remnbn
end interface

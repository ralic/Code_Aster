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
    subroutine wpsorc(lmasse, lamor, lmatra, nbeq, nbvect,&
                      nfreq, tolsor, vect, resid, workd,&
                      workl, lonwl, selec, dsor, sigma,&
                      vaux, workv, ddlexc, ddllag, neqact,&
                      maxitr, ifm, niv, priram, alpha,&
                      nconv, flage, vauc, rwork, solveu)
        integer :: nbeq
        integer :: lmasse
        integer :: lamor
        integer :: lmatra
        integer :: nbvect
        integer :: nfreq
        real(kind=8) :: tolsor
        complex(kind=8) :: vect(nbeq, *)
        complex(kind=8) :: resid(*)
        complex(kind=8) :: workd(*)
        complex(kind=8) :: workl(*)
        integer :: lonwl
        logical :: selec(*)
        complex(kind=8) :: dsor(*)
        complex(kind=8) :: sigma
        complex(kind=8) :: vaux(*)
        complex(kind=8) :: workv(*)
        integer :: ddlexc(*)
        integer :: ddllag(*)
        integer :: neqact
        integer :: maxitr
        integer :: ifm
        integer :: niv
        integer :: priram(8)
        real(kind=8) :: alpha
        integer :: nconv
        logical :: flage
        complex(kind=8) :: vauc(2*nbeq, *)
        real(kind=8) :: rwork(*)
        character(len=19) :: solveu
    end subroutine wpsorc
end interface

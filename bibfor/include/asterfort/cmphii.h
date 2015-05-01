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
    subroutine cmphii(ck, cm, ndim, nbmod, niter,&
                      xcrit, ceigen, cmod, ndimax, cmat1,&
                      cmat2, cvec, ific)
        integer :: ndimax
        integer :: nbmod
        integer :: ndim
        complex(kind=8) :: ck(*)
        complex(kind=8) :: cm(*)
        integer :: niter
        real(kind=8) :: xcrit
        complex(kind=8) :: ceigen(nbmod)
        complex(kind=8) :: cmod(ndimax, nbmod)
        complex(kind=8) :: cmat1(*)
        complex(kind=8) :: cmat2(ndim, ndim)
        complex(kind=8) :: cvec(ndim)
        integer :: ific
    end subroutine cmphii
end interface

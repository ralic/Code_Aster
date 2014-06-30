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
    subroutine xprvit(noma, fiss, ndim, nvit, nbeta,&
                      lcmin, cnsvt, cnsvn, vpoint, cnsbl,&
                      cnsdis, disfr, cnsbet, listp, damax,&
                      locdom, rdimp, rdtor, delta, ucnslt,&
                      ucnsln)
        character(len=8) :: noma
        character(len=8) :: fiss
        integer :: ndim
        character(len=24) :: nvit
        character(len=24) :: nbeta
        real(kind=8) :: lcmin
        character(len=19) :: cnsvt
        character(len=19) :: cnsvn
        character(len=19) :: vpoint
        character(len=19) :: cnsbl
        character(len=19) :: cnsdis
        character(len=19) :: disfr
        character(len=19) :: cnsbet
        character(len=19) :: listp
        real(kind=8) :: damax
        logical(kind=1) :: locdom
        real(kind=8) :: rdimp
        real(kind=8) :: rdtor
        character(len=19) :: delta
        character(len=19) :: ucnslt
        character(len=19) :: ucnsln
    end subroutine xprvit
end interface

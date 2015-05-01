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
    subroutine comatr(option, typev, nbproc, rang, vnconv,&
                      dim1i, dim2i, vecti, dim1r, dim2r,&
                      vectr, dim1c, dim2c, vectc)
        integer :: dim1c
        integer :: dim1r
        integer :: dim1i
        integer :: nbproc
        character(len=1) :: option
        character(len=1) :: typev
        integer :: rang
        integer :: vnconv(nbproc)
        integer :: dim2i
        integer :: vecti(dim1i, *)
        integer :: dim2r
        real(kind=8) :: vectr(dim1r, *)
        integer :: dim2c
        complex(kind=8) :: vectc(dim1c, *)
    end subroutine comatr
end interface

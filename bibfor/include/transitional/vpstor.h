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
    subroutine vpstor(ineg, type, modes, nbmode, neq,&
                      vecpr8, vecpc8, mxresf, nbpari, nbparr,&
                      nbpark, nopara, mod45, resufi, resufr,&
                      resufk, iprec)
        integer :: mxresf
        integer :: neq
        integer :: ineg
        character(*) :: type
        character(*) :: modes
        integer :: nbmode
        real(kind=8) :: vecpr8(neq, *)
        complex(kind=8) :: vecpc8(neq, *)
        integer :: nbpari
        integer :: nbparr
        integer :: nbpark
        character(*) :: nopara(*)
        character(len=4) :: mod45
        integer :: resufi(mxresf, *)
        real(kind=8) :: resufr(mxresf, *)
        character(*) :: resufk(mxresf, *)
        integer :: iprec
    end subroutine vpstor
end interface

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
    subroutine wp1inv(lmasse, lamor, lraide, tolf, nitf,&
                      mxresf, nbfreq, neq, resufi, resufr,&
                      resufk, vecpro, solveu)
        integer :: neq
        integer :: mxresf
        integer :: lmasse
        integer :: lamor
        integer :: lraide
        real(kind=8) :: tolf
        integer :: nitf
        integer :: nbfreq
        integer :: resufi(mxresf, *)
        real(kind=8) :: resufr(mxresf, *)
        character(len=*) :: resufk(mxresf, *)
        complex(kind=8) :: vecpro(neq, *)
        character(len=19) :: solveu
    end subroutine wp1inv
end interface

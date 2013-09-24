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
    subroutine affeno(ioc, ino, nocmp, nbcmp, ncmpgd,&
                      ncmpmx, val, kval, desc, valglo,&
                      kvalgl, type, nec)
        integer :: ioc
        integer :: ino
        character(len=8) :: nocmp(*)
        integer :: nbcmp
        character(len=8) :: ncmpgd(*)
        integer :: ncmpmx
        real(kind=8) :: val(*)
        character(len=8) :: kval(*)
        integer :: desc(*)
        real(kind=8) :: valglo(*)
        character(len=8) :: kvalgl(*)
        character(len=*) :: type
        integer :: nec
    end subroutine affeno
end interface

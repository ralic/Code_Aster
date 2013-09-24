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
    subroutine vprecu(modes, nomsy, nbvect, lposi, nomvec,&
                      nbpara, nopara, nomvai, nomvar, nomvak,&
                      neq, nbmode, typmod, nbpari, nbparr,&
                      nbpark)
        character(len=*) :: modes
        character(len=*) :: nomsy
        integer :: nbvect
        integer :: lposi(*)
        character(len=*) :: nomvec
        integer :: nbpara
        character(len=*) :: nopara
        character(len=*) :: nomvai
        character(len=*) :: nomvar
        character(len=*) :: nomvak
        integer :: neq
        integer :: nbmode
        character(len=*) :: typmod
        integer :: nbpari
        integer :: nbparr
        integer :: nbpark
    end subroutine vprecu
end interface

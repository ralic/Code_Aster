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
    subroutine xprpls(ligrel, dnoma, dcnsln, dcnslt, noma,&
                      cnsln, cnslt, grln, grlt, corres,&
                      ndim, ndomp, edomg)
        character(len=19) :: ligrel
        character(len=8) :: dnoma
        character(len=19) :: dcnsln
        character(len=19) :: dcnslt
        character(len=8) :: noma
        character(len=19) :: cnsln
        character(len=19) :: cnslt
        character(len=19) :: grln
        character(len=19) :: grlt
        character(len=16) :: corres
        integer :: ndim
        character(len=19) :: ndomp
        character(len=19) :: edomg
    end subroutine xprpls
end interface

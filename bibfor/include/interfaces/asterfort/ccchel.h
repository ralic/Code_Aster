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
    subroutine ccchel(option, modele, resuin, resuou, numord,&
                      nordm1, mateco, carael, typesd, ligrel,&
                      exipou, exitim, lischa, nbchre, ioccur,&
                      suropt, basopt, resout)
        character(len=16) :: option
        character(len=8) :: modele
        character(len=8) :: resuin
        character(len=8) :: resuou
        integer :: numord
        integer :: nordm1
        character(len=24) :: mateco
        character(len=8) :: carael
        character(len=16) :: typesd
        character(len=24) :: ligrel
        logical :: exipou
        logical :: exitim
        character(len=19) :: lischa
        integer :: nbchre
        integer :: ioccur
        character(len=24) :: suropt
        character(len=1) :: basopt
        character(len=24) :: resout
    end subroutine ccchel
end interface

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
    subroutine resloc(modele, ligrel, yaxfem, yathm, tbgrca,&
                      perman, chtime, mate, sigmam, sigmap,&
                      chsigx, chdepm, chdepp, cherrm, lchar,&
                      nchar, tabido, chvois, cvoisx, chelem)
        character(len=8) :: modele
        character(len=*) :: ligrel
        logical :: yaxfem
        logical :: yathm
        real(kind=8) :: tbgrca(3)
        logical :: perman
        character(len=24) :: chtime
        character(len=*) :: mate
        character(len=24) :: sigmam
        character(len=24) :: sigmap
        character(len=24) :: chsigx
        character(len=24) :: chdepm
        character(len=24) :: chdepp
        character(len=24) :: cherrm
        character(len=8) :: lchar(1)
        integer :: nchar
        integer :: tabido(5)
        character(len=24) :: chvois
        character(len=24) :: cvoisx
        character(len=24) :: chelem
    end subroutine resloc
end interface

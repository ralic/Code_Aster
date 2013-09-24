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
    subroutine inclis(nomres, ssta, sstb, intfa, intfb,&
                      fmlia, fplian, fplibn, fpliao, fplibo,&
                      iada, iadb, numlis, matprj)
        character(len=8) :: nomres
        character(len=8) :: ssta
        character(len=8) :: sstb
        character(len=8) :: intfa
        character(len=8) :: intfb
        character(len=24) :: fmlia
        character(len=24) :: fplian
        character(len=24) :: fplibn
        character(len=24) :: fpliao
        character(len=24) :: fplibo
        integer :: iada(3)
        integer :: iadb(3)
        integer :: numlis
        character(len=8) :: matprj
    end subroutine inclis
end interface

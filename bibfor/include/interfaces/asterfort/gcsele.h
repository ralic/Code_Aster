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
! aslint: disable=W1504
interface 
    subroutine gcsele(motcle, chvolu, ch1d2d, ch2d3d, chpres,&
                      chepsi, chpesa, chrota, lpvolu, lp1d2d,&
                      lp2d3d, lppres, lpepsi, lppesa, lprota,&
                      lfvolu, lf1d2d, lf2d3d, lfpres, lfepsi,&
                      lfpesa, lfrota, carte0, lformu, lpchar,&
                      lccomb)
        character(len=16) :: motcle
        character(len=19) :: chvolu
        character(len=19) :: ch1d2d
        character(len=19) :: ch2d3d
        character(len=19) :: chpres
        character(len=19) :: chepsi
        character(len=19) :: chpesa
        character(len=19) :: chrota
        logical :: lpvolu
        logical :: lp1d2d
        logical :: lp2d3d
        logical :: lppres
        logical :: lpepsi
        logical :: lppesa
        logical :: lprota
        logical :: lfvolu
        logical :: lf1d2d
        logical :: lf2d3d
        logical :: lfpres
        logical :: lfepsi
        logical :: lfpesa
        logical :: lfrota
        character(len=24) :: carte0
        logical :: lformu
        logical :: lpchar
        logical :: lccomb
    end subroutine gcsele
end interface 

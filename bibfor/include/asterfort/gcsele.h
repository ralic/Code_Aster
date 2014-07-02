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
#include "asterf_types.h"
!
interface
    subroutine gcsele(motcle, chvolu, ch1d2d, ch2d3d, chpres,&
                      chepsi, chpesa, chrota, lvolu , l1d2d ,&
                      l2d3d , lpres , lepsi , lpesa , lrota ,&
                      lfvolu, lf1d2d, lf2d3d, lfpres, lfepsi,&
                      lfpesa, lfrota, carte0, lformu, lpchar,&
                      lccomb)
        character(len=16) :: motcle
        character(len=19) :: carte0
        aster_logical :: lformu, lpchar, lccomb
        character(len=19) :: chvolu, ch1d2d, ch2d3d, chpres
        character(len=19) :: chepsi, chpesa, chrota
        aster_logical :: lvolu, l1d2d, l2d3d, lpres
        aster_logical :: lepsi, lpesa, lrota
        aster_logical :: lfvolu, lf1d2d, lf2d3d, lfpres
        aster_logical :: lfepsi, lfpesa, lfrota
    end subroutine gcsele
end interface

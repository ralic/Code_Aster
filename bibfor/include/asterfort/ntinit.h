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
    subroutine ntinit(result, modele, mate, carele, lischa,&
                      lisch2, solveu, para, numedd, lostat,&
                      levol, lnonl, sddisc, sdieto, mailla,&
                      sdcrit, time)
        character(len=24) :: result
        character(len=24) :: modele
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=19) :: lischa
        character(len=19) :: lisch2
        character(len=19) :: solveu
        real(kind=8) :: para(*)
        character(len=24) :: numedd
        aster_logical :: lostat
        aster_logical :: levol
        aster_logical :: lnonl
        character(len=19) :: sddisc
        character(len=24) :: sdieto
        character(len=8) :: mailla
        character(len=19) :: sdcrit
        character(len=24) :: time
    end subroutine ntinit
end interface

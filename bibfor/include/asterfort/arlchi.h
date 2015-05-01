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
#include "asterf_types.h"
!
interface
    subroutine arlchi(iocc,mail,nomo,nom1,nom2,mailar,     &
                      typmai,nbchel,chames,jma1,jma2, &
                      tabcor,proj)
        aster_logical :: proj
        integer :: iocc
        integer :: nbchel
        integer :: jma1
        integer :: jma2
        character(len=8) :: mailar
        character(len=8) :: mail
        character(len=8) :: nomo
        character(len=10) :: nom1
        character(len=10) :: nom2
        character(len=16) :: typmai
        character(len=19) :: chames(nbchel)
        character(len=24) :: tabcor
        character(len=32) :: jexnum
        character(len=32) :: jexatr
    end subroutine arlchi
end interface

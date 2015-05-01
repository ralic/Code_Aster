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
    subroutine mmimp4(ifm, noma, nummae, iptm, indcoi,&
                      indcon, indfri, indfrn, lfrot, lvites,&
                      lgliss, jeu, jeuvit, lambdc)
        integer :: ifm
        character(len=8) :: noma
        integer :: nummae
        integer :: iptm
        integer :: indcoi
        integer :: indcon
        integer :: indfri
        integer :: indfrn
        aster_logical :: lfrot
        aster_logical :: lvites
        aster_logical :: lgliss
        real(kind=8) :: jeu
        real(kind=8) :: jeuvit
        real(kind=8) :: lambdc
    end subroutine mmimp4
end interface

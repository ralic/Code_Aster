subroutine cbrota(char, noma, ndim, ligrmo)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!     BUT: TRAITE LE MOT_CLE : ROTATION
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DE LA CHARGE
!      NOMA   : NOM DU MAILLAGE
!      LIGRMO  : NOM DU LIGREL DE MODELE
!
    include 'asterc/getfac.h'
    include 'asterfort/carota.h'
    character(len=*) :: ligrmo
    character(len=8) :: char, noma
    integer :: irota, ndim
!
    call getfac('ROTATION', irota)
    if (irota .ne. 0) then
        call carota(char, noma, irota, ndim, ligrmo)
    endif
end subroutine

subroutine cbpesa(char, noma, ndim, ligrmo)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     BUT: TRAITE LE MOT_CLE : PESANTEUR
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DE LA CHARGE
!      NOMA   : NOM DU MAILLAGE
!      NDIM   : DIMENSION DU PROBLEME
!      LIGRMO : NOM DU LIGREL DE MODELE
!
#include "asterc/getfac.h"
#include "asterfort/capesa.h"
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
    integer :: ipesa, ndim
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call getfac('PESANTEUR', ipesa)
    if (ipesa .ne. 0) then
        call capesa(char, noma, ipesa, ndim, ligrmo)
    endif
end subroutine

subroutine melima(chin, ma, icode, ient, lima,&
                  nb)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
!
! ----------------------------------------------------------------------
!     RETOURNE LE NOMBRE DE MAILLE D'1 GROUPE AFFECTE D'1 CARTE
!            AINSI QUE L'ADRESSE DU DEDEBUT DE LA LISTE DANS ZI.
! ----------------------------------------------------------------------
!
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=8) :: ma
    character(len=19) :: chin
    integer :: icode, ient, lima, nb
! ----------------------------------------------------------------------
!     ENTREES:
!     CHIN : NOM D'1 CARTE
!     MA   : NOM DU MAILLAGE SUPPORT DE LA CARTE.
!     IENT : NUMERO DE L'ENTITE AFFECTE PAR LA GRANDEUR
!             IENT = NUMERO DU GROUPE_MA SI CODE=2
!             IENT = NUMERO DE LA GRANDEUR EDITEE SI CODE = 3 OU -3
!     ICODE :  2  OU -3 OU 3
!
!     SORTIES:
!     NB   : NOMBRE DE MAILLES DANS LA LISTE.
!     LIMA : ADRESSE DANS ZI DE LA LISTE DES NUMEROS DE MAILLES.
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
!
!
!     TRAITE LES 2 CAS :  - GROUPE NOMME DU MAILLAGE MA
!                         - GROUPE TARDIF DE LA CARTE CHIN
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (icode .eq. 2) then
!
!        GROUPE DE MAILLES DU MAILLAGE:
        call jelira(jexnum(ma//'.GROUPEMA', ient), 'LONUTI', nb)
        call jeveuo(jexnum(ma//'.GROUPEMA', ient), 'L', lima)
    else if (abs(icode).eq.3) then
!
!        GROUPE TARDIF :
        call jelira(jexnum(chin(1:19)//'.LIMA', ient), 'LONMAX', nb)
        call jeveuo(jexnum(chin(1:19)//'.LIMA', ient), 'L', lima)
    else
        ASSERT(.false.)
    endif
end subroutine

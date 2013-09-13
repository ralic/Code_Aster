subroutine exithm(modele, yathm, perman)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!----------------------------------------------------------------------
!    FONCTION REALISEE : DETECTE SI LE MODELE EST UNE MODELISATION THM
!    ON RETOURNE UN LOGIQUE VALANT VRAI OU FAUX SELON LE CAS ET LE NOM
!    DE LA MODELISATION EFFECTIVE
!    IL Y A ERREUR FATALE SI ON NE REUSSIT PAS A DECODER LE MODELE
!
!    ATTENTION, SI LA MODELISATION N'EST PAS LA MEME SUR TOUT LE
!    MAILLAGE, CELA NE MARCHE PAS. C'EST LA FAUTE A DISMOI.
!    IL FAUDRA FAIRE AUTREMENT. (SYMPA COMME CONSEIL)
!
!     ARGUMENTS:
!     ----------
! IN   MODELE : MODELE DU CALCUL
! OUT  YATHM  : VRAI, SI LA MODELISATION EST UNE MODELISATION THM
!               FAUX, SINON
! OUT  PERMAN : SI LA MODELISATION EST UNE MODELISATION THM :
!               VRAI, SI CALCUL PERMANENT, FAUX, SINON
! ......................................................................
!
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES :
!       MESSAGE     : U2MESK
!       UTILITAIRES : DISMOI
!   -------------------------------------------------------------------
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterfort/dismoi.h"
#include "asterfort/utmess.h"
    character(len=8) :: modele
!
    logical :: yathm, perman
!
! 0.2. ==> COMMUNS
! 0.3. ==> VARIABLES LOCALES
!
    integer :: ibid, ier
!
    character(len=5) :: repons
!
!====
! 1. A-T-ON DE LA THM DANS L'UNE DES MODELISATIONS ASSOCIEES AU MODELE ?
!    IL Y A ERREUR FATALE SI ON NE REUSSIT PAS A DECODER LE MODELE
!====
!
    call dismoi('F', 'EXI_THM', modele, 'MODELE', ibid,&
                repons, ier)
!
    if (repons .eq. 'OUI') then
        yathm = .true.
        perman = .false.
    else if (repons.eq.'OUI_P') then
        yathm = .true.
        perman = .true.
    else if (repons.eq.'NON') then
        yathm = .false.
    else
        call utmess('F', 'UTILITAI_75', sk=repons)
    endif
!
end subroutine

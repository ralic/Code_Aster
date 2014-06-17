subroutine meamac(modelz, ncha, lcha, mate, matelz)
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/meama2.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    integer :: ncha
    character(len=*) :: modelz, mate, matelz
    character(len=8) :: lcha(*)
! ----------------------------------------------------------------------
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
!
!*    CALCUL DES MATRICES ELEMENTAIRES D'AMORTISSEMENT ACOUSTIQUE
!      MATEL:
!*           ( ISO_FACE, 'AMOR_ACOU '   )
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!        MODELZ : NOM DU MODELE
!*       NCHA   : NOMBRE DE CHARGES
!*       LCHA   : LISTE  DES CHARGES
!*       MATE   : CARTE DE MATERIAU CODE
!        MATELZ : NOM  DU  MATELE (N RESUELEM) PRODUIT
!*                ( ISO_FACE, 'AMOR_ACOU '   )
!
!     SORTIES:
!        MATELZ    : LE MATELE EST REMPLI.
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!*
!
!     VARIABLES LOCALES:
!     ------------------
!
    character(len=8) ::  modele
    character(len=19) :: matel
!
!-----------------------------------------------------------------------
    integer :: i,  long2
    character(len=24), pointer :: relr(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    modele = modelz
    matel = matelz
!
!*    -- AMORTISSEMENT CORRESPONDANT AUX ELEMENTS ISO_FACE
    call meama2(modele, ncha, lcha, mate, '&MEAMAC2           ',&
                matel)
!
!*    -- ON RECOPIE LES .RELR DE &MEAMAC2 DANS MATEL.
!**
    call jelira('&MEAMAC2           .RELR', 'LONUTI', long2)
    call jeveuo('&MEAMAC2           .RELR', 'L', vk24=relr)
!**
    call jedetr(matel//'.RERR')
    call jedetr(matel//'.RELR')
!
    call memare('G', matel, modele, mate, ' ',&
                'AMOR_ACOU')
!
    do 1,i = 1,long2
    call reajre(matel, relr(i), 'G')
    1 end do
! --- MENAGE
    call jedetr('&MEAMAC2           .RELR')
    call jedetr('&MEAMAC2           .RERR')
!
    matelz = matel
    call jedema()
end subroutine

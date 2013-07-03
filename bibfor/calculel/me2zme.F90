subroutine me2zme(modelz, chsigz, vecelz)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ARGUMENTS:
!     ----------
! ......................................................................
!     BUT:
!         CALCUL DE TOUS LES SECONDS MEMBRES ELEMENTAIRES PERMETTANT
!         DE CALCULER L'ESTIMATEUR D'ERREUR SUR LES CONTRAINTES
!
!                 OPTION : 'SECM_ZZ1'
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!        MODELZ : NOM DU MODELE
!        CHSIGZ : NOM DU CHAMP DE CONTRAINTES CALCULEES
!        VECELZ : NOM DU VEC_ELE (N RESUELEM) PRODUIT
!                 SI VECEL EXISTE DEJA, ON LE DETRUIT.
!
!     SORTIES:
! ......................................................................
!
!
!
!
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    character(len=8) :: modele
    character(len=19) :: vecel
    character(len=8) :: lpain(2), lpaout(6)
    character(len=16) :: option
    character(len=24) :: lchin(2), lchout(6), ligrmo, chgeom, chsig
    character(len=*) :: modelz, chsigz, vecelz
!
!
!-----------------------------------------------------------------------
    integer :: ibid
!-----------------------------------------------------------------------
    call jemarq()
    modele = modelz
    chsig = chsigz
    vecel = vecelz
!
    call megeom(modele, chgeom)
!
    call detrsd('VECT_ELEM', vecel)
    call memare('V', vecel, modele, ' ', ' ',&
                'SECM_ZZ1')
!
    lpaout(1) = 'PVECTR1'
    lpaout(2) = 'PVECTR2'
    lpaout(3) = 'PVECTR3'
    lpaout(4) = 'PVECTR4'
    lpaout(5) = 'PVECTR5'
    lpaout(6) = 'PVECTR6'
!
    lchout(1) = '&&ME2ZME.VE001'
    lchout(2) = '&&ME2ZME.VE002'
    lchout(3) = '&&ME2ZME.VE003'
    lchout(4) = '&&ME2ZME.VE004'
    lchout(5) = '&&ME2ZME.VE005'
    lchout(6) = '&&ME2ZME.VE006'
!
    call corich('E', lchout(1), -1, ibid)
    call corich('E', lchout(2), -1, ibid)
    call corich('E', lchout(3), -1, ibid)
    call corich('E', lchout(4), -1, ibid)
    call corich('E', lchout(5), -1, ibid)
    call corich('E', lchout(6), -1, ibid)
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PSIEF_R'
    lchin(2) = chsig
    ligrmo = modele//'.MODELE'
!
    option = 'SECM_ZZ1'
!
    call calcul('S', option, ligrmo, 2, lchin,&
                lpain, 6, lchout, lpaout, 'V',&
                'OUI')
    call reajre(vecel, lchout(1), 'V')
    call reajre(vecel, lchout(2), 'V')
    call reajre(vecel, lchout(3), 'V')
    call reajre(vecel, lchout(4), 'V')
    call reajre(vecel, lchout(5), 'V')
    call reajre(vecel, lchout(6), 'V')
!
    call jedema()
end subroutine

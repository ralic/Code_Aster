subroutine qintzz(modele, ligrel, matez, sigmap, sigmad,&
                  signop, signod, resu)
    implicit none
#include "asterfort/calcul.h"
#include "asterfort/mecact.h"
#include "asterfort/megeom.h"
#include "asterfort/utmess.h"
    character(len=*) :: modele, ligrel, matez, sigmap, sigmad
    character(len=*) :: signop, signod, resu
    character(len=6) :: chtemp
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!         CALCUL DE L'ESTIMATEUR D'ERREUR QUANTITE D'INTERET AVEC LA
!         METHODE DE ZHU-ZIENKIEWICZ.
!
!                 OPTION : 'ERRE_QIZZ'
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   MODELE : NOM DU MODELE
! IN   MATEZ  : NOM DU CONCEPT MATERIAU
! IN   CHTEMP : NOM DU CHAMP DE TEMPERATURE
! IN   SIGMAP : NOM DU CHAMP DE CONTRAINTES PB.PRIMAL (CHAM_ELEM_SIEF_R)
! IN   SIGMAD : NOM DU CHAMP DE CONTRAINTES PB.DUAL (CHAM_ELEM_SIEF_R)
! IN   SIGNOP : NOM DU CHAMP DE CONTRAINTES LISSEES POUR LE PB. PRIMAL
! IN   SIGNOD : NOM DU CHAMP DE CONTRAINTES LISSEES POUR LE PB. DUAL
!
!
!      SORTIE :
!-------------
! OUT  RESU   : NOM DU CHAM_ELEM_ERREUR PRODUIT
!               SI RESU EXISTE DEJA, ON LE DETRUIT.
!
! ......................................................................
!
    integer :: ibid
!
    character(len=8) :: lpain(7), lpaout(1), k8bid
    character(len=16) :: option
    character(len=24) :: lchin(7), lchout(1), chgeom, mate
!
!
    complex(kind=8) :: cbid
!
! DEB-------------------------------------------------------------------
!
    chtemp = '&&TEMP'
    call mecact('V', chtemp, 'LIGREL', ligrel, 'TEMP_R',&
                1, 'TEMP', ibid, 0.0d0, cbid,&
                k8bid)
!
    mate = matez
    call megeom(modele, chgeom)
!
    if (mate .eq. ' ') then
        call utmess('F', 'CALCULEL4_66')
    endif
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(3) = 'PSIEFP_R'
    lchin(3) = sigmap
    lpain(4) = 'PSIEFD_R'
    lchin(4) = sigmad
    lpain(5) = 'PTEMPER'
    lchin(5) = chtemp
    lpain(6) = 'PSIGMAP'
    lchin(6) = signop
    lpain(7) = 'PSIGMAD'
    lchin(7) = signod
!
    lpaout(1) = 'PERREUR'
    lchout(1) = resu
    option = 'ERRE_QIZZ'
    call calcul('S', option, ligrel, 7, lchin,&
                lpain, 1, lchout, lpaout, 'G',&
                'OUI')
!
end subroutine

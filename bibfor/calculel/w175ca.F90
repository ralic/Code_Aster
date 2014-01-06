subroutine w175ca(modele, carele, chfer1, chefge, chfer2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
    implicit none
#include "asterfort/calcul.h"
#include "asterfort/exlim3.h"
#include "asterfort/mecara.h"
    character(len=8) :: modele, carele
    character(len=19) :: chfer1, chfer2, chefge
!
! ----------------------------------------------------------------------
!     CALCUL DE L'OPTION FERRAILLAGE
!
! IN  MODELE  : NOM DU MODELE
! IN  CARELE  : CARACTERISTIQUES COQUES
! IN  CHFER1  : CHAMP DE FER1_R
! IN  CHEFGE  : CHAMP DE EFGE_ELNO
! OUT CHFER2  : RESULTAT DU CALCUL DE FERRAILLAGE
!
    character(len=8) :: lpain(5), lpaout(1)
    character(len=16) :: option
    character(len=19) :: chcara(18)
    character(len=19) :: lchin(15), lchout(1), ligrel
!
    call exlim3('AFFE', 'G', modele, ligrel)
    option = 'FERRAILLAGE'
!
    call mecara(carele, chcara)
!
    lpain(1) = 'PCACOQU'
    lchin(1) = chcara(7)
    lpain(2) = 'PFERRA1'
    lchin(2) = chfer1
    lpain(3) = 'PEFFORR'
    lchin(3) = chefge
!
!
    lpaout(1) = 'PFERRA2'
    lchout(1) = chfer2
!
    call calcul('S', option, ligrel, 3, lchin,&
                lpain, 1, lchout, lpaout, 'G',&
                'OUI')
!
end subroutine

subroutine reslgn(ligrel, option, erree, erren)
    implicit none
#include "asterfort/calcul.h"
    character(len=*) :: ligrel, erree, erren
!----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:   CALCUL DE L'OPTION : 'ERME_ELNO' ET 'QIRE_ELNO'
!     ----
!
! IN  : LIGREL : NOM DU LIGREL
! IN  : ERREE  : NOM DU CHAM_ELEM ERREUR PAR ELEMENT
! OUT : ERREN  : NOM DU CHAM_ELEM_ERREUR PRODUIT AUX NOEUDS
!
! ......................................................................
!
    character(len=8) :: lpain(1), lpaout(1)
    character(len=16) :: option
    character(len=24) :: lchin(1), lchout(1)
!
    lpain(1) = 'PERREUR'
    lchin(1) = erree
!
    lpaout(1) = 'PERRENO'
    lchout(1) = erren
!
    call calcul('S', option, ligrel, 1, lchin,&
                lpain, 1, lchout, lpaout, 'G',&
                'OUI')
!
end subroutine

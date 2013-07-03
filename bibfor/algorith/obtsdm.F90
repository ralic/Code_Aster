subroutine obtsdm(lisnom, typcoz, marq)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
#include "jeveux.h"
!
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/obsetk.h"
    character(len=24) :: lisnom
    character(len=*) :: typcoz
    character(len=1) :: marq
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCTS - TABLEAU POUR IMPRESSION)
!
! AFFECTATION D'UNE MARQUE DANS UNE COLONNE
!
! ----------------------------------------------------------------------
!
!
! IN  LISNOM : REPERTOIRE DES NOMS DE LA STRUCT TABLEAU POUR IMPRESSION
! IN  TYPCOL : CODE TYPE DE LA COLONNE
! IN  MARQ   : MARQUAGE DE LA COLONNE
!
! ----------------------------------------------------------------------
!
    integer :: jlisno
    character(len=24) :: sdcolo, typcol
!
! ----------------------------------------------------------------------
!
    typcol = typcoz
    call jeveuo(jexnom(lisnom, typcol), 'L', jlisno)
    sdcolo = zk24(jlisno)
!
! --- AFFECTATION MARQUE
!
    call obsetk(sdcolo, 'MARQUE', marq)
!
end subroutine

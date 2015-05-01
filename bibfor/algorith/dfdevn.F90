subroutine dfdevn(action, submet, pasmin, nbrpas, niveau)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    real(kind=8) :: pasmin
    character(len=16) :: action
    character(len=16) :: submet
    integer :: nbrpas, niveau
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION LISTE INSTANTS
!
! VALEURS PAR DEFAUT POUR ACTION = DECOUPE MANUELLE
!
! ----------------------------------------------------------------------
!
!
! OUT ACTION : NOM DE L'ACTION
! OUT SUBMET : TYPE DE SUBDIVISION
! OUT PASMIN : VALEUR DE SUBD_PAS_MINI
! OUT NBRPAS : VALEUR DE SUBD_PAS
! OUT NIVEAU : VALEUR DE SUBD_NIVEAU
!
! ----------------------------------------------------------------------
!
    action = 'DECOUPE'
    submet = 'MANUEL'
    niveau = 3
    nbrpas = 4
    pasmin = 1.d-12
!
end subroutine

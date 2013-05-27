subroutine oblsap(sdlist, idnvaz, lacti)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include       'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/oblgip.h'
    include 'asterfort/oblsai.h'
    character(len=24) :: sdlist
    character(len=*) :: idnvaz
    logical :: lacti
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT - LISTE DE STRUCTS)
!
! ACTIVATION D'UN STRUCT DANS LA LISTE - ACCES PAR PARAMETRE
!
! ----------------------------------------------------------------------
!
!
! IN  SDLIST : NOM DE LA LISTE
! IN  IDNVAL : VALEUR DU PARAMETRE IDENTIFIANT LE STRUCT
! IN  LACTI  : .TRUE. SI ACTIVATION
!
! ----------------------------------------------------------------------
!
    integer :: indice
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION INDICE DU STRUCT
!
    call oblgip(sdlist, idnvaz, indice)
!
! --- ACTIVATION
!
    call oblsai(sdlist, indice, lacti)
!
    call jedema()
end subroutine

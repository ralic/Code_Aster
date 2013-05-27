subroutine apatta(sdappa, defico, jdeciv, ima, posma)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/cfinvm.h'
    character(len=19) :: sdappa
    character(len=24) :: defico
    integer :: jdeciv, ima, posma
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! RETOURNE LA POSITION DE LA MAILLE ATTACHEE
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  DEFICO : SD DEFINITION DU CONTACT
! IN  JDECIV : DECALAGE POUR LECTURE DANS SD CONN. INVERSE.
! IN  IMA    : NUMERO DE LA MAILLE (DANS LES BOUCLES)
! OUT POSMA  : POSITION DE LA MAILLE
!
!
! ----------------------------------------------------------------------
!
! --- MAILLE ATTACHEE
!
    call cfinvm(defico, jdeciv, ima, posma)
!
end subroutine

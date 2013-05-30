subroutine oblgen(subccn, idnvaz, nomstr)
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
    include 'asterfort/assert.h'
    include 'asterfort/lxlgut.h'
    character(len=*) :: idnvaz
    character(len=6) :: subccn
    character(len=24) :: nomstr
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT - LISTE DE STRUCTS)
!
! GENERATION AUTOMATIQUE D'UN NOM DE STRUCT
!
! ----------------------------------------------------------------------
!
!
! IN  SUBCCN : ROUTINE DE CREATION
! IN  IDNVAZ : VALEUR DU PARAMETRE IDENTIFIANT LE STRUCT
! OUT NOMSTR : NOM Du STRUCT DANS LA LSITE
!
! ----------------------------------------------------------------------
!
    integer :: long
!
    long = lxlgut(idnvaz)
    call assert(long.le.15)
    nomstr = '&&'//subccn//'.'//idnvaz(1:15)
!
end subroutine

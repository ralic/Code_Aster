subroutine impsdr(sdcolo, valk, valr, vali)
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
    include 'asterfort/assert.h'
    include 'asterfort/obgetb.h'
    include 'asterfort/obseti.h'
    include 'asterfort/obsetk.h'
    include 'asterfort/obsetr.h'
    character(len=24) :: sdcolo
    character(len=*) :: valk
    real(kind=8) :: valr
    integer :: vali
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (AFFICHAGE - TABLEAU)
!
! AFFECTATION DES VALEURS POUR LA COLONNE DANS UN TABLEAU
!
! ----------------------------------------------------------------------
!
!
! IN SDCOLO : SD COLONNE DU TABLEAU
! IN VALK   : VALEUR DE TYPE K16 POUR LES COLONNES DE TYPE CHAINE
! IN VALR   : VALEUR DE TYPE REAL POUR LES COLONNES DE TYPE REEL
! IN VALI   : VALEUR DE TYPE ENTIER POUR LES COLONNES DE TYPE ENTIER
!
! ----------------------------------------------------------------------
!
    logical :: linte, lreel, lchai
!
! ----------------------------------------------------------------------
!
!
!
! --- TYPE DE LA VALEUR DANS LA COLONNE
!
    call obgetb(sdcolo, 'ENTIER', linte)
    call obgetb(sdcolo, 'REEL', lreel)
    call obgetb(sdcolo, 'CHAINE', lchai)
!
! --- AFFECTATION DE LA VALEUR
!
    if (linte) then
        call obseti(sdcolo, 'VALE_I', vali)
    else if (lreel) then
        call obsetr(sdcolo, 'VALE_R', valr)
    else if (lchai) then
        call obsetk(sdcolo, 'VALE_K', valk)
    else
        call assert(.false.)
    endif
!
!
end subroutine

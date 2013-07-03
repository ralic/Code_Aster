function cotsti(typsup)
    implicit none
    character(len=16) :: typsup, cotsti
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
!======================================================================
!     BUT : DETERMINER LE TYPE "INFORMATIQUE" CORRESPONDANT A
!           UN TYPE "SUPERVISEUR"
!======================================================================
#include "jeveux.h"
!
    character(len=16) :: typinf
!----------------------------------------------------------------------
!
    if (typsup .eq. 'LISTR8_SDASTER') then
        typinf='LISTR8'
    else if (typsup.eq.'LISTIS_SDASTER') then
        typinf='LISTIS'
    else if (typsup.eq.'MAILLAGE_SDASTER') then
        typinf='MAILLAGE'
    else if (typsup.eq.'MODELE_SDASTER') then
        typinf='MODELE'
    else if (typsup.eq.'FONCTION_SDASTER') then
        typinf='FONCTION'
    else if (typsup.eq.'NAPPE_SDASTER') then
        typinf='FONCTION'
    else if (typsup.eq.'TABLE_SDASTER') then
        typinf='TABLE'
    else if (typsup.eq.'EVOL_ELAS') then
        typinf='RESULTAT'
    else if (typsup.eq.'EVOL_NOLI') then
        typinf='RESULTAT'
    else if (typsup.eq.'EVOL_THER') then
        typinf='RESULTAT'
    else if (typsup.eq.'CABL_PRECONT') then
        typinf='CABL_PRECONT'
    else
        typinf='INCONNU'
    endif
    cotsti=typinf
end function

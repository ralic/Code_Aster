subroutine nmedco(compor, option, imate, npg, lgpg,&
                  s, q, vim, vip, alphap,&
                  dalfs)
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
!
    implicit none
!
#include "asterfort/lcedex.h"
#include "asterfort/utmess.h"
    integer :: imate, npg, lgpg
    real(kind=8) :: s(2), q(2, 2), alphap(2), dalfs(2, 2)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg)
    character(len=16) :: compor(*), option
!
! ----------------------------------------------------------------------
!     INTEGRATION DES LOIS DE COMPORTEMENT NON LINEAIRE POUR LES
!     ELEMENTS A DISCONTINUITE INTERNE.
!     LE COMPORTEMENT ETANT PARTICULIER, CETTE ROUTINE FAIT OFFICE DE
!     'NMCOMP.F' POUR DE TELS ELEMENTS.
!
!     REMARQUE : CETTE ROUTINE N'EST PAS DANS UNE BOULCE SUR LES PG CAR
!                LES VI SONT CONSTANTES SUR CHAQUE ELEMENT.
!-----------------------------------------------------------------------
! IN
!     NPG     : NOMBRE DE POINTS DE GAUSS
!     LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!     IMATE   : ADRESSE DU MATERIAU CODE
!     COMPOR  : COMPORTEMENT :  (1) = TYPE DE RELATION COMPORTEMENT
!                               (2) = NB VARIABLES INTERNES / PG
!                               (3) = HYPOTHESE SUR LES DEFORMATIONS
!     OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
!     VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
!     S,Q     : QUANTITES CINEMATIQUES NECESSAIRES POUR CALCUL DU SAUT
!
! OUT
!     ALPHAP  : SAUT A L'INSTANT PLUS
!     VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
!     DALFS   : DEVIVEE DU SAUT PAR RAPPORT A S
!
!-----------------------------------------------------------------------
!
    if (compor(1) .eq. 'CZM_EXP') then
!
        call lcedex(option, imate, npg, lgpg, s,&
                    q, vim, vip, alphap, dalfs)
!
    else
!
        call utmess('F', 'ALGORITH7_69', sk=compor(1))
    endif
!
end subroutine

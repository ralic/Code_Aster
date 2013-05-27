subroutine ccchci(critz, questz, repi)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'asterfort/assert.h'
    character(len=*) :: critz, questz
    integer :: repi
! person_in_charge: mathieu.courtois at edf.fr
! ----------------------------------------------------------------------
!  CALC_CHAMP - TRAITEMENT DE CHAM_UTIL - CRITERE INFO
!  -    -                     --          -       -
!  RETOURNE DES INFOS SUR LE CRITERE CALCULE.
!  QUESTION :
!     'NBCMP' : RETOURNE LE NOMBRE DE COMPOSANTE A REMPLIR
! ----------------------------------------------------------------------
! IN  :
!   CRIT   K16  NOM DU CRITERE A CALCULER
!   QUEST  K16  QUESTION
! OUT :
!   REPI   I    VALEUR ENTIERE RETOURNEE
! ----------------------------------------------------------------------
    character(len=5) :: quest
    character(len=16) :: crit
!     ----- FIN  DECLARATIONS ------------------------------------------
    crit = critz
    quest = questz
!
    if (quest .eq. 'NBCMP') then
        if (crit .eq. 'VMIS' .or. crit .eq. 'INVA_2' .or. crit .eq. 'TRACE') then
            repi = 1
        else
            call assert(.false.)
        endif
    else
!       QUESTION INVALIDE
        call assert(.false.)
    endif
!
end subroutine

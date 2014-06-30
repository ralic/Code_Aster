subroutine cazoco(char, nomo, motfac, iform, izone,&
                  nzoco)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/assert.h"
#include "asterfort/cazocc.h"
#include "asterfort/cazocd.h"
#include "asterfort/cazocm.h"
#include "asterfort/cazocx.h"
    character(len=8) :: char, nomo
    character(len=16) :: motfac
    integer :: iform, izone, nzoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! LECTURE DES CARACTERISTIQUES DU CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMO   : NOM DU MODELE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'ZONE')
! IN  IFORM  : TYPE DE FORMULATION DE CONTACT
! IN  IZONE  : INDICE POUR LIRE LES DONNEES
!
! ----------------------------------------------------------------------
!
    logical(kind=1) :: lmail
!
! ----------------------------------------------------------------------
!
    lmail = (iform.eq.1).or.(iform.eq.2)
!
! --- LECTURE DES PRINCIPALES CARACTERISTIQUES DU CONTACT
! --- CONCERNANT L'APPARIEMENT ET SES OPTIONS
!
    if (lmail) then
        call cazocm(char, motfac, izone)
    endif
!
! --- LECTURE PARAMETRES SPECIFIQUES SUIVANT FORMULATION
!
    if (iform .eq. 1) then
        call cazocd(char, motfac, izone, nzoco)
    else if (iform.eq.2) then
        call cazocc(char, motfac, izone)
    else if (iform.eq.3) then
        call cazocx(char, nomo, motfac, izone)
    else
        ASSERT(.false.)
    endif
!
end subroutine

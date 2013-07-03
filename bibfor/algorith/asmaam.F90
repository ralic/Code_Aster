subroutine asmaam(meamor, numedd, solveu, lischa, matamo)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/asmatr.h"
    character(len=19) :: meamor
    character(len=24) :: numedd
    character(len=19) :: matamo
    character(len=19) :: solveu, lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! ASSEMBLAGE DE LA MATRICE D'AMORTISSEMENT GLOBALE
!
! ----------------------------------------------------------------------
!
!
! IN  MEAMOR : MATRICES ELEMENTAIRES D'AMORTISSEMENT
! IN  NUMEDD : NOM DE LA NUMEROTATION MECANIQUE
! IN  LISCHA : SD L_CHARGE
! IN  SOLVEU : NOM DU SOLVEUR DE NEWTON
! OUT MATAMO : MATRICE D'AMORTISSEMENT ASSEMBLEE
!
! ----------------------------------------------------------------------
!
    call asmatr(1, meamor, ' ', numedd, solveu,&
                lischa, 'ZERO', 'V', 1, matamo)
end subroutine

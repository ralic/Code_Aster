subroutine nmcvec(oper, typvez, optioz, lcalc, lasse,&
                  nbvect, ltypve, loptve, lcalve, lassve)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/assert.h'
    character(len=4) :: oper
    character(len=*) :: typvez, optioz
    logical :: lasse, lcalc
    integer :: nbvect
    character(len=6) :: ltypve(20)
    character(len=16) :: loptve(20)
    logical :: lassve(20), lcalve(20)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! GESTION DE LA LISTE DES VECT_ELEM A CALCULER ET ASSEMBLER
!
! ----------------------------------------------------------------------
!
!
! IN  OPER   : OPERATION SUR LA LISTE
!                'INIT'
!                'AJOU'
! IN  TYPVEC : TYPE DU VECT_ELEM
! IN  OPTION : OPTION DE CALCUL DU VECT_ELEM
! IN  LCALC  : LE VECT_ELEM SERA A CALCULER
! IN  LASSE  : LE VECT_ELEM SERA A ASSEMBLER
! I/O NBVECT : NOMBRE DE VECT_ELEM DANS LA LISTE
! I/O LTYPVE : LISTE DES TYPES DES VECT_ELEM
! I/O LOPTVE : LISTE DES OPTIONS DES VECT_ELEM
! I/O LCALVE : SI VECT_ELEM A CALCULER
! I/O LASSVE : SI VECT_ELEM A ASSEMBLER
!
! ----------------------------------------------------------------------
!
    character(len=16) :: option
    character(len=6) :: k6bla, typvec
    integer :: i
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    typvec = typvez
    option = optioz
    k6bla = ' '
!
! --- OPERATIONS
!
    if (oper .eq. 'INIT') then
        do 10 i = 1, 20
            ltypve(i) = k6bla
10      continue
        nbvect = 0
    else if (oper.eq.'AJOU') then
        nbvect = nbvect + 1
        if (nbvect .eq. 21) then
            call assert(.false.)
        endif
!
! --- UTILISER NMFINT !
!
        if (typvec .eq. 'CNFINT') then
            call assert(.false.)
        endif
        ltypve(nbvect) = typvec
        loptve(nbvect) = option
        lassve(nbvect) = lasse
        lcalve(nbvect) = lcalc
    else
        call assert(.false.)
    endif
!
end subroutine

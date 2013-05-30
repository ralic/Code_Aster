subroutine asmama(memasz, medirz, numedd, solveu, lischa,&
                  matmas)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/asmatr.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=24) :: numedd
    character(len=*) :: memasz, matmas, medirz
    character(len=19) :: solveu, lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! ASSEMBLAGE DE LA MATRICE DE MASSE GLOBALE
!
! ----------------------------------------------------------------------
!
!
! IN  MEMASS : MATRICES ELEMENTAIRES DE MASSE
! IN  MEDIRZ : MATRICES ELEMENTAIRES DE DIRICHLET
! IN  NUMEDD : NOM DE LA NUMEROTATION MECANIQUE
! IN  LISCHA : SD L_CHARGE
! IN  SOLVEU : NOM DU SOLVEUR DE NEWTON
! OUT MATMAS : MATRICE DE MASSE ASSEMBLEE
!
!
!
!
    character(len=19) :: mediri, memass, tlimat(2)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    mediri = medirz
    memass = memasz
!
    if (mediri .eq. ' ') then
        call asmatr(1, memass, ' ', numedd, solveu,&
                    lischa, 'ZERO', 'V', 1, matmas)
    else
        tlimat(1) = memass
        tlimat(2) = mediri
        call asmatr(2, tlimat, ' ', numedd, solveu,&
                    lischa, 'ZERO', 'V', 1, matmas)
    endif
!
    call jedema()
end subroutine

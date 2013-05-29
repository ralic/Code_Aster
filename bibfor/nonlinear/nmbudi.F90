subroutine nmbudi(modele, numedd, lischa, veclag, vebudi,&
                  cnbudi, matass)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/assvec.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmdebg.h'
    include 'asterfort/vebume.h'
    character(len=19) :: lischa, matass
    character(len=24) :: modele, numedd
    character(len=19) :: veclag
    character(len=19) :: vebudi, cnbudi
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DES CONDITIONS DE DIRICHLET B.U
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  LISCHA : LISTE DES CHARGES
! IN  VECLAG : VECTEUR D'INCONNUES PORTANT LES LAGRANGES
! OUT VEBUDI : VECT_ELEM DES CONDITIONS DE DIRICHLET B.U
! OUT CNBUDI : VECT_ASSE DES CONDITIONS DE DIRICHLET B.U
! IN  MATASS : SD MATRICE ASSEMBLEE
!
!
!
!
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... ASSEMBLAGE DES REACTIONS '//&
        'D''APPUI'
    endif
!
! --- CALCUL <CNBUDI>
!
    call vebume(modele, matass, veclag, lischa, vebudi)
!
! --- ASSEMBLAGE <CNBUDI>
!
    call assvec('V', cnbudi, 1, vebudi, 1.d0,&
                numedd, ' ', 'ZERO', 1)
!
    if (niv .ge. 2) then
        call nmdebg('VECT', cnbudi, 6)
    endif
!
    call jedema()
end subroutine

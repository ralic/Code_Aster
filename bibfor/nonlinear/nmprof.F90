subroutine nmprof(modele, result, lischa, solveu, numedd)
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
    include 'asterfort/gnomsd.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/numero.h'
    include 'asterfort/rsnume.h'
    character(len=24) :: modele, numedd
    character(len=8) :: result
    character(len=19) :: lischa, solveu
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DE LA NUMEROTATION ET DU PROFIL DE LA MATRICE
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM UTILISATEUR DU RESULTAT DE MECA_NON_LINE
! IN  MODELE : MODELE MECANIQUE
! IN  LISCHA : LISTE DES CHARGES
! IN  SOLVEU : NOM DU SOLVEUR
! OUT NUMEDD : NOM DE LA NUMEROTATION MECANIQUE
!
!
!
!
    character(len=14) :: nuposs
    character(len=24) :: noojb
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
        write (ifm,*) '<MECANONLINE> ... CREATION PROFIL DE LA MATRICE'
    endif
!
! --- CREATION PROFIL
!
    numedd = '12345678.NUMED'
    noojb = '12345678.00000.NUME.PRNO'
    call gnomsd(' ', noojb, 10, 14)
    numedd = noojb(1:14)
    call rsnume(result, 'DEPL', nuposs)
    call numero(nuposs, modele, lischa, solveu, 'VG',&
                numedd)
!
    call jedema()
!
end subroutine

subroutine nmlect(result, modele, mate, carele, compor,&
                  lischa, solveu)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'asterc/getres.h'
    include 'asterfort/cresol.h'
    include 'asterfort/nmdome.h'
    character(len=8) :: result
    character(len=19) :: lischa, solveu
    character(len=24) :: modele, mate, carele, compor
!
! ----------------------------------------------------------------------
!
! COMMANDES MECA_STATIQUE & STAT_NON_LINE
!
! LECTURE DES OPERANDES
!
! ----------------------------------------------------------------------
!
! OUT RESULT : NOM UTILISATEUR DU RESULTAT DE STAT_NON_LINE
! OUT MODELE : NOM DU MODELE
! OUT MATE   : NOM DU CHAMP DE MATERIAU
! OUT CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! OUT COMPOR : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
! OUT LISCHA : SD L_CHARGES
! OUT SOLVEU : NOM DU SOLVEUR
!
! ----------------------------------------------------------------------
!
    integer :: ibid
    character(len=16) :: k16bid, nomcmd
    character(len=8) :: k8bla
!
! ----------------------------------------------------------------------
!
    k8bla = ' '
!
! -- NOM UTILISATEUR DU CONCEPT RESULTAT CREE PAR LA COMMANDE
!
    call getres(result, k16bid, nomcmd)
!
! -- DONNEES MECANIQUES
!
    modele = ' '
    call nmdome(modele, mate, carele, lischa, k8bla,&
                ibid)
!
! --- PARAMETRES DONNES APRES LE MOT-CLE FACTEUR SOLVEUR
!
    call cresol(solveu)
!
! --- DANS LE CAS MECA_STATIQUE ON UTILISE LA CARTE COMPOR
!    STOCKEE DANS LE MATERIAU POUR LES POU_D_EM
!    (PAS UTILISEE PAR LES AUTRES ELEMENTS)
!
    if (nomcmd(1:13) .eq. 'MECA_STATIQUE') then
        compor = mate(1:8)//'.COMPOR'
    endif
!
end subroutine

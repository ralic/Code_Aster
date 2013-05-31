subroutine nxlect(result, modele, mate, carele, matcst,&
                  coecst, fomult, lischa, charge, infoch,&
                  parmei, parmer, solveu, parcri, parcrr,&
                  compor, evolsc)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: Christophe-mmn.durand at edf.fr
!
! aslint: disable=W1504
    implicit none
    include 'asterc/getres.h'
    include 'asterfort/cresol.h'
    include 'asterfort/nmdorc.h'
    include 'asterfort/ntdcom.h'
    include 'asterfort/ntdomt.h'
    include 'asterfort/ntdoth.h'
    include 'asterfort/nxdocn.h'
    include 'asterfort/nxdomt.h'
    character(len=8) :: evolsc
    logical :: matcst, coecst
    integer :: parmei(2), parcri(3)
    real(kind=8) :: parmer(2), parcrr(2)
    character(len=19) :: lischa, solveu
    character(len=24) :: result, modele, mate, carele, compor
    character(len=24) :: fomult, charge, infoch
!
! ----------------------------------------------------------------------
!
! COMMANDES DE THERMIQUE THER_LINEAIRE ET THER_NON_LINE :
! LECTURE DES OPERANDES.
!
! ----------------------------------------------------------------------
!
    integer :: ibid
    character(len=16) :: k16bid, nomcmd
    character(len=24) :: k24bid
    character(len=8) :: k8bla
!
! ----------------------------------------------------------------------
!
    k8bla = ' '
!
! --- NOM UTILISATEUR DU CONCEPT RESULTAT CREE PAR LA COMMANDE
!
    call getres(result, k16bid, nomcmd)
!
! --- DONNEES THERMIQUES
!
    charge = lischa//'.LCHA'
    infoch = lischa//'.INFC'
    fomult = lischa//'.FCHA'
    call ntdoth(modele, mate, carele, fomult, matcst,&
                coecst, lischa, k8bla, ibid)
!
! --- COMPORTEMENT
!
    if (nomcmd(1:13) .ne. 'THER_LINEAIRE') then
        call nmdorc(modele, compor, k24bid)
    endif
!
! --- PARAMETRES DONNES APRES LE MOT-CLE FACTEUR SOLVEUR
!
    call cresol(solveu)
!
    if (nomcmd(1:13) .eq. 'THER_NON_LINE') then
!
! ----- PARAMETRES DE RESOLUTION
!
        call nxdomt(parmei, parmer)
!
! ----- CHAMP THERMIQUE DU SECHAGE, ET VERIFICATION SYNTAXIQUE
!
        call ntdcom(evolsc)
!
! ----- CRITERES DE CONVERGENCE
!
        call nxdocn(parcri, parcrr)
!
    else
!
! ----- PARAMETRES DE RESOLUTION
!
        call ntdomt(parmer)
!
    endif
!
end subroutine

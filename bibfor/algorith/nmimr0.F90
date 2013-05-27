subroutine nmimr0(sdimpr, nombcl)
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
    implicit     none
    include      'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmimca.h'
    include 'asterfort/obgeti.h'
    include 'asterfort/obgetk.h'
    include 'asterfort/obgeto.h'
    include 'asterfort/oblgai.h'
    include 'asterfort/oblgoi.h'
    character(len=24) :: sdimpr
    character(len=4) :: nombcl
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! INITIALISATION AFFECTATION DES COLONNES
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  NOMBCL : NOM DE LA BOUCLE
!               'RESI' - BOUCLE SUR LES RESIDUS D'EQUILIBRE
!               'NEWT' - BOUCLE DE NEWTON
!               'FIXE' - BOUCLE DE POINT FIXE
!               'INST' - BOUCLE SUR LES PAS DE TEMPS
!               'CALC' - CALCUL
! IN  SDCONV : SD GESTION DE LA CONVERGENCE
!
! ----------------------------------------------------------------------
!
    integer :: icol, ncol
    character(len=9) :: typcol
    logical :: laffe, lacti
    character(len=24) :: slcolo, sdtabc, sdcolo
    character(len=4) :: lieuin
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    laffe = .false.
!
! --- LISTE DES COLONNES DISPONIBLES DANS LE TABLEAU DE CONVERGENCE
!
    call obgeto(sdimpr, 'TABLEAU_CONV', sdtabc)
    call obgeto(sdtabc, 'COLONNES_DISPOS', slcolo)
    call obgeti(slcolo, 'NBRE_STRUCTS', ncol)
!
! --- DESAFFECTE TOUTES LES COLONNES
!
    do 30 icol = 1, ncol
        call oblgoi(slcolo, icol, sdcolo)
        call oblgai(slcolo, icol, lacti)
        if (lacti) then
            call obgetk(sdcolo, 'TYPE_COLONNE', typcol)
            call obgetk(sdcolo, 'TYPE_COLONNE', lieuin)
            if (nombcl .eq. lieuin) call nmimca(sdimpr, typcol, laffe)
        endif
30  end do
!
    call jedema()
end subroutine

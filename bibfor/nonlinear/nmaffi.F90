subroutine nmaffi(fonact, sdconv, sdimpr, sderro, sddisc,&
                  nombcl)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmaffm.h'
    include 'asterfort/nmerim.h'
    include 'asterfort/nmevim.h'
    include 'asterfort/nmimpr.h'
    include 'asterfort/nmimps.h'
    include 'asterfort/nmimpx.h'
    include 'asterfort/nmlecv.h'
    include 'asterfort/nmltev.h'
    character(len=4) :: nombcl
    character(len=24) :: sdimpr, sderro, sdconv
    character(len=19) :: sddisc
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! AFFICHAGES PENDANT UNE BOUCLE
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDIMPR : SD AFFICHAGE
! IN  SDERRO : GESTION DES ERREURS
! IN  NOMBCL : NOM DE LA BOUCLE
!               'NEWT' - BOUCLE DE NEWTON
!               'FIXE' - BOUCLE DE POINT FIXE
!               'INST' - BOUCLE SUR LES PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
    logical :: lerrei
    logical :: cvnewt, cvinst
    logical :: ltabl
    logical :: lboucl, lexpl
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ltabl = .false.
!
! --- FONCTIONNALITES ACTIVEES
!
    lboucl = isfonc(fonact,'BOUCLE_EXTERNE')
    lexpl = isfonc(fonact,'EXPLICITE')
!
! --- CONVERGENCES
!
    call nmlecv(sderro, 'NEWT', cvnewt)
    call nmlecv(sderro, 'INST', cvinst)
!
! --- MARQUAGE DES COLONNES
!
    call nmaffm(sderro, sdimpr, nombcl)
!
! --- EVENEMENTS DE TYPE ERREURS
!
    call nmltev(sderro, 'ERRI', nombcl, lerrei)
!
! --- DOIT-ON IMPRIMER LA LIGNE DU TABLEAU ?
!
    if (nombcl .eq. 'NEWT') then
        if (cvnewt) then
            if (lboucl) then
                ltabl = .false.
            else
                ltabl = .true.
            endif
        else
            ltabl = .true.
        endif
    else if (nombcl.eq.'FIXE') then
        if (lboucl) ltabl = .true.
        if (.not.cvnewt) ltabl = .false.
    else if (nombcl.eq.'INST') then
        ltabl = .false.
    endif
!
! --- AFFICHAGE LIGNE DU TABLEAU DE CONVERGENCE
!
    if (ltabl) call nmimpr(sdimpr)
!
! --- AFFICHAGE LIGNE DE CONVERGENCE
!
    if (ltabl) then
        if (cvnewt .and. .not.(lerrei)) then
            call nmimpx(sdimpr)
        endif
    endif
!
! --- AFFICHAGE DE L'ERREUR
!
    if (lerrei) then
        call nmimpx(sdimpr)
        call nmerim(sderro)
    endif
!
! --- AFFICHAGE DE L'EVENEMENT
!
    call nmevim(sdimpr, sddisc, sderro, nombcl)
!
! --- AFFICHAGE RECAP CONVERGENCE
!
    if (cvinst .and. .not.lexpl) then
        call nmimps(sdimpr, sdconv, sderro)
    endif
!
    call jedema()
end subroutine

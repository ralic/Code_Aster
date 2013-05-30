subroutine nmcrsd(typesd, nomsd)
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
    include 'asterfort/assert.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: typesd
    character(len=*) :: nomsd
!
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! CREATION D'UNE SD
!
! ----------------------------------------------------------------------
!
!
! IN  TYPESD :  TYPE DE LA SD
!               'POST_TRAITEMENT' - MODES VIBRATOIRES OU FLAMBAGE
!               'ENERGIE        ' - ENERGIES
! IN  NOMSD  : NOM DE LA SD
!
!
!
!
    integer :: zposti, zpostr, zpostk, zener
    parameter   (zposti=11,zpostr=8,zpostk=14,zener=6)
!
    integer :: ifm, niv
    character(len=24) :: sdinfi, sdinfr, sdinfk
    integer :: jpinfi, jpinfr, jpinfk, jener
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION SD'
    endif
!
! --- CREATION
!
    if (typesd .eq. 'POST_TRAITEMENT') then
        sdinfi = nomsd(1:19)//'.INFI'
        sdinfr = nomsd(1:19)//'.INFR'
        sdinfk = nomsd(1:19)//'.INFK'
        call wkvect(sdinfi, 'V V I  ', zposti, jpinfi)
        call wkvect(sdinfr, 'V V R  ', zpostr, jpinfr)
        call wkvect(sdinfk, 'V V K24', zpostk, jpinfk)
    else if (typesd.eq.'ENERGIE') then
! COMPOSANTES DE SDENER//'.VALE'
! WEXT : TRAVAIL DES EFFORTS EXTERIEURS
! ECIN : ENERGIE CINETIQUE
! WINT : ENERGIE DE DEFORMATION IRREVERSIBLE
! AMOR : ENERGIE DISSIPEE PAR AMORTISSEMENT
! LIAI : ENERGIE DISSIPEE PAR LES LIAISONS
! WSCH : ENERGIE DISSIPEE PAR LE SCHEMA
        call wkvect(nomsd(1:19)//'.VALE', 'V V R  ', zener, jener)
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine

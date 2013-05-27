subroutine nmcrdd(noma, nomo, sdieto, sdsuiv)
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
    include 'asterc/getfac.h'
    include 'asterfort/assert.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmcrdn.h'
    include 'asterfort/nmextr.h'
    character(len=8) :: noma, nomo
    character(len=24) :: sdieto, sdsuiv
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES)
!
! LECTURE SUIVI_DDL ET CREATION DE LA SD SUIVI_DDL
!
! ----------------------------------------------------------------------
!
! IN  SDIETO : SD GESTION IN ET OUT
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  SDSUIV : NOM DE LA SD POUR SUIVI_DDL
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: ntsddl, nbocc, numreu
    character(len=16) :: motfac
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION SD SUIVI_DDL'
    endif
!
! --- INITIALISATIONS
!
    ntsddl = 0
    numreu = 0
    motfac = 'SUIVI_DDL'
!
! --- NOMBRE OCCURRENCES
!
    call getfac(motfac, nbocc)
    call assert(nbocc.le.99)
!
! --- LECTURE DES DONNEES
!
    call nmextr(noma, nomo, sdsuiv, sdieto, motfac,&
                nbocc, numreu, ntsddl)
!
! --- NOM DES COLONNES
!
    if (nbocc .ne. 0) then
        call nmcrdn(sdsuiv, motfac, ntsddl, nbocc)
    endif
!
    call jedema()
!
end subroutine

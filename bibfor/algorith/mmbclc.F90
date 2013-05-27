subroutine mmbclc(noma, nomo, numedd, iterat, numins,&
                  sddisc, sddyna, sdimpr, defico, resoco,&
                  valinc, solalg, sdtime, sdstat, mmcvca)
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
    include 'asterfort/cfdisl.h'
    include 'asterfort/copisd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mmappa.h'
    include 'asterfort/mmchml.h'
    include 'asterfort/mmligr.h'
    include 'asterfort/mmmbca.h'
    include 'asterfort/mreacg.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/nmimci.h'
    include 'asterfort/nmrinc.h'
    include 'asterfort/nmtime.h'
    character(len=8) :: noma, nomo
    integer :: numins, iterat
    character(len=19) :: sddisc, sddyna
    character(len=24) :: defico, resoco, sdtime, sdstat, sdimpr, numedd
    character(len=19) :: valinc(*), solalg(*)
    logical :: mmcvca
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - ALGORITHME)
!
! CHANGEMENT DES STATUTS SI CONTACT CONTINU NEWTON GENERALISE
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  NUMEDD : NUMEROTATION NUME_DDL
! IN  NUMINS : NUMERO D'INSTANT
! IN  SDDISC : SD DISCRETISATION
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDIMPR : SD AFFICHAGE
! IN  DEFICO : SD DEFINITION CONTACT
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! OUT MMCVCA : INDICATEUR DE CONVERGENCE POUR BOUCLE DES
!              CONTRAINTES ACTIVES
!               .TRUE. SI LA BOUCLE DES CONTRAINTES ACTIVES A CONVERGE
!
! ----------------------------------------------------------------------
!
    logical :: lallv, lnewtc, lnewtg
    logical :: loptin
    integer :: ctcsta
    character(len=19) :: depgeo, depplu
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACTIVATION DES OPTIONS *_INIT
!
    loptin = .false.
!
! --- FONCTIONNALITES ACTIVEES
!
    lallv = cfdisl(defico,'ALL_VERIF')
    lnewtc = cfdisl(defico,'CONT_NEWTON')
    lnewtg = cfdisl(defico,'GEOM_NEWTON')
    ctcsta = 0
    if (lallv) then
        mmcvca = .true.
        goto 999
    endif
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    depgeo = resoco(1:14)//'.DEPG'
!
! --- NOUVEL APPARIEMENT
!
    if (lnewtg) then
        call copisd('CHAMP_GD', 'V', depplu, depgeo)
        call nmtime(sdtime, 'INI', 'CONT_GEOM')
        call nmtime(sdtime, 'RUN', 'CONT_GEOM')
        call mreacg(noma, resoco)
        call mmappa(loptin, noma, numedd, defico, resoco)
        call nmtime(sdtime, 'END', 'CONT_GEOM')
        call nmrinc(sdstat, 'CONT_GEOM')
    endif
!
! --- NOUVELLE NUMEROTATION (ELEMENTS TARDIFS DE CONTACT)
!
    if (lnewtg) then
        call nmtime(sdtime, 'INI', 'CTCC_PREP')
        call nmtime(sdtime, 'RUN', 'CTCC_PREP')
        call mmligr(noma, nomo, defico, resoco)
        call nmtime(sdtime, 'END', 'CTCC_PREP')
    endif
!
! --- CHANGEMENT DES STATUTS
!
    if (lnewtc .or. lnewtg) then
        call nmtime(sdtime, 'INI', 'CTCC_CONT')
        call nmtime(sdtime, 'RUN', 'CTCC_CONT')
        call mmmbca(noma, sddyna, iterat, defico, resoco,&
                    valinc, solalg, ctcsta, mmcvca)
        call nmtime(sdtime, 'END', 'CTCC_CONT')
        call nmrinc(sdstat, 'CTCC_CONT')
    endif
!
! --- MISE A JOUR DE LA CARTE
!
    if (lnewtc .or. lnewtg) then
        call nmtime(sdtime, 'INI', 'CTCC_PREP')
        call nmtime(sdtime, 'RUN', 'CTCC_PREP')
        call mmchml(noma, defico, resoco, sddisc, sddyna,&
                    numins)
        call nmtime(sdtime, 'END', 'CTCC_PREP')
        call nmrinc(sdstat, 'CTCC_PREP')
    endif
!
999  continue
!
! --- STATUTS DE CONTACT EN NEWTON GENERALISE
!
    if (lnewtc) call nmimci(sdimpr, 'CONT_NEWT', ctcsta, .true.)
!
    call jedema()
!
end subroutine

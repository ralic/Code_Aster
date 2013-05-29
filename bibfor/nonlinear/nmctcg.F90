subroutine nmctcg(modele, noma, defico, resoco, loptin,&
                  sdstat, sdtime, numedd)
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
    include 'asterfort/cfdisi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/mmappa.h'
    include 'asterfort/mreacg.h'
    include 'asterfort/nmrinc.h'
    include 'asterfort/nmtime.h'
    include 'asterfort/xappar.h'
    include 'asterfort/xreacg.h'
    character(len=8) :: noma
    character(len=24) :: modele
    character(len=24) :: defico, resoco
    character(len=24) :: sdstat, sdtime
    character(len=24) :: numedd
    logical :: loptin
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGO - BOUCLE CONTACT)
!
! BOUCLE DE POINT FIXE GEOMETRIQUE - APPARIEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  LOPTIN : VAUT .TRUE. SI ACTIVATION DES OPTIONS *_INIT
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  NUMEDD : NOM DU NUME_DDL
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=8) :: nomo
    integer :: iform
!
! ----------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
!
! --- INITIALISATIONS
!
    nomo = modele(1:8)
!
! --- TYPE DE CONTACT
!
    iform = cfdisi(defico,'FORMULATION')
    if (iform .eq. 1) goto 99
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*)&
     &     '<MECANONLINE> BOUCLE DE REACTUALISATION GEOMETRIQUE'
    endif
!
! --- REACTUALISATION DE LA GEOMETRIE ET APPARIEMENT
!
    call nmtime(sdtime, 'INI', 'CONT_GEOM')
    call nmtime(sdtime, 'RUN', 'CONT_GEOM')
    if (iform .eq. 3) then
        call xreacg(nomo, resoco)
        call xappar(loptin, noma, nomo, defico, resoco)
    else if (iform.eq.2) then
        call mreacg(noma, resoco)
        call mmappa(loptin, noma, numedd, defico, resoco)
    endif
    call nmtime(sdtime, 'END', 'CONT_GEOM')
    call nmrinc(sdstat, 'CONT_GEOM')
!
99  continue
!
end subroutine

subroutine cfmxpo(noma, modelz, defico, resoco, numins,&
                  sddisc, sdstat, solalg, valinc, veasse)
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
    implicit      none
    include 'jeveux.h'
    include 'asterfort/cfdeco.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfmxre.h'
    include 'asterfort/cfverl.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mmdeco.h'
    include 'asterfort/xmdeco.h'
    character(len=24) :: resoco, defico, sdstat
    character(len=8) :: noma
    character(len=19) :: sddisc
    character(len=19) :: solalg(*), veasse(*), valinc(*)
    character(len=*) :: modelz
    integer :: numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (POST_TRAITEMENT)
!
! POST_TRAITEMENT DU CONTACT (TOUTES METHODES)
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  MODELE : SD MODELE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMINS : NUMERO DU PAS DE CHARGE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
!
!
!
    integer :: ifm, niv
    logical :: lctcd, lctcc, lallv, lxfcm
    character(len=8) :: nomo
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- INITIALISATIONS
!
    nomo = modelz
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lctcd = cfdisl(defico,'FORMUL_DISCRETE')
    lallv = cfdisl(defico,'ALL_VERIF')
    lxfcm = cfdisl(defico,'FORMUL_XFEM')
!
! --- GESTION DE LA  DECOUPE
!
    if (.not.lallv) then
        if (lctcd) then
            call cfdeco(defico, resoco)
        else if (lctcc) then
            call mmdeco(defico, resoco)
        else if (lxfcm) then
            call xmdeco(resoco)
        endif
    endif
!
! --- VERIFICATION FACETTISATION
!
    if (lctcd .or. lctcc) then
        call cfverl(defico, resoco)
    endif
!
! --- REMPLISSAGE DU CHAM_NO VALE_CONT ET PERCUSSION
!
    call cfmxre(noma, nomo, sdstat, defico, resoco,&
                numins, sddisc, solalg, valinc, veasse)
!
    call jedema()
end subroutine

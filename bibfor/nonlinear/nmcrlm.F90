subroutine nmcrlm(lisins, sddisc, provli, tpsinf)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/dfllli.h'
    include 'asterfort/dfllvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedup1.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/utdidt.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: tpsinf
    character(len=19) :: provli
    character(len=19) :: sddisc, lisins
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - DISCRETISATION)
!
! CREATION LISTE D'INSTANT PROVISOIRE ET TPSINF A PARTIR DE LIST_R8
!
! ----------------------------------------------------------------------
!
! IN  LISINS : LISTE D'INSTANTS (SD_LISTR8 OU SD_LIST_INST)
! IN  SDDISC : SD DISCRETISATION
! OUT TPSINF : INFORMATIONS SUR LA DISCRETISATION
! OUT PROVLI : LISTE D'INSTANT PROVISOIRE
!
!
!
!
    integer :: llinr, jlinr
    integer :: nbinst, ibid
    real(kind=8) :: dtmin, r8bid
    character(len=8) :: metlis, k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    llinr = dfllvd('LLINR')
!
! --- QUELQUES VERIFICATIONS SUR LA LISTE FOURNIE
!
    call dfllli(lisins, dtmin, nbinst)
!
! --- DUPLICATION LISTE D'INSTANTS DANS OBJET PROVISOIRE
!
    call jedup1(lisins(1:19)//'.VALE', 'V', provli)
!
! --- CREATION SD TPSINF
!
    call wkvect(tpsinf, 'V V R', llinr, jlinr)
!
! --- REMPLISSAGE SD TPSINF
!
    metlis = 'MANUEL'
    call utdidt('E', sddisc, 'LIST', ibid, 'METHODE',&
                r8bid, ibid, metlis)
    call utdidt('E', sddisc, 'LIST', ibid, 'DTMIN',&
                dtmin, ibid, k8bid)
    call utdidt('E', sddisc, 'LIST', ibid, 'NBINST',&
                r8bid, nbinst, k8bid)
!
    call jedema()
!
end subroutine

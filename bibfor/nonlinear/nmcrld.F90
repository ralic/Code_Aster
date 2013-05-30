subroutine nmcrld(sddisc)
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
    include 'asterfort/dfllsv.h'
    include 'asterfort/dfllvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/utdidt.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - DISCRETISATION)
!
! CREATION EVENEMENTS ERREURS: ARRET
!
! ----------------------------------------------------------------------
!
! IN  SDDISC : SD DISCRETISATION
!
!
!
!
    integer :: leevr, leevk, lesur
    integer :: iecerr
    integer :: nechec
    character(len=24) :: tpsinf
    character(len=24) :: tpsevr, tpsevk, tpsesu
    integer :: jeevr, jeevk, jesur
    real(kind=8) :: r8bid
    character(len=8) :: k8bid
    integer :: ibid
    character(len=16) :: even
    real(kind=8) :: dtmin
    real(kind=8) :: pasmin, pcplus, penmax
    character(len=16) :: submet, subaut, action
    integer :: nbrpas, niveau
    real(kind=8) :: valere
    character(len=16) :: nocham, nocmp, cricmp
    real(kind=8) :: cmmaxi, prcoll, ducoll
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nechec = 2
    iecerr = 1
    call utdidt('E', sddisc, 'LIST', ibid, 'NECHEC',&
                r8bid, nechec, k8bid)
    call utdidt('L', sddisc, 'LIST', ibid, 'DTMIN',&
                dtmin, ibid, k8bid)
!
! --- TAILLE DES VECTEURS
!
    leevr = dfllvd('LEEVR')
    leevk = dfllvd('LEEVK')
    lesur = dfllvd('LESUR')
!
! --- NOM DES SDS
!
    tpsinf = sddisc(1:19)//'.LINF'
    tpsevr = sddisc(1:19)//'.EEVR'
    tpsevk = sddisc(1:19)//'.EEVK'
    tpsesu = sddisc(1:19)//'.ESUR'
!
! --- CREATION DES SD
!
    call wkvect(tpsevr, 'V V R', nechec*leevr, jeevr)
    call wkvect(tpsevk, 'V V K16', nechec*leevk, jeevk)
    call wkvect(tpsesu, 'V V R', nechec*lesur, jesur)
!
! --- VALEURS POUR ACTION = ARRET
!
    action = 'ARRET'
    submet = ' '
    subaut = ' '
    pasmin = 0.d0
    nbrpas = 0
    niveau = 0
    pcplus = 0.d0
    cmmaxi = 0.d0
    prcoll = 0.d0
    ducoll = 0.d0
    penmax = 0.d0
    cricmp = ' '
    valere = 0.d0
    nocham = ' '
    nocmp = ' '
!
! --- CREATION EVENEMENT 'ERRE'
!
    even = 'ERRE'
    call dfllsv(tpsinf, tpsevr, tpsevk, tpsesu, iecerr,&
                even, action, submet, subaut, pasmin,&
                nbrpas, niveau, pcplus, cmmaxi, prcoll,&
                ducoll, penmax, cricmp, valere, nocham,&
                nocmp)
!
    call jedema()
!
end subroutine

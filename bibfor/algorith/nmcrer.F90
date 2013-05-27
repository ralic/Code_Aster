subroutine nmcrer(carcri, sdcriq)
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
    include 'asterc/getvtx.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: sdcriq, carcri
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (SD CRITERE QUALITE)
!
! CREATION DE LA SD
!
! ----------------------------------------------------------------------
!
!
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! OUT SDCRIQ : SD CRITERE QUALITE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: iarg, ibid, jvale
    character(len=24) :: errthm
    integer :: jerrt
    character(len=16) :: motfac, chaine
    real(kind=8) :: theta
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... LECTURE CALCUL ERREUR'
    endif
!
! --- INITIALISATIONS
!
    motfac = 'CRIT_QUALITE'
!
! --- VALEUR DE THETA
!
    call jeveuo(carcri(1:19)//'.VALV', 'L', jvale)
    theta = zr(jvale+3)
!
! --- ERREUR EN TEMPS (THM)
!
    chaine='NON'
    call getvtx(motfac, 'ERRE_TEMPS_THM', 1, iarg, 1,&
                chaine, ibid)
    if (chaine .eq. 'OUI') then
        errthm = sdcriq(1:19)//'.ERRT'
        call wkvect(errthm, 'V V R', 3, jerrt)
        zr(jerrt-1+3) = theta
    endif
!
    call jedema()
end subroutine

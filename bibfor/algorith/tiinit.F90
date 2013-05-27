subroutine tiinit(mailla, modele, resulz, lostat, lreuse,&
                  lnonl, instin, sddisc, sdieto, sdobse,&
                  levol)
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
    implicit      none
    include 'asterc/getvid.h'
    include 'asterfort/nmcrob.h'
    include 'asterfort/ntcra0.h'
    include 'asterfort/ntcrar.h'
    include 'asterfort/ntcrli.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: instin
    character(len=8) :: mailla
    character(len=19) :: sddisc
    logical :: levol, lostat, lnonl, lreuse
    character(len=19) :: sdobse
    character(len=24) :: resulz, modele
    character(len=24) :: sdieto
!
! ----------------------------------------------------------------------
!
! ROUTINE THER_NON_LINE THER_LINEAIRE (STRUCTURES DE DONNES)
!
! CREATION SD DISCRETISATION, ARCHIVAGE ET OBSERVATION
!
! ----------------------------------------------------------------------
!
! IN  RESULT : NOM UTILISATEUR DU RESULTAT
! IN  MAILLA : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  SDIETO : SD GESTION IN ET OUT
! IN  INSTIN : INSTANT INITIAL QUAND ETAT_INIT
! IN  LOSTAT : .TRUE. SI CALCUL STATIONNAIRE PREALABLE OU PAS
! IN  LNONL  : .TRUE. SI NON-LINEAIRE
! IN  LREUSE : .TRUE. SI REUSE
! OUT LEVOL  : .TRUE. SI TRANSITOIRE
! OUT SDDISC : SD DISCRETISATION
! OUT SDOBSE : NOM DE LA SD POUR OBSERVATION
!
! ----------------------------------------------------------------------
!
    integer :: n1
    character(len=8) :: nomo, result
    character(len=19) :: lisins
    integer :: iarg, numreo
!
! ----------------------------------------------------------------------
!
    lisins = ' '
    levol = .false.
    nomo = modele(1:8)
    result = resulz(1:8)
!
    call getvid('INCREMENT', 'LIST_INST', 1, iarg, 1,&
                lisins, n1)
    if (n1 .eq. 0) then
        if (.not.lostat) then
            call u2mess('F', 'DISCRETISATION_8')
        endif
        levol = .false.
        call ntcra0(sddisc)
        goto 999
    else
        levol = .true.
    endif
!
! --- CREATION SD DISCRETISATION
!
    call ntcrli(instin, lisins, sddisc)
!
! --- CREATION SD ARCHIVAGE
!
    call ntcrar(result, sddisc, lreuse, numreo)
!
! --- CREATION SD OBSERVATION
!
    if (lnonl) then
        call nmcrob(mailla, nomo, result, numreo, sdieto,&
                    sdobse)
    endif
!
999  continue
!
end subroutine

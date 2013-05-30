subroutine nmprdc(method, numedd, depmoi, sddisc, numins,&
                  incest, depest)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/copisd.h'
    include 'asterfort/diinst.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsinch.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/vtcopy.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    character(len=16) :: method(*)
    character(len=19) :: depmoi, depest
    character(len=24) :: numedd
    character(len=19) :: sddisc, incest
    integer :: numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PREDICTION)
!
! PREDICTION PAR DEPLACEMENT CALCULE
!
! ----------------------------------------------------------------------
!
!
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION
! IN  NUMEDD : NUME_DDL
! IN  NUMINS : NUMERO INSTANT COURANT
! IN  SDDISC : SD DISC_INST
! IN  DEPMOI : DEPL. EN T-
! OUT INCEST : INCREMENT DE DEPLACEMENT EN PREDICTION
! OUT DEPEST : DEPLACEMENT ESTIME
!
!
!
!
    integer :: ifm, niv
    integer :: jdepes, jdepm, jinces, neq
    integer :: iret, ibid
    real(kind=8) :: instan
    character(len=8) :: k8bid
    character(len=19) :: deplu
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... PAR DEPL. CALCULE'
    endif
!
! --- INITIALISATIONS
!
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
    instan = diinst(sddisc,numins )
!
! --- INITIALISATIONS
!
    deplu = '&&NMPRDC.DEPEST'
!
! --- LECTURE DANS LE CONCEPT EVOL_NOLI
!
    call rsinch(method(6)(1:8), 'DEPL', 'INST', instan, deplu,&
                'EXCLU', 'EXCLU', 0, 'V', iret)
    if (iret .gt. 0) then
        call u2mesg('F', 'MECANONLINE2_27', 1, method(6)(1:8), 1,&
                    ibid, 1, instan)
    endif
!
! --- COPIE DU DEPLACEMENT ESTIME
!
    if (numins .eq. 1) then
        call vtcopy(deplu, depest, 'F', iret)
    else
        call copisd('CHAMP_GD', 'V', deplu, depest)
    endif
!
    call jeveuo(depest(1:19)//'.VALE', 'L', jdepes)
    call jeveuo(depmoi(1:19)//'.VALE', 'L', jdepm)
!
! --- INITIALISATION DE L'INCREMENT: INCEST = DEPEST - DEPMOI
!
    call jeveuo(incest(1:19)// '.VALE', 'E', jinces)
    call dcopy(neq, zr(jdepes), 1, zr(jinces), 1)
    call daxpy(neq, -1.d0, zr(jdepm), 1, zr(jinces),&
               1)
!
    call jedema()
end subroutine

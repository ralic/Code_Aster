subroutine nmevdg(sddisc, vale, iechec, ievdac)
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
    include 'asterfort/assert.h'
    include 'asterfort/extdch.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/tbacce.h'
    include 'asterfort/tbliva.h'
    include 'asterfort/utdidt.h'
    integer :: iechec, ievdac
    character(len=19) :: sddisc, vale(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
!
! GESTION DE L'EVENEMENT DELTA_GRANDEUR
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  VALE   : INCREMENTS DES VARIABLES
!               OP0070: VARIABLE CHAPEAU
!               OP0033: TABLE
! IN  IECHEC : OCCURRENCE DE L'ECHEC
! OUT IEVDAC : VAUT IECHEC SI EVENEMENT DECLENCHE
!                   0 SINON
!
!
!
!
    integer :: ifm, niv, ier
    real(kind=8) :: r8bid, valref, dval
    integer :: ibid
    character(len=8) :: k8bid, crit, typext
    complex(kind=8) :: c16bid
    character(len=16) :: nocham, nocmp
    parameter   (typext = 'MAX_ABS')
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... DELTA_GRANDEUR'
    endif
!
! --- INITIALISATIONS
!
    ievdac = 0
!
! --- PARAMETRES
!
    call utdidt('L', sddisc, 'ECHE', iechec, 'NOM_CHAM',&
                r8bid, ibid, nocham)
    call utdidt('L', sddisc, 'ECHE', iechec, 'NOM_CMP',&
                r8bid, ibid, nocmp)
    call utdidt('L', sddisc, 'ECHE', iechec, 'VALE_REF',&
                valref, ibid, k8bid)
    call utdidt('L', sddisc, 'ECHE', iechec, 'CRIT_COMP',&
                r8bid, ibid, crit)
!
! --- DVAL :MAX EN VALEUR ABSOLUE DU DELTA(CHAMP+CMP)
!
    if (vale(1)(1:8) .eq. '&&OP0033') then
!
!       RESULTAT DE CALC_POINT_MAT OP0033, FORMAT_TABLE='CMP_COLONNE',
        call tbacce(vale(1)(1:16), 1, nocmp, 'L', ibid,&
                    dval, c16bid, k8bid)
    else if (vale(1)(1:8).eq.'&&OPB033') then
!
!       RESULTAT DE CALC_POINT_MAT OP0033, FORMAT_TABLE='CMP_LIGNE',
        call tbliva(vale(1)(1:16), 1, 'CMP', ibid, r8bid,&
                    c16bid, nocmp, 'EGAL', 0.d0, 'VALEUR',&
                    k8bid, ibid, dval, c16bid, k8bid,&
                    ier)
        if (ier .ne. 0) then
            dval=0.d0
        endif
    else
!
!       RESULTAT DE STAT_NON_LINE
        call extdch(typext, vale, nocham, nocmp, dval)
    endif
!
    call assert(crit.eq.'GT')
!
    if (dval .gt. valref) then
        ievdac = iechec
    endif
!
    call jedema()
end subroutine

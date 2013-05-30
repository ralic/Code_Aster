subroutine mdchdl(nbnli, noecho, lnoue2, iliai, ddlcho,&
                  ier)
    implicit  none
    include 'asterfort/posddl.h'
    include 'asterfort/u2mesk.h'
    integer :: nbnli, iliai, ddlcho(*), ier
    logical :: lnoue2
    character(len=8) :: noecho(nbnli, *)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     ROUTINE APPELEE PAR MDCHOC
!     TRAITEMENT DES DDL
!
! IN  : NBNLI  : DIMENSION DES TABLEAUX (NBCHOC+NBSISM+NBFLAM)
! IN  : NOECHO : DEFINITION DES NOEUDS DE CHOC
! IN  : LNOUE2 : CHOC DEFINIT ENTRE 2 NOEUDS
! IN  : ILIAI  : NUMERO DE LA LIAISON TRAITEE
! OUT : DDLCHO : TABLEAU DES NUMEROTATIONS DES NOEUDS DE CHOC
! OUT : IER    : NIVEAU D'ERREUR
!     ------------------------------------------------------------------
    integer :: nunoe, nuddl
    character(len=8) :: nume1, noeu1, nume2, noeu2
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
!
    nume1 = noecho(iliai,3)
    noeu1 = noecho(iliai,1)
    nume2 = noecho(iliai,7)
    noeu2 = noecho(iliai,5)
!
    call posddl('NUME_DDL', nume1, noeu1, 'DX', nunoe,&
                nuddl)
    if (nunoe .eq. 0) then
        ier = ier + 1
        valk(1) = noeu1
        valk(2) = noecho(iliai,4)
        call u2mesk('E', 'ALGORITH5_27', 2, valk)
    endif
    if (nuddl .eq. 0) then
        ier = ier + 1
        call u2mesk('E', 'ALGORITH5_28', 1, noeu1)
    endif
    ddlcho(6*(iliai-1)+1) = nuddl
!
    call posddl('NUME_DDL', nume1, noeu1, 'DY', nunoe,&
                nuddl)
    if (nuddl .eq. 0) then
        ier = ier + 1
        call u2mesk('E', 'ALGORITH5_29', 1, noeu1)
    endif
    ddlcho(6*(iliai-1)+2) = nuddl
!
    call posddl('NUME_DDL', nume1, noeu1, 'DZ', nunoe,&
                nuddl)
    if (nuddl .eq. 0) then
        ier = ier + 1
        call u2mesk('E', 'ALGORITH5_30', 1, noeu1)
    endif
    ddlcho(6*(iliai-1)+3) = nuddl
!
    if (lnoue2) then
        call posddl('NUME_DDL', nume2, noeu2, 'DX', nunoe,&
                    nuddl)
        if (nunoe .eq. 0) then
            ier = ier + 1
            valk(1) = noeu2
            valk(2) = noecho(iliai,8)
            call u2mesk('E', 'ALGORITH5_27', 2, valk)
        endif
        if (nuddl .eq. 0) then
            ier = ier + 1
            call u2mesk('E', 'ALGORITH5_28', 1, noeu2)
        endif
        ddlcho(6*(iliai-1)+4) = nuddl
!
        call posddl('NUME_DDL', nume2, noeu2, 'DY', nunoe,&
                    nuddl)
        if (nuddl .eq. 0) then
            ier = ier + 1
            call u2mesk('E', 'ALGORITH5_29', 1, noeu2)
        endif
        ddlcho(6*(iliai-1)+5) = nuddl
!
        call posddl('NUME_DDL', nume2, noeu2, 'DZ', nunoe,&
                    nuddl)
        if (nuddl .eq. 0) then
            ier = ier + 1
            call u2mesk('E', 'ALGORITH5_30', 1, noeu2)
        endif
        ddlcho(6*(iliai-1)+6) = nuddl
    else
        ddlcho(6*(iliai-1)+4) = ddlcho(6*(iliai-1)+1)
        ddlcho(6*(iliai-1)+5) = ddlcho(6*(iliai-1)+2)
        ddlcho(6*(iliai-1)+6) = ddlcho(6*(iliai-1)+3)
    endif
!
end subroutine

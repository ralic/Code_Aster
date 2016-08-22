subroutine mdchdl(lnoue2, iliai, ddlcho, ier)
    implicit none
#include "asterf_types.h"
#include "asterfort/posddl.h"
#include "asterfort/utmess.h"
#include "asterfort/nlget.h"
#include "asterfort/nlsav.h"

    integer :: iliai, ddlcho(*), ier
    aster_logical :: lnoue2
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8) :: nume1, noeu1, nume2, noeu2, sd_nl
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
!
    sd_nl = '&&OP29NL'
    call nlget(sd_nl, _NUMDDL_1, iocc=iliai, kscal=nume1)
    call nlget(sd_nl, _NO1_NAME, iocc=iliai, kscal=noeu1)
!
    call posddl('NUME_DDL', nume1, noeu1, 'DX', nunoe,&
                nuddl)
    if (nunoe .eq. 0) then
        ier = ier + 1
        valk(1) = noeu1
        call nlget(sd_nl, _MESH_1, iocc=iliai, kscal=valk(2))
        call utmess('E', 'ALGORITH5_27', nk=2, valk=valk)
    endif
    if (nuddl .eq. 0) then
        ier = ier + 1
        call utmess('E', 'ALGORITH5_28', sk=noeu1)
    endif
    ddlcho(1) = nuddl
!
    call posddl('NUME_DDL', nume1, noeu1, 'DY', nunoe,&
                nuddl)
    if (nuddl .eq. 0) then
        ier = ier + 1
        call utmess('E', 'ALGORITH5_29', sk=noeu1)
    endif
    ddlcho(2) = nuddl
!
    call posddl('NUME_DDL', nume1, noeu1, 'DZ', nunoe,&
                nuddl)
    if (nuddl .eq. 0) then
        ier = ier + 1
        call utmess('E', 'ALGORITH5_30', sk=noeu1)
    endif
    ddlcho(3) = nuddl
!
    if (lnoue2) then

        call nlget(sd_nl, _NUMDDL_2, iocc=iliai, kscal=nume2)
        call nlget(sd_nl, _NO2_NAME, iocc=iliai, kscal=noeu2)

        call posddl('NUME_DDL', nume2, noeu2, 'DX', nunoe,&
                    nuddl)
        if (nunoe .eq. 0) then
            ier = ier + 1
            valk(1) = noeu2
            call nlget(sd_nl, _MESH_2, iocc=iliai, kscal=valk(2))
            call utmess('E', 'ALGORITH5_27', nk=2, valk=valk)
        endif
        if (nuddl .eq. 0) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_28', sk=noeu2)
        endif
        ddlcho(4) = nuddl
!
        call posddl('NUME_DDL', nume2, noeu2, 'DY', nunoe,&
                    nuddl)
        if (nuddl .eq. 0) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_29', sk=noeu2)
        endif
        ddlcho(5) = nuddl
!
        call posddl('NUME_DDL', nume2, noeu2, 'DZ', nunoe,&
                    nuddl)
        if (nuddl .eq. 0) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_30', sk=noeu2)
        endif
        ddlcho(6) = nuddl
    else
        ddlcho(4) = ddlcho(1)
        ddlcho(5) = ddlcho(2)
        ddlcho(6) = ddlcho(3)
    endif
!
end subroutine

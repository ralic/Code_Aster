subroutine ntinit(result, modele, mate, carele, lischa,&
                  lisch2, solveu, para, numedd, lostat,&
                  levol, lnonl, sddisc, sdieto, mailla,&
                  sdcrit, time)
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jessica.haelewyn at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/gcucon.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gnomsd.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ntcrch.h"
#include "asterfort/ntcrcv.h"
#include "asterfort/ntetcr.h"
#include "asterfort/numero.h"
#include "asterfort/nxdoet.h"
#include "asterfort/nxnoli.h"
#include "asterfort/rsnume.h"
#include "asterfort/tiinit.h"
    logical(kind=1) :: lostat, levol, lnonl
    character(len=19) :: lischa, lisch2, solveu
    character(len=19) :: sddisc, sdcrit
    character(len=24) :: modele, mate, carele
    character(len=24) :: result, numedd, time
    character(len=24) :: sdieto
    character(len=8) :: mailla
    real(kind=8) :: para(*)
!
! ---------------------------------------------------------------------
!     THERMIQUE : INITIALISATIONS.
!     *           ****
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       JEVEUX: JEDETR,COPISD,UTCRRE.
!       PARAMETRE DE CMD: GETRES,NTINST.
!       MSG: INFNIV.
!       MANIP SD: VTCREB.
!       LECTURE DONNEES: NTDOET,NTDOED.
!       MATRICE: NUMERO.
!
!     FONCTIONS INTRINSEQUES:
!       AUCUNE.
!   -------------------------------------------------------------------
!
!
!
    integer :: iret, initpr
    integer :: niv, ifm
    character(len=14) :: nuposs
    character(len=19) :: k19bla
    character(len=24) :: noojb, k24bla, vhydr, hydr0
    real(kind=8) :: instin
    logical(kind=1) :: lreuse
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- INITIALISATIONS
!
    lreuse = .false.
    k24bla = ' '
    k19bla = ' '
    vhydr = k24bla
    lostat = .false.
    lnonl = .false.
    levol = .false.
    time = result(1:8)//'.CHTPS'
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=mailla)
!
! --- CONCEPT REENTRANT ?
!
    call gcucon(result, 'EVOL_THER', iret)
    if (iret .gt. 0) then
        lreuse = .true.
    endif
!
! --- NUMEROTATION ET CREATION DU PROFIL DE LA MATRICE
!
    numedd = '12345678.NUMED'
    noojb = '12345678.00000.NUME.PRNO'
    call gnomsd(' ', noojb, 10, 14)
    numedd = noojb(1:14)
    call rsnume(result, 'TEMP', nuposs)
    call numero(nuposs, modele, lischa, solveu, 'VG',&
                numedd)
!
! --- CREATION DES CHAMPS
!
    call ntcrch(modele, numedd, hydr0, vhydr)
!
! --- CREATION DE LA SD IN ET OUT
!
    call ntetcr(numedd, k24bla, sdieto, lnonl, k24bla,&
                k24bla)
!
! --- LECTURE ETAT INITIAL
!
    call nxdoet(modele, numedd, lreuse, lostat, sdieto,&
                initpr, instin)
!
! --- CREATION SD DISCRETISATION, ARCHIVAGE ET OBSERVATION
!
    call tiinit(mailla, modele, result, lostat, lreuse,&
                lnonl, instin, sddisc, sdieto, k19bla,&
                levol)
!
! --- CREATION DE LA SD EVOL_THER
!
    call nxnoli(modele, mate, carele, lostat, lreuse,&
                lnonl, levol, para, sddisc, sdcrit,&
                sdieto, lisch2)
    call copisd('LISTE_CHARGES', 'G', lischa, lisch2)
!
! --- CREATION DE LA SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
!
    call ntcrcv(sdcrit)
!
    call jedema()
end subroutine

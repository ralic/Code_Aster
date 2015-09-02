subroutine rigflu(modele, time, nomcmp, tps, nbchar,&
                  char, mate, solvez, ma, nu)
    implicit none
#include "jeveux.h"
#include "asterfort/asmatr.h"
#include "asterfort/getvid.h"
#include "asterfort/mecact.h"
#include "asterfort/merith.h"
#include "asterfort/numero.h"
#include "asterfort/preres.h"
#include "asterfort/wkvect.h"
    integer :: nbchar
    character(len=*) :: mate, solvez
!
!---------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!---------------------------------------------------------------------
!
!
! BUT : CETTE ROUTINE CALCULE LA MATRICE ASSEMBLEE DE RIGIDITE
!       FLUIDE S'APPUYANT SUR UN MODELE THERMIQUE
!     IN : MODELE : NOM DU MODELE FLUIDE UTILISE
!        : TIME   : INSTANT DU CALCUL
!        : NBCHAR : NOMBRE DE CHARGE
!        : CHAR   : NOM DE LA CHARGE
!        : MATE   : CHAMP DE MATERIAU
!        : SOLVEZ : METHODE DE RESOLUTION 'MULT_FRONT','LDLT' OU 'GCPC'
!     OUT: MA     : MATRICE ASSEMBLEE DE RIGIDITE FLUIDE
!        : NU     : NUMEROTATION ASSOCIEE
!----------------------------------------------------------------------
    integer :: ibid, ialich, jinf, ierr, nchar, ialifc, nh
    real(kind=8) :: tps(6)
    character(len=14) :: nu
    character(len=8) :: modele, nomcmp(6), char, ma, mel
    character(len=24) :: time, fomult
    character(len=19) :: solveu, list_load, maprec
    data maprec   /'&&OP0152.MAPREC'/
    data list_load   /'&&OP0152.INFCHA'/
    data fomult   /'&&OP0152.LIFCTS'/
!   ------------------------------------------------------------------
!
    ma = '&MATAS'
    nu = '&&RIGFLU.NUM'
    mel = '&MATEL'
    solveu = solvez
!
!-----  CALCUL DE LA MATRICE ELEMENTAIRE DE RAIDEUR DU FLUIDE
!
    call mecact('V', time, 'MODELE', modele//'.MODELE', 'INST_R',&
                ncmp=6, lnomcmp=nomcmp, vr=tps)
!
    call merith(modele, nbchar, char, mate, ' ',&
                time, mel, nh, 'V')
!
    call getvid(' ', 'CHARGE', scal=char, nbret=nchar)
    call wkvect(list_load//'.LCHA', 'V V K24', nchar, ialich)
    call wkvect(list_load//'.INFC', 'V V IS', 4*nchar+5, jinf)
    zi(jinf) = nchar
    zk24(ialich) = char
    call wkvect(fomult, 'V V K24', nchar, ialifc)
!
!----------------  NUMEROTATION
!
    call numero(nu, solveu, 'VV',&
                modelz = modele , list_loadz = list_load)

!
!---------------- ASSEMBLAGE
!
    call asmatr(1, mel, ' ', nu, &
                list_load, 'ZERO', 'V', 1, ma)
!
!------- FACTORISATION LDLT DE LA MATRICE DE RAIDEUR
!
    call preres(solveu, 'V', ierr, maprec, ma,&
                ibid, -9999)
!
!
!-----------------------------------------------------
end subroutine

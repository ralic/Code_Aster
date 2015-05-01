subroutine vpini0(compex, modes, typcon, solveu, eigsol, matpsc, matopa, veclag,&
                  vecblo, vecrig, vecrer, vecrei, vecrek, vecvp)

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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! -------------------------------------------------------------------------------------------------
! INITIALISATIONS LIEES A L'OPERATEUR MODE_ITER_SIMULT.
! -------------------------------------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
    implicit none

#include "asterc/getres.h"
#include "asterfort/onerrf.h"

!
! --- INPUT
! None
!
! --- OUTPUT
!
    character(len=8)  , intent(out) :: modes
    character(len=16) , intent(out) :: compex, typcon
    character(len=19) , intent(out) :: matpsc, matopa, solveu, eigsol
    character(len=24) , intent(out) :: veclag, vecblo, vecrig, vecrer, vecrei, vecrek, vecvp
!
! --- INPUT/OUTPUT
! None
!
! --- VARIABLES LOCALES
!
    integer           :: lenout
    character(len=16) :: nomcmd
!
! -----------------------
! --- CORPS DE LA ROUTINE
! -----------------------
!

! --- INITS. OBJETS JEVEUX PROPRES A CET OPERATEUR
    solveu='&&OP0045.SOLVEUR'
    eigsol='&&OP0045.EIGSOL'
!
    matpsc = '&&OP0045.DYN_FAC_R '
    matopa = '&&OP0045.DYN_FAC_C '
!
    veclag = '&&OP0045.POSITION.DDL'
    vecblo = '&&OP0045.DDL.BLOQ.CINE'
    vecrig = '&&OP0045.MODE.RIGID'
!
    vecrer = '&&OP0045.RESU_'
    vecrei = '&&OP0045.RESU_I'
    vecrek = '&&OP0045.RESU_K'
    vecvp  = '&&OP0045.VECTEUR_PROPRE'

! -- ON STOCKE LE COMPORTEMENT EN CAS D'ERREUR : COMPEX
    compex=''
    call onerrf(' ', compex, lenout)

! -- RECUPERATION DU RESULTAT
    modes=''
    typcon=''
    nomcmd=''
    call getres(modes, typcon, nomcmd)
!
!     FIN DE VPINI0
!
end subroutine

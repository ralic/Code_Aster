subroutine dfllpe(mcfact, iechec, even, penmax, nocham,&
                  nocmp, cricmp, valere)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
    character(len=16) :: mcfact
    integer :: iechec
    character(len=16) :: even
    real(kind=8) :: penmax
    real(kind=8) :: valere
    character(len=16) :: nocham, nocmp, cricmp
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_LIST_INST
!
! LECTURE DES PARAMETRES DE L'EVENEMENT
!
! ----------------------------------------------------------------------
!
! IN  MCFACT : MOT-CLEF FACTEUR POUR LIRE L'ECHEC
! IN  IECHEC : NUMERO OCCURRENCE ECHEC
! IN  EVEN   : NOM DE L'EVENEMENT
! OUT PENMAX : VALEUR DE PENE_MAXI
! OUT CRICMP : VALEUR DE CRIT_COMP
! OUT VALERE : VALEUR DE VALE_REF
! OUT NOCHAM : VALEUR DE NOM_CHAM
! OUT NOCMP  : VALEUR DE NOM_CMP
!
! ----------------------------------------------------------------------
!
    integer :: ibid
!
! ----------------------------------------------------------------------
!
    penmax = 0.d0
    valere = 0.d0
    nocham = ' '
    nocmp = ' '
    cricmp = ' '
!
! --- EVENEMENT
!
    if (even .eq. 'DELTA_GRANDEUR') then
        call getvr8(mcfact, 'VALE_REF', iocc=iechec, scal=valere, nbret=ibid)
        call getvtx(mcfact, 'NOM_CHAM', iocc=iechec, scal=nocham, nbret=ibid)
        call getvtx(mcfact, 'NOM_CMP', iocc=iechec, scal=nocmp, nbret=ibid)
        cricmp = 'GT'
    else if (even.eq.'INTERPENETRATION') then
        call getvr8(mcfact, 'PENE_MAXI', iocc=iechec, scal=penmax, nbret=ibid)
    endif
!
end subroutine

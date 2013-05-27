subroutine chpnua(nx, chpt, lno, nuage)
    implicit none
    include 'asterfort/cnonua.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/u2mess.h'
    integer :: nx
    character(len=*) :: chpt, lno, nuage
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     PASSAGE D'UNE SD CHAM_GD A UNE SD NUAGE
!
! IN  NX     : DIMENSION D'ESPACE DU NUAGE (1,2 OU 3)
! IN  CHPT   : NOM DE LA SD CHAM_GD
! IN  LNO    : LISTE DES NOEUDS A PRENDRE EN COMPTE
! OUT NUAGE  : SD NUAGE PRODUITE
!     ------------------------------------------------------------------
    integer :: ibid, ie
    character(len=4) :: type
!     ------------------------------------------------------------------
!
    call dismoi('F', 'TYPE_CHAMP', chpt, 'CHAMP', ibid,&
                type, ie)
!
    if (type .eq. 'NOEU') then
        call cnonua(nx, chpt, lno, nuage)
    else if (type(1:2) .eq. 'EL') then
        call u2mess('F', 'UTILITAI_34')
    else
        call u2mess('F', 'CALCULEL_17')
    endif
!
end subroutine

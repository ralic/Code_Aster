subroutine rc32rs(pmpb, sn, snet, fatigu, lrocht,&
                  mater, symax)
    implicit   none
    include 'asterc/getres.h'
    include 'asterfort/rc32r0.h'
    include 'asterfort/rc32r1.h'
    include 'asterfort/rc32r8.h'
    include 'asterfort/tbcrsd.h'
    logical :: pmpb, sn, snet, fatigu, lrocht
    real(kind=8) :: symax
    character(len=8) :: mater
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     STOCKAGE DES RESULTATS DANS LA TABLE DE SORTIE
!
!     ------------------------------------------------------------------
    character(len=8) :: nomres
    character(len=16) :: concep, nomcmd
! DEB ------------------------------------------------------------------
!
    call getres(nomres, concep, nomcmd)
!
    call tbcrsd(nomres, 'G')
!
!     -----------------------------------------------------------------
!
    if (fatigu) then
!
        call rc32r1(nomres)
!
!     -----------------------------------------------------------------
    else
!
        call rc32r0(nomres, pmpb, sn, snet)
!
    endif
!
!     -----------------------------------------------------------------
    if (lrocht) call rc32r8(nomres, mater, symax)
!
end subroutine

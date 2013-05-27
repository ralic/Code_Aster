subroutine jedbg2(dbgav, dbgap)
    implicit none
    include 'asterfort/assert.h'
    integer :: dbgav, dbgap
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! BUT :  CONSULTER ET EVENTUELLEMENT METTRE A JOUR
!        LA VALEUR IDEBUG DE JEVEUX
!
!      IDEBUG=1 => "DEBUG_JEVEUX"
!      IDEBUG=0 => PAS DE "DEBUG_JEVEUX"
!
!
! OUT  I  DBGAV : VALEUR DE IDEBUG AVANT L'APPEL A JEDBG2 (0 OU 1)
! IN   I  DBGAP : VALEUR DE IDEBUG APRES L'APPEL A JEDBG2 (-1,0 OU 1)
!                 SI DBGAP=-1 : ON NE MODIFIE PAS IDEBUG
!-----------------------------------------------------------------------
    integer :: lundef, idebug
    common /undfje/  lundef,idebug
!
    dbgav=idebug
!
    if (dbgap .eq. -1) then
    else if (dbgap.eq.0) then
        idebug=0
    else if (dbgap.eq.1) then
        idebug=1
    else
        call assert(.false.)
    endif
end subroutine

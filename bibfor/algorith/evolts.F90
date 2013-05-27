subroutine evolts(tseuil, tsretu, viet, vits, iterat)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
    integer :: iterat
    real(kind=8) :: vits, viet, tseuil, tsretu
! ----------------------------------------------------------------------
!     EVOLUTION DE LA VARIABLE GUIDANT LES PROPORTIONS TANGENTE ET
!     SECANTE DE LA MATRICE (LOCALE) UTILISEE PAR NEWTON (GLOBAL)
!
! IN  ITERAT   : ITERATION GLOBALE DE NEWTON
! IN  VIET     : VARIABLE INTERNE INDIQUANT L ETAT (ELAS=0, ENDO>0)
! IN/OUT  VITS : VARIABLE INTERNE A TRAITER
!
    if ((vits .ge. 1.0d0) .and. (viet .eq. 0.0d0)) then
        vits = -vits
    else if ((vits .lt. 0.0d0) .and. (viet.gt.0.0d0)) then
        vits = -vits + 1.0d0
    else if ((vits .gt. tseuil) .and. (viet .gt. 0.0d0)) then
        vits = vits - tsretu
        if (vits .lt. tseuil) vits = tseuil
    else if ((vits .eq. 0.0d0) .and. (viet .gt. 0.0d0)) then
        vits = 1.0d0
    endif
end subroutine

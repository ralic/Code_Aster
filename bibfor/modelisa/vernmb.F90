subroutine vernmb(icl, cnl, ier, irteti)
    implicit none
!       ----------------------------------------------------------------
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
!       ----------------------------------------------------------------
!       VERIFIE QUE L ITEM LUT EST UN NOMBRE ( NOMBRE ATTENDU )
!       ----------------------------------------------------------------
!       NOMBRE          =       REEL OU ENTIER (LXSCAN)
!       IN      ICL     =       CLASSE ITEM
!               CNL     =       NUMERO LIGNE
!       OUT     IER     =       0 > VRAI ( RETURN )
!                       =       1 > FAUX ( RETURN 1 )
!       ----------------------------------------------------------------
    include 'asterfort/u2mesk.h'
    integer :: icl, ier
    character(len=14) :: cnl
    character(len=16) :: cmd
    common          /opmail/                cmd
!
!-----------------------------------------------------------------------
    integer :: irteti
!-----------------------------------------------------------------------
    irteti = 0
    if (icl .ne. 1 .and. icl .ne. 2) then
        call u2mesk('E', 'MODELISA7_85', 1, cnl)
        ier=1
        irteti = 1
        goto 9999
    else
        irteti = 0
        goto 9999
    endif
!
9999  continue
end subroutine

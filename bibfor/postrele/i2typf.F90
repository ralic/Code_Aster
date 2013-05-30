subroutine i2typf(epsi, x1, y1, x2, y2,&
                  x3, y3, tm, droi)
    implicit  none
!
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
!
!******************************************************************
!
!          ENTREE : TM    <--- LE NOM DU TYPE DE LA MAILLE
!
!                   XI,YI <--- COORDONNES DES NOEUDS DU COTE
!
!          SORTIE : DROI, UN LOGIQUE VALANT
!
!                         VRAI SI LE COTE EST DROIT
!
!                         FAUX SINON
!
!******************************************************************
!
    include 'asterfort/rvdet2.h'
    character(len=*) :: tm
    logical :: droi
    real(kind=8) :: epsi, d, l2, x1, x2, x3, y1, y2, y3, crit
!
    droi = .false.
!
    if (( tm .eq. 'TRIA3' ) .or. ( tm .eq. 'QUAD4' )) then
!
        droi = .true.
!
    else
!
        call rvdet2(x3-x1, y3-y1, x2-x1, y2-y1, d)
        l2 = (x3-x1)*(x3-x1) + (y3-y1)*(y3-y1)
        crit = abs( 2.0d0 * d ) / l2
!
        if (crit .lt. epsi) droi = .true.
!
    endif
!
end subroutine

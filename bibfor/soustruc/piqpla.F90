subroutine piqpla(x1, y1, z1, xp, yp,&
                  zp, zone7, zone8, l4, l6,&
                  epsi)
    implicit   none
    include 'asterc/r8pi.h'
    real(kind=8) :: x1, y1, z1, xp, yp, zp, l4, l6, epsi
    logical :: zone7, zone8
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
!     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
!     AUTEUR Y. WADIER
!
!     REALISE LA TRANSFO : GEOMETRIE DE REF. --> PIQUAGE / PLAQUE CARREE
!     POUR LES ZONES : ZONE7,ZONE8
! IN  : X1, Y1, Z1  : COORD. DU POINT DANS LA GEOMETRIE DE REFERENCE
! OUT : XP, YP, ZP  : COORD. DU POINT DANS LA GEOMETRIE PIQUAGE/PLAQUE
!
!-----------------------------------------------------------------------
!
    real(kind=8) :: alp, lan, rmp, pis2, pis4
    real(kind=8) :: x2, y2
!     ------------------------------------------------------------------
!
    xp = x1
    yp = y1
    zp = z1
!
    if (zone7 .or. zone8) then
!
        alp = atan2( y1 , x1 )
        pis2 = r8pi() / 2.0d0
        pis4 = r8pi() / 4.0d0
!
        if ((0.0d0.le.alp) .and. (alp.lt.pis4)) then
            x2 = l6
            y2 = l6 * tan(alp)
        endif
!
        if ((pis4.le.alp) .and. (alp.lt.(pis2-epsi))) then
            x2 = l6 * cos(alp) / sin(alp)
            y2 = l6
        endif
!
        if (abs(alp-pis2) .le. epsi) then
            x2 = 0.0d0
            y2 = l6
        endif
!
        rmp = sqrt( x1**2 + y1**2 )
        lan = ( rmp - l4 ) / ( l6 - l4 )
!
        xp = ( 1.0d0-lan ) * x1 + ( lan*lan ) * x2
        yp = ( 1.0d0-lan )* y1 + ( lan*lan ) * y2
        zp = z1
!
    endif
!
end subroutine

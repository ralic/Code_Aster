subroutine asctub(mailla)
    implicit   none
    include 'jeveux.h'
    include 'asterc/getvr8.h'
    include 'asterc/r8pi.h'
    include 'asterfort/asctri.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=8) :: mailla
!-----------------------------------------------------------------------
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
!     OPERATEUR: "MODI_MAILLAGE" , MOTCLE FACTEUR "PLAQ_TUBE"
!
!     REALISE LA TRANSFORMATION PLAQUE-TUBE
!
!-----------------------------------------------------------------------
!-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
!
!     RM        = RAYON MOYEN DU TUBE
!     AZIMUT    = ANGLE DE ROTATION DU TUBE
!     L_TUBE_P1 = LONGUEUR DE L'EMBOUT DROIT INFERIEUR
!
!-----------------------------------------------------------------------
!
    integer :: nbno, icoor, idime, n1, ino, ndim
    real(kind=8) :: rm, azim, theta, rho, azimr, pi, xp, yp, ltchar, dext, ep
    character(len=24) :: coord, dime
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call getvr8('PLAQ_TUBE', 'DEXT', 1, iarg, 1,&
                dext, n1)
    call getvr8('PLAQ_TUBE', 'EPAIS', 1, iarg, 1,&
                ep, n1)
    call getvr8('PLAQ_TUBE', 'AZIMUT', 1, iarg, 1,&
                azim, n1)
    call getvr8('PLAQ_TUBE', 'L_TUBE_P1', 1, iarg, 1,&
                ltchar, n1)
!
    rm = (dext-ep)/2.d0
!
    coord = mailla//'.COORDO    .VALE'
    dime = mailla//'.DIME           '
!
    call jeveuo(coord, 'E', icoor)
    call jeveuo(dime, 'L', idime)
    nbno = zi(idime)
    ndim = zi(idime+5)
!
    pi = r8pi()
    azimr = azim*pi/180.d0
!
!     TRI DE GROUPES DE NOEUDS
!
    call asctri(mailla, rm)
!
    do 100 ino = 1, nbno
        xp = zr(icoor+ndim*(ino-1))
        yp = zr(icoor+ndim*(ino-1)+1)
        rho = xp
        if ((yp+azimr*rm) .gt. (0.d0)) then
            theta = yp/rm + azimr
        else
            theta = 2.d0*pi + yp/rm + azimr
        endif
        zr(icoor+ndim*(ino-1)) = rho*sin(theta)
        zr(icoor+ndim*(ino-1)+1) = - rho*cos(theta)
100  end do
!
    call jedema()
!
end subroutine

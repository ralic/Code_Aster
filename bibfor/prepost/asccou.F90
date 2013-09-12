subroutine asccou(mailla)
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
!     OPERATEUR: "MODI_MAILLAGE" , MOTCLE FACTEUR "TUBE-COUDE"
!
!     REALISE LA TRANSFORMATION TUBE-COUDE
!
!-----------------------------------------------------------------------
!-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
!
!     RC        = RAYON DE CINTRAGE DU COUDE
!     ALPHA     = ANGLE DU COUDE
!
!-----------------------------------------------------------------------
!
    integer :: nbno, icoor, idime, n1, ino, ndim
    real(kind=8) :: rc, alpha, xt, yt, zt, pi, beta, alphar, xcou, ycou, zcou
    character(len=24) :: coord, dime
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call getvr8('TUBE_COUDE', 'R_CINTR', iocc=1, scal=rc, nbret=n1)
    call getvr8('TUBE_COUDE', 'ANGLE', iocc=1, scal=alpha, nbret=n1)
!
    coord = mailla//'.COORDO    .VALE'
    dime = mailla//'.DIME           '
!
    call jeveuo(coord, 'E', icoor)
    call jeveuo(dime, 'L', idime)
    nbno = zi(idime)
    ndim = zi(idime+5)
    pi = r8pi()
    alphar = alpha*pi/180.d0
!
    do 100 ino = 1, nbno
!
        xt = zr(icoor+ndim*(ino-1))
        yt = zr(icoor+ndim*(ino-1)+1)
        zt = zr(icoor+ndim*(ino-1)+2)
        if ((zt.ge.0.d0) .and. (zt.le.alphar*rc)) then
            beta = zt/rc
            xcou = xt
            ycou = rc * (1.d0-cos(beta)) + yt * cos(beta)
            zcou = (rc - yt ) * sin (beta)
        else if (zt.gt.alphar*rc) then
            xcou = xt
            ycou = rc * (1.d0-cos(alphar)) + sin(alphar) * ( zt - alphar*rc ) + yt * cos(alphar)
            zcou = rc * sin (alphar) + cos(alphar) * ( zt - alphar*rc ) - yt * sin(alphar)
        else
            xcou = xt
            ycou = yt
            zcou = zt
        endif
        zr(icoor+ndim*(ino-1)) = xcou
        zr(icoor+ndim*(ino-1)+1) = ycou
        zr(icoor+ndim*(ino-1)+2) = zcou
!
100  end do
!
    call jedema()
!
end subroutine

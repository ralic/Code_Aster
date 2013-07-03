subroutine oreino(noma, lnoeud, nbno, nori, next,&
                  coor, crit, prec, iera, ier)
    implicit   none
#include "jeveux.h"
!
#include "asterc/r8prem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: lnoeud(*), nbno, nori, next, ier, iera
    real(kind=8) :: coor(*), prec
    character(len=8) :: noma
    character(len=*) :: crit
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
!-----------------------------------------------------------------------
!     BUT : CLASSER DES NUMEROS DE NOEUDS SELON LEUR PROJECTION
!           SUR UN SEGMENT
!-----------------------------------------------------------------------
!     I/O : LNOEUD: NUMEROS DES NOEUDS
!     IN  : NBNO  : NOMBRE DE NOEUDS
!     IN  : NORI  : NUMERO DU NOEUD ORIGINE
!     IN  : NEXT  : NUMERO DU NOEUD EXTREMITE
!     IN  : COOR  : COORDONNEES DES NOEUDS
!     IN  : CRIT  : CRITERE
!     IN  : PREC  : PRECISION
!     IN  : IER   : CODE RETOUR,  = 0  OK
!-----------------------------------------------------------------------
    integer :: i, j, k, n, idis, inoe, inod
    real(kind=8) :: xa, ya, za, xb, yb, zb, xab, yab, zab, ab2, xm, ym, zm, xam
    real(kind=8) :: yam, zam, c, c2, xv, yv, zv, v2, r8b, ecart, valr
    character(len=8) :: nomn
    character(len=24) :: nomnoe
    character(len=24) :: valk(2)
! DEB ------------------------------------------------------------------
    call jemarq()
    nomnoe = noma//'.NOMNOE         '
!
    ier = 0
!
    xa = coor(3*(nori-1)+1)
    ya = coor(3*(nori-1)+2)
    za = coor(3*(nori-1)+3)
!
    xb = coor(3*(next-1)+1)
    yb = coor(3*(next-1)+2)
    zb = coor(3*(next-1)+3)
!
    xab = xb-xa
    yab = yb-ya
    zab = zb-za
    ab2 = xab**2 + yab**2 + zab**2
    if (ab2 .eq. 0.0d0) then
        call u2mess('A', 'SOUSTRUC_20')
        ier = ier + 1
        goto 9999
    endif
!
    call wkvect('&&OREINO.BARY', 'V V R', nbno, idis)
!
!     --- CALCUL DE LA CORDONNEE BARYCENTRIQUE ---
!
    do 100 inoe = 1, nbno
        inod = lnoeud(inoe)
        xm = coor(3*(inod-1)+1)
        ym = coor(3*(inod-1)+2)
        zm = coor(3*(inod-1)+3)
        xam = xm-xa
        yam = ym-ya
        zam = zm-za
        c = (xam*xab+yam*yab+zam*zab) / ab2
        c2 = xam**2 + yam**2 + zam**2
        xv = xam - c*xab
        yv = yam - c*yab
        zv = zam - c*zab
        v2 = xv**2 + yv**2 + zv**2
!        --- VERIFICATION QUE LA DISTANCE A L'AXE
!                         NE DEPASSE PAS LA TOLERANCE ---
        if (crit(1:4) .eq. 'ABSO') then
            r8b = v2
        else if (crit(1:4) .eq. 'RELA') then
            r8b = v2 / ab2
        else
            call u2mess('A', 'SOUSTRUC_21')
            ier = ier + 1
            goto 9999
        endif
        r8b = sqrt( r8b )
        if (r8b .gt. prec) then
            v2 = sqrt( v2 )
            call jenuno(jexnum(nomnoe, inod), nomn)
            if (iera .eq. 0) then
                call u2mesg('A', 'SOUSTRUC_22', 1, nomn, 0,&
                            0, 1, v2)
                iera = iera+1
            else
                call u2mesg('I', 'SOUSTRUC_22', 1, nomn, 0,&
                            0, 1, v2)
            endif
            ier = ier + 1
        endif
!        --- VERIFICATION QUE LA PROJECTION EST BIEN
!                         SITUEE ENTRE LES POINTS A ET B ---
        ecart = (c2-ab2)/ab2
        if (c .lt. 0.0d0 .or. c2 .gt. ab2) then
            if (ecart .gt. r8prem()) then
                call jenuno(jexnum(nomnoe, inod), nomn)
                valk (1) = nomn
                valk (2) = nomn
                valr = c
                call u2mesg('A', 'SOUSTRUC_86', 2, valk, 0,&
                            0, 1, valr)
                ier = ier + 1
            endif
        endif
        zr(idis-1+inoe) = c
100  end do
!
!     --- TRI PAR BUBBLE SORT ---
!
    do 300 k = 1, nbno-1
        do 200 i = nbno-1, k, -1
            j = i+1
            if (zr(idis-1+i) .gt. zr(idis-1+j)) then
                c=zr(idis-1+j)
                zr(idis-1+j)=zr(idis-1+i)
                zr(idis-1+i)=c
                n=lnoeud(j)
                lnoeud(j)=lnoeud(i)
                lnoeud(i)=n
            endif
200      continue
300  end do
!
!     --- VERIFICATION QUE DEUX NOEUDS CONSECUTIFS
!                          N'ONT PAS LA MEME PROJECTION ---
    do 400 inoe = 1, nbno-1
        if (zr(idis-1+inoe) .eq. zr(idis-1+inoe+1)) then
            call u2mess('A', 'SOUSTRUC_23')
            ier = ier + 1
        endif
400  end do
!
9999  continue
!
    call jedetr('&&OREINO.BARY')
!
    call jedema()
end subroutine

subroutine nuadrf(nuag1, nuag2, ic1, ic2, dref)
    implicit none
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
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=19) :: nuag1, nuag2
    integer :: ic1, ic2
    real(kind=8) :: dref(*)
!
!  BUT : CALCULER POUR TOUS LES POINTS DE NUAG2 UNE DISTANCE
!        DE REFERENCE POUR QU L'INTERPOLATION NE "CAPTE"
!        QU'UN NOMBRE LIMITE DE POINTS NE NUAG1 :
!        ON CHERCHE UNE INTERPOLATION LA PLUS LOCALE POSSIBLE
!
!        EN UN POINT DONNE IP2 DE NUAG2, ON ASSOCIE LA DISTANCE DREF
!        DREF EST TELLE QUE :
!                LA BOULE (IP2,SQRT(DREF)) CONTIENNE :
!                 . 2 POINTS EN 1D (NON CONFONDUS)
!                 . 3 POINTS EN 2D (NON ALIGNES)
!                 . 4 POINTS EN 3D (NON COPLANAIRES)
!
! IN/JXIN  NUAG1   : NUAGE A PROJETER
! IN/JXIN  NUAG2   : NUAGE A EVALUER
! IN       IC1     : NUMERO DE LA CMP DANS NUAG1
! IN       IC2     : NUMERO DE LA CMP DANS NUAG2
! OU       DREF    : VECTEUR QUI CONTIENDRA LES DISTANCE**2 CHERCHEES
!                    DIMENSION : NP2 = NOMBRE DE POINTS DE NUAG2
! VARIABLES LOCALES :
    integer :: inuai1, inuai2, inual1, inual2, inuax1, inuax2, iadm0
    integer :: np1, np2, nx1, nx2, nc1, nc2, ip1, ip2, im1, im2, im3, im4
    real(kind=8) :: x2, y2, z2, x1, y1, z1, xm1, ym1, zm1
    real(kind=8) :: d, dm0, dm, l2, s, s2, v, l
    real(kind=8) :: m1m2(3), m1m3(3), m1p1(3), n2(3), n(3), epsabs
!
! DEB-------------------------------------------------------------------
    call jemarq()
!
    epsabs=sqrt(1.d0/r8gaem())
!
    call jeveuo(nuag1//'.NUAI', 'L', inuai1)
    call jeveuo(nuag2//'.NUAI', 'L', inuai2)
    call jeveuo(nuag1//'.NUAX', 'L', inuax1)
    call jeveuo(nuag2//'.NUAX', 'L', inuax2)
    call jeveuo(nuag1//'.NUAL', 'L', inual1)
    call jeveuo(nuag2//'.NUAL', 'L', inual2)
!
    np1 = zi(inuai1-1+1)
    np2 = zi(inuai2-1+1)
    nx1 = zi(inuai1-1+2)
    nx2 = zi(inuai2-1+2)
    nc1 = zi(inuai1-1+3)
    nc2 = zi(inuai2-1+3)
!
!
!     RECHERCHE DE LA PLUS GRANDE DISTANCE**2 ENTRE CHAQUE IP2
!     ET L'ENSEMBLE DES IP1 :
!     ------------------------------------------------------
    call wkvect('&&NUADRF.DM0', 'V V R', np2, iadm0)
    do 1 ,ip2 = 1,np2
    if (.not.zl(inual2-1+ (ip2-1)*nc2+ic2)) goto 1
!
!       -- DM0 EST LA PLUS GRANDE DISTANCE**2 ENTRE IP2 ET
!          L'ENSEMBLE DES IP1
    dm0 = 0.d0
!
    if (nx1 .eq. 1) then
        x2 = zr(inuax2-1+ (ip2-1)*nx2+1)
        do 2,ip1 = 1,np1
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 2
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        dm0 = max(dm0,(x2-x1)**2)
 2      continue
    else if (nx1.eq.2) then
        x2 = zr(inuax2-1+ (ip2-1)*nx2+1)
        y2 = zr(inuax2-1+ (ip2-1)*nx2+2)
        do 3,ip1 = 1,np1
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 3
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        y1 = zr(inuax1-1+ (ip1-1)*nx1+2)
        dm0 = max(dm0,(x2-x1)**2+(y2-y1)**2)
 3      continue
    else if (nx1.eq.3) then
        x2 = zr(inuax2-1+ (ip2-1)*nx2+1)
        y2 = zr(inuax2-1+ (ip2-1)*nx2+2)
        z2 = zr(inuax2-1+ (ip2-1)*nx2+3)
        do 4,ip1 = 1,np1
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 4
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        y1 = zr(inuax1-1+ (ip1-1)*nx1+2)
        z1 = zr(inuax1-1+ (ip1-1)*nx1+3)
        dm0 = max(dm0,(x2-x1)**2+(y2-y1)**2+(z2-z1)**2)
 4      continue
    endif
!
    if (dm0 .eq. 0.d0) goto 9994
    zr(iadm0-1+ip2)=dm0
    1 end do
!
!
    if (nx1 .eq. 1) then
!     ------------
        do 10,ip2 = 1,np2
        if (.not.zl(inual2-1+ (ip2-1)*nc2+ic2)) goto 10
        x2 = zr(inuax2-1+ (ip2-1)*nx2+1)
!
!         -- IM1 EST L'INDICE DU POINT LE + PROCHE DE IP2
        im1 = 0
        dm = zr(iadm0-1+ip2)
        do 12,ip1 = 1,np1
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 12
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        d = (x1-x2)**2
        if (d .le. dm) then
            dm = d
            im1 = ip1
        endif
12      continue
        if (im1 .eq. 0) goto 9995
        xm1=zr(inuax1-1+ (im1-1)*nx1+1)
!
!         -- IM2 EST L'INDICE DU POINT LE + PROCHE DE IP2
!            ET DIFFERENT DE IM1
        im2 = 0
        dm = zr(iadm0-1+ip2)
        do 13,ip1 = 1,np1
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 13
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        if ((x1-xm1)**2 .lt. epsabs) goto 13
        d = (x1-x2)**2
        if (d .le. dm) then
            dm = d
            im2 = ip1
        endif
13      continue
        if (im2 .eq. 0) goto 9996
        dref(ip2) = dm
!
10      continue
        goto 9999
!
    else if (nx1.eq.2) then
!     ------------------
        do 20,ip2 = 1,np2
        if (.not.zl(inual2-1+ (ip2-1)*nc2+ic2)) goto 20
        x2 = zr(inuax2-1+ (ip2-1)*nx2+1)
        y2 = zr(inuax2-1+ (ip2-1)*nx2+2)
!
!
!         -- IM1 EST L'INDICE DU POINT LE + PROCHE DE IP2
        im1 = 0
        dm = zr(iadm0-1+ip2)
        do 22,ip1 = 1,np1
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 22
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        y1 = zr(inuax1-1+ (ip1-1)*nx1+2)
        d = (x2-x1)**2+(y2-y1)**2
        if (d .le. dm) then
            dm = d
            im1 = ip1
        endif
22      continue
        if (im1 .eq. 0) goto 9995
        xm1=zr(inuax1-1+ (im1-1)*nx1+1)
        ym1=zr(inuax1-1+ (im1-1)*nx1+2)
!
!         -- IM2 EST L'INDICE DU POINT LE + PROCHE DE IP2
!            ET DIFFERENT DE IM1
        im2 = 0
        dm = zr(iadm0-1+ip2)
        do 23,ip1 = 1,np1
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 23
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        y1 = zr(inuax1-1+ (ip1-1)*nx1+2)
        if ((x1-xm1)**2+(y1-ym1)**2 .le. epsabs) goto 23
        d = (x2-x1)**2+(y2-y1)**2
        if (d .le. dm) then
            dm = d
            im2 = ip1
        endif
23      continue
        if (im2 .eq. 0) goto 9996
!
!         VECTEUR M1M2 :
        m1m2(1)=zr(inuax1-1+(im1-1)*nx1+1)-zr(inuax1-1+(im2-1)*&
            nx1+1)
        m1m2(2)=zr(inuax1-1+(im1-1)*nx1+2)-zr(inuax1-1+(im2-1)*&
            nx1+2)
        l2=m1m2(1)**2+m1m2(2)**2
!
!         -- IM3 EST L'INDICE DU POINT M3 LE + PROCHE DE P2
!            DIFFERENT DE M1 ET M2 ET TEL QUE M1 M2 M3 FORMENT UN PLAN
        im3 = 0
        dm = zr(iadm0-1+ip2)
        do 24,ip1 = 1,np1
!           IF ((IP1.EQ.IM1).OR.(IP1.EQ.IM2)) GOTO 24
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 24
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        y1 = zr(inuax1-1+ (ip1-1)*nx1+2)
!
!           SI LES POINTS M1 M2 ET P1 NE FORMENT PAS UN PLAN GOTO 24
        m1p1(1)=zr(inuax1-1+(im1-1)*nx1+1)-x1
        m1p1(2)=zr(inuax1-1+(im1-1)*nx1+2)-y1
        s=abs(m1m2(1)*m1p1(2)-m1m2(2)*m1p1(1))
        if (s .le. (1.d-3*l2)) goto 24
        d = (x2-x1)**2+(y2-y1)**2
        if (d .le. dm) then
            dm = d
            im3 = ip1
        endif
24      continue
        if (im3 .eq. 0) goto 9997
        dref(ip2) = dm
20      continue
        goto 9999
!
!
    else if (nx1.eq.3) then
!     ------------------
        do 30,ip2 = 1,np2
        if (.not.zl(inual2-1+ (ip2-1)*nc2+ic2)) goto 30
        x2 = zr(inuax2-1+ (ip2-1)*nx2+1)
        y2 = zr(inuax2-1+ (ip2-1)*nx2+2)
        z2 = zr(inuax2-1+ (ip2-1)*nx2+3)
!
!
!         -- IM1 EST L'INDICE DU POINT LE + PROCHE DE IP2
        im1 = 0
        dm = zr(iadm0-1+ip2)
        do 32,ip1 = 1,np1
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 32
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        y1 = zr(inuax1-1+ (ip1-1)*nx1+2)
        z1 = zr(inuax1-1+ (ip1-1)*nx1+3)
        d = (x2-x1)**2+(y2-y1)**2+(z2-z1)**2
        if (d .le. dm) then
            dm = d
            im1 = ip1
        endif
32      continue
        if (im1 .eq. 0) goto 9995
        xm1=zr(inuax1-1+ (im1-1)*nx1+1)
        ym1=zr(inuax1-1+ (im1-1)*nx1+2)
        zm1=zr(inuax1-1+ (im1-1)*nx1+3)
!
!         -- IM2 EST L'INDICE DU POINT LE + PROCHE DE IP2
!            ET DIFFERENT DE IM1
        im2 = 0
        dm = zr(iadm0-1+ip2)
        do 33,ip1 = 1,np1
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 33
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        y1 = zr(inuax1-1+ (ip1-1)*nx1+2)
        z1 = zr(inuax1-1+ (ip1-1)*nx1+3)
        if ((x1-xm1)**2+(y1-ym1)**2+(z1-zm1)**2 .le. epsabs) goto 33
        d = (x2-x1)**2+(y2-y1)**2+(z2-z1)**2
        if (d .le. dm) then
            dm = d
            im2 = ip1
        endif
33      continue
        if (im2 .eq. 0) goto 9996
!
!         -- VECTEUR M1M2 :
        m1m2(1)=zr(inuax1-1+(im2-1)*nx1+1)-zr(inuax1-1+(im1-1)*&
            nx1+1)
        m1m2(2)=zr(inuax1-1+(im2-1)*nx1+2)-zr(inuax1-1+(im1-1)*&
            nx1+2)
        m1m2(3)=zr(inuax1-1+(im2-1)*nx1+3)-zr(inuax1-1+(im1-1)*&
            nx1+3)
        l2=m1m2(1)**2+m1m2(2)**2+m1m2(3)**2
        l=sqrt(l2)
!
!         -- IM3 EST L'INDICE DU POINT M3 LE + PROCHE DE P2
!            DIFFERENT DE M1 ET M2 ET TEL QUE M1 M2 M3 FORMENT UN PLAN
        im3 = 0
        dm = zr(iadm0-1+ip2)
        do 34,ip1 = 1,np1
!           IF ((IP1.EQ.IM1).OR.(IP1.EQ.IM2)) GOTO 34
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 34
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        y1 = zr(inuax1-1+ (ip1-1)*nx1+2)
        z1 = zr(inuax1-1+ (ip1-1)*nx1+3)
!
!           SI LES POINTS M1 M2 ET P1 NE FORMENT PAS UN PLAN GOTO 34
        m1p1(1)=zr(inuax1-1+(im1-1)*nx1+1)-x1
        m1p1(2)=zr(inuax1-1+(im1-1)*nx1+2)-y1
        m1p1(3)=zr(inuax1-1+(im1-1)*nx1+3)-z1
        n2(1)=m1m2(2)*m1p1(3)-m1m2(3)*m1p1(2)
        n2(2)=m1m2(3)*m1p1(1)-m1m2(1)*m1p1(3)
        n2(3)=m1m2(1)*m1p1(2)-m1m2(2)*m1p1(1)
        s2=n2(1)**2+n2(2)**2+n2(3)**2
!
        if (s2 .le. (1.d-3*l2)**2) goto 34
        d = (x2-x1)**2+(y2-y1)**2+(z2-z1)**2
        if (d .le. dm) then
            dm = d
            im3 = ip1
        endif
34      continue
        if (im3 .eq. 0) goto 9997
!
!         -- VECTEUR M1M3 :
        m1m3(1)=zr(inuax1-1+(im3-1)*nx1+1)-zr(inuax1-1+(im1-1)*&
            nx1+1)
        m1m3(2)=zr(inuax1-1+(im3-1)*nx1+2)-zr(inuax1-1+(im1-1)*&
            nx1+2)
        m1m3(3)=zr(inuax1-1+(im3-1)*nx1+3)-zr(inuax1-1+(im1-1)*&
            nx1+3)
!
!         -- N = M1M2 X M1M3 :
        n(1)=m1m2(2)*m1m3(3)-m1m2(3)*m1m3(2)
        n(2)=m1m2(3)*m1m3(1)-m1m2(1)*m1m3(3)
        n(3)=m1m2(1)*m1m3(2)-m1m2(2)*m1m3(1)
        s2=n(1)**2+n(2)**2+n(3)**2
        s=sqrt(s2)
!
!         -- IM4 EST L'INDICE DU POINT LE + PROCHE DE P2
!            DIFFERENT DE M1 M2 M3 ET TEL QUE M1 M2 M3 M4
!            FORMENT UN VOLUME
        im4 = 0
        dm = zr(iadm0-1+ip2)
        do 35,ip1 = 1,np1
!           IF ((IP1.EQ.IM1).OR.(IP1.EQ.IM2).OR.(IP1.EQ.IM3)) GOTO 35
        if (.not.zl(inual1-1+ (ip1-1)*nc1+ic1)) goto 35
        x1 = zr(inuax1-1+ (ip1-1)*nx1+1)
        y1 = zr(inuax1-1+ (ip1-1)*nx1+2)
        z1 = zr(inuax1-1+ (ip1-1)*nx1+3)
!
!           SI LES POINTS M1 M2 M3 ET P1 NE FORMENT PAS
!           UN VOLUME GOTO 35
        m1p1(1)=zr(inuax1-1+(im1-1)*nx1+1)-x1
        m1p1(2)=zr(inuax1-1+(im1-1)*nx1+2)-y1
        m1p1(3)=zr(inuax1-1+(im1-1)*nx1+3)-z1
!
        v=abs(m1p1(1)*n(1)+m1p1(2)*n(2)+m1p1(3)*n(3))
        if (v .le. (1.d-3*s*l)) goto 35
!
        d = (x2-x1)**2+(y2-y1)**2+(z2-z1)**2
        if (d .le. dm) then
            dm = d
            im4 = ip1
        endif
35      continue
        if (im4 .eq. 0) goto 9998
!
        dref(ip2) = dm
30      continue
        goto 9999
!
!
    else
        call utmess('F', 'UTILITAI2_54')
    endif
!
9994  continue
    call utmess('F', 'UTILITAI2_55')
!
9995  continue
    call utmess('F', 'UTILITAI2_56')
!
9996  continue
    call utmess('F', 'UTILITAI2_57')
!
9997  continue
    call utmess('F', 'UTILITAI2_58')
!
!
9998  continue
    call utmess('F', 'UTILITAI2_59')
!
!
9999  continue
    call jedetr('&&NUADRF.DM0')
    call jedema()
end subroutine

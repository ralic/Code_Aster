subroutine pj2dfb(boite, tria3, geom1, geom2)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
    real(kind=8) :: geom1(*), geom2(*)
    integer :: tria3(*)
    character(len=14) :: boite
! ----------------------------------------------------------------------
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
!     BUT :
!       CONSTRUIRE LA STRUCTURE DE DONNEES BOITE_2D QUI PERMET DE SAVOIR
!       QUELS SONT LES TRIA3 QUI SE TROUVE DANS UNE BOITE(P,Q)
!
!  IN/JXOUT   BOITE      K14 : NOM DE LA SD BOITE_2D A CREER
!  IN         GEOM2(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M2
!  IN         GEOM1(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M1
!  IN         TRIA3(*)   I  : OBJET '&&PJXXCO.TRIA3'
! ----------------------------------------------------------------------
!
!
    real(kind=8) :: stotal, sboite, dx, dy, ddx, ddy, rbig, xxmax, xxmin, xmax
    real(kind=8) :: xmin
    real(kind=8) :: yymax, yymin, ymax, ymin
    integer :: p1, q1, p2, q2, p, q, nx, ny
    aster_logical :: dbg=.false.
!
! DEB ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iabtco, iabtdi, iabtlc, iabtnb, iabtvr
    integer :: ib, ifm, ino, iposi, k
    integer :: lont, nno1, nno2, ntr3
    integer, pointer :: lino2(:) => null()
    integer, pointer :: lino1(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    ntr3 = tria3(1)
    rbig = r8maem()
    ASSERT(ntr3.ne.0)
!
    call jeveuo('&&PJXXCO.LINO1', 'L', vi=lino1)
    call jeveuo('&&PJXXCO.LINO2', 'L', vi=lino2)
    call jelira('&&PJXXCO.LINO1', 'LONMAX', nno1)
    call jelira('&&PJXXCO.LINO2', 'LONMAX', nno2)
!
!
!     1. : ON CALCULE XMIN,XMAX,YMIN,YMAX,NX,NY,DX,DY
!     -------------------------------------------------------
    xmin = rbig
    ymin = rbig
    xmax = -rbig
    ymax = -rbig
    do 10 i = 1, nno1
        if (lino1(i) .eq. 0) goto 10
        xmin = min(xmin,geom1(3* (i-1)+1))
        xmax = max(xmax,geom1(3* (i-1)+1))
        ymin = min(ymin,geom1(3* (i-1)+2))
        ymax = max(ymax,geom1(3* (i-1)+2))
 10 end do
    do 20 i = 1, nno2
        if (lino2(i) .eq. 0) goto 20
        xmin = min(xmin,geom2(3* (i-1)+1))
        xmax = max(xmax,geom2(3* (i-1)+1))
        ymin = min(ymin,geom2(3* (i-1)+2))
        ymax = max(ymax,geom2(3* (i-1)+2))
 20 end do
    stotal = (xmax-xmin)* (ymax-ymin)
    sboite = (stotal/ntr3)*5.d0
    dx = sqrt(sboite)
    dy = dx
    nx = int((xmax-xmin)*1.05d0/dx) + 1
    ny = int((ymax-ymin)*1.05d0/dy) + 1
    ASSERT(nx*ny.ne.0)
    ddx = (nx*dx- (xmax-xmin))/2.d0
    ddy = (ny*dy- (ymax-ymin))/2.d0
    xmin = xmin - ddx
    xmax = xmax + ddx
    ymin = ymin - ddy
    ymax = ymax + ddy
!
!
!     2. : ALLOCATION DE LA SD BOITE_2D :
!     ---------------------------------------
    call wkvect(boite//'.BT2DDI', 'V V I', 2, iabtdi)
    call wkvect(boite//'.BT2DVR', 'V V R', 6, iabtvr)
    call wkvect(boite//'.BT2DNB', 'V V I', nx*ny, iabtnb)
    call wkvect(boite//'.BT2DLC', 'V V I', 1+nx*ny, iabtlc)
!
    zi(iabtdi-1+1) = nx
    zi(iabtdi-1+2) = ny
!
    zr(iabtvr-1+1) = xmin
    zr(iabtvr-1+2) = xmax
    zr(iabtvr-1+3) = ymin
    zr(iabtvr-1+4) = ymax
    zr(iabtvr-1+5) = dx
    zr(iabtvr-1+6) = dy
!
!
!
!     3. : ON COMPTE COMBIEN DE TRIA3 SERONT CONTENUS
!             DANS CHAQUE BOITE(P,Q)
!     -------------------------------------------------------
    do 60 i = 1, ntr3
        xxmin = rbig
        yymin = rbig
        xxmax = -rbig
        yymax = -rbig
        do 30 k = 1, 3
            ino = tria3(1+4* (i-1)+k)
            xxmin = min(xxmin,geom1(3* (ino-1)+1))
            xxmax = max(xxmax,geom1(3* (ino-1)+1))
            yymin = min(yymin,geom1(3* (ino-1)+2))
            yymax = max(yymax,geom1(3* (ino-1)+2))
 30     continue
        p1 = int((xxmin-xmin)/dx) + 1
        p2 = int((xxmax-xmin)/dx) + 1
        q1 = int((yymin-ymin)/dy) + 1
        q2 = int((yymax-ymin)/dy) + 1
        do 50 p = p1, p2
            do 40 q = q1, q2
                zi(iabtnb-1+ (q-1)*nx+p) = zi(iabtnb-1+ (q-1)*nx+p) + 1
 40         continue
 50     continue
!
 60 end do
!
!
!
!     4. : ON REMPLIT .BT2DCO  ET .BT2DLC :
!     -------------------------------------------------------
    zi(iabtlc-1+1) = 0
    do 70 ib = 1, nx*ny
        zi(iabtlc-1+ib+1) = zi(iabtlc-1+ib) + zi(iabtnb-1+ib)
        zi(iabtnb-1+ib) = 0
 70 end do
!
!
    lont = zi(iabtlc-1+1+nx*ny)
    call wkvect(boite//'.BT2DCO', 'V V I', lont, iabtco)
!
    do 110 i = 1, ntr3
        xxmin = rbig
        yymin = rbig
        xxmax = -rbig
        yymax = -rbig
        do 80 k = 1, 3
            ino = tria3(1+4* (i-1)+k)
            xxmin = min(xxmin,geom1(3* (ino-1)+1))
            xxmax = max(xxmax,geom1(3* (ino-1)+1))
            yymin = min(yymin,geom1(3* (ino-1)+2))
            yymax = max(yymax,geom1(3* (ino-1)+2))
 80     continue
        p1 = int((xxmin-xmin)/dx) + 1
        p2 = int((xxmax-xmin)/dx) + 1
        q1 = int((yymin-ymin)/dy) + 1
        q2 = int((yymax-ymin)/dy) + 1
        do 100 p = p1, p2
            do 90 q = q1, q2
                zi(iabtnb-1+ (q-1)*nx+p) = zi(iabtnb-1+ (q-1)*nx+p) + 1
                iposi = zi(iabtlc-1+ (q-1)*nx+p) + zi(iabtnb-1+ (q-1)* nx+p)
                ASSERT((iposi.ge.1) .and. (iposi.le.lont))
                zi(iabtco-1+iposi) = i
 90         continue
100     continue
!
110 end do
!
    if (dbg) then
        ifm = iunifi('MESSAGE')
        call utimsd(ifm, 2, .false._1, .true._1, boite,&
                    1, ' ')
    endif
    call jedema()
end subroutine

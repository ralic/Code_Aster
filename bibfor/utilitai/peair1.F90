subroutine peair1(modele, nbma, lisma, aire, long)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/vdiff.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
    integer :: nbma, lisma(*)
    real(kind=8) :: aire, long
    character(len=*) :: modele
!
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
!
!     CALCUL DE L'AIRE_INTERNE A UN CONTOUR
!     IN : MODELE : NOM DU MODELE
!     IN : LISMA (NBMA) : LISTE DES NUMEROS DE MAILLES DU CONTOUR FERME
!     OUT : AIRE : AIRE DELIMITEE PAR LE CONTOUR
!     OUT : LONG : LONGUEUR DU CONTOUR
!
!
    integer :: jma, ifm, niv, jnoma, idtyma, jn1, jn2, ima, numa, idcoor
    integer :: nutyma, nbel, jdno, nbext1, nbext2, iext1
    integer :: iext2, ni1, ni2, nj1, nj2, nbe, nj3, nj0, jdco, jm1
    real(kind=8) :: orig(3), zero, vgn1(3), vn1n2(3), aire1, aire2, vgn3(3)
    real(kind=8) :: vn1n3(3)
    real(kind=8) :: xx1(3), xx2(3), xx3(3), xn(3), pv(3), xnorm, vn3n2(3)
    real(kind=8) :: vgn2(3)
    real(kind=8) :: x1, y1, z1, x2, y2, z2, xxl
    character(len=8) :: noma, nomail, typel
    character(len=24) :: mlgnma, mlgcnx, mlgcoo
    character(len=24) :: valk(2)
!
    call jemarq()
!
    call infniv(ifm, niv)
!
    zero = 0.0d0
    orig(1) = zero
    orig(2) = zero
    orig(3) = zero
    call jeveuo(modele(1:8)//'.MODELE    .LGRF', 'L', jnoma)
    noma = zk8(jnoma)
    mlgnma = noma//'.NOMMAI'
    mlgcnx = noma//'.CONNEX'
!
    call jeveuo(noma//'.TYPMAIL', 'L', idtyma)
    call jeveuo(noma//'.COORDO    .VALE', 'L', idcoor)
!
    call wkvect('&&PEAIR1.NOEUD1', 'V V I', nbma*3, jn1)
    call wkvect('&&PEAIR1.NOEUD2', 'V V I', nbma*3, jn2)
    call wkvect('&&PEAIR1.MAILLES', 'V V I', nbma, jm1)
!
!     VERIFICATION DU TYPE DES MAILLES ET STOCKAGE DES CONNECTIVITES
!
    long = 0.d0
    nbel = 0
    do  ima = 1, nbma
        numa = lisma(ima)
        call jenuno(jexnum(noma//'.NOMMAI', numa), nomail)
!
!        TYPE DE LA MAILLE COURANTE :
!
        nutyma = zi(idtyma+numa-1)
        call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), typel)
!
        if (typel(1:3) .ne. 'SEG') then
            call jenuno(jexnum(mlgnma, numa), nomail)
            valk(1) = nomail
            valk(2) = typel
            call u2mesk('F', 'UTILITY_1', 2, valk)
        endif
        nbel = nbel + 1
        call jeveuo(jexnum(mlgcnx, numa), 'L', jdno)
        zi(jn1-1+3*nbel-2) = zi(jdno)
        zi(jn1-1+3*nbel-1) = zi(jdno+1)
        if (typel(1:4) .eq. 'SEG3') then
            zi(jn1-1+3*nbel) = zi(jdno+2)
            x1 = zr(idcoor+3*(zi(jdno )-1)+1-1)
            y1 = zr(idcoor+3*(zi(jdno )-1)+2-1)
            z1 = zr(idcoor+3*(zi(jdno )-1)+3-1)
            x2 = zr(idcoor+3*(zi(jdno+2)-1)+1-1)
            y2 = zr(idcoor+3*(zi(jdno+2)-1)+2-1)
            z2 = zr(idcoor+3*(zi(jdno+2)-1)+3-1)
            xxl = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)
            long = long + sqrt( xxl )
            x1 = zr(idcoor+3*(zi(jdno+1)-1)+1-1)
            y1 = zr(idcoor+3*(zi(jdno+1)-1)+2-1)
            z1 = zr(idcoor+3*(zi(jdno+1)-1)+3-1)
            xxl = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)
            long = long + sqrt( xxl )
        else
            x1 = zr(idcoor+3*(zi(jdno )-1)+1-1)
            y1 = zr(idcoor+3*(zi(jdno )-1)+2-1)
            z1 = zr(idcoor+3*(zi(jdno )-1)+3-1)
            x2 = zr(idcoor+3*(zi(jdno+1)-1)+1-1)
            y2 = zr(idcoor+3*(zi(jdno+1)-1)+2-1)
            z2 = zr(idcoor+3*(zi(jdno+1)-1)+3-1)
            xxl = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)
            long = long + sqrt( xxl )
        endif
    end do
    ASSERT(nbma.eq.nbel)
!
!     VERIFICATION QUE LE CONTOUR EST FERME
!
    nbext1=0
    nbext2=0
    do  ima = 1, nbel
        iext1=0
        iext2=0
        ni1 = zi(jn1-1+3*ima-2)
        ni2 = zi(jn1-1+3*ima-1)
        zi(jm1-1+ima)=ima
        do  jma = 1, nbel
            if (jma .ne. ima) then
                nj1 = zi(jn1-1+3*jma-2)
                nj2 = zi(jn1-1+3*jma-1)
                if ((ni1.eq.nj2) .or. (ni1.eq.nj1)) iext1=1
                if ((ni2.eq.nj1) .or. (ni2.eq.nj2)) iext2=1
            endif
        enddo
        if (iext1 .eq. 0) nbext1=nbext1+1
        if (iext2 .eq. 0) nbext2=nbext2+1
    end do
    if ((nbext1.ne.0) .and. (nbext2.ne.0)) then
        call u2mess('F', 'UTILITY_2')
    endif
!
!     VERIFICATION QUE LE CONTOUR EST CONTINU ET REORIENTATION
!
    nbe=1
    zi(jm1)=0
    zi(jn2)=zi(jn1)
    zi(jn2+1)=zi(jn1+1)
    zi(jn2+2)=zi(jn1+2)
41  continue
    ni1 = zi(jn2-1+3*nbe-2)
    ni2 = zi(jn2-1+3*nbe-1)
    do jma = 1, nbel
        if ((zi(jm1-1+jma).ne.0)) then
            nj1 = zi(jn1-1+3*jma-2)
            nj2 = zi(jn1-1+3*jma-1)
            nj3 = zi(jn1-1+3*jma)
            if (ni2 .eq. nj1) then
                nbe = nbe+1
                zi(jn2-1+3*nbe-2)=nj1
                zi(jn2-1+3*nbe-1)=nj2
                if (nj3 .ne. 0) zi(jn2-1+3*nbe)=nj3
                goto 43
            else if (ni2.eq.nj2) then
                nbe = nbe+1
                zi(jn2-1+3*nbe-2)=nj2
                zi(jn2-1+3*nbe-1)=nj1
                if (nj3 .ne. 0) zi(jn2-1+3*nbe)=nj3
                goto 43
            endif
        endif
    end do
    call u2mess('F', 'UTILITY_2')
43  continue
    zi(jm1-1+jma)=0
    if (nbe .ge. nbma) then
        goto 11
    else
        goto 41
    endif
11  continue
    ASSERT(nbma.eq.nbe)
    nj2=zi(jn2-1+3*nbe-1)
    nj0=zi(jn2)
    ASSERT(nj2.eq.nj0)
!
!     CALCUL DU CDG APPROXIMATIF
!
    mlgcoo = noma//'.COORDO    .VALE'
    call jeveuo(mlgcoo, 'L', jdco)
    do  ima = 1, nbma
        nj1 = zi(jn2-1+3*ima-2)
        orig(1) = orig(1)+zr(jdco-1+3*nj1-2)
        orig(2) = orig(2)+zr(jdco-1+3*nj1-1)
        orig(3) = orig(3)+zr(jdco-1+3*nj1)
    end do
    orig(1)=orig(1)/nbma
    orig(2)=orig(2)/nbma
    orig(3)=orig(3)/nbma
!
!     CALCUL DE L'AIRE GM.VECT.DL
!
    nj1 = zi(jn2-1+1)
    nj2 = zi(jn2-1+2)
!
!     CALCUL DE LA NORMALE A LA COURBE SUPPOSEE PLANE
!
    xx1(1) = zr(jdco-1+3*nj1-2)
    xx1(2) = zr(jdco-1+3*nj1-1)
    xx1(3) = zr(jdco-1+3*nj1)
    xx2(1) = zr(jdco-1+3*nj2-2)
    xx2(2) = zr(jdco-1+3*nj2-1)
    xx2(3) = zr(jdco-1+3*nj2)
    call vdiff(3, xx1, orig, vgn1)
    call vdiff(3, xx2, orig, vgn2)
    call provec(vgn1, vgn2, xn)
    call normev(xn, xnorm)
    aire=0.d0
    do  ima = 1, nbma
        nj1 = zi(jn2-1+3*ima-2)
        nj2 = zi(jn2-1+3*ima-1)
        nj3 = zi(jn2-1+3*ima)
        if (nj3 .eq. 0) then
            xx1(1) = zr(jdco-1+3*nj1-2)
            xx1(2) = zr(jdco-1+3*nj1-1)
            xx1(3) = zr(jdco-1+3*nj1)
            xx2(1) = zr(jdco-1+3*nj2-2)
            xx2(2) = zr(jdco-1+3*nj2-1)
            xx2(3) = zr(jdco-1+3*nj2)
            call vdiff(3, xx1, orig, vgn1)
            call vdiff(3, xx2, xx1, vn1n2)
            call provec(vgn1, vn1n2, pv)
            aire1=ddot(3,pv,1,xn,1)
            aire=aire+aire1/2.d0
        else
            xx1(1) = zr(jdco-1+3*nj1-2)
            xx1(2) = zr(jdco-1+3*nj1-1)
            xx1(3) = zr(jdco-1+3*nj1)
            xx2(1) = zr(jdco-1+3*nj2-2)
            xx2(2) = zr(jdco-1+3*nj2-1)
            xx2(3) = zr(jdco-1+3*nj2)
            xx3(1) = zr(jdco-1+3*nj3-2)
            xx3(2) = zr(jdco-1+3*nj3-1)
            xx3(3) = zr(jdco-1+3*nj3)
            call vdiff(3, xx1, orig, vgn1)
            call vdiff(3, xx3, xx1, vn1n3)
            call provec(vgn1, vn1n3, pv)
            aire1=ddot(3,pv,1,xn,1)
            aire=aire+aire1/2.d0
            call vdiff(3, xx3, orig, vgn3)
            call vdiff(3, xx2, xx3, vn3n2)
            call provec(vgn3, vn3n2, pv)
            aire2=ddot(3,pv,1,xn,1)
            aire=aire+aire2/2.d0
        endif
    end do
    call jedetr('&&PEAIR1.NOEUD1')
    call jedetr('&&PEAIR1.NOEUD2')
    call jedetr('&&PEAIR1.MAILLES')
    call jedema()
end subroutine

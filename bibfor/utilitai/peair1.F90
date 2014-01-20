subroutine peair1(modele, nbma, lisma, aire, long)
    implicit none
#include "jeveux.h"
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
#include "asterfort/utmess.h"
#include "asterfort/vdiff.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/ddot.h"
!
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
    integer :: jma, ifm, niv, jnoma, idtyma,   ima, numa, idcoor
    integer :: nutyma, nbel, jdno, nbext1, nbext2, iext1
    integer :: iext2, ni1, ni2, nj1, nj2, nbe, nj3, nj0, jdco
    real(kind=8) :: orig(3), zero, vgn1(3), vn1n2(3), aire1, aire2, vgn3(3)
    real(kind=8) :: vn1n3(3)
    real(kind=8) :: xx1(3), xx2(3), xx3(3), xn(3), pv(3), xnorm, vn3n2(3)
    real(kind=8) :: vgn2(3)
    real(kind=8) :: x1, y1, z1, x2, y2, z2, xxl
    character(len=8) :: noma, nomail, typel
    character(len=24) :: mlgnma, mlgcnx, mlgcoo
    character(len=24) :: valk(2)
    integer, pointer :: mailles(:) => null()
    integer, pointer :: noeud1(:) => null()
    integer, pointer :: noeud2(:) => null()
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
    AS_ALLOCATE(vi=noeud1, size=nbma*3)
    AS_ALLOCATE(vi=noeud2, size=nbma*3)
    AS_ALLOCATE(vi=mailles, size=nbma)
!
!     VERIFICATION DU TYPE DES MAILLES ET STOCKAGE DES CONNECTIVITES
!
    long = 0.d0
    nbel = 0
    do ima = 1, nbma
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
            call utmess('F', 'UTILITY_1', nk=2, valk=valk)
        endif
        nbel = nbel + 1
        call jeveuo(jexnum(mlgcnx, numa), 'L', jdno)
        noeud1(3*nbel-2) = zi(jdno)
        noeud1(3*nbel-1) = zi(jdno+1)
        if (typel(1:4) .eq. 'SEG3') then
            noeud1(3*nbel) = zi(jdno+2)
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
    do ima = 1, nbel
        iext1=0
        iext2=0
        ni1 = noeud1(3*ima-2)
        ni2 = noeud1(3*ima-1)
        mailles(ima)=ima
        do jma = 1, nbel
            if (jma .ne. ima) then
                nj1 = noeud1(3*jma-2)
                nj2 = noeud1(3*jma-1)
                if ((ni1.eq.nj2) .or. (ni1.eq.nj1)) iext1=1
                if ((ni2.eq.nj1) .or. (ni2.eq.nj2)) iext2=1
            endif
        enddo
        if (iext1 .eq. 0) nbext1=nbext1+1
        if (iext2 .eq. 0) nbext2=nbext2+1
    end do
    if ((nbext1.ne.0) .and. (nbext2.ne.0)) then
        call utmess('F', 'UTILITY_2')
    endif
!
!     VERIFICATION QUE LE CONTOUR EST CONTINU ET REORIENTATION
!
    nbe=1
    mailles(1)=0
    noeud2(1)=noeud1(1)
    noeud2(1+1)=noeud1(1+1)
    noeud2(1+2)=noeud1(1+2)
41  continue
    ni1 = noeud2(3*nbe-2)
    ni2 = noeud2(3*nbe-1)
    do jma = 1, nbel
        if ((mailles(jma).ne.0)) then
            nj1 = noeud1(3*jma-2)
            nj2 = noeud1(3*jma-1)
            nj3 = noeud1(3*jma)
            if (ni2 .eq. nj1) then
                nbe = nbe+1
                noeud2(3*nbe-2)=nj1
                noeud2(3*nbe-1)=nj2
                if (nj3 .ne. 0) noeud2(3*nbe)=nj3
                goto 43
            else if (ni2.eq.nj2) then
                nbe = nbe+1
                noeud2(3*nbe-2)=nj2
                noeud2(3*nbe-1)=nj1
                if (nj3 .ne. 0) noeud2(3*nbe)=nj3
                goto 43
            endif
        endif
    end do
    call utmess('F', 'UTILITY_2')
43  continue
    mailles(jma)=0
    if (nbe .ge. nbma) then
        goto 11
    else
        goto 41
    endif
11  continue
    ASSERT(nbma.eq.nbe)
    nj2=noeud2(3*nbe-1)
    nj0=noeud2(1)
    ASSERT(nj2.eq.nj0)
!
!     CALCUL DU CDG APPROXIMATIF
!
    mlgcoo = noma//'.COORDO    .VALE'
    call jeveuo(mlgcoo, 'L', jdco)
    do ima = 1, nbma
        nj1 = noeud2(3*ima-2)
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
    nj1 = noeud2(1)
    nj2 = noeud2(2)
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
    do ima = 1, nbma
        nj1 = noeud2(3*ima-2)
        nj2 = noeud2(3*ima-1)
        nj3 = noeud2(3*ima)
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
    AS_DEALLOCATE(vi=noeud1)
    AS_DEALLOCATE(vi=noeud2)
    AS_DEALLOCATE(vi=mailles)
    call jedema()
end subroutine

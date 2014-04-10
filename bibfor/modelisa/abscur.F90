subroutine abscur(ma)
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
    implicit none

#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/alcart.h"
#include "asterfort/arcseg34.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/imprsd.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/sdmail.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/dismoi.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"

    character(len=8) :: ma
!-----------------------------------------------------------------------
!     calcul d'une abscisse curviligne pour un groupe de mailles :
!     toutes les mailles doivent etre du type 'poi1' ou 'seg2,3,4'

!     arguments en entree
!     ------------------
!     ma   : nom du maillage

!     en sortie
!     ---------
!     creation d'une carte contenant l'abscisse curviligne
!-----------------------------------------------------------------------

    character(len=8) :: typm,noma, nono
    character(len=24) :: mesmai, mesnoe
    character(len=24) :: nommai, nomnoe, cooval, coodsc, cooref, grpnoe
    character(len=24) :: gpptnn, grpmai, gpptnm, connex, titre, typmai, adapma
    character(len=16) :: motcle(3), typmcl(3)
    integer :: adrm, iseg1,iseg2,isegprev,jtmp,kseg,nbextr,nbnot
    integer :: iab1, iab2, iadr2, iancmp,numa2,nuno1,nuno2
    integer :: iavalv, icoo1, icoo2, icoo3, icoo4, icor2, kma, nunosuiv, vuorig
    integer :: jpoi, jseg, ind, ing, ino
    integer :: ipoi1, iseg, itypm, jcoor
    integer :: mi, n, n1, n2, nunorig
    integer :: nbpoi1, nbma, nbseg, nbno
    integer :: jmesma,jmesno,numa,iexi,nbnoseg
    real(kind=8) :: s, stot
    real(kind=8) :: s13, s32, s34, s42, abscurv(4), coor(3,4)
    logical :: dbg=.false.
    integer, pointer :: icoseg(:) => null()
    integer, pointer :: nu2seg(:) => null()
    integer, pointer :: segordo(:) => null()
!-----------------------------------------------------------------------
    call jemarq()

    call sdmail(ma, nommai, nomnoe, cooval, coodsc,&
                cooref, grpnoe, gpptnn, grpmai, gpptnm,&
                connex, titre, typmai, adapma)
    call dismoi('NB_NO_MAILLA', ma, 'MAILLAGE', repi=nbnot)
    call jeveuo(cooval, 'L', jcoor)


!   -- 1. numero du noeud origine :
!   -------------------------------
    mesnoe='&&ABSCUR.MESNOE'
    motcle(1) = 'GROUP_NO_ORIG'
    motcle(2) = 'NOEUD_ORIG'
    typmcl(1) = 'GROUP_NO'
    typmcl(2) = 'NOEUD'
    call reliem(' ', ma, 'NU_NOEUD', 'ABSC_CURV', 1,&
                2, motcle, typmcl, mesnoe, nbno)
    ASSERT(nbno.eq.1)
    call jeveuo(mesnoe, 'L', jmesno)
    nunorig=zi(jmesno)


!   -- 2. liste des mailles a traiter :
!   -----------------------------------
    mesmai='&&ABSCUR.MESMAI'
    motcle(1) = 'TOUT'
    motcle(2) = 'GROUP_MA'
    motcle(3) = 'MAILLE'
    typmcl(1) = 'TOUT'
    typmcl(2) = 'GROUP_MA'
    typmcl(3) = 'MAILLE'
    call reliem(' ', ma, 'NU_MAILLE', 'ABSC_CURV', 1,&
                3, motcle, typmcl, mesmai, nbma)
    call jeveuo(mesmai, 'L', jmesma)


!   --  3. creation d'objets temporaires :
!   ---------------------------------------
    call wkvect('&&ABSCUR.IPOI1', 'V V I', nbma, jpoi)
    call wkvect('&&ABSCUR.SEG', 'V V I', nbma, jseg)
    call wkvect('&&ABSCUR.AB1  ', 'V V R', nbma, iab1)
    call wkvect('&&ABSCUR.AB2  ', 'V V R', nbma, iab2)
    call wkvect('&&ABSCUR.COR2 ', 'V V I', nbma, icor2)


!   --  4. tri des mailles poi1 et seg
!   -----------------------------------
    call jeveuo(typmai, 'L', itypm)
    nbseg=0
    nbpoi1=0
    do 12 kma = 1, nbma
        numa=zi(jmesma-1+kma)
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(itypm+numa-1)), typm)
        if (typm .eq. 'SEG2' .or. typm .eq. 'SEG3' .or. typm .eq. 'SEG4') then
            nbseg=nbseg+1
            zi(jseg+nbseg-1)=numa
        else if (typm .eq. 'POI1') then
            nbpoi1=nbpoi1+1
            zi(jpoi+nbpoi1-1)=numa
        else
            call utmess('F', 'MODELISA_2')
        endif
12  end do


!   --  5. Les segments doivent former une ligne ouverte avec
!          deux extremites.
!   --------------------------------------------------------
!   -- 5.1 : on note les noeuds extremites des segments :
    AS_ALLOCATE(vi=icoseg,size=nbnot)
    AS_ALLOCATE(vi=nu2seg,size=2*nbnot)
    do iseg = 1, nbseg
        numa=zi(jseg-1+iseg)
        call jeveuo(jexnum(connex, numa), 'L', jtmp)
        nuno1=zi(jtmp-1+1)
        nuno2=zi(jtmp-1+2)
        icoseg(nuno1)=icoseg(nuno1)+1
        icoseg(nuno2)=icoseg(nuno2)+1
        if (icoseg(nuno1).le.2) nu2seg(2*(nuno1-1)+icoseg(nuno1))=iseg
        if (icoseg(nuno2).le.2) nu2seg(2*(nuno2-1)+icoseg(nuno2))=iseg
    enddo

!   -- 5.2 : on verifie que les segments forment une ligne ouverte
!            et que l'une des 2 extremites est nunorig :
    vuorig=0
    nbextr=0
    do ino = 1, nbnot
        if (icoseg(ino).eq.2) then
        else if (icoseg(ino).eq.1) then
            nbextr=nbextr+1
            if (ino.eq.nunorig) then
                vuorig=vuorig+1
            endif
        else if (icoseg(ino).eq.0) then
        else
            call jenuno(jexnum(nomnoe, ino), nono)
            call utmess('F','INTEMAIL_36',sk=nono)
        endif
    enddo
    if (nbextr.ne.2) call utmess('F','INTEMAIL_37',si=nbextr)
    if (vuorig.ne.1) call utmess('F','INTEMAIL_38')

!   -- 5.3 : on etablit la liste ordonnee des segments
!   ---------------------------------------------------
    AS_ALLOCATE(vi=segordo,size=nbseg)

!   -- on initialise la recherche (1er segment):
    kseg=1
    nunosuiv=nunorig
    iseg=nu2seg(2*nunosuiv-1)
    ASSERT(iseg.gt.0)
    numa=zi(jseg-1+iseg)
    call jeveuo(jexnum(connex, numa), 'L', jtmp)
    ASSERT(nunosuiv.eq.zi(jtmp) .or. nunosuiv.eq.zi(jtmp+1))
    if (nunosuiv.eq.zi(jtmp)) then
        segordo(kseg)=+iseg
        nunosuiv=zi(jtmp+1)
    else
        segordo(kseg)=-iseg
        nunosuiv=zi(jtmp)
    endif
    isegprev=iseg

!   -- on met les segments bout a bout :
    do while (kseg.lt.nbseg)
        kseg=kseg+1
        iseg1=nu2seg(2*nunosuiv-1)
        iseg2=nu2seg(2*nunosuiv)
        if (iseg1.eq.isegprev) then
            iseg=iseg2
        elseif (iseg2.eq.isegprev) then
            iseg=iseg1
        else
            ASSERT(.false.)
        endif
        numa=zi(jseg-1+iseg)
        call jeveuo(jexnum(connex, numa), 'L', jtmp)
        ASSERT(nunosuiv.eq.zi(jtmp) .or. nunosuiv.eq.zi(jtmp+1))
        if (nunosuiv.eq.zi(jtmp)) then
            segordo(kseg)=+iseg
            nunosuiv=zi(jtmp+1)
        else
            segordo(kseg)=-iseg
            nunosuiv=zi(jtmp)
        endif
        isegprev=iseg
    enddo


!   -- 6. On verifie que les POI1 sont sur des segments :
!   --------------------------------------------------------
    do 15 ipoi1 = 1, nbpoi1
        numa=zi(jpoi+ipoi1-1)
        call jeveuo(jexnum(connex, numa), 'L', adrm)
        n = zi(adrm)
        do 16 iseg = 1, nbseg
            numa2=zi(jseg-1+iseg)
            call jeveuo(jexnum(connex, numa2), 'L', iadr2)
            n1 = zi(iadr2)
            n2 = zi(iadr2 + 1)
            if (n1 .eq. n) then
                zi(icor2+ipoi1-1)= iseg
                goto 15
            else if (n2.eq.n) then
                zi(icor2+ipoi1-1)=-iseg
                goto 15
            endif
16      continue
        call jenuno(jexnum(nommai, numa), noma)
        call utmess('F', 'MODELISA_3',sk=noma)
15  continue



!   --  7. allocation de la carte :
!   ------------------------------
    call exisd('CHAMP', ma//'.ABSC_CURV', iexi)
    if (iexi.eq.1) then
        call utmess('F','INTEMAIL_35')
        call detrsd('CHAMP', ma//'.ABSC_CURV')
    endif
    call alcart('G', ma//'.ABSC_CURV', ma, 'ABSC_R')
    call jeveuo(ma//'.ABSC_CURV .NCMP', 'E', iancmp)
    call jeveuo(ma//'.ABSC_CURV .VALV', 'E', iavalv)
    zk8(iancmp)   = 'ABSC1'
    zk8(iancmp+1) = 'ABSC2'
    zk8(iancmp+2) = 'ABSC3'
    zk8(iancmp+3) = 'ABSC4'
    stot = 0.d0


!   --  8. calcul de l'abscisse curviligne
!   ---------------------------------------
    do 10 kseg = 1, nbseg
        mi = segordo(kseg)
        numa=zi(jseg-1+abs(mi))
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(itypm+numa-1)), typm)
        call jeveuo(jexnum(connex, numa), 'L', jtmp)
        if (mi.gt.0) then
            ing=zi(jtmp-1+1)
            ind=zi(jtmp-1+2)
        else
            ing=zi(jtmp-1+2)
            ind=zi(jtmp-1+1)
        endif

!       noeuds 1 et 2 (extremites) :
        icoo1 = 3*(ing-1)
        icoo2 = 3*(ind-1)
        coor(:,1)=zr(jcoor+icoo1-1+1:jcoor+icoo1-1+3)
        coor(:,2)=zr(jcoor+icoo2-1+1:jcoor+icoo2-1+3)

!       noeuds 3 et 4 (si necessaire) :
        if (typm.eq.'SEG3') then
            nbnoseg=3
            icoo3 = 3*(zi(jtmp-1+3)-1)
            coor(:,3)=zr(jcoor+icoo3-1+1:jcoor+icoo3-1+3)
        elseif (typm.eq.'SEG4') then
            nbnoseg=4
            icoo3 = 3*(zi(jtmp-1+3)-1)
            icoo4 = 3*(zi(jtmp-1+4)-1)
            coor(:,3)=zr(jcoor+icoo3-1+1:jcoor+icoo3-1+3)
            coor(:,4)=zr(jcoor+icoo4-1+1:jcoor+icoo4-1+3)
        else
            ASSERT(typm.eq.'SEG2')
            nbnoseg=2
        endif

!       -- calcul des abscisses curvilignes :
        call arcseg34(nbnoseg,coor,abscurv)

        if (nbnoseg.eq.2) then
            s = abscurv(2)-abscurv(1)
        elseif (nbnoseg.eq.3) then
            s13 = abscurv(3)-abscurv(1)
            s32 = abscurv(2)-abscurv(3)
            s=s13+s32
        elseif (nbnoseg.eq.4) then
            s13 = abscurv(3)-abscurv(1)
            s34 = abscurv(4)-abscurv(3)
            s42 = abscurv(2)-abscurv(4)
            s=s13+s34+s42
        endif

        if (mi .gt. 0) then
            zr(iavalv ) = stot
            zr(iab1+abs(mi)-1)= stot

            if (nbnoseg.eq.3) then
                zr(iavalv+2) = stot+s13
            elseif (nbnoseg.eq.4) then
                zr(iavalv+2) = stot+s13
                zr(iavalv+3) = stot+s13+s34
            endif

            stot = stot+s
            zr(iavalv+1) = stot
            zr(iab2+abs(mi)-1)= stot
        else
            zr(iavalv+1 ) = stot
            zr(iab2+abs(mi)-1)= stot

            if (nbnoseg.eq.3) then
                zr(iavalv+2) = stot+s13
            elseif (nbnoseg.eq.4) then
                zr(iavalv+2) = stot+s13
                zr(iavalv+3) = stot+s13+s34
            endif

            stot = stot+s
            zr(iavalv) = stot
            zr(iab1+abs(mi)-1)= stot
        endif
        call nocart(ma//'.ABSC_CURV', 3, nbnoseg, mode='NUM', nma=1, limanu=[numa])

10  end do


!   --  cas des poi1 :
!   --------------------
    do 20 ipoi1 = 1, nbpoi1
        numa=zi(jpoi+ipoi1-1)
        mi=zi(icor2+ipoi1-1)
        if (mi .gt. 0) then
            s=zr(iab1+mi-1)
        else
            s=zr(iab2-mi-1)
        endif
        zr(iavalv ) = s
        call nocart(ma//'.ABSC_CURV', 3, 1, mode='NUM', nma=1, limanu=[numa])
20  end do


    dbg=.false.
    if (dbg) call imprsd('CHAMP', ma//'.ABSC_CURV', 6, 'ABSC_CURV')


!   -- menage
!   ---------
    AS_DEALLOCATE(vi=icoseg)
    AS_DEALLOCATE(vi=nu2seg)
    AS_DEALLOCATE(vi=segordo)
    call jedetr('&&ABSCUR.AB1')
    call jedetr('&&ABSCUR.AB2')
    call jedetr('&&ABSCUR.COR2')
    call jedetr('&&ABSCUR.IPOI1')
    call jedetr('&&ABSCUR.SEG')
    call jedetr(mesnoe)
    call jedetr(mesmai)
    call jedetr(ma//'.ABSC_CURV .NCMP')
    call jedetr(ma//'.ABSC_CURV .VALV')

    call jedema()
end subroutine

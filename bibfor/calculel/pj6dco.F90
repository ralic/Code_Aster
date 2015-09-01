subroutine pj6dco(mocle, moa1, moa2, nbma1, lima1,&
                  nbno2, lino2, geom1, geom2, corres,&
                  l_dmax, dmax, dala)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/inslri.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/pj1dtr.h"
#include "asterfort/pj3dfb.h"
#include "asterfort/pj6dap.h"
#include "asterfort/pjxxut.h"
#include "asterfort/pjloin.h"
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!
    character(len=16) :: corres
    character(len=*) :: geom1, geom2
    character(len=8) :: moa1, moa2
    character(len=*) :: mocle
    integer :: nbma1, lima1(*), nbno2, lino2(*)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! but :
!   creer une sd corresp_2_mailla
!   donnant la correspondance entre les noeuds de moa2 et les mailles de
!   moa1 dans le cas ou moa1 est 1.5d (ligne en 2d ou 3d)
! ======================================================================

!  pour les arguments : mocle, moa1, moa2, nbma1, lima1, nbno2, lino2
!  voir le cartouche de pjxxut.f

!  in/jxin   geom1    i   : objet jeveux contenant la geometrie des
!                           noeuds du maillage 1 (ou ' ')
!  in/jxin   geom2    i   : objet jeveux contenant la geometrie des
!                           noeuds du maillage 2 (ou ' ')
!                remarque:  les objets geom1 et geom2 ne sont utiles
!                           que lorsque l'on veut truander la geometrie
!                           des maillages

!  in/jxout  corres  k16 : nom de la sd corresp_2_mailla
! ----------------------------------------------------------------------



    character(len=8) :: m1, m2, nono2
    character(len=14) :: boite
    character(len=16) :: cortr3
    integer :: nbtm, nbtmx
    parameter   (nbtmx=15)
    integer :: nutm(nbtmx)
    character(len=8) :: elrf(nbtmx)

    integer :: ifm, niv, nno1, nno2, nma1, nma2, k
    integer :: ima, ino2, ico
    integer :: iatr3, iacoo1, iacoo2
    integer :: iabtco, jxxk1, iaconu, iacocf, iacotr
    integer :: ialim1, ialin1, ilcnx1, ialin2
    integer :: iaconb, itypm, idecal, itr3, nbtrou

    aster_logical :: dbg, l_dmax, loin, loin2
    real(kind=8) :: dmax, dmin, dala
    real(kind=8) :: cobary(2)

    integer :: nbmax
    parameter (nbmax=5)
    integer :: tino2m(nbmax), nbnod, nbnodm
    real(kind=8) :: tdmin2(nbmax)
    integer, pointer :: bt3dnb(:) => null()
    integer, pointer :: lino_loin(:) => null()
    real(kind=8), pointer :: bt3dvr(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: bt3ddi(:) => null()
    integer, pointer :: typmail(:) => null()
    integer, pointer :: bt3dlc(:) => null()

! DEB ------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)

    call pjxxut('1D', mocle, moa1, moa2, nbma1,&
                lima1, nbno2, lino2, m1, m2,&
                nbtmx, nbtm, nutm, elrf)

    call dismoi('NB_NO_MAILLA', m1, 'MAILLAGE', repi=nno1)
    call dismoi('NB_NO_MAILLA', m2, 'MAILLAGE', repi=nno2)
    call dismoi('NB_MA_MAILLA', m1, 'MAILLAGE', repi=nma1)
    call dismoi('NB_MA_MAILLA', m2, 'MAILLAGE', repi=nma2)

    call jeveuo('&&PJXXCO.LIMA1', 'L', ialim1)
    call jeveuo('&&PJXXCO.LINO1', 'L', ialin1)
    call jeveuo('&&PJXXCO.LINO2', 'L', ialin2)

!   -- l'objet lino_loin contiendra la liste des noeuds projetes un peu loin
    AS_ALLOCATE(vi=lino_loin, size=nno2)


!   2. on decoupe toutes les mailles 1d en seg2
!   ------------------------------------------------
!      (en conservant le lien de parente):
!      on cree l'objet v='&&pjxxco.seg2' : ojb s v i
!         long(v)=1+3*ntr3
!         v(1) : ntr3(=nombre de seg2)
!         v(1+3(i-1)+1) : numero du 1er  noeud du ieme seg2
!         v(1+3(i-1)+2) : numero du 2eme noeud du ieme seg2
!         v(1+3(i-1)+3) : numero de la maille mere du ieme seg2

    call jeveuo(m1//'.TYPMAIL', 'L', vi=typmail)
    ico=0
    do ima = 1, nma1
        if (zi(ialim1-1+ima) .eq. 0) cycle
        itypm=typmail(ima)
        if ((itypm.eq.nutm(1)) .or. (itypm.eq.nutm(2)) .or. ( itypm.eq.nutm(3))) then
            ico=ico+1
        else
            ASSERT(.false.)
            if (ico .eq. 0) then
                call utmess('F', 'CALCULEL4_55')
            endif
        endif
    enddo
    call wkvect('&&PJXXCO.SEG2', 'V V I', 1+3*ico, iatr3)
    zi(iatr3-1+1)=ico

    call jeveuo(m1//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(m1//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    ico=0
    do ima = 1, nma1
        if (zi(ialim1-1+ima) .eq. 0) cycle
        itypm=typmail(ima)
!       -- CAS DES SEGMENTS :
        if ((itypm.eq.nutm(1)) .or. (itypm.eq.nutm(2)) .or. ( itypm.eq.nutm(3))) then
            ico=ico+1
            zi(iatr3+(ico-1)*3+3)=ima
            zi(iatr3+(ico-1)*3+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*3+2)=connex(1+ zi(ilcnx1-1+ima)-2+2)
        endif
    enddo


!   3. on met les seg2 en boites :
!   ---------------------------------------------------
    if (geom1 .eq. ' ') then
        call jeveuo(m1//'.COORDO    .VALE', 'L', iacoo1)
    else
        call jeveuo(geom1, 'L', iacoo1)
    endif
    if (geom2 .eq. ' ') then
        call jeveuo(m2//'.COORDO    .VALE', 'L', iacoo2)
    else
        call jeveuo(geom2, 'L', iacoo2)
    endif

    boite='&&PJ6DCO.BOITE'
    call pj3dfb(boite, '&&PJXXCO.SEG2', zr(iacoo1), zr(iacoo2))
    call jeveuo(boite//'.BT3DDI', 'L', vi=bt3ddi)
    call jeveuo(boite//'.BT3DVR', 'L', vr=bt3dvr)
    call jeveuo(boite//'.BT3DNB', 'L', vi=bt3dnb)
    call jeveuo(boite//'.BT3DLC', 'L', vi=bt3dlc)
    call jeveuo(boite//'.BT3DCO', 'L', iabtco)

!----------------------------------------------------------------
!     description de la sd boite_3d :
!     boite_3d (k14) ::= record
!      .bt3ddi   : ojb s v i  long=3
!      .bt3dvr   : ojb s v r  long=9
!      .bt3dnb   : ojb s v i  long=nx*ny*nz
!      .bt3dlc   : ojb s v i  long=1+nx*ny*nz
!      .bt3dco   : ojb s v i  long=*

!      .bt3ddi(1) : nx=nombre de boites dans la direction x
!      .bt3ddi(2) : ny=nombre de boites dans la direction y
!      .bt3ddi(3) : nz=nombre de boites dans la direction z

!      .bt3dvr(1) : xmin     .bt3dvr(2) : xmax
!      .bt3dvr(3) : ymin     .bt3dvr(4) : ymax
!      .bt3dvr(5) : zmin     .bt3dvr(6) : zmax
!      .bt3dvr(7) : dx = (xmax-xmin)/nbx
!      .bt3dvr(8) : dy = (ymax-ymin)/nby
!      .bt3dvr(9) : dz = (zmax-zmin)/nbz

!      .bt3dnb    : longueurs des boites
!      .bt3dnb(1) : nombre de seg2 contenus dans la boite(1,1,1)
!      .bt3dnb(2) : nombre de seg2 contenus dans la boite(2,1,1)
!      .bt3dnb(3) : ...
!      .bt3dnb(nx*ny*nz) : nombre de seg2 contenus dans la
!                          derniere boite(nx,ny,nz)

!      .bt3dlc    : longueurs cumulees de .bt3dco
!      .bt3dlc(1) : 0
!      .bt3dlc(2) : bt3dlc(1)+nbtr3(boite(1,1))
!      .bt3dlc(3) : bt3dlc(2)+nbtr3(boite(2,1))
!      .bt3dlc(4) : ...

!      .bt3dco    : contenu des boites
!       soit :
!        nbtr3 =nbtr3(boite(p,q,r)=bt3dnb((r-1)*ny*nx+(q-1)*nx+p)
!        debtr3=bt3dlc((r-1)*ny*nx+(q-1)*nx+p)
!        do k=1,nbtr3
!          tr3=.bt3dco(debtr3+k)
!        done
!        tr3 est le numero du kieme seg2 de la boite (p,q,r)
!----------------------------------------------------------------


!   4. construction d'un corresp_2_mailla temporaire :cortr3
!      (en utilisant les seg2 deduits du maillage m1)
!   ---------------------------------------------------
    cortr3='&&PJ6DCO.CORRESP'
    call wkvect(cortr3//'.PJXX_K1', 'V V K24', 5, jxxk1)
    zk24(jxxk1-1+1)=m1
    zk24(jxxk1-1+2)=m2
    zk24(jxxk1-1+3)='COLLOCATION'
    call wkvect(cortr3//'.PJEF_NB', 'V V I', nno2, iaconb)
    call wkvect(cortr3//'.PJEF_NU', 'V V I', 4*nno2, iaconu)
    call wkvect(cortr3//'.PJEF_CF', 'V V R', 2*nno2, iacocf)
    call wkvect(cortr3//'.PJEF_TR', 'V V I', nno2, iacotr)


!   on cherche pour chaque noeud ino2 de m2 le seg2
!   auquel il appartient ainsi que ses coordonnees
!   barycentriques dans ce seg2 :
!   ------------------------------------------------
    idecal=0
    loin2=.false.
    nbnod = 0
    nbnodm = 0
    do ino2 = 1, nno2
        if (zi(ialin2-1+ino2) .eq. 0) cycle
        call pj6dap(ino2, zr(iacoo2), zr(iacoo1), zi(iatr3),&
                    cobary, itr3, nbtrou, bt3ddi, bt3dvr,&
                    bt3dnb, bt3dlc, zi( iabtco),&
                    l_dmax, dmax, dala, loin, dmin)
        if (loin) then
            loin2=.true.
            nbnodm = nbnodm + 1
            lino_loin(nbnodm)=ino2
        endif
        call inslri(nbmax, nbnod, tdmin2, tino2m, dmin, ino2)
        if (l_dmax .and. (nbtrou.eq.0)) then
            zi(iaconb-1+ino2)=2
            zi(iacotr-1+ino2)=0
            cycle
        endif
        if (nbtrou .eq. 0) then
            call jenuno(jexnum(m2//'.NOMNOE', ino2), nono2)
            call utmess('F', 'CALCULEL4_56', sk=nono2)
        endif
        zi(iaconb-1+ino2)=2
        zi(iacotr-1+ino2)=itr3
        do k = 1, 2
            zi(iaconu-1+idecal+k)= zi(iatr3+3*(itr3-1)+k)
            zr(iacocf-1+idecal+k)= cobary(k)
        enddo
        idecal=idecal+zi(iaconb-1+ino2)
    enddo

!     -- emission d'un eventuel message d'alarme:
    if (loin2) then
        call pjloin(nbnod,nbnodm,m2,zr(iacoo2),nbmax,tino2m,tdmin2,lino_loin)
    endif


!   5. on transforme cortr3 en corres (retour aux vraies mailles)
!   --------------------------------------------------------------
    call pj1dtr(cortr3, corres, nutm, elrf)
    dbg=.false.
    if (dbg) then
        call utimsd(ifm, 2, ASTER_FALSE, ASTER_TRUE, '&&PJ6DCO', 1, ' ')
        call utimsd(ifm, 2, ASTER_FALSE, ASTER_TRUE, corres, 1, ' ')
    endif
    call detrsd('CORRESP_2_MAILLA', cortr3)

    call jedetr(boite//'.BT3DDI')
    call jedetr(boite//'.BT3DVR')
    call jedetr(boite//'.BT3DNB')
    call jedetr(boite//'.BT3DLC')
    call jedetr(boite//'.BT3DCO')

    call jedetr('&&PJXXCO.SEG2')
    call jedetr('&&PJXXCO.LIMA1')
    call jedetr('&&PJXXCO.LIMA2')
    call jedetr('&&PJXXCO.LINO1')
    call jedetr('&&PJXXCO.LINO2')

    AS_DEALLOCATE(vi=lino_loin)

    call jedema()
end subroutine

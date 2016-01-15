subroutine pj2dco(mocle, moa1, moa2, nbma1, lima1,&
                  nbno2, lino2, geom1, geom2, corres,&
                  l_dmax, dmax, dala)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/inslri.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/pj2dap.h"
#include "asterfort/pj2dfb.h"
#include "asterfort/pj2dtr.h"
#include "asterfort/pjxxut.h"
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! but :
!   creer une sd corresp_2_mailla
!   donnant la correspondance entre les noeuds de moa2 et les mailles de
!   moa1 dans le cas ou moa1 est 2d (surface plane en 2d)
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
    integer :: iatr3, iacoo1, iacoo2, nbpt0, ino2_0, idecal_0
    integer :: iabtco, jxxk1, iaconu, iacocf, iacotr
    integer :: ialim1, ialin1, ilcnx1, ialin2
    integer :: iaconb, itypm, idecal, itr3, nbtrou

    aster_logical :: dbg=.false., l_dmax, loin, lraff
    real(kind=8) :: dmax, dmin, dala, cobary(3)

    integer :: nbnod
    integer, pointer :: bt2dlc(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: bt2ddi(:) => null()
    integer, pointer :: typmail(:) => null()
    integer, pointer :: bt2dnb(:) => null()
    real(kind=8), pointer :: bt2dvr(:) => null()

! DEB ------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)

    call pjxxut('2D', mocle, moa1, moa2, nbma1,&
                lima1, nbno2, lino2, m1, m2,&
                nbtmx, nbtm, nutm, elrf)

    call dismoi('NB_NO_MAILLA', m1, 'MAILLAGE', repi=nno1)
    call dismoi('NB_NO_MAILLA', m2, 'MAILLAGE', repi=nno2)
    call dismoi('NB_MA_MAILLA', m1, 'MAILLAGE', repi=nma1)
    call dismoi('NB_MA_MAILLA', m2, 'MAILLAGE', repi=nma2)

    call jeveuo('&&PJXXCO.LIMA1', 'L', ialim1)
    call jeveuo('&&PJXXCO.LINO1', 'L', ialin1)
    call jeveuo('&&PJXXCO.LINO2', 'L', ialin2)




!   2. on decoupe toutes les mailles 2d en tria3
!   ------------------------------------------------
!      (en conservant le lien de parente):
!      on cree l'objet v='&&pjxxco.tria3' : ojb s v i
!         long(v)=1+4*ntr3
!         v(1) : ntr3(=nombre de tria3)
!         v(1+4(i-1)+1) : numero du 1er  noeud du ieme tria3
!         v(1+4(i-1)+2) : numero du 2eme noeud du ieme tria3
!         v(1+4(i-1)+3) : numero du 3eme noeud du ieme tria3
!         v(1+4(i-1)+4) : numero de la maille mere du ieme tria3

    call jeveuo(m1//'.TYPMAIL', 'L', vi=typmail)
    ico=0
    do ima = 1, nma1
        if (zi(ialim1-1+ima) .eq. 0) goto 51
        itypm=typmail(ima)
        if (itypm .eq. nutm(1)) then
            ico=ico+1
        else if (itypm.eq.nutm(2)) then
            ico=ico+1
        else if (itypm.eq.nutm(3)) then
            ico=ico+1
        else if (itypm.eq.nutm(4)) then
            ico=ico+2
        else if (itypm.eq.nutm(5)) then
            ico=ico+2
        else if (itypm.eq.nutm(6)) then
            ico=ico+2
        else
            ASSERT(.false.)
        endif
 51     continue
    end do
    call wkvect('&&PJXXCO.TRIA3', 'V V I', 1+4*ico, iatr3)
    zi(iatr3-1+1)=ico
    if (ico .eq. 0) then
        call utmess('F', 'CALCULEL4_55')
    endif

    call jeveuo(m1//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(m1//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    ico=0
    do ima = 1, nma1
        if (zi(ialim1-1+ima) .eq. 0) goto 52
        itypm=typmail(ima)
!       -- CAS DES TRIANGLES :
        if ((itypm.eq.nutm(1)) .or. (itypm.eq.nutm(2)) .or. ( itypm.eq.nutm(3))) then
            ico=ico+1
            zi(iatr3+(ico-1)*4+4)=ima
            zi(iatr3+(ico-1)*4+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*4+2)=connex(1+ zi(ilcnx1-1+ima)-2+2)
            zi(iatr3+(ico-1)*4+3)=connex(1+ zi(ilcnx1-1+ima)-2+3)
!       -- CAS DES QUADRANGLES :
            else if ((itypm.eq.nutm(4)).or.(itypm.eq.nutm(5)) .or.(&
        itypm.eq.nutm(6))) then
            ico=ico+1
            zi(iatr3+(ico-1)*4+4)=ima
            zi(iatr3+(ico-1)*4+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*4+2)=connex(1+ zi(ilcnx1-1+ima)-2+2)
            zi(iatr3+(ico-1)*4+3)=connex(1+ zi(ilcnx1-1+ima)-2+3)
            ico=ico+1
            zi(iatr3+(ico-1)*4+4)=ima
            zi(iatr3+(ico-1)*4+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*4+2)=connex(1+ zi(ilcnx1-1+ima)-2+3)
            zi(iatr3+(ico-1)*4+3)=connex(1+ zi(ilcnx1-1+ima)-2+4)
        endif
 52     continue
    end do


!   3. on met les tria3 en boites :
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

    boite='&&PJ2DCO.BOITE'
    call pj2dfb(boite, zi(iatr3), zr(iacoo1), zr(iacoo2))
    call jeveuo(boite//'.BT2DDI', 'L', vi=bt2ddi)
    call jeveuo(boite//'.BT2DVR', 'L', vr=bt2dvr)
    call jeveuo(boite//'.BT2DNB', 'L', vi=bt2dnb)
    call jeveuo(boite//'.BT2DLC', 'L', vi=bt2dlc)
    call jeveuo(boite//'.BT2DCO', 'L', iabtco)

!---------------------------------------------------------------------
!     description de la sd boite_2d :
!     boite_2d (k14) ::= record
!      .bt2ddi   : ojb s v i  long=2
!      .bt2dvr   : ojb s v r  long=6
!      .bt2dnb   : ojb s v i  long=nx*ny
!      .bt2dlc   : ojb s v i  long=1+nx*ny
!      .bt2dco   : ojb s v i  long=*

!      .bt2ddi(1) : nx=nombre de boites dans la direction x
!      .bt2ddi(2) : ny=nombre de boites dans la direction y

!      .bt2dvr(1) : xmin     .bt2dvr(2) : xmax
!      .bt2dvr(3) : ymin     .bt2dvr(4) : ymax
!      .bt2dvr(5) : dx = (xmax-xmin)/nbx
!      .bt2dvr(6) : dy = (ymax-ymin)/nby

!      .bt2dnb    : longueurs des boites
!      .bt2dnb(1) : nombre de tria3 contenus dans la boite(1,1)
!      .bt2dnb(2) : nombre de tria3 contenus dans la boite(2,1)
!      .bt2dnb(3) : ...
!      .bt2dnb(nx*ny) : nombre de tria3 contenus dans la boite(nx,ny)

!      .bt2dlc    : longueurs cumulees de .bt2dco
!      .bt2dlc(1) : 0
!      .bt2dlc(2) : bt2dlc(1)+nbtr3(boite(1,1))
!      .bt2dlc(3) : bt2dlc(2)+nbtr3(boite(2,1))
!      .bt2dlc(4) : ...

!      .bt2dco    : contenu des boites
!       soit   nbtr3 =nbtr3(boite(p,q)=bt2dnb((q-1)*nx+p)
!              debtr3=bt2dlc((q-1)*nx+p)
!        do k=1,nbtr3
!          tr3=.bt2dco(debtr3+k)
!        done
!        tr3 est le numero du kieme tria3 de la boite (p,q)
!---------------------------------------------------------------------


!   4. construction d'un corresp_2_mailla temporaire :cortr3
!        (en utilisant les tria3 deduits du maillage m1)
!   ---------------------------------------------------
    cortr3='&&PJ2DCO.CORRESP'
    call wkvect(cortr3//'.PJXX_K1', 'V V K24', 5, jxxk1)
    zk24(jxxk1-1+1)=m1
    zk24(jxxk1-1+2)=m2
    zk24(jxxk1-1+3)='COLLOCATION'
    call wkvect(cortr3//'.PJEF_NB', 'V V I', nno2, iaconb)
    call wkvect(cortr3//'.PJEF_NU', 'V V I', 3*nno2, iaconu)
    call wkvect(cortr3//'.PJEF_CF', 'V V R', 3*nno2, iacocf)
    call wkvect(cortr3//'.PJEF_TR', 'V V I', nno2, iacotr)


!   ON CHERCHE POUR CHAQUE NOEUD INO2 DE M2 LE TRIA3
!   AUQUEL IL APPARTIENT AINSI QUE SES COORDONNEES
!   BARYCENTRIQUES DANS CE TRIA3 :
!   ------------------------------------------------
    idecal=0
    nbnod = 0
    nbpt0=0
    do ino2 = 1, nno2
        if (zi(ialin2-1+ino2) .eq. 0) cycle

!       -- un petit bloc pour resoudre le probleme des points de Gauss
!       fictifs des modeles XFEM (issue23983). Ils sont places en (0,0) :
        if (zr(iacoo2-1+3*(ino2-1)+1).eq.0.d0 &
            .and. zr(iacoo2-1+3*(ino2-1)+2).eq.0.d0) then
            nbpt0=nbpt0+1
            if (nbpt0.eq.1) then
                ino2_0=ino2
                idecal_0=idecal
            else
!               -- on a deja trouve l'element le plus proche de (0,0) :
                zi(iaconb-1+ino2)=3
                itr3=zi(iacotr-1+ino2_0)
                zi(iacotr-1+ino2)=itr3
                if (itr3.eq.0) cycle
                do k = 1, 3
                    zi(iaconu-1+idecal+k)= zi(iaconu-1+idecal_0+k)
                    zr(iacocf-1+idecal+k)= zr(iacocf-1+idecal_0+k)
                end do
                idecal=idecal+zi(iaconb-1+ino2)
                cycle
            endif
        endif

        call pj2dap(ino2, zr(iacoo2), zr(iacoo1), zi(iatr3),&
                    cobary, itr3, nbtrou, bt2ddi, bt2dvr,&
                    bt2dnb, bt2dlc, zi( iabtco),&
                    l_dmax, dmax, dala, loin, dmin)
        if (l_dmax .and. (nbtrou.eq.0)) then
            zi(iaconb-1+ino2)=3
            zi(iacotr-1+ino2)=0
            cycle
        endif
        if (nbtrou .eq. 0) then
            call jenuno(jexnum(m2//'.NOMNOE', ino2), nono2)
            call utmess('F', 'CALCULEL4_56', sk=nono2)
        endif

        zi(iaconb-1+ino2)=3
        zi(iacotr-1+ino2)=itr3
        do k = 1, 3
            zi(iaconu-1+idecal+k)= zi(iatr3+4*(itr3-1)+k)
            zr(iacocf-1+idecal+k)= cobary(k)
        end do
        idecal=idecal+zi(iaconb-1+ino2)
    end do


!  -- emission d'un eventuel message d'alarme:
!  a ce moment de l'algorithme, l'alarme est peut etre injustifiee
!  (voir fiche 16186). on alarmera mieux plus tard (pj2dtr).


!   5. on transforme cortr3 en corres (retour aux vraies mailles)
!   -------------------------------------------------------------
    lraff=.true.
    call pj2dtr(cortr3, corres, nutm, elrf, zr(iacoo1),&
                zr(iacoo2), lraff, dala)
    if (dbg) then
        call utimsd(ifm, 2, .false._1, .true._1, '&&PJ2DCO',&
                    1, ' ')
        call utimsd(ifm, 2, .false._1, .true._1, corres,&
                    1, ' ')
    endif
    call detrsd('CORRESP_2_MAILLA', cortr3)



    call jedetr(boite//'.BT2DDI')
    call jedetr(boite//'.BT2DVR')
    call jedetr(boite//'.BT2DNB')
    call jedetr(boite//'.BT2DLC')
    call jedetr(boite//'.BT2DCO')

    call jedetr('&&PJXXCO.TRIA3')
    call jedetr('&&PJXXCO.LIMA1')
    call jedetr('&&PJXXCO.LIMA2')
    call jedetr('&&PJXXCO.LINO1')
    call jedetr('&&PJXXCO.LINO2')
    call jedema()
end subroutine

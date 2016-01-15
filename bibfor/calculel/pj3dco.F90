subroutine pj3dco(mocle, moa1, moa2, nbma1, lima1,&
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
#include "asterfort/pj3dap.h"
#include "asterfort/pj3dfb.h"
#include "asterfort/pj3dtr.h"
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
! BUT :
!   CREER UNE SD CORRESP_2_MAILLA
!   DONNANT LA CORRESPONDANCE ENTRE LES NOEUDS DE MOA2 ET LES MAILLES DE
!   MOA1 DANS LE CAS OU MOA1 EST 3D (VOLUME EN 3D)
! ======================================================================

!  POUR LES ARGUMENTS : MOCLE, MOA1, MOA2, NBMA1, LIMA1, NBNO2, LINO2
!  VOIR LE CARTOUCHE DE PJXXUT.F

!  IN/JXIN   GEOM1    I   : OBJET JEVEUX CONTENANT LA GEOMETRIE DES
!                           NOEUDS DU MAILLAGE 1 (OU ' ')
!  IN/JXIN   GEOM2    I   : OBJET JEVEUX CONTENANT LA GEOMETRIE DES
!                           NOEUDS DU MAILLAGE 2 (OU ' ')
!                REMARQUE:  LES OBJETS GEOM1 ET GEOM2 NE SONT UTILES
!                           QUE LORSQUE L'ON VEUT TRUANDER LA GEOMETRIE
!                           DES MAILLAGES

!  IN/JXOUT  CORRES  K16 : NOM DE LA SD CORRESP_2_MAILLA
! ----------------------------------------------------------------------



    character(len=8) :: m1, m2, nono2
    character(len=16) :: cortr3
    character(len=14) :: boite
    integer :: nbtm, nbtmx
    parameter   (nbtmx=10)
    integer :: nutm(nbtmx)
    character(len=8) :: elrf(nbtmx)

    integer :: ifm, niv, nno1, nno2, nma1, nma2, k
    integer :: ima, ino2, ico
    integer :: iatr3, iacoo1, iacoo2
    integer :: iabtco, jxxk1, iaconu, iacocf, iacotr
    integer :: ialim1, ialin1, ilcnx1, ialin2, nbpt0, ino2_0, idecal_0
    integer :: iaconb, itypm, idecal, itr3, nbtrou

    aster_logical :: dbg=.false., l_dmax, loin
    real(kind=8) :: dmax, dmin, dala
    real(kind=8) :: cobary(4)

    integer :: nbnod
    integer, pointer :: bt3ddi(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: bt3dlc(:) => null()
    integer, pointer :: typmail(:) => null()
    real(kind=8), pointer :: bt3dvr(:) => null()
    integer, pointer :: bt3dnb(:) => null()

! DEB ------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)

    call pjxxut('3D', mocle, moa1, moa2, nbma1,&
                lima1, nbno2, lino2, m1, m2,&
                nbtmx, nbtm, nutm, elrf)

    call dismoi('NB_NO_MAILLA', m1, 'MAILLAGE', repi=nno1)
    call dismoi('NB_NO_MAILLA', m2, 'MAILLAGE', repi=nno2)
    call dismoi('NB_MA_MAILLA', m1, 'MAILLAGE', repi=nma1)
    call dismoi('NB_MA_MAILLA', m2, 'MAILLAGE', repi=nma2)

    call jeveuo('&&PJXXCO.LIMA1', 'L', ialim1)
    call jeveuo('&&PJXXCO.LINO1', 'L', ialin1)
    call jeveuo('&&PJXXCO.LINO2', 'L', ialin2)



!   2. on decoupe toutes les mailles 3d en tetra4
!   ------------------------------------------------
!      (en conservant le lien de parente):
!      on cree l'objet v='&&pjxxco.tetr4' : ojb s v i
!         long(v)=1+6*ntr3
!         v(1) : ntr3(=nombre de tetr4)
!         v(1+6(i-1)+1) : numero du 1er  noeud du ieme tetr4
!         v(1+6(i-1)+2) : numero du 2eme noeud du ieme tetr4
!         v(1+6(i-1)+3) : numero du 3eme noeud du ieme tetr4
!         v(1+6(i-1)+4) : numero du 4eme noeud du ieme tetr4
!         v(1+6(i-1)+5) : numero de la maille mere du ieme tetr4
!         v(1+6(i-1)+6) : numero du tetraedre dans la maille

    call jeveuo(m1//'.TYPMAIL', 'L', vi=typmail)
    ico=0
    do ima = 1, nma1
        if (zi(ialim1-1+ima) .eq. 0) cycle
        itypm=typmail(ima)
!       -- TETRA :
        if ((itypm.eq.nutm(1)) .or. (itypm.eq.nutm(2))) then
            ico=ico+1
!       -- PENTA :
            else if ((itypm.eq.nutm(3)).or.(itypm.eq.nutm(4)).or. (&
        itypm.eq.nutm(5))) then
            ico=ico+3
!       -- HEXA :
            else if ((itypm.eq.nutm(6)).or.(itypm.eq.nutm(7)).or. (&
        itypm.eq.nutm(8))) then
            ico=ico+6
!       -- PYRA :
        else if ((itypm.eq.nutm(9)).or.(itypm.eq.nutm(10))) then
            ico=ico+2
        else
            ASSERT(.false.)
        endif
    end do

    call wkvect('&&PJXXCO.TETR4', 'V V I', 1+6*ico, iatr3)
    zi(iatr3-1+1)=ico
    if (ico .eq. 0) then
        call utmess('F', 'CALCULEL4_55')
    endif

    call jeveuo(m1//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(m1//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    ico=0
    do ima = 1, nma1
        if (zi(ialim1-1+ima) .eq. 0) cycle
        itypm=typmail(ima)

!       -- TETRA :
        if ((itypm.eq.nutm(1)) .or. (itypm.eq.nutm(2))) then
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=1
            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+2)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+3)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+4)

!       -- PENTA :
            else if ((itypm.eq.nutm(3)).or.(itypm.eq.nutm(4)).or. (&
        itypm.eq.nutm(5))) then
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=1
            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+3)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+6)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+5)
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=2
            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+6)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+4)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+5)
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=3
            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+3)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+5)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+2)

!       -- HEXA :
            else if ((itypm.eq.nutm(6)).or.(itypm.eq.nutm(7)).or. (&
        itypm.eq.nutm(8))) then
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=1
            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+4)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+8)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+6)
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=2
            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+8)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+6)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+5)
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=3
            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+4)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+6)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+2)
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=4
            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+2)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+4)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+8)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+7)
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=5
            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+2)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+8)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+6)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+7)
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=6

            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+2)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+4)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+7)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+3)

!       -- PYRA :
        else if ((itypm.eq.nutm(9)).or.(itypm.eq.nutm(10))) then
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=1
            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+2)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+3)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+5)
            ico=ico+1
            zi(iatr3+(ico-1)*6+5)=ima
            zi(iatr3+(ico-1)*6+6)=2
            zi(iatr3+(ico-1)*6+1)=connex(1+ zi(ilcnx1-1+ima)-2+1)
            zi(iatr3+(ico-1)*6+2)=connex(1+ zi(ilcnx1-1+ima)-2+3)
            zi(iatr3+(ico-1)*6+3)=connex(1+ zi(ilcnx1-1+ima)-2+4)
            zi(iatr3+(ico-1)*6+4)=connex(1+ zi(ilcnx1-1+ima)-2+5)
        endif
    end do


!   3. on met les tetr4 en boites :
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

    boite='&&PJ3DCO.BOITE'
    call pj3dfb(boite, '&&PJXXCO.TETR4', zr(iacoo1), zr(iacoo2))
    call jeveuo(boite//'.BT3DDI', 'L', vi=bt3ddi)
    call jeveuo(boite//'.BT3DVR', 'L', vr=bt3dvr)
    call jeveuo(boite//'.BT3DNB', 'L', vi=bt3dnb)
    call jeveuo(boite//'.BT3DLC', 'L', vi=bt3dlc)
    call jeveuo(boite//'.BT3DCO', 'L', iabtco)

!-------------------------------------------------------------------
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
!      .bt3dnb(1) : nombre de tetr4 contenus dans la boite(1,1,1)
!      .bt3dnb(2) : nombre de tetr4 contenus dans la boite(2,1,1)
!      .bt3dnb(3) : ...
!      .bt3dnb(nx*ny*nz) : nombre de tetr4 contenus dans la
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
!        TR3 EST LE NUMERO DU KIEME TETR4 DE LA BOITE (P,Q,R)
!-------------------------------------------------------------------


!   4. construction d'un corresp_2_mailla temporaire :cortr3
!      (en utilisant les tetr4 deduits du maillage m1)
!   ---------------------------------------------------------
    cortr3='&&PJ3DCO.CORRESP'
    call wkvect(cortr3//'.PJXX_K1', 'V V K24', 5, jxxk1)
    zk24(jxxk1-1+1)=m1
    zk24(jxxk1-1+2)=m2
    zk24(jxxk1-1+3)='COLLOCATION'
    call wkvect(cortr3//'.PJEF_NB', 'V V I', nno2, iaconb)
    call wkvect(cortr3//'.PJEF_NU', 'V V I', 4*nno2, iaconu)
    call wkvect(cortr3//'.PJEF_CF', 'V V R', 4*nno2, iacocf)
    call wkvect(cortr3//'.PJEF_TR', 'V V I', nno2, iacotr)


!   on cherche pour chaque noeud ino2 de m2 le tetr4
!   auquel il appartient ainsi que ses coordonnees
!   barycentriques dans ce tetr4 :
!   ------------------------------------------------
    idecal=0
    nbnod = 0
    nbpt0=0
    do ino2 = 1, nno2
        if (zi(ialin2-1+ino2) .eq. 0) cycle

!       -- un petit bloc pour resoudre le probleme des points de Gauss
!       fictifs des modeles XFEM (issue23983). Ils sont places en (0,0,0) :
        if (zr(iacoo2-1+3*(ino2-1)+1).eq.0.d0 &
            .and. zr(iacoo2-1+3*(ino2-1)+2).eq.0.d0 &
            .and. zr(iacoo2-1+3*(ino2-1)+3).eq.0.d0 ) then
            nbpt0=nbpt0+1
            if (nbpt0.eq.1) then
                ino2_0=ino2
                idecal_0=idecal
            else
!               -- on a deja trouve l'element le plus proche de (0,0,0) :
                zi(iaconb-1+ino2)=4
                itr3=zi(iacotr-1+ino2_0)
                zi(iacotr-1+ino2)=itr3
                if (itr3.eq.0) cycle
                do k = 1, 4
                    zi(iaconu-1+idecal+k)= zi(iaconu-1+idecal_0+k)
                    zr(iacocf-1+idecal+k)= zr(iacocf-1+idecal_0+k)
                end do
                idecal=idecal+zi(iaconb-1+ino2)
                cycle
            endif
        endif

        call pj3dap(ino2, zr(iacoo2), zr(iacoo1), zi(iatr3),&
                    cobary, itr3, nbtrou, bt3ddi, bt3dvr,&
                    bt3dnb, bt3dlc, zi( iabtco),&
                    l_dmax, dmax, dala, loin, dmin)

        if (l_dmax .and. (nbtrou.eq.0)) then
            zi(iaconb-1+ino2)=4
            zi(iacotr-1+ino2)=0
            cycle
        endif
        if (nbtrou .eq. 0) then
            call jenuno(jexnum(m2//'.NOMNOE', ino2), nono2)
            call utmess('F', 'CALCULEL4_56', sk=nono2)
        endif

        zi(iaconb-1+ino2)=4
        zi(iacotr-1+ino2)=itr3
        do k = 1, 4
            zi(iaconu-1+idecal+k)= zi(iatr3+6*(itr3-1)+k)
            zr(iacocf-1+idecal+k)= cobary(k)
        end do
        idecal=idecal+zi(iaconb-1+ino2)
    end do

!   -- emission d'un eventuel message d'alarme:
!   a ce moment de l'algorithme, l'alarme est peut etre injustifiee
!   (voir fiche 16186). on alarmera mieux plus tard (pj3dtr).


!   5. on transforme cortr3 en corres (retour aux vraies mailles)
!   -------------------------------------------------------------
    call pj3dtr(cortr3, corres, nutm, elrf, zr(iacoo1),&
                zr(iacoo2), dala)
    if (dbg) then
        call utimsd(ifm, 2, .false._1, .true._1, '&&PJ3DCO',&
                    1, ' ')
        call utimsd(ifm, 2, .false._1, .true._1, corres,&
                    1, ' ')
    endif
    call detrsd('CORRESP_2_MAILLA', cortr3)



    call jedetr(boite//'.BT3DDI')
    call jedetr(boite//'.BT3DVR')
    call jedetr(boite//'.BT3DNB')
    call jedetr(boite//'.BT3DLC')
    call jedetr(boite//'.BT3DCO')

    call jedetr('&&PJXXCO.TETR4')
    call jedetr('&&PJXXCO.LIMA1')
    call jedetr('&&PJXXCO.LIMA2')
    call jedetr('&&PJXXCO.LINO1')
    call jedetr('&&PJXXCO.LINO2')
    call jedema()
end subroutine

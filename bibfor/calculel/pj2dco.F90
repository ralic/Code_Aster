subroutine pj2dco(mocle, moa1, moa2, nbma1, lima1,&
                  nbno2, lino2, geom1, geom2, corres,&
                  ldmax, distma)
    implicit none
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
! BUT :
!   CREER UNE SD CORRESP_2_MAILLA
!   DONNANT LA CORRESPONDANCE ENTRE LES NOEUDS DE MOA2 ET LES MAILLES DE
!   MOA1 DANS LE CAS OU MOA1 EST 2D (SURFACE PLANE EN 2D)
! ======================================================================
!
!  POUR LES ARGUMENTS : MOCLE, MOA1, MOA2, NBMA1, LIMA1, NBNO2, LINO2
!  VOIR LE CARTOUCHE DE PJXXUT.F
!
!  IN/JXIN   GEOM1    I   : OBJET JEVEUX CONTENANT LA GEOMETRIE DES
!                           NOEUDS DU MAILLAGE 1 (OU ' ')
!  IN/JXIN   GEOM2    I   : OBJET JEVEUX CONTENANT LA GEOMETRIE DES
!                           NOEUDS DU MAILLAGE 2 (OU ' ')
!                REMARQUE:  LES OBJETS GEOM1 ET GEOM2 NE SONT UTILES
!                           QUE LORSQUE L'ON VEUT TRUANDER LA GEOMETRIE
!                           DES MAILLAGES
!
!  IN/JXOUT  CORRES  K16 : NOM DE LA SD CORRESP_2_MAILLA
! ----------------------------------------------------------------------
!
!
!
    character(len=8) :: m1, m2, nono2
    character(len=14) :: boite
    character(len=16) :: cortr3
    integer :: nbtm, nbtmx
    parameter   (nbtmx=15)
    integer :: nutm(nbtmx)
    character(len=8) :: elrf(nbtmx)
!
    integer :: ifm, niv, nno1, nno2, nma1, nma2, k
    integer :: ima, ino2, ico
    integer :: iatr3, iacoo1, iacoo2
    integer :: iabtco, jxxk1, iaconu, iacocf, iacotr
    integer :: ialim1, ialin1,  ilcnx1, ialin2
    integer :: iaconb, itypm, idecal, itr3, nbtrou
!
    logical(kind=1) :: dbg, ldmax, loin, lraff
    real(kind=8) :: distma, dmin, cobary(3)
!
    integer :: nbmax
    parameter (nbmax=5)
    integer :: tino2m(nbmax), nbnod, nbnodm
    real(kind=8) :: tdmin2(nbmax)
    integer, pointer :: bt2dlc(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: bt2ddi(:) => null()
    integer, pointer :: typmail(:) => null()
    integer, pointer :: bt2dnb(:) => null()
    real(kind=8), pointer :: bt2dvr(:) => null()
!
! DEB ------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
    call pjxxut('2D', mocle, moa1, moa2, nbma1,&
                lima1, nbno2, lino2, m1, m2,&
                nbtmx, nbtm, nutm, elrf)
!
    call dismoi('NB_NO_MAILLA', m1, 'MAILLAGE', repi=nno1)
    call dismoi('NB_NO_MAILLA', m2, 'MAILLAGE', repi=nno2)
    call dismoi('NB_MA_MAILLA', m1, 'MAILLAGE', repi=nma1)
    call dismoi('NB_MA_MAILLA', m2, 'MAILLAGE', repi=nma2)
!
    call jeveuo('&&PJXXCO.LIMA1', 'L', ialim1)
    call jeveuo('&&PJXXCO.LINO1', 'L', ialin1)
    call jeveuo('&&PJXXCO.LINO2', 'L', ialin2)
!
!
!
!
!     2. ON DECOUPE TOUTES LES MAILLES 2D EN TRIA3
!     ------------------------------------------------
!        (EN CONSERVANT LE LIEN DE PARENTE):
!        ON CREE L'OBJET V='&&PJXXCO.TRIA3' : OJB S V I
!           LONG(V)=1+4*NTR3
!           V(1) : NTR3(=NOMBRE DE TRIA3)
!           V(1+4(I-1)+1) : NUMERO DU 1ER  NOEUD DU IEME TRIA3
!           V(1+4(I-1)+2) : NUMERO DU 2EME NOEUD DU IEME TRIA3
!           V(1+4(I-1)+3) : NUMERO DU 3EME NOEUD DU IEME TRIA3
!           V(1+4(I-1)+4) : NUMERO DE LA MAILLE MERE DU IEME TRIA3
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
!
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
!
!
!     3. ON MET LES TRIA3 EN BOITES :
!     ---------------------------------------------------
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
!
    boite='&&PJ2DCO.BOITE'
    call pj2dfb(boite, zi(iatr3), zr(iacoo1), zr(iacoo2))
    call jeveuo(boite//'.BT2DDI', 'L', vi=bt2ddi)
    call jeveuo(boite//'.BT2DVR', 'L', vr=bt2dvr)
    call jeveuo(boite//'.BT2DNB', 'L', vi=bt2dnb)
    call jeveuo(boite//'.BT2DLC', 'L', vi=bt2dlc)
    call jeveuo(boite//'.BT2DCO', 'L', iabtco)
!
!     DESCRIPTION DE LA SD BOITE_2D :
!     BOITE_2D (K14) ::= RECORD
!      .BT2DDI   : OJB S V I  LONG=2
!      .BT2DVR   : OJB S V R  LONG=6
!      .BT2DNB   : OJB S V I  LONG=NX*NY
!      .BT2DLC   : OJB S V I  LONG=1+NX*NY
!      .BT2DCO   : OJB S V I  LONG=*
!
!      .BT2DDI(1) : NX=NOMBRE DE BOITES DANS LA DIRECTION X
!      .BT2DDI(2) : NY=NOMBRE DE BOITES DANS LA DIRECTION Y
!
!      .BT2DVR(1) : XMIN     .BT2DVR(2) : XMAX
!      .BT2DVR(3) : YMIN     .BT2DVR(4) : YMAX
!      .BT2DVR(5) : DX = (XMAX-XMIN)/NBX
!      .BT2DVR(6) : DY = (YMAX-YMIN)/NBY
!
!      .BT2DNB    : LONGUEURS DES BOITES
!      .BT2DNB(1) : NOMBRE DE TRIA3 CONTENUS DANS LA BOITE(1,1)
!      .BT2DNB(2) : NOMBRE DE TRIA3 CONTENUS DANS LA BOITE(2,1)
!      .BT2DNB(3) : ...
!      .BT2DNB(NX*NY) : NOMBRE DE TRIA3 CONTENUS DANS LA BOITE(NX,NY)
!
!      .BT2DLC    : LONGUEURS CUMULEES DE .BT2DCO
!      .BT2DLC(1) : 0
!      .BT2DLC(2) : BT2DLC(1)+NBTR3(BOITE(1,1))
!      .BT2DLC(3) : BT2DLC(2)+NBTR3(BOITE(2,1))
!      .BT2DLC(4) : ...
!
!      .BT2DCO    : CONTENU DES BOITES
!       SOIT   NBTR3 =NBTR3(BOITE(P,Q)=BT2DNB((Q-1)*NX+P)
!              DEBTR3=BT2DLC((Q-1)*NX+P)
!        DO K=1,NBTR3
!          TR3=.BT2DCO(DEBTR3+K)
!        DONE
!        TR3 EST LE NUMERO DU KIEME TRIA3 DE LA BOITE (P,Q)
!
!
!     4. CONSTRUCTION D'UN CORRESP_2_MAILLA TEMPORAIRE :CORTR3
!        (EN UTILISANT LES TRIA3 DEDUITS DU MAILLAGE M1)
!     ---------------------------------------------------
    cortr3='&&PJ2DCO.CORRESP'
    call wkvect(cortr3//'.PJXX_K1', 'V V K24', 5, jxxk1)
    zk24(jxxk1-1+1)=m1
    zk24(jxxk1-1+2)=m2
    zk24(jxxk1-1+3)='COLLOCATION'
    call wkvect(cortr3//'.PJEF_NB', 'V V I', nno2, iaconb)
    call wkvect(cortr3//'.PJEF_NU', 'V V I', 3*nno2, iaconu)
    call wkvect(cortr3//'.PJEF_CF', 'V V R', 3*nno2, iacocf)
    call wkvect(cortr3//'.PJEF_TR', 'V V I', nno2, iacotr)
!
!
!     ON CHERCHE POUR CHAQUE NOEUD INO2 DE M2 LE TRIA3
!     AUQUEL IL APPARTIENT AINSI QUE SES COORDONNEES
!     BARYCENTRIQUES DANS CE TRIA3 :
!     ------------------------------------------------
    idecal=0
    nbnod = 0
    nbnodm = 0
    do ino2 = 1, nno2
        if (zi(ialin2-1+ino2) .eq. 0) goto 6
        call pj2dap(ino2, zr(iacoo2), m2, zr(iacoo1), zi(iatr3),&
                    cobary, itr3, nbtrou, bt2ddi, bt2dvr,&
                    bt2dnb, bt2dlc, zi( iabtco), ifm, niv,&
                    ldmax, distma, loin, dmin)
        if (loin) then
            nbnodm = nbnodm + 1
        endif
        call inslri(nbmax, nbnod, tdmin2, tino2m, dmin,&
                    ino2)
        if (ldmax .and. (nbtrou.eq.0)) then
            zi(iaconb-1+ino2)=3
            zi(iacotr-1+ino2)=0
            goto 6
        endif
        if (nbtrou .eq. 0) then
            call jenuno(jexnum(m2//'.NOMNOE', ino2), nono2)
            call utmess('F', 'CALCULEL4_56', sk=nono2)
        endif
!
        zi(iaconb-1+ino2)=3
        zi(iacotr-1+ino2)=itr3
        do k = 1, 3
            zi(iaconu-1+idecal+k)= zi(iatr3+4*(itr3-1)+k)
            zr(iacocf-1+idecal+k)= cobary(k)
        end do
        idecal=idecal+zi(iaconb-1+ino2)
  6     continue
    end do
!
!
!     -- EMISSION D'UN EVENTUEL MESSAGE D'ALARME:
!     A CE MOMENT DE L'ALGORITHME, L'ALARME EST PEUT ETRE INJUSTIFIEE
!     (VOIR FICHE 16186). ON ALARMERA MIEUX PLUS TARD (PJ2DTR).
!
!
!  5. ON TRANSFORME CORTR3 EN CORRES (RETOUR AUX VRAIES MAILLES)
!     ----------------------------------------------------------
    lraff=.true.
    call pj2dtr(cortr3, corres, nutm, elrf, zr(iacoo1),&
                zr(iacoo2), lraff)
    dbg=.false.
    if (dbg) then
        call utimsd(ifm, 2, .false._1, .true._1, '&&PJ2DCO',&
                    1, ' ')
        call utimsd(ifm, 2, .false._1, .true._1, corres,&
                    1, ' ')
    endif
    call detrsd('CORRESP_2_MAILLA', cortr3)
!
!
!
    call jedetr(boite//'.BT2DDI')
    call jedetr(boite//'.BT2DVR')
    call jedetr(boite//'.BT2DNB')
    call jedetr(boite//'.BT2DLC')
    call jedetr(boite//'.BT2DCO')
!
    call jedetr('&&PJXXCO.TRIA3')
    call jedetr('&&PJXXCO.LIMA1')
    call jedetr('&&PJXXCO.LIMA2')
    call jedetr('&&PJXXCO.LINO1')
    call jedetr('&&PJXXCO.LINO2')
    call jedema()
end subroutine

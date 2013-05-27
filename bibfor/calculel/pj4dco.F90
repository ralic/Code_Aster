subroutine pj4dco(mocle, moa1, moa2, nbma1, lima1,&
                  nbno2, lino2, geom1, geom2, corres,&
                  ldmax, distma, alarm2)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infniv.h'
    include 'asterfort/inslri.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/pj2dtr.h'
    include 'asterfort/pj3dfb.h'
    include 'asterfort/pj4dap.h'
    include 'asterfort/pjxxut.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utimsd.h'
    include 'asterfort/wkvect.h'
    character(len=16) :: corres, k16bid, nomcmd
    character(len=*) :: geom1, geom2
    character(len=8) :: moa1, moa2
    character(len=*) :: mocle, alarm2
    integer :: nbma1, lima1(*), nbno2, lino2(*), ino2m
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   MOA1 DANS LE CAS OU MOA1 EST 2.5D (SURFACE EN 3D)
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
!  IN   ALARM2  K3   : /' '/'OUI'/'NON' :
!                       POUR "FORCER" L'EMMISSION (OU NON) DES ALARMES
!
!  IN/JXOUT  CORRES  K16 : NOM DE LA SD CORRESP_2_MAILLA
! ----------------------------------------------------------------------
!
!
!
    character(len=8) :: kb, m1, m2, nono2, alarme
    character(len=14) :: boite
    character(len=16) :: cortr3
    integer :: nbtm, nbtmx
    parameter   (nbtmx=15)
    integer :: nutm(nbtmx)
    character(len=8) :: elrf(nbtmx)
!
    integer :: ifm, niv, ibid, ie, nno1, nno2, nma1, nma2, k
    integer :: ima, ino2, ico
    integer :: iatr3, iacoo1, iacoo2, iabtdi, iabtvr, iabtnb, iabtlc
    integer :: iabtco, jxxk1, iaconu, iacocf, iacotr
    integer :: ialim1, ialin1, iacnx1, ilcnx1, ialin2, iatym1
    integer :: iaconb, itypm, idecal, itr3, nbtrou
!
    logical :: dbg, ldmax, loin, loin2, lraff
    real(kind=8) :: distma, dmin, cobary(3)
!
    integer :: nbmax
    parameter (nbmax=5)
    integer :: tino2m(nbmax), nbnod, nbnodm, ii
    real(kind=8) :: tdmin2(nbmax), umessr(4)
    integer :: iarg
!
! DEB ------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
    call pjxxut('2D', mocle, moa1, moa2, nbma1,&
                lima1, nbno2, lino2, m1, m2,&
                nbtmx, nbtm, nutm, elrf)
!
    call dismoi('F', 'NB_NO_MAILLA', m1, 'MAILLAGE', nno1,&
                kb, ie)
    call dismoi('F', 'NB_NO_MAILLA', m2, 'MAILLAGE', nno2,&
                kb, ie)
    call dismoi('F', 'NB_MA_MAILLA', m1, 'MAILLAGE', nma1,&
                kb, ie)
    call dismoi('F', 'NB_MA_MAILLA', m2, 'MAILLAGE', nma2,&
                kb, ie)
!
    call jeveuo('&&PJXXCO.LIMA1', 'L', ialim1)
    call jeveuo('&&PJXXCO.LINO1', 'L', ialin1)
    call jeveuo('&&PJXXCO.LINO2', 'L', ialin2)
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
    call jeveuo(m1//'.TYPMAIL', 'L', iatym1)
    ico=0
    do 51,ima=1,nma1
    if (zi(ialim1-1+ima) .eq. 0) goto 51
    itypm=zi(iatym1-1+ima)
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
        call assert(.false.)
        if (ico .eq. 0) call u2mess('F', 'CALCULEL4_55')
    endif
    51 end do
    call wkvect('&&PJXXCO.TRIA3', 'V V I', 1+4*ico, iatr3)
    zi(iatr3-1+1)=ico
    call jeveuo(m1//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(m1//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    ico=0
    do 52,ima=1,nma1
    if (zi(ialim1-1+ima) .eq. 0) goto 52
    itypm=zi(iatym1-1+ima)
!       -- CAS DES TRIANGLES :
    if ((itypm.eq.nutm(1)) .or. (itypm.eq.nutm(2)) .or. ( itypm.eq.nutm(3))) then
        ico=ico+1
        zi(iatr3+(ico-1)*4+4)=ima
        zi(iatr3+(ico-1)*4+1)=zi(iacnx1+ zi(ilcnx1-1+ima)-2+1)
        zi(iatr3+(ico-1)*4+2)=zi(iacnx1+ zi(ilcnx1-1+ima)-2+2)
        zi(iatr3+(ico-1)*4+3)=zi(iacnx1+ zi(ilcnx1-1+ima)-2+3)
!       -- CAS DES QUADRANGLES :
        else if ((itypm.eq.nutm(4)).or.(itypm.eq.nutm(5)) .or.(&
        itypm.eq.nutm(6))) then
        ico=ico+1
        zi(iatr3+(ico-1)*4+4)=ima
        zi(iatr3+(ico-1)*4+1)=zi(iacnx1+ zi(ilcnx1-1+ima)-2+1)
        zi(iatr3+(ico-1)*4+2)=zi(iacnx1+ zi(ilcnx1-1+ima)-2+2)
        zi(iatr3+(ico-1)*4+3)=zi(iacnx1+ zi(ilcnx1-1+ima)-2+3)
        ico=ico+1
        zi(iatr3+(ico-1)*4+4)=ima
        zi(iatr3+(ico-1)*4+1)=zi(iacnx1+ zi(ilcnx1-1+ima)-2+1)
        zi(iatr3+(ico-1)*4+2)=zi(iacnx1+ zi(ilcnx1-1+ima)-2+3)
        zi(iatr3+(ico-1)*4+3)=zi(iacnx1+ zi(ilcnx1-1+ima)-2+4)
    endif
    52 end do
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
    boite='&&PJ4DCO.BOITE'
    call pj3dfb(boite, '&&PJXXCO.TRIA3', zr(iacoo1), zr(iacoo2))
    call jeveuo(boite//'.BT3DDI', 'L', iabtdi)
    call jeveuo(boite//'.BT3DVR', 'L', iabtvr)
    call jeveuo(boite//'.BT3DNB', 'L', iabtnb)
    call jeveuo(boite//'.BT3DLC', 'L', iabtlc)
    call jeveuo(boite//'.BT3DCO', 'L', iabtco)
!
!     DESCRIPTION DE LA SD BOITE_3D :
!     BOITE_3D (K14) ::= RECORD
!      .BT3DDI   : OJB S V I  LONG=3
!      .BT3DVR   : OJB S V R  LONG=9
!      .BT3DNB   : OJB S V I  LONG=NX*NY*NZ
!      .BT3DLC   : OJB S V I  LONG=1+NX*NY*NZ
!      .BT3DCO   : OJB S V I  LONG=*
!
!      .BT3DDI(1) : NX=NOMBRE DE BOITES DANS LA DIRECTION X
!      .BT3DDI(2) : NY=NOMBRE DE BOITES DANS LA DIRECTION Y
!      .BT3DDI(3) : NZ=NOMBRE DE BOITES DANS LA DIRECTION Z
!
!      .BT3DVR(1) : XMIN     .BT3DVR(2) : XMAX
!      .BT3DVR(3) : YMIN     .BT3DVR(4) : YMAX
!      .BT3DVR(5) : ZMIN     .BT3DVR(6) : ZMAX
!      .BT3DVR(7) : DX = (XMAX-XMIN)/NBX
!      .BT3DVR(8) : DY = (YMAX-YMIN)/NBY
!      .BT3DVR(9) : DZ = (ZMAX-ZMIN)/NBZ
!
!      .BT3DNB    : LONGUEURS DES BOITES
!      .BT3DNB(1) : NOMBRE DE TRIA3 CONTENUS DANS LA BOITE(1,1,1)
!      .BT3DNB(2) : NOMBRE DE TRIA3 CONTENUS DANS LA BOITE(2,1,1)
!      .BT3DNB(3) : ...
!      .BT3DNB(NX*NY*NZ) : NOMBRE DE TRIA3 CONTENUS DANS LA
!                          DERNIERE BOITE(NX,NY,NZ)
!
!      .BT3DLC    : LONGUEURS CUMULEES DE .BT3DCO
!      .BT3DLC(1) : 0
!      .BT3DLC(2) : BT3DLC(1)+NBTR3(BOITE(1,1))
!      .BT3DLC(3) : BT3DLC(2)+NBTR3(BOITE(2,1))
!      .BT3DLC(4) : ...
!
!      .BT3DCO    : CONTENU DES BOITES
!       SOIT :
!        NBTR3 =NBTR3(BOITE(P,Q,R)=BT3DNB((R-1)*NY*NX+(Q-1)*NX+P)
!        DEBTR3=BT3DLC((R-1)*NY*NX+(Q-1)*NX+P)
!        DO K=1,NBTR3
!          TR3=.BT3DCO(DEBTR3+K)
!        DONE
!        TR3 EST LE NUMERO DU KIEME TRIA3 DE LA BOITE (P,Q,R)
!
!
!     4. CONSTRUCTION D'UN CORRESP_2_MAILLA TEMPORAIRE :CORTR3
!        (EN UTILISANT LES TRIA3 DEDUITS DU MAILLAGE M1)
!     ---------------------------------------------------
    cortr3='&&PJ4DCO.CORRESP'
    call wkvect(cortr3//'.PJXX_K1', 'V V K24', 5, jxxk1)
    zk24(jxxk1-1+1)=m1
    zk24(jxxk1-1+2)=m2
    zk24(jxxk1-1+3)='COLLOCATION'
    call wkvect(cortr3//'.PJEF_NB', 'V V I', nno2, iaconb)
    call wkvect(cortr3//'.PJEF_NU', 'V V I', 4*nno2, iaconu)
    call wkvect(cortr3//'.PJEF_CF', 'V V R', 3*nno2, iacocf)
    call wkvect(cortr3//'.PJEF_TR', 'V V I', nno2, iacotr)
!
!
!     ON CHERCHE POUR CHAQUE NOEUD INO2 DE M2 LE TRIA3
!     AUQUEL IL APPARTIENT AINSI QUE SES COORDONNEES
!     BARYCENTRIQUES DANS CE TRIA3 :
!     ------------------------------------------------
    idecal= 0
    loin2 = .false.
    nbnod = 0
    nbnodm = 0
    do 6,ino2=1,nno2
    if (zi(ialin2-1+ino2) .eq. 0) goto 6
    call pj4dap(ino2, zr(iacoo2), m2, zr(iacoo1), zi(iatr3),&
                cobary, itr3, nbtrou, zi(iabtdi), zr(iabtvr),&
                zi(iabtnb), zi(iabtlc), zi( iabtco), ifm, niv,&
                ldmax, distma, loin, dmin)
    if (loin) then
        loin2=.true.
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
        call u2mesk('F', 'CALCULEL4_56', 1, nono2)
    endif
!
    zi(iaconb-1+ino2)=3
    zi(iacotr-1+ino2)=itr3
    do 61,k=1,3
    zi(iaconu-1+idecal+k)= zi(iatr3+4*(itr3-1)+k)
    zr(iacocf-1+idecal+k)= cobary(k)
61  continue
    idecal=idecal+zi(iaconb-1+ino2)
    6 end do
!
!
!
!     -- EMISSION D'UN EVENTUEL MESSAGE D'ALARME:
    if (loin2) then
        alarme='OUI'
        call getres(k16bid, k16bid, nomcmd)
        if (nomcmd .eq. 'PROJ_CHAMP') then
            call getvtx(' ', 'ALARME', 1, iarg, 1,&
                        alarme, ibid)
        endif
        if (alarm2 .ne. ' ') alarme=alarm2
!
        if (alarme .eq. 'OUI') then
            do 70,ii=1,nbnod
            ino2m = tino2m(ii)
            call jenuno(jexnum(m2//'.NOMNOE', ino2m), nono2)
            umessr(1) = zr(iacoo2+3*(ino2m-1) )
            umessr(2) = zr(iacoo2+3*(ino2m-1)+1)
            umessr(3) = zr(iacoo2+3*(ino2m-1)+2)
            umessr(4) = tdmin2(ii)
            call u2mesg('I', 'CALCULEL5_43', 1, nono2, 0,&
                        0, 4, umessr)
70          continue
            call jenuno(jexnum(m2//'.NOMNOE', tino2m(1)), nono2)
            call u2mesg('A', 'CALCULEL5_48', 1, nono2, 1,&
                        nbnodm, 1, tdmin2(1))
        endif
    endif
!
!
!  5. ON TRANSFORME CORTR3 EN CORRES (RETOUR AUX VRAIES MAILLES)
!     ----------------------------------------------------------
    lraff=.false.
    call pj2dtr(cortr3, corres, nutm, elrf, zr(iacoo1),&
                zr(iacoo2), lraff)
    dbg=.false.
    if (dbg) then
        call utimsd(ifm, 2, .false., .true., '&&PJ4DCO',&
                    1, ' ')
        call utimsd(ifm, 2, .false., .true., corres,&
                    1, ' ')
    endif
    call detrsd('CORRESP_2_MAILLA', cortr3)
!
!
!
    call jedetr(boite//'.BT3DDI')
    call jedetr(boite//'.BT3DVR')
    call jedetr(boite//'.BT3DNB')
    call jedetr(boite//'.BT3DLC')
    call jedetr(boite//'.BT3DCO')
!
    call jedetr('&&PJXXCO.TRIA3')
    call jedetr('&&PJXXCO.LIMA1')
    call jedetr('&&PJXXCO.LIMA2')
    call jedetr('&&PJXXCO.LINO1')
    call jedetr('&&PJXXCO.LINO2')
    call jedema()
end subroutine

subroutine pj3dtr(cortr3, corres, nutm3d, elrf3d, geom1,&
                  geom2)
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
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/elraca.h'
    include 'asterfort/elrfvf.h'
    include 'asterfort/indiis.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/pjeflo.h'
    include 'asterfort/pjefmi.h'
    include 'asterfort/reereg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=16) :: corres, cortr3
    character(len=8) :: elrf3d(10)
    integer :: nutm3d(10)
    real(kind=8) :: geom1(*), geom2(*)
! ----------------------------------------------------------------------
!     BUT :
!       TRANSFORMER CORTR3 EN CORRES EN UTILISANT LES FONC. DE FORME
!       DES MAILLES DU MAILLAGE1 (EN 3D ISOPARAMETRIQUE)
!
!  IN/JXIN   CORTR3    K16 : NOM DU CORRESP_2_MAILLA FAIT AVEC LES TETR4
!  IN/JXOUT  CORRES    K16 : NOM DU CORRESP_2_MAILLA FINAL
!  IN        NUTM3D(10) I  : NUMEROS DES 10 TYPES DE MAILLES 3D
!  IN        ELRF3D(10) K8 : NOMS DES 10 TYPES DE MAILLES 3D
! ----------------------------------------------------------------------
!
    logical :: lext
    integer :: nbnomx, nbfamx
    parameter    ( nbnomx=27, nbfamx=20)
    integer :: cntetr(4, 1), cnpent(4, 3), cnhexa(4, 6), cnpyra(4, 2)
    integer :: nbpg(nbfamx)
    real(kind=8) :: ksi, eta, dzeta, x1, x2, x3, vol
    real(kind=8) :: crrefe(3*nbnomx), xr1(3), xr2(3), xr3(3)
    real(kind=8) :: ff(nbnomx), cooele(3*nbnomx)
    character(len=8) :: elrefa, m1, m2, kb, fapg(nbfamx), alarme
    character(len=16) :: k16bid, nomcmd
    integer :: j1xxk1, i1conb, i1conu, i1cocf, i1cotr, nno1, ie, nno2
    integer :: nma1, nma2, ialim1, ialin1, ialin2, iatr3, iacnx1, ilcnx1, iatyma
    integer :: j2xxk1, i2conb, i2com1, ideca2, ino2, itr, ima1, nbno, i2conu
    integer :: i2cocf, i2coco, ideca1, itypm, nutm, ityp, ndim, nno
    integer :: nnos, nbfpg, kk, ino, nuno, kdim, iret, ibid
    integer :: iarg
! --- DEB --------------------------------------------------------------
!
    call jemarq()
!
!     0. DECOUPAGE DES ELEMENTS 3D EN TETRA (VOIR PJ3DC0) :
!     ----------------------------------------------------
!
!     0.1 : TETRAEDRE :
!     -----------------
    cntetr(1,1)=1
    cntetr(2,1)=2
    cntetr(3,1)=3
    cntetr(4,1)=4
!
!     0.2 : PENTAEDRE :
!     -----------------
    cnpent(1,1)=1
    cnpent(2,1)=3
    cnpent(3,1)=6
    cnpent(4,1)=5
!
    cnpent(1,2)=1
    cnpent(2,2)=6
    cnpent(3,2)=4
    cnpent(4,2)=5
!
    cnpent(1,3)=1
    cnpent(2,3)=3
    cnpent(3,3)=5
    cnpent(4,3)=2
!
!     0.3 : HEXAEDRE :
!     -----------------
    cnhexa(1,1)=1
    cnhexa(2,1)=4
    cnhexa(3,1)=8
    cnhexa(4,1)=6
!
    cnhexa(1,2)=1
    cnhexa(2,2)=8
    cnhexa(3,2)=6
    cnhexa(4,2)=5
!
    cnhexa(1,3)=1
    cnhexa(2,3)=4
    cnhexa(3,3)=6
    cnhexa(4,3)=2
!
    cnhexa(1,4)=2
    cnhexa(2,4)=4
    cnhexa(3,4)=8
    cnhexa(4,4)=7
!
    cnhexa(1,5)=2
    cnhexa(2,5)=8
    cnhexa(3,5)=6
    cnhexa(4,5)=7
!
    cnhexa(1,6)=2
    cnhexa(2,6)=4
    cnhexa(3,6)=7
    cnhexa(4,6)=3
!
!     0.4 : PYRAMIDE :
!     -----------------
    cnpyra(1,1)=1
    cnpyra(2,1)=2
    cnpyra(3,1)=3
    cnpyra(4,1)=5
!
    cnpyra(1,2)=1
    cnpyra(2,2)=3
    cnpyra(3,2)=4
    cnpyra(4,2)=5
!
!     1. RECUPERATION DES INFORMATIONS GENERALES :
!     -----------------------------------------------
    call jeveuo(cortr3//'.PJXX_K1', 'L', j1xxk1)
    call jeveuo(cortr3//'.PJEF_NB', 'L', i1conb)
    call jeveuo(cortr3//'.PJEF_NU', 'L', i1conu)
    call jeveuo(cortr3//'.PJEF_CF', 'L', i1cocf)
    call jeveuo(cortr3//'.PJEF_TR', 'L', i1cotr)
!
    m1=zk24(j1xxk1-1+1)(1:8)
    m2=zk24(j1xxk1-1+2)(1:8)
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
    call jeveuo('&&PJXXCO.TETR4', 'L', iatr3)
!
    call jeveuo(m1//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(m1//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    call jeveuo(m1//'.TYPMAIL', 'L', iatyma)
!
!
!     2. ALLOCATION DE CORRES :
!     -----------------------------------------------
    call wkvect(corres//'.PJXX_K1', 'V V K24', 5, j2xxk1)
    zk24(j2xxk1-1+1)=m1
    zk24(j2xxk1-1+2)=m2
    zk24(j2xxk1-1+3)='COLLOCATION'
!
!     2.1 REMPLISSAGE DE .PJEF_NB ET .PJEF_M1:
!     -----------------------------------------
    call wkvect(corres//'.PJEF_NB', 'V V I', nno2, i2conb)
    call wkvect(corres//'.PJEF_M1', 'V V I', nno2, i2com1)
    ideca2=0
    do 10, ino2=1,nno2
!       ITR : TETR4 ASSOCIE A INO2
    itr=zi(i1cotr-1+ino2)
    if (itr .eq. 0) goto 10
!       IMA1 : MAILLE DE M1 ASSOCIE AU TETR4 ITR
    ima1=zi(iatr3+6*(itr-1)+5)
    nbno=zi(ilcnx1+ima1)-zi(ilcnx1-1+ima1)
    zi(i2conb-1+ino2)=nbno
    zi(i2com1-1+ino2)=ima1
    ideca2=ideca2+nbno
    10 end do
    if (ideca2 .eq. 0) call u2mess('F', 'CALCULEL3_97')
!
!     2.2 ALLOCATION DE .PJEF_NU .PJEF_CF .PJEF_CO:
!         (ET REMPLISSAGE DE CES 3 OBJETS)
!     ------------------------------------------------------
    call wkvect(corres//'.PJEF_NU', 'V V I', ideca2, i2conu)
    call wkvect(corres//'.PJEF_CF', 'V V R', ideca2, i2cocf)
    call wkvect(corres//'.PJEF_CO', 'V V R', 3*nno2, i2coco)
    ideca1=0
    ideca2=0
    do 20, ino2=1,nno2
!       ITR : TETR4 ASSOCIE A INO2
    itr = zi(i1cotr-1+ino2)
    if (itr .eq. 0) goto 20
!       IMA1 : MAILLE DE M1 ASSOCIE AU TETR4 ITR
    ima1 = zi(iatr3+6*(itr-1)+5)
!       ITYPM : TYPE DE LA MAILLE IMA1
    itypm = zi(iatyma-1+ima1)
    nutm = indiis(nutm3d,itypm,1,10)
    elrefa = elrf3d(nutm)
    ityp = zi(iatr3+6*(itr-1)+6)
    nbno = zi(ilcnx1+ima1)-zi(ilcnx1-1+ima1)
!
    call elraca(elrefa, ndim, nno, nnos, nbfpg,&
                fapg, nbpg, crrefe, vol)
!
    call assert(nbno .eq. nno)
!
!       2.2.1 DETERMINATION DES COORDONNEES DE INO2 DANS L'ELEMENT
!             DE REFERENCE : KSI , ETA ET DZETA
!     -----------------------------------------------------------
    ksi =0.d0
    eta =0.d0
    dzeta=0.d0
!
    if (elrefa .eq. 'TE4' .or. elrefa .eq. 'T10') then
        do 771,kk=1,4
        x1 = crrefe(ndim*(cntetr(kk,ityp)-1)+1)
        x2 = crrefe(ndim*(cntetr(kk,ityp)-1)+2)
        x3 = crrefe(ndim*(cntetr(kk,ityp)-1)+3)
        ksi = ksi + zr(i1cocf-1+ideca1+kk)*x1
        eta = eta + zr(i1cocf-1+ideca1+kk)*x2
        dzeta = dzeta + zr(i1cocf-1+ideca1+kk)*x3
771      continue
!
        else if (elrefa.eq.'PE6' .or. elrefa.eq.'P15'.or.&
     &                                elrefa.eq.'P18' ) then
        do 772,kk=1,4
        x1 = crrefe(ndim*(cnpent(kk,ityp)-1)+1)
        x2 = crrefe(ndim*(cnpent(kk,ityp)-1)+2)
        x3 = crrefe(ndim*(cnpent(kk,ityp)-1)+3)
        ksi = ksi + zr(i1cocf-1+ideca1+kk)*x1
        eta = eta + zr(i1cocf-1+ideca1+kk)*x2
        dzeta = dzeta + zr(i1cocf-1+ideca1+kk)*x3
772      continue
!
        else if (elrefa.eq.'HE8' .or. elrefa.eq.'H20' .or.&
     &                                elrefa.eq.'H27' ) then
        do 773,kk=1,4
        x1 = crrefe(ndim*(cnhexa(kk,ityp)-1)+1)
        x2 = crrefe(ndim*(cnhexa(kk,ityp)-1)+2)
        x3 = crrefe(ndim*(cnhexa(kk,ityp)-1)+3)
        ksi = ksi + zr(i1cocf-1+ideca1+kk)*x1
        eta = eta + zr(i1cocf-1+ideca1+kk)*x2
        dzeta = dzeta + zr(i1cocf-1+ideca1+kk)*x3
773      continue
!
    else if (elrefa.eq.'PY5' .or. elrefa.eq.'P13') then
        do 774,kk=1,4
        x1 = crrefe(ndim*(cnpyra(kk,ityp)-1)+1)
        x2 = crrefe(ndim*(cnpyra(kk,ityp)-1)+2)
        x3 = crrefe(ndim*(cnpyra(kk,ityp)-1)+3)
        ksi = ksi + zr(i1cocf-1+ideca1+kk)*x1
        eta = eta + zr(i1cocf-1+ideca1+kk)*x2
        dzeta = dzeta + zr(i1cocf-1+ideca1+kk)*x3
774      continue
!
    else
        call u2mesk('F', 'ELEMENTS_55', 1, elrefa)
    endif
!
    xr1(1) = ksi
    xr1(2) = eta
    xr1(3) = dzeta
!
!       -- ON ESSAYE D'AMELIORER LA PRECISION DE XR1(*)
!          EN UTILISANT REEREG :
    do 72,ino=1,nbno
    nuno = zi(iacnx1+ zi(ilcnx1-1+ima1)-2+ino)
    do 73,kdim=1,ndim
    cooele(ndim*(ino-1)+kdim)=geom1(3*(nuno-1)+kdim)
73  continue
72  continue
    call reereg('C', elrefa, nno, cooele, geom2(3*(ino2-1)+1),&
                ndim, xr2, iret)
!
!       -- ON REGARDE SI INO2 EST EXTERIEUR A IMA1 :
    alarme='OUI'
    call getres(k16bid, k16bid, nomcmd)
    if (nomcmd .eq. 'PROJ_CHAMP') then
        call getvtx(' ', 'ALARME', 1, iarg, 1,&
                    alarme, ibid)
    endif
    call pjeflo(elrefa, ndim, iret, xr2, alarme,&
                m2, ino2, m1, ima1, lext)
!
!       -- ON CHOISIT LA MEILLEURE APPROXIMATION ENTRE XR1 ET XR2:
    call pjefmi(elrefa, nno, cooele, geom2(3*(ino2-1)+1), ndim,&
                xr1, xr2, lext, xr3)
!
    zr(i2coco-1+3*(ino2-1)+1)=xr3(1)
    zr(i2coco-1+3*(ino2-1)+2)=xr3(2)
    zr(i2coco-1+3*(ino2-1)+3)=xr3(3)
!
!
!       2.2.2 :
!       CALCUL DES F. DE FORME AUX NOEUDS POUR LE POINT KSI,ETA,DZETA:
!       --------------------------------------------------------------
    call elrfvf(elrefa, xr3, 27, ff, nno)
!
    do 22,ino=1,nbno
    nuno = zi(iacnx1+ zi(ilcnx1-1+ima1)-2+ino)
    zi(i2conu-1+ideca2+ino) = nuno
    zr(i2cocf-1+ideca2+ino) = ff(ino)
22  continue
!
    ideca1=ideca1+4
    ideca2=ideca2+nbno
!
    20 end do
!
    call jedema()
end subroutine

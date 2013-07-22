subroutine chrpel(champ1, repere, nbcmp, icham, type,&
                  nomch, modele, carele, champ0)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1501
    implicit      none
#include "jeveux.h"
!
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8dgrd.h"
#include "asterfort/angvxy.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/cesvar.h"
#include "asterfort/chrpan.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/manopg.h"
#include "asterfort/matrot.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/ut2vgl.h"
#include "asterfort/utpsgl.h"
#include "asterfort/utpvgl.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
    integer :: nbcmp, icham
    character(len=*) :: champ1, champ0, repere, type, nomch, modele, carele
! ----------------------------------------------------------------------
!
!     BUT : CHANGEMENT DE REPERE DANS LE CAS D'UN CHAM_ELEM
! ----------------------------------------------------------------------
!     ARGUMENTS :
!     CHAMP1   IN  K16  : NOM DU CHAMP A TRAITER (CHAMP OUT)
!     REPERE   IN  K16  : TYPE DE REPERE (UTILISATEUR OU CYLINDRIQUE)
!     NBCMP    IN  I    : NOMBRE DE COMPOSANTES A TRAITER
!     ICHAM    IN  I    : NUMERO D'OCCURRENCE
!     TYPE     IN  K16  : TYPE DU CHAMP :'TENS' 'VECT' OU 'COQUE'
!     NOMCH    IN  K16  : NOM DE CHAMP
!     CHAMP0   IN  K16  : NOM DU CHAMP IN
! ----------------------------------------------------------------------
! ---------------------------------------------------------------------
!
    integer :: i, ii, ino, iad, ipt, isp
    integer :: jcesd, jcesv, jcesl, nbpt, axyzm, ncmp
    integer :: jconx1, jconx2, nbsp, inel, jcmp, ipt2
    integer :: ibid, nbma, jcesk, iret, inot, inbno
    integer :: ndim, licmpu(6), nbm, idmail, nbmail, imai
    integer :: inoeu, iret0, iret1, nbgno, igno, nncp, i2
    integer :: ierk, mnogak, mnogad, mnogal, mnogav, iadr, igaaxe
    integer :: imaref, nbno, nbpg, nbno2, nbpg2, nuno, ipg
    logical :: test, exicar
    real(kind=8) :: angnot(3), pgl(3, 3), valer(6), valed(6)
    real(kind=8) :: valr, valei(6), xx, yy, zz
    real(kind=8) :: valet(6), epsi, xnormr, prosca
    real(kind=8) :: orig(3), axez(3), axer(3), axet(3)
    real(kind=8) :: vectx(3), vecty(3), angrep(3)
    real(kind=8) :: x(27), y(27), z(27)
    real(kind=8) :: xpg(27), ypg(27), zpg(27)
    complex(kind=8) :: valetc(6), cbid
    character(len=1) :: k1b
    character(len=3) :: tsca
    character(len=8) :: ma, k8b, typmcl(2), nomgd, tych, param
    character(len=8) :: lpain(4), paout, licmp(3), kbid
    character(len=16) :: option, motcle(2), nomch2
    character(len=19) :: chams1, chams0, ligrel, manoga, canbsp
    character(len=19) :: changl, carte
    character(len=24) :: mesmai, chgeom, lchin(4), chaout
    character(len=24) :: valk(3), chcara(18)
    integer :: iarg
!
    call jemarq()
    epsi = 1.0d-6
    igaaxe = 0
    motcle(1) = 'GROUP_MA'
    typmcl(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(2) = 'MAILLE'
    canbsp = '&&CHRPEL.NBSP'
!
    mesmai = '&&CHRPEL.MES_MAILLES'
!
    if (nbcmp .gt. 0) then
        call wkvect('&&CHRPEL.NOM_CMP', 'V V K8', nbcmp, jcmp)
        call getvtx('MODI_CHAM', 'NOM_CMP', icham, iarg, nbcmp,&
                    zk8(jcmp), ibid)
    else
        call u2mess('F', 'ALGORITH2_6')
    endif
!
    call dismoi('F', 'NOM_LIGREL', champ1, 'CHAM_ELEM', ibid,&
                ligrel, ibid)
!
!
! ----- DEFINITION ET CREATION DU CHAM_ELEM SIMPLE CHAMS1
! ----- A PARTIR DU CHAM_ELEM CHAMP1
!
    chams0='&&CHRPEL.CHAMS0'
    chams1='&&CHRPEL.CHAMS1'
    call celces(champ1, 'V', chams0)
    call cesred(chams0, 0, 0, nbcmp, zk8(jcmp),&
                'V', chams1)
    call detrsd('CHAM_ELEM_S', chams0)
    call jeveuo(chams1//'.CESK', 'L', jcesk)
    call jeveuo(chams1//'.CESD', 'L', jcesd)
    ma = zk8(jcesk-1+1)
    nomgd = zk8(jcesk-1+2)
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
!
!     ON EXCLUT LES MOT-CLES 'NOEUD' ET 'GROUP_NO'
    call jeveuo(ma//'.DIME   ', 'L', inbno)
    call wkvect('&&CHRPEL.NOEUDS', 'V V K8', zi(inbno), inoeu)
    call getvtx('AFFE', 'NOEUD', icham, iarg, 0,&
                zk8(inoeu), iret0)
    call jeexin(ma//'.GROUPENO', ierk)
    if (ierk .ne. 0) then
        call jelira(ma//'.GROUPENO', 'NMAXOC', nbgno, k1b)
        call wkvect('&&CHRPEL.GROUP_NO', 'V V K24', nbgno, igno)
        call getvtx('AFFE', 'GROUP_NO', icham, iarg, 0,&
                    zk24(igno), iret1)
    else
        iret1=0
    endif
    if (iret0 .lt. 0) then
        k8b='NOEUD   '
    else if (iret1.lt.0) then
        k8b='GROUP_NO'
    else
        goto 100
    endif
    valk (1) = k8b
    valk (2) = nomch
    valk (3) = ' '
    call u2mesg('F', 'ALGORITH12_42', 3, valk, 0,&
                0, 0, 0.d0)
100  continue
    call jedetr('&&CHRPEL.NOEUDS')
    call jedetr('&&CHRPEL.GROUP_NO')
!
    nbma = zi(jcesd-1+1)
    ncmp = zi(jcesd-1+2)
    call dismoi('F', 'Z_CST', ma, 'MAILLAGE', ndim,&
                k8b, iret)
    ndim = 3
    if (k8b .eq. 'OUI') ndim = 2
!
    call reliem(' ', ma, 'NU_MAILLE', 'AFFE', icham,&
                2, motcle, typmcl, mesmai, nbm)
    if (nbm .gt. 0) then
        nbmail = nbm
        call jeveuo(mesmai, 'L', idmail)
    else
        nbmail = nbma
    endif
!
    call jeexin(ma//'.CONNEX', iret)
    call assert(iret.ne.0)
    call jeveuo(ma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    call jeveuo(chams1//'.CESV', 'E', jcesv)
    call jeveuo(chams1//'.CESL', 'L', jcesl)
!
    do 1 i = 1, 6
        valed(i) = 0.0d0
        valer(i) = 0.0d0
        valet(i) = 0.0d0
 1  end do
    do 2 i = 1, 3
        axer(i) = 0.0d0
        axet(i) = 0.0d0
        axez(i) = 0.0d0
        orig(i) = 0.0d0
        angnot(i) = 0.0d0
 2  end do
    licmpu(1) = 1
    licmpu(2) = 2
    licmpu(3) = 3
    licmpu(4) = 4
    licmpu(5) = 5
    licmpu(6) = 6
!
! ----- CHANGEMENT DE REPERE SUIVANT LE CHOIX UTILISATEUR
!
    if (repere(1:11) .eq. 'UTILISATEUR') then
!        SI LE NOUVEAU REPERE EST DONNE VIA DES VECTEURS
        call getvr8('AFFE', 'VECT_X', 1, iarg, 3,&
                    vectx, ibid)
        if (ibid .ne. 0) then
            call getvr8('AFFE', 'VECT_Y', 1, iarg, 3,&
                        vecty, ibid)
            if (ndim .ne. 3) then
                call u2mess('F', 'ALGORITH2_4')
            endif
            call angvxy(vectx, vecty, angnot)
        else
            if (ndim .eq. 3) then
                call getvr8('AFFE', 'ANGL_NAUT', 1, iarg, 3,&
                            angnot, ibid)
                if (ibid .ne. 3) then
                    call u2mess('F', 'ALGORITH2_7')
                endif
            else
                call getvr8('AFFE', 'ANGL_NAUT', 1, iarg, 1,&
                            angnot(1), ibid)
                if (ibid .ne. 1) then
                    valr = angnot(1)
                    call u2mesg('A', 'ALGORITH12_43', 0, ' ', 0,&
                                0, 1, valr)
                endif
            endif
            angnot(1) = angnot(1)*r8dgrd()
            angnot(2) = angnot(2)*r8dgrd()
            angnot(3) = angnot(3)*r8dgrd()
        endif
        call matrot(angnot, pgl)
        if (type(1:4) .eq. 'TENS') then
! TENSEUR
            do 10 inel = 1, nbmail
                if (nbm .ne. 0) then
                    imai = zi(idmail+inel-1)
                else
                    imai = inel
                endif
                nbpt = zi(jcesd-1+5+4* (imai-1)+1)
                nbsp = zi(jcesd-1+5+4* (imai-1)+2)
                if (tsca .eq. 'R') then
! CHAMP REEL
                    do 11,ipt = 1,nbpt
                    do 12,isp = 1,nbsp
                    do 13 ii = 1, ncmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            valet(ii)=zr(jcesv-1+iad)
                        else
                            goto 10
                        endif
13                  continue
                    valed(1) = valet(1)
                    valed(2) = valet(4)
                    valed(3) = valet(2)
                    valed(4) = valet(5)
                    valed(5) = valet(6)
                    valed(6) = valet(3)
                    call utpsgl(1, 3, pgl, valed, valet)
                    valer(1) = valet(1)
                    valer(2) = valet(3)
                    valer(3) = valet(6)
                    valer(4) = valet(2)
                    valer(5) = valet(4)
                    valer(6) = valet(5)
                    do 14 ii = 1, nbcmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            zr(jcesv-1+iad) = valer(ii)
                        else
                            goto 10
                        endif
14                  continue
12                  continue
11                  continue
                else
! CHAMP COMPLEXE
                    do 111,ipt = 1,nbpt
                    do 112,isp = 1,nbsp
                    do 113 ii = 1, ncmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            valetc(ii)=zc(jcesv-1+iad)
                        else
                            goto 10
                        endif
113                  continue
                    valed(1) = dble(valetc(1))
                    valed(2) = dble(valetc(4))
                    valed(3) = dble(valetc(2))
                    valed(4) = dble(valetc(5))
                    valed(5) = dble(valetc(6))
                    valed(6) = dble(valetc(3))
                    call utpsgl(1, 3, pgl, valed, valet)
                    valer(1) = valet(1)
                    valer(2) = valet(3)
                    valer(3) = valet(6)
                    valer(4) = valet(2)
                    valer(5) = valet(4)
                    valer(6) = valet(5)
!
                    valed(1) = dimag(valetc(1))
                    valed(2) = dimag(valetc(4))
                    valed(3) = dimag(valetc(2))
                    valed(4) = dimag(valetc(5))
                    valed(5) = dimag(valetc(6))
                    valed(6) = dimag(valetc(3))
                    call utpsgl(1, 3, pgl, valed, valet)
                    valei(1) = valet(1)
                    valei(2) = valet(3)
                    valei(3) = valet(6)
                    valei(4) = valet(2)
                    valei(5) = valet(4)
                    valei(6) = valet(5)
!
                    do 114 ii = 1, nbcmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            zc(jcesv-1+iad) = dcmplx(valer(ii) ,valei(ii))
                        else
                            goto 10
                        endif
114                  continue
112                  continue
111                  continue
                endif
10          continue
        else
!  VECTEUR
            do 15 inel = 1, nbmail
                if (nbm .ne. 0) then
                    imai = zi(idmail+inel-1)
                else
                    imai = inel
                endif
                nbpt = zi(jcesd-1+5+4* (imai-1)+1)
                nbsp = zi(jcesd-1+5+4* (imai-1)+2)
                if (tsca .eq. 'R') then
! CHAMP REEL
                    do 16,ipt = 1,nbpt
                    do 17,isp = 1,nbsp
                    do 18 ii = 1, ncmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            valed(ii) = zr(jcesv-1+iad)
                        else
                            goto 15
                        endif
18                  continue
                    if (ndim .eq. 3) then
                        call utpvgl(1, ncmp, pgl, valed, valer)
                    else
                        call ut2vgl(1, ncmp, pgl, valed, valer)
                    endif
                    do 19 ii = 1, nbcmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            zr(jcesv-1+iad) = valer(ii)
                        else
                            goto 15
                        endif
19                  continue
17                  continue
16                  continue
                else
! CHAMP COMPLEXE
                    do 116,ipt = 1,nbpt
                    do 117,isp = 1,nbsp
                    do 118 ii = 1, ncmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            valetc(ii) = zc(jcesv-1+iad)
                            valed(ii) = dble(valetc(ii))
                            valet(ii) = dimag(valetc(ii))
                        else
                            goto 15
                        endif
118                  continue
                    if (ndim .eq. 3) then
                        call utpvgl(1, ncmp, pgl, valed, valer)
                        call utpvgl(1, ncmp, pgl, valet, valei)
                    else
                        call ut2vgl(1, ncmp, pgl, valed, valer)
                        call ut2vgl(1, ncmp, pgl, valet, valei)
                    endif
                    do 119 ii = 1, nbcmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            zc(jcesv-1+iad) = dcmplx(valer(ii) ,valei(ii))
                        else
                            goto 15
                        endif
119                  continue
117                  continue
116                  continue
                endif
15          continue
        endif
        call dismoi('F', 'NOM_OPTION', champ1, 'CHAM_ELEM', ibid,&
                    option, ibid)
        call cescel(chams1, ligrel, option, ' ', 'OUI',&
                    nncp, 'G', champ1, 'F', ibid)
        call detrsd('CHAM_ELEM_S', chams1)
    else if (repere(1:11).eq.'CYLINDRIQUE') then
! REPERE CYLINDRIQUE
        call dismoi('C', 'TYPE_CHAMP', champ1, 'CHAMP', ibid,&
                    tych, iret)
        if (ndim .eq. 3) then
            call getvr8('AFFE', 'ORIGINE', 1, iarg, 3,&
                        orig, ibid)
            if (ibid .ne. 3) then
                call u2mess('F', 'ALGORITH2_8')
            endif
            call getvr8('AFFE', 'AXE_Z', 1, iarg, 3,&
                        axez, ibid)
            if (ibid .eq. 0) then
                call u2mess('F', 'ALGORITH2_9')
            endif
        else
            call getvr8('AFFE', 'ORIGINE', 1, iarg, 2,&
                        orig, ibid)
            if (ibid .ne. 2) then
                call u2mess('A', 'ALGORITH2_10')
            endif
            call getvr8('AFFE', 'AXE_Z', 1, iarg, 0,&
                        axez, ibid)
            if (ibid .ne. 0) then
                call u2mess('A', 'ALGORITH2_11')
            endif
            axez(1) = 0.0d0
            axez(2) = 0.0d0
            axez(3) = 1.0d0
        endif
        xnormr = 0.0d0
        call normev(axez, xnormr)
        call jeveuo(ma//'.COORDO    .VALE', 'L', axyzm)
!
        manoga='&&CHRPEL.MANOGA'
!
        if (nomch(1:4) .eq. 'SIEF' .or. nomch(1:4) .eq. 'SIGM') then
            param='PCONTRR'
        else if (nomch(1:2).eq.'EP') then
            param='PDEFOPG'
        else if (nomch(1:4).eq.'VARI') then
            param='PVARIGR'
        else
            call u2mesg('F', 'ALGORITH2_14', 1, nomch, 0,&
                        0, 0, 0.d0)
        endif
!
        nomch2 = nomch
        call manopg(ligrel, nomch2, param, manoga)
! ----- TYPE DE COMPOSANTES
!
        if (type(1:4) .eq. 'TENS') then
            if (ndim .eq. 2) then
                licmpu(1)=1
                licmpu(2)=2
                licmpu(3)=3
                licmpu(4)=5
            endif
!
!  TRAITEMENT DIFFERENT SI CHAMP ELNO OU ELGA
!
            if (tych(1:4) .eq. 'ELNO') then
!
                do 20 inel = 1, nbmail
                    if (nbm .ne. 0) then
                        imai = zi(idmail+inel-1)
                    else
                        imai = inel
                    endif
                    nbpt = zi(jcesd-1+5+4* (imai-1)+1)
                    nbsp = zi(jcesd-1+5+4* (imai-1)+2)
                    ncmp = zi(jcesd-1+5+4* (imai-1)+3)
                    do 21,ipt = 1,nbpt
                    do 22,isp = 1,nbsp
                    test = .true.
                    do 23 ii = 1, ncmp
                        call cesexi('S', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            test = .false.
                        endif
23                  continue
                    if (test) goto 20
                    ino = zi(jconx1-1+zi(jconx2+imai-1)+ipt-1)
                    axer(1) = zr(axyzm+3*(ino-1) ) - orig(1)
                    axer(2) = zr(axyzm+3*(ino-1)+1) - orig(2)
                    if (ndim .eq. 3) then
                        axer(3) = zr(axyzm+3*(ino-1)+2) - orig(3)
                    else
                        axer(3) = 0.0d0
                    endif
                    prosca=ddot(3,axer,1,axez,1)
                    axer(1) = axer(1) - prosca*axez(1)
                    axer(2) = axer(2) - prosca*axez(2)
                    if (ndim .eq. 3) then
                        axer(3) = axer(3) - prosca*axez(3)
                    else
                        axer(3) = 0.0d0
                    endif
                    xnormr = 0.0d0
                    call normev(axer, xnormr)
                    if (xnormr .lt. epsi) then
                        call jenuno(jexnum(ma//'.NOMNOE', ino), k8b)
                        call u2mess('A', 'ALGORITH2_12')
                        axer(1) = 0.0d0
                        axer(2) = 0.0d0
                        axer(3) = 0.0d0
                        do 24 ipt2 = 1, nbpt
                            inot = zi(jconx1-1+zi(jconx2+imai- 1)+ipt2-1)
                            axer(1) = axer(1) + zr(axyzm+3*( inot-1) )
                            axer(2) = axer(2) + zr(axyzm+3*( inot-1)+1)
                            if (ndim .eq. 3) then
                                axer(3) = axer(3) + zr(axyzm+ 3*(inot-1)+2)
                            endif
24                      continue
                        axer(1) = axer(1)/nbpt - orig(1)
                        axer(2) = axer(2)/nbpt - orig(2)
                        axer(3) = axer(3)/nbpt - orig(3)
                        prosca=ddot(3,axer,1,axez,1)
                        axer(1) = axer(1) - prosca*axez(1)
                        axer(2) = axer(2) - prosca*axez(2)
                        if (ndim .eq. 3) then
                            axer(3) = axer(3) - prosca*axez(3)
                        else
                            axer(3) = 0.0d0
                        endif
                        xnormr = 0.0d0
                        call normev(axer, xnormr)
                        if (xnormr .lt. epsi) then
                            call jenuno(jexnum(ma//'.NOMNOE', ino), k8b)
                            valk (1) = k8b
                            call u2mesg('F', 'ALGORITH12_44', 1, valk, 0,&
                                        0, 0, 0.d0)
                        endif
                    endif
                    call provec(axez, axer, axet)
                    xnormr = 0.0d0
                    call normev(axet, xnormr)
                    do 26 i = 1, 3
                        pgl(1,i) = axer(i)
                        pgl(2,i) = axez(i)
                        pgl(3,i) = axet(i)
26                  continue
                    if (tsca .eq. 'R') then
! CHAMP REEL
                        do 27 ii = 1, ncmp
                            call cesexi('S', jcesd, jcesl, imai, ipt,&
                                        isp, ii, iad)
                            if (iad .gt. 0) then
                                valet(ii)=zr(jcesv-1+iad)
                            else
                                goto 20
                            endif
27                      continue
                        valed(1) = valet(1)
                        valed(2) = valet(4)
                        valed(3) = valet(2)
                        valed(4) = valet(5)
                        valed(5) = valet(6)
                        valed(6) = valet(3)
                        call utpsgl(1, 3, pgl, valed, valet)
                        valer(1) = valet(1)
                        valer(2) = valet(3)
                        valer(3) = valet(6)
                        valer(4) = valet(2)
                        valer(5) = valet(4)
                        valer(6) = valet(5)
                        do 28 ii = 1, nbcmp
                            call cesexi('C', jcesd, jcesl, imai, ipt,&
                                        isp, ii, iad)
                            if (iad .gt. 0) then
                                zr(jcesv-1+iad) = valer( licmpu(ii))
                            else
                                goto 20
                            endif
28                      continue
                    else
! CHAMP COMPLEXE
                        do 213 ii = 1, ncmp
                            call cesexi('C', jcesd, jcesl, imai, ipt,&
                                        isp, ii, iad)
                            if (iad .gt. 0) then
                                valetc(ii)=zc(jcesv-1+iad)
                            else
                                goto 20
                            endif
213                      continue
                        valed(1) = dble(valetc(1))
                        valed(2) = dble(valetc(4))
                        valed(3) = dble(valetc(2))
                        valed(4) = dble(valetc(5))
                        valed(5) = dble(valetc(6))
                        valed(6) = dble(valetc(3))
                        call utpsgl(1, 3, pgl, valed, valet)
                        valer(1) = valet(1)
                        valer(2) = valet(3)
                        valer(3) = valet(6)
                        valer(4) = valet(2)
                        valer(5) = valet(4)
                        valer(6) = valet(5)
!
                        valed(1) = dimag(valetc(1))
                        valed(2) = dimag(valetc(4))
                        valed(3) = dimag(valetc(2))
                        valed(4) = dimag(valetc(5))
                        valed(5) = dimag(valetc(6))
                        valed(6) = dimag(valetc(3))
                        call utpsgl(1, 3, pgl, valed, valet)
                        valei(1) = valet(1)
                        valei(2) = valet(3)
                        valei(3) = valet(6)
                        valei(4) = valet(2)
                        valei(5) = valet(4)
                        valei(6) = valet(5)
!
                        do 214 ii = 1, nbcmp
                            call cesexi('C', jcesd, jcesl, imai, ipt,&
                                        isp, ii, iad)
                            if (iad .gt. 0) then
                                zc(jcesv-1+iad) = dcmplx( valer(ii),valei(ii))
                            else
                                goto 20
                            endif
214                      continue
                    endif
22                  continue
21                  continue
20              continue
!
            else if (tych(1:4).eq.'ELGA') then
!        ----------------------------
                call jeveuo(manoga//'.CESK', 'L', mnogak)
                call jeveuo(manoga//'.CESD', 'L', mnogad)
                call jeveuo(manoga//'.CESL', 'L', mnogal)
                call jeveuo(manoga//'.CESV', 'L', mnogav)
                call assert(zk8(mnogak).eq.ma)
!
                do 120 inel = 1, nbmail
                    if (nbm .ne. 0) then
                        imai = zi(idmail+inel-1)
                    else
                        imai = inel
                    endif
!
!  RECUP DE L'ADRESSE DE LA MATRICE DE PASSAGE NOEUDS ==> GAUSS
!
                    call cesexi('C', mnogad, mnogal, imai, 1,&
                                1, 1, iad)
                    if (iad .le. 0) goto 120
                    if (nint(zr(mnogav-1+iad)) .gt. 0) then
                        imaref=imai
                    else
                        imaref=-nint(zr(mnogav-1+iad))
                    endif
                    call cesexi('C', mnogad, mnogal, imaref, 1,&
                                1, 1, iad)
                    if (iad .le. 0) goto 120
!
                    nbno2 = nint(zr(mnogav-1+iad))
                    nbpg2 = nint(zr(mnogav-1+iad+1))
!
                    nbno = zi(jconx2+imai) - zi(jconx2-1+imai)
                    nbpg = zi(jcesd-1+5+4* (imai-1)+1)
                    nbsp = zi(jcesd-1+5+4* (imai-1)+2)
                    ncmp = zi(jcesd-1+5+4* (imai-1)+3)
                    call assert(nbno.eq.nbno2)
                    call assert(nbpg.eq.nbpg2)
!
!  RECUP DES COORDONNEES DES NOEUDS
!
                    do 130 ino = 1, nbno
                        nuno = zi(jconx1-1+zi(jconx2+imai-1)+ino-1)
                        x(ino) = zr(axyzm+3*(nuno-1) )
                        y(ino) = zr(axyzm+3*(nuno-1)+1)
                        z(ino) = zr(axyzm+3*(nuno-1)+2)
130                  continue
!
!  CALCUL DES COORDONNEES DES POINTS DE GAUSS DE LA MAILLE COURANTE
!
                    do 140 ipg = 1, nbpg
                        xx=0.d0
                        yy=0.d0
                        zz=0.d0
                        iadr=mnogav-1+iad+1+nbno*(ipg-1)
                        do 141 ino = 1, nbno
                            xx = xx + x(ino)*zr(iadr+ino)
                            yy = yy + y(ino)*zr(iadr+ino)
                            zz = zz + z(ino)*zr(iadr+ino)
141                      continue
                        xpg(ipg) = xx
                        ypg(ipg) = yy
                        zpg(ipg) = zz
140                  continue
!
                    do 121,ipg = 1,nbpg
                    do 122,isp = 1,nbsp
                    test = .true.
                    do 123 ii = 1, ncmp
                        call cesexi('S', jcesd, jcesl, imai, ipg,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            test = .false.
                        endif
123                  continue
                    if (test) goto 120
                    axer(1) = xpg(ipg) - orig(1)
                    axer(2) = ypg(ipg) - orig(2)
                    if (ndim .eq. 3) then
                        axer(3) = zpg(ipg) - orig(3)
                    else
                        axer(3) = 0.0d0
                    endif
                    prosca=ddot(3,axer,1,axez,1)
                    axer(1) = axer(1) - prosca*axez(1)
                    axer(2) = axer(2) - prosca*axez(2)
                    if (ndim .eq. 3) then
                        axer(3) = axer(3) - prosca*axez(3)
                    else
                        axer(3) = 0.0d0
                    endif
                    xnormr = 0.0d0
                    call normev(axer, xnormr)
                    if (xnormr .lt. epsi) then
!   SI LE PT DE GAUSS EST SUR L'AXE ALORS L'AXE R N'EST PAS DEFINI
!   ON PREND UN AXE ARBITRAIRE ORTHOGONAL A OZ POUR DEFINIR LE REPERE
!   CYLINDRIQUE EN CE POINT
                        if (axez(1) .ne. 0.d0 .or. axez(2) .ne. 0.d0) then
                            axer(1) = axez(2)
                            axer(2) = -axez(1)
                            axer(3) = 0.0d0
                        else
                            axer(1) = 1.0d0
                            axer(2) = 0.0d0
                            axer(3) = 0.0d0
                        endif
                        igaaxe = igaaxe + 1
                    endif
                    call provec(axez, axer, axet)
                    xnormr = 0.0d0
                    call normev(axet, xnormr)
                    do 126 i = 1, 3
                        pgl(1,i) = axer(i)
                        pgl(2,i) = axez(i)
                        pgl(3,i) = axet(i)
126                  continue
                    if (tsca .eq. 'R') then
! CHAMP REEL
                        do 127 ii = 1, ncmp
                            call cesexi('S', jcesd, jcesl, imai, ipg,&
                                        isp, ii, iad)
                            if (iad .gt. 0) then
                                valet(ii)=zr(jcesv-1+iad)
                            else
                                goto 120
                            endif
127                      continue
                        valed(1) = valet(1)
                        valed(2) = valet(4)
                        valed(3) = valet(2)
                        valed(4) = valet(5)
                        valed(5) = valet(6)
                        valed(6) = valet(3)
                        call utpsgl(1, 3, pgl, valed, valet)
                        valer(1) = valet(1)
                        valer(2) = valet(3)
                        valer(3) = valet(6)
                        valer(4) = valet(2)
                        valer(5) = valet(4)
                        valer(6) = valet(5)
                        do 128 ii = 1, nbcmp
                            call cesexi('C', jcesd, jcesl, imai, ipg,&
                                        isp, ii, iad)
                            if (iad .gt. 0) then
                                zr(jcesv-1+iad) = valer( licmpu(ii))
                            else
                                goto 120
                            endif
128                      continue
                    else
! CHAMP COMPLEXE
                        do 313 ii = 1, ncmp
                            call cesexi('C', jcesd, jcesl, imai, ipg,&
                                        isp, ii, iad)
                            if (iad .gt. 0) then
                                valetc(ii)=zc(jcesv-1+iad)
                            else
                                goto 120
                            endif
313                      continue
                        valed(1) = dble(valetc(1))
                        valed(2) = dble(valetc(4))
                        valed(3) = dble(valetc(2))
                        valed(4) = dble(valetc(5))
                        valed(5) = dble(valetc(6))
                        valed(6) = dble(valetc(3))
                        call utpsgl(1, 3, pgl, valed, valet)
                        valer(1) = valet(1)
                        valer(2) = valet(3)
                        valer(3) = valet(6)
                        valer(4) = valet(2)
                        valer(5) = valet(4)
                        valer(6) = valet(5)
!
                        valed(1) = dimag(valetc(1))
                        valed(2) = dimag(valetc(4))
                        valed(3) = dimag(valetc(2))
                        valed(4) = dimag(valetc(5))
                        valed(5) = dimag(valetc(6))
                        valed(6) = dimag(valetc(3))
                        call utpsgl(1, 3, pgl, valed, valet)
                        valei(1) = valet(1)
                        valei(2) = valet(3)
                        valei(3) = valet(6)
                        valei(4) = valet(2)
                        valei(5) = valet(4)
                        valei(6) = valet(5)
!
                        do 314 ii = 1, nbcmp
                            call cesexi('C', jcesd, jcesl, imai, ipg,&
                                        isp, ii, iad)
                            if (iad .gt. 0) then
                                zc(jcesv-1+iad) = dcmplx( valer(ii),valei(ii))
                            else
                                goto 120
                            endif
314                      continue
                    endif
122                  continue
121                  continue
120              continue
            endif
        else
! VECTEUR
            if (ndim .eq. 2) then
                licmpu(1)=1
                licmpu(2)=3
                licmpu(3)=2
            endif
            do 29 inel = 1, nbmail
                if (nbm .ne. 0) then
                    imai = zi(idmail+inel-1)
                else
                    imai = inel
                endif
                nbpt = zi(jcesd-1+5+4* (imai-1)+1)
                nbsp = zi(jcesd-1+5+4* (imai-1)+2)
                do 30,ipt = 1,nbpt
                do 31,isp = 1,nbsp
                test = .true.
                do 32 ii = 1, ncmp
                    call cesexi('C', jcesd, jcesl, imai, ipt,&
                                isp, ii, iad)
                    if (iad .gt. 0) then
                        test = .false.
                    endif
32              continue
                if (test) goto 29
                ino = zi(jconx1-1+zi(jconx2+imai-1)+ipt-1)
                axer(1) = zr(axyzm+3*(ino-1) ) - orig(1)
                axer(2) = zr(axyzm+3*(ino-1)+1) - orig(2)
                if (ndim .eq. 3) then
                    axer(3) = zr(axyzm+3*(ino-1)+2) - orig(3)
                else
                    axer(3) = 0.0d0
                endif
                prosca=ddot(3,axer,1,axez,1)
                axer(1) = axer(1) - prosca*axez(1)
                axer(2) = axer(2) - prosca*axez(2)
                if (ndim .eq. 3) then
                    axer(3) = axer(3) - prosca*axez(3)
                else
                    axer(3) = 0.0d0
                endif
                xnormr = 0.0d0
                call normev(axer, xnormr)
                if (xnormr .lt. epsi) then
                    call jenuno(jexnum(ma//'.NOMNOE', ino), k8b)
                    call u2mess('A', 'ALGORITH2_12')
                    axer(1) = 0.0d0
                    axer(2) = 0.0d0
                    axer(3) = 0.0d0
                    do 33 ipt2 = 1, nbpt
                        inot = zi(jconx1-1+zi(jconx2+imai-1)+ ipt2-1)
                        axer(1) = axer(1) + zr(axyzm+3*(inot- 1) )
                        axer(2) = axer(2) + zr(axyzm+3*(inot- 1)+1)
                        if (ndim .eq. 3) then
                            axer(3) = axer(3) + zr(axyzm+3*( inot-1)+2)
                        endif
33                  continue
                    axer(1) = axer(1)/nbpt - orig(1)
                    axer(2) = axer(2)/nbpt - orig(2)
                    axer(3) = axer(3)/nbpt - orig(3)
                    prosca=ddot(3,axer,1,axez,1)
                    axer(1) = axer(1) - prosca*axez(1)
                    axer(2) = axer(2) - prosca*axez(2)
                    if (ndim .eq. 3) then
                        axer(3) = axer(3) - prosca*axez(3)
                    else
                        axer(3) = 0.0d0
                    endif
                    xnormr = 0.0d0
                    call normev(axer, xnormr)
                    if (xnormr .lt. epsi) then
                        call jenuno(jexnum(ma//'.NOMNOE', ino), k8b)
                        valk (1) = k8b
                        call u2mesg('F', 'ALGORITH12_44', 1, valk, 0,&
                                    0, 0, 0.d0)
                    endif
                endif
                call provec(axez, axer, axet)
                xnormr = 0.0d0
                call normev(axet, xnormr)
                do 35 i = 1, 3
                    pgl(1,i) = axer(i)
                    pgl(2,i) = axez(i)
                    pgl(3,i) = axet(i)
35              continue
                if (tsca .eq. 'R') then
! CHAMP REEL
                    do 36 ii = 1, ncmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            valed(ii)=zr(jcesv-1+iad)
                        else
                            goto 29
                        endif
36                  continue
                    if (ndim .eq. 3) then
                        call utpvgl(1, 3, pgl, valed, valer)
                    else
                        call ut2vgl(1, 3, pgl, valed, valer)
                    endif
                    do 37 ii = 1, nbcmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            zr(jcesv-1+iad) = valer(licmpu(ii) )
                        else
                            goto 29
                        endif
37                  continue
                else
! CHAMP COMPLEXE
                    do 136 ii = 1, ncmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            valetc(ii) = zc(jcesv-1+iad)
                            valed(ii) = dble(valetc(ii))
                            valet(ii) = dimag(valetc(ii))
                        else
                            goto 29
                        endif
136                  continue
                    if (ndim .eq. 3) then
                        call utpvgl(1, 3, pgl, valed, valer)
                        call utpvgl(1, 3, pgl, valet, valei)
                    else
                        call ut2vgl(1, 3, pgl, valed, valer)
                        call ut2vgl(1, 3, pgl, valet, valei)
                    endif
                    do 137 ii = 1, nbcmp
                        call cesexi('C', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            i2=licmpu(ii)
                            zc(jcesv-1+iad) = dcmplx(valer(i2) ,valei(i2))
                        else
                            goto 29
                        endif
137                  continue
                endif
!
31              continue
30              continue
29          continue
        endif
        call dismoi('F', 'NOM_OPTION', champ1, 'CHAM_ELEM', ibid,&
                    option, ibid)
        call cescel(chams1, ligrel, option, ' ', 'OUI',&
                    nncp, 'G', champ1, 'F', ibid)
        call detrsd('CHAM_ELEM_S', chams1)
        if (igaaxe .ne. 0) then
            call u2mesi('A', 'ALGORITH17_22', 1, igaaxe)
        endif
!
        else if((repere(1:5) .eq.'COQUE') .or. (repere(1:15)&
    .eq.'COQUE_INTR_UTIL').or. (repere(1:15).eq.'COQUE_UTIL_INTR'))&
    then
!
        call megeom(modele, chgeom)
        call mecara(carele, exicar, chcara)
!
!    GENERATION D UN CHAMP D'ANGLES (CARTE CONSTANTE)
!
        carte = '&&CHRPEL.ANGL_REP'
        angrep(1) = 0.0d0
        angrep(2) = 0.0d0
        angrep(3) = 0.0d0
!
        if (repere .eq. 'COQUE_INTR_UTIL') then
            angrep(3)=1.d0
        else if (repere.eq.'COQUE_UTIL_INTR') then
            angrep(3)=2.d0
        endif
        licmp(1) = 'ALPHA'
        licmp(2) = 'BETA'
        licmp(3) = 'REP'
        call mecact('V', carte, 'MODELE', modele, 'CAORIE',&
                    3, licmp, ibid, angrep, cbid,&
                    ' ')
!
!  CREATION D UN CHAM_ELEM D'ANGLES EN LISANT LES ANGL_REP
!
        changl = '&&CHRPEL.ANGL'
        call chrpan(modele, carte, changl)
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PCACOQU'
        lchin(2) = chcara(7)
        lpain(3) = 'PANGREP'
        lchin(3) = changl
        lchin(4) = champ0
        if (type .eq. 'COQUE_GENE') then
            option = 'REPE_GENE'
            if (nomch .eq. 'EFGE_ELGA') then
                lpain(4) = 'PEFGAIN'
                paout = 'PEFGAOUT'
            else if (nomch.eq.'EFGE_ELNO') then
                lpain(4) = 'PEFNOIN'
                paout = 'PEFNOOUT'
            else if (nomch.eq.'DEGE_ELGA') then
                lpain(4) = 'PDGGAIN'
                paout = 'PDGGAOUT'
            else if (nomch.eq.'DEGE_ELNO') then
                lpain(4) = 'PDGNOIN'
                paout = 'PDGNOOUT'
            else if (nomch.eq.'SIEF_ELGA') then
                lpain(4) = 'PEFGAIN'
                paout = 'PEFGAOUT'
            else
                call u2mesk('F', 'ELEMENTS5_51', 1, nomch)
            endif
        else if (type.eq.'TENS_3D') then
            option = 'REPE_TENS'
            if (nomch .eq. 'SIGM_ELGA') then
                lpain(4) = 'PCOGAIN'
                paout = 'PCOGAOUT'
            else if (nomch.eq.'SIGM_ELNO') then
                lpain(4) = 'PCONOIN'
                paout = 'PCONOOUT'
            else if (nomch.eq.'EPSI_ELGA') then
                lpain(4) = 'PDEGAIN'
                paout = 'PDEGAOUT'
            else if (nomch.eq.'EPSI_ELNO') then
                lpain(4) = 'PDENOIN'
                paout = 'PDENOOUT'
            else
                call u2mesk('F', 'ELEMENTS5_52', 1, nomch)
            endif
        else
            call u2mesk('F', 'ELEMENTS5_53', 1, type)
        endif
        call exisd('CHAM_ELEM_S', canbsp, iret1)
        if (iret1 .ne. 1) then
            call dismoi('F', 'MXNBSP', champ0(1:19), 'CHAM_ELEM', nbsp,&
                        kbid, iret)
!
! SI LE CHAMP A DEJA ETE EXTRAIT IL FAUT APPELER CESVAR AVEC CE CHAMP
!
            if (nbsp .eq. 1) then
                call cesvar(champ0(1:19), ' ', ligrel, canbsp)
            else
                call cesvar(carele, ' ', ligrel, canbsp)
            endif
        endif
        chaout = chams1
        call copisd('CHAM_ELEM_S', 'V', canbsp, chaout)
        call calcul('S', option, ligrel, 4, lchin,&
                    lpain, 1, chaout, paout, 'V',&
                    'OUI')
        call detrsd('CHAM_ELEM_S', chaout)
        call copisd('CHAMP_GD', 'G', chaout, champ1)
    endif
    call jedetr('&&CHRPEL.NOM_CMP')
    call exisd('CHAM_ELEM_S', canbsp, iret1)
    if (iret1 .ne. 0) call detrsd('CHAM_ELEM_S', canbsp)
    call jeexin(mesmai, iret)
    if (iret .ne. 0) call jedetr(mesmai)
    call jedema()
!
end subroutine

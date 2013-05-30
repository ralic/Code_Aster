subroutine chrpno(champ1, repere, nbcmp, icham, type)
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
! TOLE CRP_20
    implicit       none
    include 'jeveux.h'
!
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8dgrd.h'
    include 'asterfort/angvxy.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/cnscno.h'
    include 'asterfort/cnsred.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/matrot.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/ut2vgl.h'
    include 'asterfort/utpsgl.h'
    include 'asterfort/utpvgl.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
    integer :: nbcmp, icham
    character(len=*) :: champ1, repere, type
! ----------------------------------------------------------------------
!
!     BUT : CHANGEMENT DE REPERE DANS LE CAS D'UN CHAM_NO
! ----------------------------------------------------------------------
!     ARGUMENTS :
!     CHAMP1   IN  K16  : NOM DU CHAMP A TRAITER
!     REPERE   IN  K16  : TYPE DE REPERE (UTILISATEUR OU CYLINDRIQUE)
!     NBCMP    IN  I    : NOMBRE DE COMPOSANTES A TRAITER
!     ICHAM    IN  I    : NUMERO D'OCCURRENCE
! ----------------------------------------------------------------------
! ---------------------------------------------------------------------
!
    integer :: i, nbno, ino, jcmp, ibid
    integer :: axyzm, ii, nbma, ipt2, ier, inel
    integer :: jcnsd, jcnsk, jcnsv, jconx1, jconx2, nbpt
    integer :: ipt, inot, ndim, licmpu(6), jcnsl
    integer :: nbn, idnoeu, nbnoeu, inoe
    real(kind=8) :: angnot(3), pgl(3, 3), valer(6), valed(6)
    real(kind=8) :: valr, valei(6)
    real(kind=8) :: orig(3), axez(3), axer(3), axet(3)
    real(kind=8) :: epsi, prosca, xnormr, valet(6)
    real(kind=8) :: vectx(3), vecty(3)
    complex(kind=8) :: valetc(6)
    character(len=3) :: tsca
    character(len=8) :: ma, k8b, typmcl(4), nomgd
    character(len=16) :: motcle(4)
    character(len=19) :: chams1, chams0
    character(len=24) :: mesnoe
    character(len=24) :: valk
    integer :: iarg
!
    call jemarq()
    epsi = 1.0d-6
    motcle(1) = 'GROUP_NO'
    typmcl(1) = 'GROUP_NO'
    motcle(2) = 'NOEUD'
    typmcl(2) = 'NOEUD'
    motcle(3) = 'GROUP_MA'
    typmcl(3) = 'GROUP_MA'
    motcle(4) = 'MAILLE'
    typmcl(4) = 'MAILLE'
    mesnoe = '&&CHRPEL.MES_NOEUDS'
!
    if (nbcmp .gt. 0) then
        call wkvect('&&CHRPNO.NOM_CMP', 'V V K8', nbcmp, jcmp)
        call getvtx('MODI_CHAM', 'NOM_CMP', icham, iarg, nbcmp,&
                    zk8(jcmp), ibid)
    else
        call u2mess('F', 'ALGORITH2_6')
    endif
!
! ----- DEFINITION ET CREATION DU CHAM_NO SIMPLE CHAMS1
! ----- A PARTIR DU CHAM_NO CHAMP1
!
    chams0='&&CHRPNO.CHAMS0'
    chams1='&&CHRPNO.CHAMS1'
    call cnocns(champ1, 'V', chams0)
    call cnsred(chams0, 0, 0, nbcmp, zk8(jcmp),&
                'V', chams1)
    call detrsd('CHAM_NO_S', chams0)
    call jeveuo(chams1//'.CNSK', 'L', jcnsk)
    call jeveuo(chams1//'.CNSD', 'L', jcnsd)
    ma = zk8(jcnsk-1+1)
    nomgd = zk8(jcnsk-1+2)
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
!
    nbno = zi(jcnsd-1+1)
    call jeveuo(chams1//'.CNSV', 'E', jcnsv)
    call jeveuo(chams1//'.CNSL', 'E', jcnsl)
    call dismoi('F', 'Z_CST', ma, 'MAILLAGE', ndim,&
                k8b, ier)
    ndim = 3
    if (k8b .eq. 'OUI') ndim = 2
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                k8b, ier)
!
!
    call reliem(' ', ma, 'NU_NOEUD', 'AFFE', icham,&
                4, motcle, typmcl, mesnoe, nbn)
!
    if (nbn .gt. 0) then
        nbnoeu = nbn
        call jeveuo(mesnoe, 'L', idnoeu)
    else
        nbnoeu = nbno
    endif
!
!    SI UNE DES COMPOSANTES EST ABSENTE SUR UN NOEUD
!    IL EST IGNORE
!
    do 110 ino = 1, nbnoeu
        if (nbn .ne. 0) then
            inoe = zi(idnoeu+ino-1)
        else
            inoe = ino
        endif
        do 111 ii = 1, nbcmp
            if (.not.zl(jcnsl-1+(inoe-1)*nbcmp+ii)) goto 112
111      continue
        goto 110
112      continue
        do 114 ii = 1, nbcmp
            zl(jcnsl-1+(inoe-1)*nbcmp+ii)=.false.
114      continue
110  end do
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
            do 10 ino = 1, nbnoeu
                if (nbn .ne. 0) then
                    inoe = zi(idnoeu+ino-1)
                else
                    inoe = ino
                endif
                if (.not.zl(jcnsl-1+(inoe-1)*nbcmp+1)) goto 10
                if (tsca .eq. 'R') then
! CHAMP REEL
                    do 11 ii = 1, nbcmp
                        valet(ii)=zr(jcnsv-1+(inoe-1)*nbcmp+ii)
11                  continue
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
                    do 12 ii = 1, nbcmp
                        zr(jcnsv-1+(inoe-1)*nbcmp+ii) = valer(ii)
12                  continue
                else
! CHAMP COMPLEXE
                    do 211 ii = 1, nbcmp
                        valetc(ii)=zc(jcnsv-1+(inoe-1)*nbcmp+ii)
211                  continue
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
                    do 212 ii = 1, nbcmp
                        zc(jcnsv-1+(inoe-1)*nbcmp+ii) = dcmplx( valer( ii),valei(ii))
212                  continue
!
                endif
!
10          continue
        else
! VECTEUR
            do 13 ino = 1, nbnoeu
                if (nbn .ne. 0) then
                    inoe = zi(idnoeu+ino-1)
                else
                    inoe = ino
                endif
                if (.not.zl(jcnsl-1+(inoe-1)*nbcmp+1)) goto 13
                if (tsca .eq. 'R') then
! VECTEUR REEL
                    do 14 ii = 1, nbcmp
                        valed(ii)=zr(jcnsv-1+(inoe-1)*nbcmp+ii)
14                  continue
                    if (ndim .eq. 3) then
                        call utpvgl(1, nbcmp, pgl, valed, valer)
                    else
                        call ut2vgl(1, nbcmp, pgl, valed, valer)
                    endif
                    do 15 ii = 1, nbcmp
                        zr(jcnsv-1+(inoe-1)*nbcmp+ii) = valer(ii)
15                  continue
                else
! VECTEUR COMPLEXE
                    do 214 ii = 1, nbcmp
                        valetc(ii)=zc(jcnsv-1+(inoe-1)*nbcmp+ii)
                        valed(ii) = dble(valetc(ii))
                        valet(ii) = dimag(valetc(ii))
214                  continue
                    if (ndim .eq. 3) then
                        call utpvgl(1, nbcmp, pgl, valed, valer)
                        call utpvgl(1, nbcmp, pgl, valet, valei)
                    else
                        call ut2vgl(1, nbcmp, pgl, valed, valer)
                        call ut2vgl(1, nbcmp, pgl, valet, valei)
                    endif
                    do 215 ii = 1, nbcmp
                        zc(jcnsv-1+(inoe-1)*nbcmp+ii) = dcmplx( valer( ii),valei(ii))
215                  continue
!
                endif
13          continue
        endif
    else if (repere(1:5).eq.'COQUE') then
        call u2mess('F', 'ALGORITH2_3')
    else
! REPERE CYLINDRIQUE
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
        if (type(1:4) .eq. 'TENS') then
            if (ndim .eq. 2) then
                licmpu(1)=1
                licmpu(2)=2
                licmpu(3)=3
                licmpu(4)=5
            endif
!
            do 16 ino = 1, nbnoeu
                if (nbn .ne. 0) then
                    inoe = zi(idnoeu+ino-1)
                else
                    inoe = ino
                endif
                axer(1) = zr(axyzm+3*(inoe-1) ) - orig(1)
                axer(2) = zr(axyzm+3*(inoe-1)+1) - orig(2)
                if (ndim .eq. 3) then
                    axer(3) = zr(axyzm+3*(inoe-1)+2) - orig(3)
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
                    call jenuno(jexnum(ma//'.NOMNOE', inoe), k8b)
                    call u2mess('A', 'ALGORITH2_13')
                    call jeveuo(ma//'.CONNEX', 'L', jconx1)
                    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', jconx2)
                    ipt2=0
                    do 17 inel = 1, nbma
                        nbpt = zi(jconx2-1+inel+1)-zi(jconx2-1+inel)
                        do 18 ipt = 1, nbpt
                            inot = zi(jconx1-1+zi(jconx2-1+inel)+ipt- 1)
                            if (inot .eq. inoe) then
                                axer(1) = 0.0d0
                                axer(2) = 0.0d0
                                axer(3) = 0.0d0
                                do 19 ipt2 = 1, nbpt
                                    inot = zi( jconx1-1 + zi(jconx2-1+ inel)+ipt2-1 )
                                    axer(1) = axer(1) + zr(axyzm+3*( inot-1) )
                                    axer(2) = axer(2) + zr(axyzm+3*( inot-1)+1)
                                    if (ndim .eq. 3) then
                                        axer(3) = axer(3) + zr(axyzm+ 3*(inot-1)+2)
                                    endif
19                              continue
                                axer(1) = axer(1)/nbpt
                                axer(2) = axer(2)/nbpt
                                axer(3) = axer(3)/nbpt
                                goto 17
                            endif
18                      continue
17                  continue
!
!                LE NOEUD SUR L'AXE N'APPARTIENT A AUCUNE MAILLE
                    if (ipt2 .eq. 0) then
                        do 117 ii = 1, nbcmp
                            zl(jcnsl-1+(inoe-1)*nbcmp+ii)=.false.
117                      continue
                        goto 16
                    endif
!
                    axer(1) = axer(1) - orig(1)
                    axer(2) = axer(2) - orig(2)
                    axer(3) = axer(3) - orig(3)
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
                        call jenuno(jexnum(ma//'.NOMNOE', inoe), k8b)
                        valk = k8b
                        call u2mesg('F', 'ALGORITH14_91', 1, valk, 0,&
                                    0, 0, 0.d0)
                    endif
                endif
                call provec(axez, axer, axet)
                xnormr = 0.0d0
                call normev(axet, xnormr)
                do 20 i = 1, 3
                    pgl(1,i) = axer(i)
                    pgl(2,i) = axez(i)
                    pgl(3,i) = axet(i)
20              continue
                if (.not.zl(jcnsl-1+(inoe-1)*nbcmp+1)) goto 16
                if (tsca .eq. 'R') then
! CHAMP REEL
                    do 21 ii = 1, nbcmp
                        valet(ii)=zr(jcnsv-1+(inoe-1)*nbcmp+ii)
21                  continue
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
                    do 22 ii = 1, nbcmp
                        zr(jcnsv-1+(inoe-1)*nbcmp+ii)=valer(licmpu(ii)&
                        )
22                  continue
                else
! CHAMP COMPLEXE
                    do 311 ii = 1, nbcmp
                        valetc(ii)=zc(jcnsv-1+(inoe-1)*nbcmp+ii)
311                  continue
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
                    do 312 ii = 1, nbcmp
                        zc(jcnsv-1+(inoe-1)*nbcmp+ii) = dcmplx( valer( ii),valei(ii))
312                  continue
!
                endif
16          continue
        else
! VECTEUR
            if (ndim .eq. 2) then
                licmpu(1)=1
                licmpu(2)=3
                licmpu(3)=2
            endif
!
            do 23 ino = 1, nbnoeu
                if (nbn .ne. 0) then
                    inoe = zi(idnoeu+ino-1)
                else
                    inoe = ino
                endif
                axer(1) = zr(axyzm+3*(inoe-1) ) - orig(1)
                axer(2) = zr(axyzm+3*(inoe-1)+1) - orig(2)
                if (ndim .eq. 3) then
                    axer(3) = zr(axyzm+3*(inoe-1)+2) - orig(3)
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
                    call jenuno(jexnum(ma//'.NOMNOE', inoe), k8b)
                    call u2mess('A', 'ALGORITH2_13')
                    call jeveuo(ma//'.CONNEX', 'L', jconx1)
                    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', jconx2)
                    ipt2=0
                    do 24 inel = 1, nbma
                        nbpt = zi(jconx2-1+inel+1)-zi(jconx2-1+inel)
                        do 25 ipt = 1, nbpt
                            inot = zi(jconx1-1+zi(jconx2-1+inel)+ipt- 1)
                            if (inot .eq. inoe) then
                                axer(1) = 0.0d0
                                axer(2) = 0.0d0
                                axer(3) = 0.0d0
                                do 26 ipt2 = 1, nbpt
                                    inot = zi( jconx1-1 + zi(jconx2-1+ inel)+ipt2-1 )
                                    axer(1) = axer(1) + zr(axyzm+3*( inot-1) )
                                    axer(2) = axer(2) + zr(axyzm+3*( inot-1)+1)
                                    if (ndim .eq. 3) then
                                        axer(3) = axer(3) + zr(axyzm+ 3*(inot-1)+2)
                                    endif
26                              continue
                                axer(1) = axer(1)/nbpt
                                axer(2) = axer(2)/nbpt
                                axer(3) = axer(3)/nbpt
                                goto 24
                            endif
25                      continue
24                  continue
!                LE NOEUD SUR L'AXE N'APPARTIENT A AUCUNE MAILLE
                    if (ipt2 .eq. 0) then
                        do 124 ii = 1, nbcmp
                            zl(jcnsl-1+(inoe-1)*nbcmp+ii)=.false.
124                      continue
                        goto 23
                    endif
                    axer(1) = axer(1) - orig(1)
                    axer(2) = axer(2) - orig(2)
                    axer(3) = axer(3) - orig(3)
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
                        call jenuno(jexnum(ma//'.NOMNOE', inoe), k8b)
                        valk = k8b
                        call u2mesg('F', 'ALGORITH14_91', 1, valk, 0,&
                                    0, 0, 0.d0)
                    endif
                endif
                call provec(axez, axer, axet)
                do 27 i = 1, 3
                    pgl(1,i) = axer(i)
                    pgl(2,i) = axez(i)
                    pgl(3,i) = axet(i)
27              continue
                if (.not.zl(jcnsl-1+(inoe-1)*nbcmp+1)) goto 23
                if (tsca .eq. 'R') then
! VECTEUR REEL
                    do 28 ii = 1, nbcmp
                        valed(ii)=zr(jcnsv-1+(inoe-1)*nbcmp+ii)
28                  continue
                    if (ndim .eq. 3) then
                        call utpvgl(1, nbcmp, pgl, valed, valer)
                    else
                        call ut2vgl(1, nbcmp, pgl, valed, valer)
                    endif
                    do 29 ii = 1, nbcmp
                        zr(jcnsv-1+(inoe-1)*nbcmp+ii)=valer(licmpu(ii)&
                        )
29                  continue
                else
! VECTEUR COMPLEXE
                    do 328 ii = 1, nbcmp
                        valetc(ii)=zc(jcnsv-1+(inoe-1)*nbcmp+ii)
                        valed(ii) = dble(valetc(ii))
                        valet(ii) = dimag(valetc(ii))
328                  continue
                    if (ndim .eq. 3) then
                        call utpvgl(1, nbcmp, pgl, valed, valer)
                        call utpvgl(1, nbcmp, pgl, valet, valei)
                    else
                        call ut2vgl(1, nbcmp, pgl, valed, valer)
                        call ut2vgl(1, nbcmp, pgl, valet, valei)
                    endif
                    do 329 ii = 1, nbcmp
                        zc(jcnsv-1+(inoe-1)*nbcmp+ii) = dcmplx( valer( ii),valei(ii))
329                  continue
!
                endif
!
23          continue
        endif
    endif
    call cnscno(chams1, ' ', 'NON', 'G', champ1,&
                'F', ibid)
    call detrsd('CHAM_NO_S', chams1)
    call jedetr('&&CHRPNO.NOM_CMP')
    call jedetr(mesnoe)
    call jedema()
!
end subroutine

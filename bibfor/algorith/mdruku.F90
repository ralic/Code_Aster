subroutine mdruku(method, tinit, tfin, dt, dtmin,&
                  dtmax, nbsauv, nbobjs, neqgen, pulsat,&
                  pulsa2, masgen, descmm, riggen, descmr,&
                  rgygen, lamor, amogen, descma, gyogen,&
                  foncv, fonca, typbas, basemo, nbchoc,&
                  intitu, logcho, dplmod, parcho, noecho,&
                  nbrede, dplred, fonred, nbrevi, dplrev,&
                  fonrev, coefm, liad, inumor, idescf,&
                  nofdep, nofvit, nofacc, nomfon, psidel,&
                  monmot, nbrfis, fk, dfk, angini,&
                  foncp, nbpal, dtsto, vrotat, prdeff,&
                  nomres, nbexci, nommas, nomrig, nomamo)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterc/r8prem.h"
#include "asterfort/amgene.h"
#include "asterfort/codent.h"
#include "asterfort/concrk.h"
#include "asterfort/fointe.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdacce.h"
#include "asterfort/mdallo.h"
#include "asterfort/mdarnl.h"
#include "asterfort/mdbs32.h"
#include "asterfort/mddp54.h"
#include "asterfort/mdfext.h"
#include "asterfort/mdfnli.h"
#include "asterfort/mdinit.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/preres.h"
#include "asterfort/r8inir.h"
#include "asterfort/rigene.h"
#include "asterfort/sigusr.h"
#include "asterfort/trlds.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
!
    integer :: descmm, descmr, descma, palmax, nbrede, nbrevi, dimnas
    integer :: neqgen, nbsauv, nbchoc, nbrfis, nbexci, logcho(nbchoc, *)
    integer :: liad(*), inumor(*), idescf(*), nbpal, nbobjs, iapp, iarchi, isto1
    integer :: isto2, isto3, isto4, jchor, jredi, jredr, jrevi, jrevr, jvint
    integer :: nbconv, nbmxcv, nbsaui, nbscho, im, ind, iadrk, ibid
    integer :: jacce, ier, iret, jacci, jaccs, jamgy, jdcho, jdepi, jdepl, jdeps
    integer :: jfcho, jfext, jicho, jinst, jkde, jkvi, jm, jmass, jchoi
    integer :: jordr, jpass, jredc, jredd, jrevc, jrevv, jrigy, jtra1, jvcho
    integer :: jvite, jviti, jvits, n1, npm, nbschor, nbvint
    parameter    (palmax=20)
    parameter    (dimnas=8)
    real(kind=8) :: pulsat(*), pulsa2(*), masgen(*), riggen(*), amogen(*)
    real(kind=8) :: parcho(*), dplrev(*), dplred(*), rgygen(*)
    real(kind=8) :: dplmod(nbchoc, neqgen, *), gyogen(*), dt, dtsto, tfin
    real(kind=8) :: vrotat, conv, facobj, tinit, angini, epsi, errt, r8bid1
    real(kind=8) :: temps, coefm(*), psidel(*), deux, pow, fsauv(palmax, 3)
    real(kind=8) :: vrotin, arotin, dtmax, dtmin, tol, coeff, seuil1, seuil2
    aster_logical :: lamor, prdeff, adapt, flagdt, condrepri
    character(len=3) :: finpal(palmax)
    character(len=4) :: intk
    character(len=6) :: typal(palmax)
    character(len=8) :: cnpal(palmax), basemo, noecho(nbchoc, *), fonred(*)
    character(len=8) :: fonrev(*), intitu, nomres, monmot, fk(2), dfk(2), vitvar
    character(len=8) :: nommas, nomrig, nofdep(*), nofvit(*), nofacc(*)
    character(len=8) :: nomfon(*), foncv, fonca, foncp, namerk, nomamo
    character(len=14) :: matpre
    character(len=16) :: typbas, method
    character(len=19) :: matasm
    character(len=24) :: cpal
!
    aster_logical :: okprem
!
!   ------------------------------------------------------------------------------------
!   Definition of statement functions giving the appropriate (i,j) term in the mass,
!   rigidity and damping matrices
#define rgen(row,col) rigene(row, col, riggen, neqgen, typbas, 'RUNGE_KUTTA')
#define agen(row,col) amgene(row, col, amogen, neqgen, typbas, 'RUNGE_KUTTA', lamor)
!   ------------------------------------------------------------------------------------
!
!
! ======================================================================
!
    call jemarq()
!
    nbobjs=1
    call codent(nbobjs, 'D0', intk)
    namerk='&&RK'//intk
    call mdallo(namerk, 'TRAN', nbsauv, sauve='VOLA', checkarg=.false._1,&
                method=method, base=basemo, nbmodes=neqgen, rigi=nomrig, mass=nommas,&
                amor=nomamo, jordr=jordr, jdisc=jinst, jdepl=jdeps, jvite=jvits,&
                jacce=jaccs, dt=dt, jptem=jpass, nbchoc=nbchoc, noecho=noecho,&
                intitu=intitu, jfcho=jfcho, jdcho=jdcho, jvcho=jvcho, jadcho=jicho,&
                nbrede=nbrede, fonred=fonred, jredc=jredc, jredd=jredd, nbrevi=nbrevi,&
                fonrev=fonrev, jrevc=jrevc, jrevv=jrevv)
!
    deux = 2.d0
    epsi = r8prem()
    jchor = 1
    jchoi = 1
    jredr = 1
    jredi = 1
    jrevr = 1
    jrevi = 1
    jvint = 1
    isto1 = 0
    isto2 = 0
    isto3 = 0
    isto4 = 0
    iarchi = 0
    nbscho = nbsauv * 3 * nbchoc
    nbsaui = nbsauv
    vitvar = 'NON'
!   RAPPORT D'AUGMENTATION DES OBJETS VOLATILES
    facobj=1.5d0
!   COUPLAGE EDYOS : CONVERGENCE EDYOS :
    conv = 1.d0
    nbconv = 0
!   COUPLAGE EDYOS : NOMBRE MAXIMAL DE TENTATIVES DE REPRISE DES DONNEES
!   PRECEDENTES EN CAS DE NON-CONVERGENCE EDYOS :
    nbmxcv = 10
!
    do iapp = 1, palmax
        typal(iapp)='      '
        finpal(iapp)='   '
        cnpal(iapp)=' '
    enddo
!
!   GESTION DE LA VITESSE VARIABLE MACHINES TOURNANTES
    call wkvect('&&RUKUT.AMOGYR', 'V V R8', neqgen*neqgen, jamgy)
    call wkvect('&&RUKUT.RIGGYR', 'V V R8', neqgen*neqgen, jrigy)
    if (lamor) then
        vitvar=' '
        do im = 1, neqgen
            amogen(im) = deux * amogen(im) * pulsat(im)
        enddo
    else
        call getvtx(' ', 'VITESSE_VARIABLE', nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvtx(' ', 'VITESSE_VARIABLE', scal=vitvar, nbret=n1)
        else
            vitvar=' '
        endif
        vrotin = 0.d0
        arotin = 0.d0
        if (vitvar .eq. 'OUI') then
            call fointe('F ', foncv, 1, ['INST'], [tinit],&
                        vrotin, ier)
            call fointe('F ', fonca, 1, ['INST'], [tinit],&
                        arotin, ier)
            do im = 1, neqgen
                do jm = 1, neqgen
                    ind = jm + neqgen*(im-1)
                    zr(jamgy+ind-1) = agen(im,jm) + vrotin * gyogen( ind)
                    zr(jrigy+ind-1) = rgen(im,jm) + arotin * rgygen( ind)
                enddo
            enddo
        else
            do im = 1, neqgen
                do jm = 1, neqgen
                    ind = jm + neqgen*(im-1)
                    zr(jamgy+ind-1) = agen(im,jm)
                    zr(jrigy+ind-1) = rgen(im,jm)
                enddo
            enddo
        endif
    endif
!
!   FACTORISATION DE LA MATRICE MASSE
    if (typbas .eq. 'BASE_MODA') then
        call wkvect('&&RUKUT.MASS', 'V V R8', neqgen*neqgen, jmass)
        call dcopy(neqgen*neqgen, masgen, 1, zr(jmass), 1)
        call trlds(zr(jmass), neqgen, neqgen, iret)
        if (iret .ne. 0) then
            call utmess('F', 'ALGORITH5_22')
        endif
        call dcopy(neqgen*neqgen, masgen, 1, zr(jmass), 1)
    else if (typbas.eq.'MODELE_GENE     ') then
        matpre='&&RUKUT.MATPRE'
        matasm=zk24(zi(descmm+1))(1:19)
        call preres(' ', 'V', iret, matpre, matasm,&
                    ibid, -9999)
    else
        call wkvect('&&RUKUT.MASS', 'V V R8', neqgen, jmass)
        call dcopy(neqgen, masgen, 1, zr(jmass), 1)
    endif
!
!   parametres du schema adaptatif
    call getvr8('SCHEMA_TEMPS', 'TOLERANCE', iocc=1, scal=tol, nbret=n1)
!   on laisse l'option pour schema adaptatif ou pas
    adapt=.true.
!
!   factorisation de la matrice masse
!   vecteurs de travail
    call wkvect('&&RUKUT.DEPL', 'V V R8', neqgen, jdepl)
    call wkvect('&&RUKUT.VITE', 'V V R8', neqgen, jvite)
    call wkvect('&&RUKUT.ACCE', 'V V R8', neqgen, jacce)
    call wkvect('&&RUKUT.TRA1', 'V V R8', neqgen, jtra1)
    call wkvect('&&RUKUT.FEXT', 'V V R8', neqgen, jfext)
!
    if (nbchoc .ne. 0 .and. nbpal .eq. 0) then
        nbschor = nbchoc*(mdtr74grd('SCHOR')+mdtr74grd('MAXVINT'))
        call wkvect('&&RUKUT.SCHOR', 'V V R8', nbschor, jchor)
!       initialisation pour les variables internes
        nbvint = nbchoc * nbsauv * mdtr74grd('MAXVINT')
        call jeveuo(namerk//'           .VINT', 'E', jvint)
        call r8inir(nbvint, 0.d0, zr(jvint), 1)
    endif
    if (nbrede .ne. 0) then
        call wkvect('&&RUKUT.SREDR', 'V V R8', nbrede, jredr)
        call wkvect('&&RUKUT.SREDI', 'V V I', nbrede, jredi)
    endif
    if (nbrevi .ne. 0) then
        call wkvect('&&RUKUT.SREVR', 'V V R8', nbrevi, jrevr)
        call wkvect('&&RUKUT.SREVI', 'V V I', nbrevi, jrevi)
    endif
!   vecteurs specifiques a RUNGE-KUTTA
    call wkvect('&&RUKUT.DEPI', 'V V R8', neqgen, jdepi)
    call wkvect('&&RUKUT.VITI', 'V V R8', neqgen, jviti)
    call wkvect('&&RUKUT.ACCI', 'V V R8', neqgen, jacci)
!   Pour les parametres de choc et les variables internes
    if (nbchoc .ne. 0) then
        nbschor = nbchoc*(mdtr74grd('SCHOR')+mdtr74grd('MAXVINT'))
        call wkvect('&&RUKUT.SCHOI', 'V V R8', nbschor, jchoi)
    endif
    if (method(13:14) .eq. '54') then
        call wkvect('&&RUKUT.KKDE', 'V V R8', neqgen*6, jkde)
        call wkvect('&&RUKUT.KKVI', 'V V R8', neqgen*6, jkvi)
    else if (method(13:14).eq.'32') then
        call wkvect('&&RUKUT.KKDE', 'V V R8', neqgen*3, jkde)
        call wkvect('&&RUKUT.KKVI', 'V V R8', neqgen*3, jkvi)
    endif
!
!   conditions initiales
    call mdinit(basemo, neqgen, nbchoc, zr(jdepl), zr(jvite),&
                zr(jvint), iret, tinit, intitu=intitu, noecho=noecho,&
                reprise=condrepri, accgen=zr(jacce))
    if (iret .ne. 0) goto 9999
    if (nbchoc .gt. 0 .and. nbpal .eq. 0) then
        nbvint = nbchoc*mdtr74grd('MAXVINT')
        call dcopy(nbvint, zr(jvint), 1, zr(jchor+mdtr74grd('SCHOR')*nbchoc), 1)
    endif
!
!   forces exterieures
    if (nbexci .ne. 0) then
        call mdfext(tinit, r8bid1, neqgen, nbexci, idescf,&
                    nomfon, coefm, liad, inumor, 1,&
                    zr(jfext))
    endif
!
!   couplage avec edyos
    if (nbpal .gt. 0) then
        cpal='C_PAL'
!       recuperation des donnees sur les paliers
        call jeveuo(cpal, 'L', iadrk)
        do iapp = 1, nbpal
            fsauv(iapp,1)= 0.d0
            fsauv(iapp,2)= 0.d0
            fsauv(iapp,3)= 0.d0
            typal(iapp)=zk8(iadrk+(iapp-1))(1:6)
            finpal(iapp)=zk8(iadrk+(iapp-1)+palmax)(1:3)
            cnpal(iapp)=zk8(iadrk+(iapp-1)+2*palmax)(1:dimnas)
        enddo
    endif
!   fin couplage avec edyos
!
!   cas classique
    if (nbpal .ne. 0) nbchoc = 0
!   Si ce n'est pas une reprise : on calcule l'état initial
    if (.not. condrepri) then
        call mdfnli(neqgen, zr(jdepl), zr(jvite), zr(jacce), zr(jfext),&
                    nbchoc, logcho, dplmod, parcho, noecho,&
                    zr(jchor), nbrede, dplred, fonred, zr(jredr),&
                    zi(jredi), nbrevi, dplrev, fonrev, zr(jrevr),&
                    zi(jrevi), tinit, nofdep, nofvit, nofacc,&
                    nbexci, psidel, monmot, nbrfis, fk,&
                    dfk, angini, foncp, 1, nbpal,&
                    dt, dtsto, vrotat, typal, finpal,&
                    cnpal, prdeff, conv, fsauv)
    endif
!
    if ((conv.le.0.d0) .and. (nbconv.gt.nbmxcv)) then
        call utmess('F', 'EDYOS_46')
    else if ((conv.le.0.d0) .and. (nbconv.le.nbmxcv)) then
        nbconv = nbconv + 1
    endif
!
!   accélérations généralisées initiales : si pas de reprise on calcule
    if (.not. condrepri) then
        call mdacce(typbas, neqgen, pulsa2, masgen, descmm,&
                    riggen, descmr, zr(jfext), lamor, zr(jamgy),&
                    descma, zr(jtra1), zr(jdepl), zr(jvite), zr(jacce))
    endif
!
!   INITIALISATION DU TEMPS ET DT. L'UTILISATEUR IMPOSE UN DTMAX ?
    flagdt=.false.
    call getvr8('INCREMENT', 'PAS_MAXI', iocc=1, scal=r8bid1, nbret=npm)
    if (npm .ne. 0) flagdt=.true.
!
    temps = tinit
    if (flagdt .and. (dt.gt.dtmax)) dt=dtmax
    if (dt .lt. dtmin) dt=dtmin
!
!   ARCHIVAGE DONNEES INITIALES
    call mdarnl(isto1, iarchi, tinit, dt, neqgen,&
                zr(jdepl), zr(jvite), zr(jacce), isto2, nbchoc,&
                zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                zi(jredi), isto4, nbrevi, zr(jrevr), zi(jrevi),&
                zr(jdeps), zr(jvits), zr(jaccs), zr(jpass), zi(jordr),&
                zr(jinst), zr(jfcho), zr(jdcho), zr(jvcho), zi(jicho),&
                zr(jvint), zi(jredc), zr(jredd), zi(jrevc), zr(jrevv))
!
    isto1 = isto1 + 1
    iarchi = iarchi + 1
!
!   INITIALISATION AVANT LA BOUCLE DE L'ETAT INITIAL
    call dcopy(neqgen, zr(jvite), 1, zr(jviti), 1)
    call dcopy(neqgen, zr(jdepl), 1, zr(jdepi), 1)
    call dcopy(neqgen, zr(jacce), 1, zr(jacci), 1)
!   Pour les parametres de choc et les variables internes
    if (nbchoc .ne. 0) then
        nbschor = nbchoc*(mdtr74grd('SCHOR')+mdtr74grd('MAXVINT'))
        call dcopy(nbschor, zr(jchor), 1, zr(jchoi), 1)
    endif
!
!   BOUCLE TEMPORELLE
    okprem = .true.
6666 continue
    if (temps .lt. tfin) then
!       GESTION DU DERNIER PAS DE TEMPS
        if (temps+dt .ge. tfin) dt=tfin-temps
!       ESTIMATION DE L'ETAT ET DE L'ERREUR A L'INSTANT SUIVANT
        if (method(13:14) .eq. '54') then
            call mddp54(neqgen, zr(jdepl), zr(jvite), zr(jacce), zr(jfext),&
                        dt, dtsto, nbexci, idescf, nomfon,&
                        coefm, liad, inumor, nbchoc, logcho,&
                        dplmod, parcho, noecho, zr(jchor), nbrede,&
                        dplred, fonred, zr(jredr), zi(jredi), nbrevi,&
                        dplrev, fonrev, zr(jrevr), zi(jrevi), nofdep,&
                        nofvit, nofacc, psidel, monmot, nbrfis,&
                        fk, dfk, angini, foncp, nbpal,&
                        vrotat, typal, finpal, cnpal, prdeff,&
                        conv, fsauv, typbas, pulsa2, masgen,&
                        descmm, riggen, descmr, lamor, descma,&
                        zr(jtra1), temps, tol, zr(jdepi), zr(jviti),&
                        zr(jkde), zr(jkvi), fonca, foncv, iarchi,&
                        zr(jrigy), zr(jamgy), nbconv, nbmxcv, vitvar,&
                        gyogen, rgygen, amogen, errt)
        else if (method(13:14).eq.'32') then
            call mdbs32(neqgen, zr(jdepl), zr(jvite), zr(jacce), zr(jfext),&
                        dt, dtsto, nbexci, idescf, nomfon,&
                        coefm, liad, inumor, nbchoc, logcho,&
                        dplmod, parcho, noecho, zr(jchor), nbrede,&
                        dplred, fonred, zr(jredr), zi(jredi), nbrevi,&
                        dplrev, fonrev, zr(jrevr), zi(jrevi), nofdep,&
                        nofvit, nofacc, psidel, monmot, nbrfis,&
                        fk, dfk, angini, foncp, nbpal,&
                        vrotat, typal, finpal, cnpal, prdeff,&
                        conv, fsauv, typbas, pulsa2, masgen,&
                        descmm, riggen, descmr, lamor, descma,&
                        zr(jtra1), temps, tol, zr(jdepi), zr(jviti),&
                        zr(jkde), zr(jkvi), fonca, foncv, iarchi,&
                        zr(jrigy), zr(jamgy), nbconv, nbmxcv, vitvar,&
                        gyogen, rgygen, amogen, errt)
        endif
!
!       ON PASSE A L'INSTANT SUIVANT OU ON ADAPTE LE PAS?
        if ((errt.lt.1.d0) .or. (dt.le.dtmin) .or. (.not.adapt)) then
            call dcopy(neqgen, zr(jacce), 1, zr(jacci), 1)
            call dcopy(neqgen, zr(jvite), 1, zr(jviti), 1)
            call dcopy(neqgen, zr(jdepl), 1, zr(jdepi), 1)
!           Pour les parametres de choc et les variables internes
            if (nbchoc .ne. 0) then
                nbschor = nbchoc*(mdtr74grd('SCHOR')+mdtr74grd('MAXVINT'))
                call dcopy(nbschor, zr(jchor), 1, zr(jchoi), 1)
            endif
!
            temps=temps+dt
!
            if (isto1 .lt. (nbsauv)) then
                call mdarnl(isto1, iarchi, temps, dt, neqgen,&
                            zr(jdepl), zr(jvite), zr(jacce), isto2, nbchoc,&
                            zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                            zi(jredi), isto4, nbrevi, zr(jrevr), zi(jrevi),&
                            zr(jdeps), zr(jvits), zr(jaccs), zr( jpass), zi(jordr),&
                            zr(jinst), zr(jfcho), zr(jdcho), zr( jvcho), zi(jicho),&
                            zr(jvint), zi(jredc), zr(jredd), zi( jrevc), zr(jrevv))
                isto1 = isto1 + 1
                iarchi = iarchi + 1
            else
                isto1 = 0
                isto2 = 0
                isto3 = 0
                isto4 = 0
                nbobjs = nbobjs + 1
                call codent(nbobjs, 'D0', intk)
!
                namerk ='&&RK'//intk
                nbsauv = int(nbsauv*facobj)
                nbscho = nbsauv * 3 * nbchoc
!
                call mdallo(namerk, 'TRAN', nbsauv, sauve='VOLA', checkarg=.false._1,&
                            method=method, base=basemo, nbmodes=neqgen, rigi=nomrig, mass=nommas,&
                            amor=nomamo, jordr=jordr, jdisc=jinst, jdepl=jdeps, jvite=jvits,&
                            jacce=jaccs, dt=dt, jptem=jpass, nbchoc=nbchoc, noecho=noecho,&
                            intitu=intitu, jfcho=jfcho, jdcho=jdcho, jvcho=jvcho, jadcho=jicho,&
                            nbrede=nbrede, fonred=fonred, jredc=jredc, jredd=jredd,&
                            nbrevi=nbrevi, fonrev=fonrev, jrevc=jrevc, jrevv=jrevv)
!               Le pointeur des variables internes n'est pas en OUT de mdallo.
!               Il faut le mettre à jour sinon on cartonne. La taille du tableau est *facobj
                if (nbchoc .ne. 0) then
                    call jeveuo(namerk//'           .VINT', 'E', jvint)
                endif
!
                call mdarnl(isto1, iarchi, temps, dt, neqgen,&
                            zr(jdepl), zr(jvite), zr(jacce), isto2, nbchoc,&
                            zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                            zi(jredi), isto4, nbrevi, zr( jrevr), zi(jrevi),&
                            zr(jdeps), zr(jvits), zr(jaccs), zr( jpass), zi(jordr),&
                            zr(jinst), zr(jfcho), zr(jdcho), zr( jvcho), zi(jicho),&
                            zr(jvint), zi(jredc), zr(jredd), zi( jrevc), zr(jrevv))
                isto1 = isto1 + 1
                iarchi = iarchi + 1
            endif
!
        else
!           On ne passe pas a l'instant suivant. On remet a l'état du pas de temps courant les
!           valeurs de l'accélération, de la vitesse et du déplacement (jacce, jvite, jdepl)
            call dcopy(neqgen, zr(jacci), 1, zr(jacce), 1)
            call dcopy(neqgen, zr(jviti), 1, zr(jvite), 1)
            call dcopy(neqgen, zr(jdepi), 1, zr(jdepl), 1)
!           Même chose pour les parametres de choc et les variables internes
            if (nbchoc .ne. 0) then
                nbschor = nbchoc*(mdtr74grd('SCHOR')+mdtr74grd('MAXVINT'))
                call dcopy(nbschor, zr(jchoi), 1, zr(jchor), 1)
            endif
        endif
!
!       ESTIMATION DU PROCHAIN PAS DE TEMPS
        if (adapt) then
!           puissance pour le calcul du dt adaptatif
            if (method(13:14) .eq. '54') pow=1.d0/6.d0
            if (method(13:14) .eq. '32') pow=1.d0/4.d0
!           on empeche des changements brutaux de pas de temps
!                   augmentation maximale de : 5.0*dt
!                   diminution   maximale de : 0.2*dt
            seuil1 = (0.9/5.0d0)**(1.0d0/pow)
            seuil2 = (0.9/0.2d0)**(1.0d0/pow)
            if (errt .lt. seuil1) then
                coeff = 5.0d0
            else if (errt .gt. seuil2) then
                coeff = 0.2d0
            else
                coeff=0.9d0*(1.d0/errt)**pow
            endif
            dt = dt * coeff
!
            if ((dt.le.dtmin) .and. (abs(tfin-(temps+dt)).gt.epsi)) then
                call utmess('F', 'ALGORITH5_23')
            endif
        endif
!       BLOCAGE DE DT A DTMAX SI DEMANDE PAR L'UTILISATEUR
        if (flagdt .and. (dt.gt.dtmax)) dt=dtmax
!
!       VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
        if (etausr() .eq. 1) then
            call concrk(nomres, iarchi, facobj, nbobjs, '&&RK',&
                        nbsaui, basemo, nommas, nomrig, nomamo,&
                        neqgen, dt, nbchoc, noecho, intitu,&
                        nbrede, fonred, nbrevi, fonrev, method)
!
            call sigusr()
        endif
!
        goto 6666
!
    endif
!
!   concatenation de tous les resultats dans un seul objet sur base globale de bonne taille
    call concrk(nomres, iarchi, facobj, nbobjs, '&&RK',&
                nbsaui, basemo, nommas, nomrig, nomamo,&
                neqgen, dt, nbchoc, noecho, intitu,&
                nbrede, fonred, nbrevi, fonrev, method)
!   Le nombre de sauvegarde est iarchi
    nbsauv = iarchi
!
9999 continue
!
    call jedetr('&&RUKUT.DEPL')
    call jedetr('&&RUKUT.VITE')
    call jedetr('&&RUKUT.ACCE')
    call jedetr('&&RUKUT.TRA1')
    call jedetr('&&RUKUT.FEXT')
    call jedetr('&&RUKUT.MASS')
    call jedetr('&&RUKUT.AMOGYR')
    call jedetr('&&RUKUT.RIGGYR')
!
    call jedetr('&&RUKUT.DEPI')
    call jedetr('&&RUKUT.VITI')
    call jedetr('&&RUKUT.ACCI')
    call jedetr('&&RUKUT.ERDE')
    call jedetr('&&RUKUT.ERVI')
    call jedetr('&&RUKUT.KKDE')
    call jedetr('&&RUKUT.KKVI')
!
    if (nbchoc .ne. 0) then
        call jedetr('&&RUKUT.SCHOR')
        call jedetr('&&RUKUT.SCHOI')
    endif
    if (nbrede .ne. 0) then
        call jedetr('&&RUKUT.SREDR')
        call jedetr('&&RUKUT.SREDI')
    endif
    if (nbrevi .ne. 0) then
        call jedetr('&&RUKUT.SREVR')
        call jedetr('&&RUKUT.SREVI')
    endif
!
    if (iret .ne. 0) then
        call utmess('F', 'ALGORITH5_24')
    endif
    call jedema()
end subroutine

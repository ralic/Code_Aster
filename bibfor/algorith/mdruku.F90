subroutine mdruku(method, tinit, tfin, dt, dtmin,&
                  dtmax, nbsauv, nbobjs, neqgen, pulsat,&
                  pulsa2, masgen, descmm, riggen, descmr,&
                  rgygen, lamor, amogen, descma, gyogen,&
                  foncv, fonca, typbas, basemo, lflu,&
                  nbchoc, intitu, logcho, dplmod, parcho,&
                  noecho, nbrede, dplred, parred, fonred,&
                  nbrevi, dplrev, fonrev, coefm, liad,&
                  inumor, idescf, nofdep, nofvit, nofacc,&
                  nomfon, psidel, monmot, nbrfis, fk,&
                  dfk, angini, foncp, nbpal, dtsto,&
                  vrotat, prdeff, nomres, nbexci, nommas,&
                  nomrig, nomamo)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! aslint: disable=W1501,W1504
    implicit none
#include "jeveux.h"
!
#include "asterc/etausr.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8prem.h"
#include "asterfort/codent.h"
#include "asterfort/concrk.h"
#include "asterfort/fointe.h"
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
#include "asterfort/preres.h"
#include "asterfort/r8inir.h"
#include "asterfort/sigusr.h"
#include "asterfort/trlds.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: descmm, descmr, descma, palmax, nbrede, nbrevi, dimnas
    integer :: neqgen, nbsauv, nbchoc, nbrfis, nbexci, logcho(nbchoc, *)
    integer :: liad(*), inumor(*), idescf(*), nbpal, nbobjs, iapp, iarchi, isto1
    integer :: isto2, isto3, isto4, jchor, jredi, jredr, jrevi, jrevr, jvint
    integer :: nbconv, nbmxcv, nbsaui, nbscho, im, ind, iadrk, iarg, ibid
    integer :: jacce, ier, iret, jacci, jaccs, jamgy, jdcho, jdepi, jdepl, jdeps
    integer :: jerde, jervi, jfcho, jfext, jicho, jinst, jkde, jkvi, jm, jmass
    integer :: jordr, jpass, jredc, jredd, jrevc, jrevd, jrigy, jtra1, jvcho
    integer :: jvite, jviti, jvits, n1, npm
    parameter    (palmax=20)
    parameter    (dimnas=8)
    real(kind=8) :: pulsat(*), pulsa2(*), masgen(*), riggen(*), amogen(*)
    real(kind=8) :: parcho(*), parred(*), dplrev(*), dplred(*), rgygen(*)
    real(kind=8) :: dplmod(nbchoc, neqgen, *), gyogen(*), dt, dt2, dtsto, tfin
    real(kind=8) :: vrotat, conv, facobj, tinit, angini, epsi, errt, r8bid1
    real(kind=8) :: temps, coefm(*), psidel(*), deux, pow, fsauv(palmax, 3)
    real(kind=8) :: vrotin, arotin, dtmax, dtmin, tol
    logical :: lamor, lflu, prdeff, adapt, flagdt
    character(len=3) :: finpal(palmax)
    character(len=4) :: intk, nomsym(3)
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
!
!
! ======================================================================
!
    call jemarq()
!
    nbobjs=1
    call codent(nbobjs, 'D0', intk)
    namerk='&&RK'//intk
    call mdallo(namerk, basemo, nommas, nomrig, nomamo,&
                neqgen, dt, nbsauv, nbchoc, noecho,&
                intitu, nbrede, fonred, nbrevi, fonrev,&
                jdeps, jvits, jaccs, jpass, jordr,&
                jinst, jfcho, jdcho, jvcho, jicho,&
                jredc, jredd, jrevc, jrevd, method,&
                ibid, nomsym, 'TRAN', 'VOLA')
!
!
    deux = 2.d0
    epsi = r8prem()
    jchor = 1
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
!     PUISSANCE POUR LE CALCUL DU DT ADAPTATIF
    if (method(13:14) .eq. '54') pow=1.d0/6.d0
    if (method(13:14) .eq. '32') pow=1.d0/4.d0
!     RAPPORT D'AUGMENTATION DES OBJETS VOLATILES
    facobj=1.5d0
!  COUPLAGE EDYOS : CONVERGENCE EDYOS :
    conv = 1.d0
    nbconv = 0
!  COUPLAGE EDYOS : NOMBRE MAXIMAL DE TENTATIVES DE REPRISE DES DONNEES
!  PRECEDENTES EN CAS DE NON-CONVERGENCE EDYOS :
    nbmxcv = 10
!     PAS DE LAME FLUIDE POUR LE SCHEMA RUNGE-KUTTA
    if (lflu) then
        call u2mess('F', 'ALGORITH5_21')
    endif
!
    do 111 iapp = 1, palmax
        typal(iapp)='      '
        finpal(iapp)='   '
        cnpal(iapp)=' '
111  continue
!
!  GESTION DE LA VITESSE VARIABLE MACHINES TOURNANTES
    call wkvect('&&RUKUT.AMOGYR', 'V V R8', neqgen*neqgen, jamgy)
    call wkvect('&&RUKUT.RIGGYR', 'V V R8', neqgen*neqgen, jrigy)
    if (lamor) then
        do 100 im = 1, neqgen
            amogen(im) = deux * amogen(im) * pulsat(im)
100      continue
    else
        call getvtx(' ', 'VITESSE_VARIABLE', 1, iarg, 0,&
                    vitvar, n1)
        if (n1 .ne. 0) then
            call getvtx(' ', 'VITESSE_VARIABLE', 1, iarg, 1,&
                        vitvar, n1)
        else
            vitvar=' '
        endif
        vrotin = 0.d0
        arotin = 0.d0
        if (vitvar .eq. 'OUI') then
            call fointe('F ', foncv, 1, 'INST', tinit,&
                        vrotin, ier)
            call fointe('F ', fonca, 1, 'INST', tinit,&
                        arotin, ier)
            do 113 im = 1, neqgen
                do 114 jm = 1, neqgen
                    ind = jm + neqgen*(im-1)
                    zr(jamgy+ind-1) = amogen(ind) + vrotin * gyogen( ind)
                    zr(jrigy+ind-1) = riggen(ind) + arotin * rgygen( ind)
114              continue
113          continue
        else
            do 117 im = 1, neqgen
                do 118 jm = 1, neqgen
                    ind = jm + neqgen*(im-1)
                    zr(jamgy+ind-1) = amogen(ind)
                    zr(jrigy+ind-1) = riggen(ind)
118              continue
117          continue
        endif
    endif
!
!     --- FACTORISATION DE LA MATRICE MASSE ---
!
    if (typbas .eq. 'BASE_MODA') then
        call wkvect('&&RUKUT.MASS', 'V V R8', neqgen*neqgen, jmass)
        call dcopy(neqgen*neqgen, masgen, 1, zr(jmass), 1)
        call trlds(zr(jmass), neqgen, neqgen, iret)
        if (iret .ne. 0) then
            call u2mess('F', 'ALGORITH5_22')
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
!       ICI IL FAUT NORMALEMENT TRAITER LE CAS DE LAME FLUIDE
!       MAIS IL N'EST PAS PREVU DANS RUNGE-KUTTA
    endif
!
!     --- PARAMETRES DU SCHEMA ADAPTATIF ---
!
    call getvr8('SCHEMA_TEMPS', 'TOLERANCE', 1, iarg, 1,&
                tol, n1)
!      ON LAISSE L'OPTION POUR SCHEMA ADAPTATIF OU PAS
    adapt=.true.
!
!     --- FACTORISATION DE LA MATRICE MASSE ---
!
!     --- VECTEURS DE TRAVAIL ---
!
    call wkvect('&&RUKUT.DEPL', 'V V R8', neqgen, jdepl)
    call wkvect('&&RUKUT.VITE', 'V V R8', neqgen, jvite)
    call wkvect('&&RUKUT.ACCE', 'V V R8', neqgen, jacce)
    call wkvect('&&RUKUT.TRA1', 'V V R8', neqgen, jtra1)
    call wkvect('&&RUKUT.FEXT', 'V V R8', neqgen, jfext)
!
    if (nbchoc .ne. 0 .and. nbpal .eq. 0) then
        call wkvect('&&RUKUT.SCHOR', 'V V R8', nbchoc*14, jchor)
!        INITIALISATION POUR LE FLAMBAGE
        call jeveuo(namerk//'           .VINT', 'E', jvint)
        call r8inir(nbchoc, 0.d0, zr(jvint), 1)
    endif
    if (nbrede .ne. 0) then
        call wkvect('&&RUKUT.SREDR', 'V V R8', nbrede, jredr)
        call wkvect('&&RUKUT.SREDI', 'V V I', nbrede, jredi)
    endif
    if (nbrevi .ne. 0) then
        call wkvect('&&RUKUT.SREVR', 'V V R8', nbrevi, jrevr)
        call wkvect('&&RUKUT.SREVI', 'V V I', nbrevi, jrevi)
    endif
!    --- VECTEURS SPECIFIQUES A RUNGE-KUTTA ---
    call wkvect('&&RUKUT.DEPI', 'V V R8', neqgen, jdepi)
    call wkvect('&&RUKUT.VITI', 'V V R8', neqgen, jviti)
    call wkvect('&&RUKUT.ACCI', 'V V R8', neqgen, jacci)
    call wkvect('&&RUKUT.ERDE', 'V V R8', neqgen, jerde)
    call wkvect('&&RUKUT.ERVI', 'V V R8', neqgen, jervi)
    if (method(13:14) .eq. '54') then
        call wkvect('&&RUKUT.KKDE', 'V V R8', neqgen*6, jkde)
        call wkvect('&&RUKUT.KKVI', 'V V R8', neqgen*6, jkvi)
    else if (method(13:14).eq.'32') then
        call wkvect('&&RUKUT.KKDE', 'V V R8', neqgen*3, jkde)
        call wkvect('&&RUKUT.KKVI', 'V V R8', neqgen*3, jkvi)
    endif
!     --- CONDITIONS INITIALES ---
!
    call mdinit(basemo, neqgen, nbchoc, zr(jdepl), zr(jvite),&
                zr(jvint), iret, tinit)
    if (iret .ne. 0) goto 9999
    if (nbchoc .gt. 0 .and. nbpal .eq. 0) then
        call dcopy(nbchoc, zr(jvint), 1, zr(jchor+13*nbchoc), 1)
    endif
!
!     --- FORCES EXTERIEURES ---
!
    if (nbexci .ne. 0) then
        call mdfext(tinit, r8bid1, neqgen, nbexci, idescf,&
                    nomfon, coefm, liad, inumor, 1,&
                    zr(jfext))
    endif
!
!   COUPLAGE AVEC EDYOS
!
    if (nbpal .gt. 0) then
        cpal='C_PAL'
!     RECUPERATION DES DONNEES SUR LES PALIERS
!     -------------------------------------------------
        call jeveuo(cpal, 'L', iadrk)
        do 21 iapp = 1, nbpal
            fsauv(iapp,1)= 0.d0
            fsauv(iapp,2)= 0.d0
            fsauv(iapp,3)= 0.d0
            typal(iapp)=zk8(iadrk+(iapp-1))(1:6)
            finpal(iapp)=zk8(iadrk+(iapp-1)+palmax)(1:3)
            cnpal(iapp)=zk8(iadrk+(iapp-1)+2*palmax)(1:dimnas)
21      continue
    endif
!  FIN COUPLAGE AVEC EDYOS
!
!       CAS CLASSIQUE
!
    if (nbpal .ne. 0) nbchoc = 0
    call mdfnli(neqgen, zr(jdepl), zr(jvite), zr(jacce), zr(jfext),&
                masgen, r8bid1, pulsa2, zr(jamgy), nbchoc,&
                logcho, dplmod, parcho, noecho, zr(jchor),&
                nbrede, dplred, parred, fonred, zr(jredr),&
                zi(jredi), nbrevi, dplrev, fonrev, tinit,&
                nofdep, nofvit, nofacc, nbexci, psidel,&
                monmot, nbrfis, fk, dfk, angini,&
                foncp, 1, nbpal, dt, dtsto,&
                vrotat, typal, finpal, cnpal, prdeff,&
                conv, fsauv)
    if ((conv.le.0.d0) .and. (nbconv.gt.nbmxcv)) then
        call u2mess('F', 'EDYOS_46')
    else if ((conv.le.0.d0) .and. (nbconv.le.nbmxcv)) then
        nbconv = nbconv + 1
    endif
!
!     --- ACCELERATIONS GENERALISEES INITIALES ---
!
    call mdacce(typbas, neqgen, pulsa2, masgen, descmm,&
                riggen, descmr, zr(jfext), lamor, zr(jamgy),&
                descma, zr(jtra1), zr(jdepl), zr(jvite), zr(jacce))
!
!      ENDIF
!
!--- INITIALISATION DU TEMPS ET DT ---
!
!     L'UTILISATEUR IMPOSE UN DTMAX?
    flagdt=.false.
    call getvr8('INCREMENT', 'PAS_MAXI', 1, iarg, 1,&
                r8bid1, npm)
    if (npm .ne. 0) flagdt=.true.
!
    temps = tinit
    if (flagdt .and. (dt.gt.dtmax)) dt=dtmax
    if (dt .lt. dtmin) dt=dtmin
!
!     --- ARCHIVAGE DONNEES INITIALES ---
!
    call mdarnl(isto1, iarchi, tinit, dt, neqgen,&
                zr(jdepl), zr(jvite), zr(jacce), isto2, nbchoc,&
                zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                zi(jredi), isto4, nbrevi, zr(jrevr), zi(jrevi),&
                zr(jdeps), zr(jvits), zr(jaccs), zr(jpass), zi(jordr),&
                zr(jinst), zr(jfcho), zr(jdcho), zr(jvcho), zi(jicho),&
                zr(jvint), zi(jredc), zr(jredd), zi(jrevc), zr(jrevd))
!
    isto1 = isto1 + 1
    iarchi = iarchi + 1
!
!     INITIALISATION AVANT LA BOUCLE DE L'ETAT INITIAL
    call dcopy(neqgen, zr(jvite), 1, zr(jviti), 1)
    call dcopy(neqgen, zr(jdepl), 1, zr(jdepi), 1)
    call dcopy(neqgen, zr(jacce), 1, zr(jacci), 1)
!
!     --- BOUCLE TEMPORELLE ---
!
6666  continue
    if (temps .lt. tfin) then
!      GESTION DU DERNIER PAS DE TEMPS
        if (temps+dt .ge. tfin) dt=tfin-temps
!
! ESTIMATION DE L'ETAT ET DE L'ERREUR A L'INSTANT SUIVANT
        if (method(13:14) .eq. '54') then
            call mddp54(neqgen, zr(jdepl), zr(jvite), zr(jacce), zr(jfext),&
                        dt, dtsto, lflu, nbexci, idescf,&
                        nomfon, coefm, liad, inumor, nbchoc,&
                        logcho, dplmod, parcho, noecho, zr(jchor),&
                        nbrede, dplred, parred, fonred, zr(jredr),&
                        zi(jredi), nbrevi, dplrev, fonrev, nofdep,&
                        nofvit, nofacc, psidel, monmot, nbrfis,&
                        fk, dfk, angini, foncp, nbpal,&
                        vrotat, typal, finpal, cnpal, prdeff,&
                        conv, fsauv, typbas, pulsa2, masgen,&
                        descmm, riggen, descmr, lamor, descma,&
                        zr(jtra1), temps, tol, zr(jdepi), zr(jviti),&
                        zr(jerde), zr(jervi), zr(jkde), zr(jkvi), fonca,&
                        foncv, iarchi, zr(jrigy), zr(jamgy), nbconv,&
                        nbmxcv, vitvar, gyogen, rgygen, amogen,&
                        errt)
        else if (method(13:14).eq.'32') then
            call mdbs32(neqgen, zr(jdepl), zr(jvite), zr(jacce), zr(jfext),&
                        dt, dtsto, lflu, nbexci, idescf,&
                        nomfon, coefm, liad, inumor, nbchoc,&
                        logcho, dplmod, parcho, noecho, zr(jchor),&
                        nbrede, dplred, parred, fonred, zr(jredr),&
                        zi(jredi), nbrevi, dplrev, fonrev, nofdep,&
                        nofvit, nofacc, psidel, monmot, nbrfis,&
                        fk, dfk, angini, foncp, nbpal,&
                        vrotat, typal, finpal, cnpal, prdeff,&
                        conv, fsauv, typbas, pulsa2, masgen,&
                        descmm, riggen, descmr, lamor, descma,&
                        zr(jtra1), temps, tol, zr(jdepi), zr(jviti),&
                        zr(jerde), zr(jervi), zr(jkde), zr(jkvi), fonca,&
                        foncv, iarchi, zr(jrigy), zr(jamgy), nbconv,&
                        nbmxcv, vitvar, gyogen, rgygen, amogen,&
                        errt)
        endif
!
!     ON PASSE A L'INSTANT SUIVANT OU ON ADAPTE LE PAS?
!
        if ((errt.lt.1.d0) .or. (dt.le.dtmin) .or. (.not.adapt)) then
            call dcopy(neqgen, zr(jacce), 1, zr(jacci), 1)
            call dcopy(neqgen, zr(jvite), 1, zr(jviti), 1)
            call dcopy(neqgen, zr(jdepl), 1, zr(jdepi), 1)
!
            temps=temps+dt
!
            if (isto1 .lt. (nbsauv)) then
!
                call mdarnl(isto1, iarchi, temps, dt, neqgen,&
                            zr(jdepl), zr(jvite), zr(jacce), isto2, nbchoc,&
                            zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                            zi(jredi), isto4, nbrevi, zr( jrevr), zi(jrevi),&
                            zr(jdeps), zr(jvits), zr(jaccs), zr( jpass), zi(jordr),&
                            zr(jinst), zr(jfcho), zr(jdcho), zr( jvcho), zi(jicho),&
                            zr(jvint), zi(jredc), zr(jredd), zi( jrevc), zr(jrevd))
                isto1 = isto1 + 1
                iarchi = iarchi + 1
!
            else
!
                isto1 = 0
                isto2 = 0
                isto3 = 0
                isto4 = 0
                nbobjs = nbobjs + 1
                call codent(nbobjs, 'D0', intk)
!
                namerk='&&RK'//intk
                nbsauv = int(nbsauv*facobj)
                nbscho = nbsauv * 3 * nbchoc
!
                call mdallo(namerk, basemo, nommas, nomrig, nomamo,&
                            neqgen, dt, nbsauv, nbchoc, noecho,&
                            intitu, nbrede, fonred, nbrevi, fonrev,&
                            jdeps, jvits, jaccs, jpass, jordr,&
                            jinst, jfcho, jdcho, jvcho, jicho,&
                            jredc, jredd, jrevc, jrevd, method,&
                            ibid, nomsym, 'TRAN', 'VOLA')
!
                call mdarnl(isto1, iarchi, temps, dt, neqgen,&
                            zr(jdepl), zr(jvite), zr(jacce), isto2, nbchoc,&
                            zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                            zi(jredi), isto4, nbrevi, zr( jrevr), zi(jrevi),&
                            zr(jdeps), zr(jvits), zr(jaccs), zr( jpass), zi(jordr),&
                            zr(jinst), zr(jfcho), zr(jdcho), zr( jvcho), zi(jicho),&
                            zr(jvint), zi(jredc), zr(jredd), zi( jrevc), zr(jrevd))
                isto1 = isto1 + 1
                iarchi = iarchi + 1
            endif
!
        else
!     ON REMET A L'ETAT DU TEMPS COURANT LES VALEURS DE JACCE ET JVITE
!     CAR ELLES ONT ETE MODIFIEES ENTRETEMPS DANS MDDP54 ET MDBS32
!     ALORS QU'ON NE PASSE PAS A L'INSTANT SUIVANT
            call dcopy(neqgen, zr(jviti), 1, zr(jvite), 1)
            call dcopy(neqgen, zr(jacci), 1, zr(jacce), 1)
!
        endif
!
!      ESTIMATION DU PROCHAIN PAS DE TEMPS
        if (adapt) then
            dt2=0.9d0*dt*(1.d0/errt)**pow
!         ON EMPECHE DES CHANGEMENTS BRUTAUX DE PAS DE TEMPS
            if (dt2 .gt. (5.d0*dt)) then
                dt=5.d0*dt
            else if (dt2.lt.(0.2d0*dt)) then
                dt=0.2d0*dt
            else
                dt=dt2
            endif
!
            if ((dt.le.dtmin) .and. (abs(tfin-(temps+dt)).gt.epsi)) then
                call u2mess('F', 'ALGORITH5_23')
            endif
        endif
!      BLOCAGE DE DT A DTMAX SI DEMANDE PAR L'UTILISATEUR
        if (flagdt .and. (dt.gt.dtmax)) dt=dtmax
!
!        --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1 ---
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
!     CONCATENATION DE TOUS LES RESULTATS DANS UN SEUL OBJET SUR BASE
!     GLOBALE DE LA BONNE TAILLE
    call concrk(nomres, iarchi, facobj, nbobjs, '&&RK',&
                nbsaui, basemo, nommas, nomrig, nomamo,&
                neqgen, dt, nbchoc, noecho, intitu,&
                nbrede, fonred, nbrevi, fonrev, method)
!
9999  continue
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
    call jedema()
end subroutine

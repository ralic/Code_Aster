subroutine ssdt74(nomres, nomcmd)
    implicit none
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
!
!     BUT: SOUS-STRUCTURATION DYNAMIQUE TRANSITOIRE
!          CALCUL TRANSITOIRE PAR DYNA_TRAN_MODAL EN SOUS-STRUCTURATION
!
! ----------------------------------------------------------------------
!
!
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/gettco.h"
#include "asterc/r8prem.h"
#include "asterfort/ajlagr.h"
#include "asterfort/dismoi.h"
#include "asterfort/dyarch.h"
#include "asterfort/extdia.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/limsst.h"
#include "asterfort/mdadap.h"
#include "asterfort/mdallo.h"
#include "asterfort/mdeul1.h"
#include "asterfort/mdgene.h"
#include "asterfort/mdptem.h"
#include "asterfort/mdrecf.h"
#include "asterfort/mdruku.h"
#include "asterfort/mgutdm.h"
#include "asterfort/mtdscr.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: descr, descm, desca
    integer :: vali(3)
    real(kind=8) :: acrit, agene
    real(kind=8) :: valr(2)
    real(kind=8) :: dt, dts, dtu, dtmax, dtmin
    character(len=8) :: k8b, kbid, nomres, masgen, riggen, amogen, monmot
    character(len=8) :: basemo, modgen, mastem, amotem, vecgen, resgen, bamo1
    character(len=8) :: bamo2
    character(len=14) :: numgen
    character(len=16) :: nomcmd, typbas, method
    character(len=19) :: raid, mass, amor, lisarc
    character(len=8) :: fbid(2)
    character(len=24) :: numg24, lisins
    character(len=24) :: valk(2)
    logical :: lamor
    integer :: nexcit, nexcir, ntotex, nbobjs
!
!  COUPLAGE EDYOS
! =>
    integer :: nbpal
    real(kind=8) :: vrotat, dtsto
    logical :: prdeff
! =<
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, ifm, info, iparch, iret
    integer :: j1, jaccs, jadcho, jamog, jarch
    integer :: jcoefm, jconl, jdcho, jdepl, jdeps, jfcho, jfond
    integer :: jfonv, jiadve, jidesc, jinst, jinti, jinumo, jmasg
    integer :: jnoacc, jnodep, jnoec, jnomfo, jnovit, jordr, jparc
    integer :: jpard, jpass, jpsdel, jpsid, jpul2, jpuls, jraig
    integer :: jranc, jredc, jredd, jrede, jrefa, jrevi, jscde
    integer :: jrevc, jrevv
    integer :: jvcho, jvec, jvecr, jvits, k, n1, namor
    integer :: nbbas, nbchoc, nbmode, nbmodi, nbmody, nbmost, nbpas
    integer :: nbrede, nbrevi, nbsauv, nbsst, neq, neqgen, nm
    integer :: nr, nv
    real(kind=8) :: deux, dtarch, omeg2, tfin, tinit
!-----------------------------------------------------------------------
    data k8b/'        '/
    data mastem/'MASSTEMP'/
    data amotem/'AMORTEMP'/
    call jemarq()
    deux = 2.d0
!-----------------------------------------------------------------------
    fbid(1)=k8b
    fbid(2)=k8b
!
    jinti = 1
    jranc = 1
    jparc = 1
    jnoec = 1
    jrede = 1
    jpard = 1
    jfond = 1
    jrevi = 1
    jfonv = 1
    jdepl = 1
    jcoefm = 1
    jiadve = 1
    jinumo = 1
    jidesc = 1
    jnodep = 1
    jnovit = 1
    jnomfo = 1
    jpsdel = 1
    jpsid = 1
    lamor = .false.
    nbobjs = 1
    lisins = ' '
    call infniv(ifm, info)
!
!     --- VERIFICATION DES DONNEES UTILISATEUR EN FONCTION DES LIMITES
!     --- DU CALCUL TRANSITOIRE PAR SOUS-STRUCTURATION
!
    call limsst(nomcmd)
!
!     --- RECUPERATION DES ARGUMENTS DE LA COMMANDE ---
!
    call getvtx('SCHEMA_TEMPS', 'SCHEMA', iocc=1, scal=method, nbret=n1)
    call getfac('EXCIT', nexcit)
    call getfac('EXCIT_RESU', nexcir)
    nbchoc = 0
    nbrede = 0
    nbrevi = 0
!
!     --- RECUPERATION DES MATRICES PROJETEES ---
!
    call getvid(' ', 'MATR_MASS', scal=masgen, nbret=nm)
    call getvid(' ', 'MATR_RIGI', scal=riggen, nbret=nr)
    call getvid(' ', 'MATR_AMOR', scal=amogen, nbret=namor)
    if (nexcit .ne. 0) then
        call wkvect('&&SSDT74.NOMVEC', 'V V K8', nexcit, jvec)
        do i = 1, nexcit
            call getvid('EXCIT', 'VECT_ASSE_GENE', iocc=i, scal=vecgen, nbret=nv)
            zk8(jvec-1+i) = vecgen
        end do
    endif
    if (nexcir .ne. 0) then
        call wkvect('&&SSDT74.NOMVER', 'V V K8', nexcir, jvecr)
        do i = 1, nexcir
            call getvid('EXCIT_RESU', 'RESULTAT', iocc=i, scal=resgen, nbret=nv)
            zk8(jvecr-1+i) = resgen
! ------- VERIF : LA BASE DE MODES ASSOCIEE EST CELLE DES MATRICES GENE
            call jeveuo(masgen//'           .REFA', 'L', j1)
            bamo1=zk24(j1-1+1)(1:8)
            call dismoi('BASE_MODALE', resgen, 'RESU_DYNA', repk=bamo2)
            if (bamo1 .ne. bamo2) then
                call utmess('F', 'ALGORITH17_18', si=i)
            endif
        end do
    endif
!
!     --- NUMEROTATION GENERALISEE ET NOMBRE DE MODES ---
!
    call jeveuo(masgen//'           .REFA', 'L', jrefa)
    numgen = zk24(jrefa-1+2)(1:14)
    call jeveuo(numgen//'.NUME.REFN', 'L', jrefa)
    modgen = zk24(jrefa-1+1)(1:8)
    call gettco(modgen, typbas)
    call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst)
    nbmode = 0
    nbmody = 0
    do k = 1, nbsst
        kbid = '        '
        call mgutdm(modgen, kbid, k, 'NOM_BASE_MODALE', ibid,&
                    basemo)
        call dismoi('NB_MODES_TOT', basemo, 'RESULTAT', repi=nbbas)
        nbmode = nbmode + nbbas
        call dismoi('NB_MODES_DYN', basemo, 'RESULTAT', repi=nbbas)
        nbmody = nbmody + nbbas
    end do
    nbmost = nbmode - nbmody
    call jeveuo(numgen//'.SLCS.SCDE', 'L', jscde)
    neqgen = zi(jscde-1+1)
!
!     --- RECOPIE DES GRANDEURS GENERALISEES ---
!
    call wkvect('&&SSDT74.MASSEGEN', 'V V R', neqgen, jmasg)
    call wkvect('&&SSDT74.RAIDEGEN', 'V V R', neqgen, jraig)
    call wkvect('&&SSDT74.AMORTGEN', 'V V R', neqgen, jamog)
    call wkvect('&&SSDT74.PULSATIO', 'V V R', neqgen, jpuls)
    call wkvect('&&SSDT74.PULSAT2', 'V V R', neqgen, jpul2)
    numg24(1:14) = numgen
    call extdia(masgen, numg24, 1, zr(jmasg))
    call extdia(riggen, numg24, 1, zr(jraig))
!
    do k = 1, neqgen
        zr(jpuls+k-1) = 0
        zr(jpul2+k-1) = 0
    end do
    nbmodi = 0
    do k = 1, nbsst
        kbid = '        '
        call mgutdm(modgen, kbid, k, 'NOM_BASE_MODALE', ibid,&
                    basemo)
        call dismoi('NB_MODES_DYN', basemo, 'RESULTAT', repi=nbbas)
        do i = 1, nbbas
            omeg2 = abs(zr(jraig+nbmodi+i-1)/zr(jmasg+nbmodi+i-1))
            zr(jpuls+nbmodi+i-1) = sqrt(omeg2)
            zr(jpul2+nbmodi+i-1) = omeg2
        end do
        call dismoi('NB_MODES_TOT', basemo, 'RESULTAT', repi=nbbas)
        nbmodi = nbmodi + nbbas
    end do
!
    if (namor .ne. 0) then
        call extdia(amogen, numg24, 1, zr(jamog))
        do i = 1, neqgen
            if (zr(jpuls+i-1) .gt. r8prem()) then
                acrit = deux*sqrt(zr(jmasg+i-1)*zr(jraig+i-1))
                agene = zr(jamog+i-1)
                if (agene .gt. acrit) then
                    vali (1) = i
                    valr (1) = agene
                    valr (2) = acrit
                    valk (1) = ' '
                    call utmess('A', 'ALGORITH16_38', sk=valk(1), si=vali(1), nr=2,&
                                valr=valr)
                endif
            endif
        end do
    endif
!
!     --- VERIFICATION DES DONNEES GENERALISEES ---
!
    if (namor .eq. 0) amogen = k8b
    call mdgene(k8b, nbmode, numgen, masgen, riggen,&
                amogen, nexcit, jvec, iret)
    if (iret .ne. 0) goto 60
!
!     --- RECUPERATION DES PARAMETRES D'EXCITATION
!
!     NTOTEX : NBRE D'EXCITATION TOTAL (EXCIT + EXCIT_RESU*NBMODE)
    ntotex = nexcit + nexcir*nbmode
    neq = 0
    if (nexcit .ne. 0) then
        call wkvect('&&SSDT74.COEFM', 'V V R8', ntotex, jcoefm)
        call wkvect('&&SSDT74.IADVEC', 'V V IS', ntotex, jiadve)
        call wkvect('&&SSDT74.INUMOR', 'V V IS', ntotex, jinumo)
        call wkvect('&&SSDT74.IDESCF', 'V V IS', ntotex, jidesc)
        call wkvect('&&SSDT74.NOMFON', 'V V K8', 2*ntotex, jnomfo)
        call wkvect(nomres//'           .FDEP', 'G V K8', 2*ntotex, jnodep)
        call wkvect(nomres//'           .FVIT', 'G V K8', 2*ntotex, jnovit)
        call wkvect(nomres//'           .FACC', 'G V K8', 2*ntotex, jnoacc)
        call mdrecf(nexcit, nexcir, zi(jidesc), zk8(jnomfo), zr(jcoefm),&
                    zi(jiadve), zi(jinumo), zk8(jnodep), zk8(jnovit), zk8(jnoacc),&
                    neq, typbas, basemo, nbmody, zr(jraig),&
                    monmot, nomres)
        call jeexin(nomres//'           .IPSD', iret)
        if (iret .ne. 0) call jeveuo(nomres//'           .IPSD', 'E', jpsdel)
    endif
!
!     --- VERIFICATION DU PAS DE TEMPS ---
!
    call mdptem(neqgen, zr(jmasg), zr(jpuls), nbchoc, zr(jdepl),&
                zr(jparc), zk8(jnoec), dt, dts, dtu,&
                dtmax, dtmin, tinit, tfin, nbpas,&
                info, iret, lisins)
    if (iret .ne. 0) goto 60
!
!     --- ARCHIVAGE ---
    if (method(1:5) .eq. 'ADAPT') then
        call getvis('ARCHIVAGE', 'PAS_ARCH', iocc=1, scal=iparch, nbret=n1)
        if (n1 .eq. 0) iparch = 1
        dtarch = dtmax*iparch
        nbsauv = int((tfin-tinit)/dtarch) + 1
        if ((tfin - (tinit+(nbsauv-1)*dtarch)) .ge. r8prem()) then
            nbsauv=nbsauv+1
        endif
    else if (method(1:5).eq.'RUNGE') then
!         DANS LE CAS RUNGE ON ARCHIVE TOUS LES PAS DE CALCUL
        iparch = 1
        if (dt .gt. dtmax) then
            nbsauv = int((tfin-tinit)/dtmax) + 1
        else
            nbsauv = nbpas+1
        endif
    else
        lisarc = '&&SSDT74.ARCHIVAGE'
        call dyarch(nbpas, lisins, lisarc, nbsauv, 0,&
                    ibid, k8b)
        call jeveuo(lisarc, 'E', jarch)
    endif
!
!     --- AJOUT DES "LAGRANGE" DANS LA MATRICE DE MASSE ---
!
    call ajlagr(riggen, masgen, mastem)
!
!     --- TRAITEMENT DE LA MATRICE D'AMORTISSEMENT ---
!
    if (namor .ne. 0) then
        call ajlagr(riggen, amogen, amotem)
!
! ----- ON MODIFIE LE CONDITIONNEMENT POUR RENDRE CRITIQUE
! ----- L'AMORTISSEMENT ASSOCIE AUX "LAGRANGE"
!
        call jeveuo(amotem//'           .CONL', 'E', jconl)
        do i = 1, neqgen
            zr(jconl-1+i) = zr(jconl-1+i)/deux
        end do
    endif
!
!     --- RECUPERATION DES DESCRIPTEURS DES MATRICES ---
!
    raid = riggen//'           '
    mass = mastem//'           '
    if (namor .ne. 0) amor = amotem//'           '
!
    call mtdscr(raid)
    call jeveuo(raid(1:19)//'.&INT', 'E', descr)
    call mtdscr(mass)
    call jeveuo(mass//'.REFA', 'E', jrefa)
    zk24(jrefa-1+7)='&&OP0074.SOLVEUR'
    call jeveuo(mass(1:19)//'.&INT', 'E', descm)
    if (namor .ne. 0) then
        call mtdscr(amor)
        call jeveuo(amor(1:19)//'.&INT', 'E', desca)
    else
        desca = 0
    endif
!
!     --- ALLOCATION DES VECTEURS DE SORTIE ---
!
    if (namor .eq. 0) amogen = k8b
!
    if (method(1:5) .ne. 'RUNGE') then
        call mdallo(nomres, 'TRAN', nbsauv, sauve='GLOB', checkarg=.false.,&
                    method=method, base=numgen, nbmodes=neqgen, rigi=riggen, mass=masgen,&
                    amor=amogen, jordr=jordr, jdisc=jinst, jdepl=jdeps, jvite=jvits,&
                    jacce=jaccs, dt=dt, jptem=jpass, nbchoc=nbchoc, noecho=zk8(jnoec),&
                    intitu=zk8(jinti), jfcho=jfcho, jdcho=jdcho, jvcho=jvcho, jadcho=jadcho,&
                    nbrede=nbrede, fonred=zk8(jfond), jredc=jredc, jredd=jredd, nbrevi=nbrevi,&
                    fonrev=zk8(jfonv), jrevc=jrevc, jrevv=jrevv)

!     DANS LE CAS DE RUNGE KUTTA, L'ALLOCATION SE FAIT A L'INTERIEUR DE
!     LA ROUTINE MDRUKU
    endif
!
    if (info .eq. 1 .or. info .eq. 2) then
        valk (1) = numgen
        valk (2) = method
        vali (1) = neqgen
        vali (2) = nbmody
        vali (3) = nbmost
        call utmess('I', 'ALGORITH16_39', nk=2, valk=valk, ni=3,&
                    vali=vali)
        if (method(1:5) .eq. 'ADAPT') then
            valr (1) = dt
            call utmess('I', 'ALGORITH16_40', sr=valr(1))
        else
            valr (1) = dt
            vali (1) = nbpas
            call utmess('I', 'ALGORITH16_41', si=vali(1), sr=valr(1))
        endif
        vali (1) = nbsauv
        call utmess('I', 'ALGORITH16_42', si=vali(1))
        if (nbchoc .ne. 0) then
            vali(1) = nbchoc
            call utmess('I', 'ALGORITH16_80', si=vali(1))
        endif
        if (nbrede .ne. 0) then
            vali(1) = nbrede
            call utmess('I', 'ALGORITH16_83', si=vali(1))
        endif
        if (nbrevi .ne. 0) then
            vali(1) = nbrevi
            call utmess('I', 'ALGORITH16_84', si=vali(1))
        endif
    endif
!
!  COUPLAGE EDYOS NON PRIS EN COMPTE (Il FAUT UTILISER MDTR74)
! =>
    nbpal = 0
    vrotat = 0.d0
    dtsto = 0.d0
    prdeff = .false.
! <=
    if (method .eq. 'EULER') then
        call mdeul1(nbpas, dt, neqgen, zr(jpuls), zr(jpul2),&
                    zr(jmasg), descm, zr(jraig), descr, [0.d0],&
                    lamor, zr(jamog), desca, [0.d0], fbid(1),&
                    fbid(1), typbas, k8b, tinit, zi(jarch),&
                    nbsauv, nbchoc, zi(jranc), zr(jdepl), zr(jparc),&
                    zk8(jnoec), nbrede, zr(jrede), zk8(jfond), nbrevi,&
                    zr(jrevi), zk8(jfonv), zr(jdeps), zr(jvits), zr(jaccs),&
                    zi(jordr), zr(jinst), zr(jfcho), zr(jdcho), zr(jvcho),&
                    zi(jadcho), zi(jredc), zr(jredd), zi(jrevc), zr(jrevv),&
                    zr(jcoefm), zi(jiadve), zi(jinumo), zi(jidesc), zk8( jnodep),&
                    zk8(jnovit), zk8(jnoacc), zk8(jnomfo), zr(jpsid), monmot,&
                    0, fbid, fbid(1), 0.d0, fbid(1),&
                    nbpal, dtsto, vrotat, prdeff, nomres,&
                    ntotex, zr(jpass), zk8(jinti) )
!
    else if (method(1:5).eq.'ADAPT') then
        call mdadap(dt, dtmax, neqgen, zr(jpuls), zr(jpul2),&
                    zr(jmasg), descm, zr(jraig), descr, lamor,&
                    zr(jamog), desca, typbas, k8b, tinit,&
                    tfin, dtarch, nbsauv, nbchoc, zi( jranc),&
                    zr(jdepl), zr(jparc), zk8(jnoec), nbrede, zr(jrede),&
                    zk8(jfond), nbrevi, zr(jrevi), zk8(jfonv), zr(jdeps),&
                    zr( jvits), zr(jaccs), zr(jpass), zi(jordr), zr(jinst),&
                    zr(jfcho), zr( jdcho), zr(jvcho), zi(jadcho), zi(jredc),&
                    zr(jredd), zr(jcoefm), zi(jiadve), zi(jinumo), zi(jidesc),&
                    zk8(jnodep), zk8(jnovit), zk8( jnoacc), zk8(jnomfo), zr(jpsid),&
                    monmot, nbpal, dtsto, vrotat, prdeff,&
                    method, nomres, ntotex, zi(jrevc), zr(jrevv), &
                    zk8(jinti) )
!
    else if (method(1:5).eq.'RUNGE') then
        call mdruku(method, tinit, tfin, dt, dtmin,&
                    dtmax, nbsauv, nbobjs, neqgen, zr(jpuls),&
                    zr(jpul2), zr(jmasg), descm, zr(jraig), descr,&
                    [0.d0], lamor, zr(jamog), desca, [0.d0],&
                    fbid(1), fbid(1), typbas, k8b, nbchoc,&
                    zk8(jinti), zi(jranc), zr(jdepl), zr(jparc), zk8(jnoec),&
                    nbrede, zr(jrede), zk8(jfond), nbrevi, zr(jrevi),&
                    zk8(jfonv), zr(jcoefm), zi(jiadve), zi(jinumo), zi(jidesc),&
                    zk8( jnodep), zk8(jnovit), zk8(jnoacc), zk8(jnomfo), zr(jpsid),&
                    monmot, 0, fbid, fbid, 0.d0,&
                    fbid(1), nbpal, dtsto, vrotat, prdeff,&
                    nomres, ntotex, masgen, riggen, amogen)
!
!
!
    endif
!
    call titre()
!
 60 continue
    call jedetc('V', '&&SSDT74', 1)
    if (iret .ne. 0) then
        call utmess('F', 'ALGORITH5_24')
    endif
!
    if (namor .ne. 0) then
        call jedetr(amotem//'           .UALF')
        call jedetr(amotem//'           .VALM')
        call jedetr(amotem//'           .REFA')
        call jedetr(amotem//'           .CONL')
        call jedetr(amotem//'           .LIME')
    endif
    call jedetr(mastem//'           .UALF')
    call jedetr(mastem//'           .VALM')
    call jedetr(mastem//'           .REFA')
    call jedetr(mastem//'           .CONL')
    call jedetr(mastem//'           .LIME')
    if (nexcit .ne. 0) call jedetr('&&SSDT74.NOMVEC')
!
    call jedema()
end subroutine

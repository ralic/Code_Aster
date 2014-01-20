subroutine mdadap(dti, dtmax, neqgen, pulsat, pulsa2,&
                  masgen, descm, riggen, descr, lamor,&
                  amogen, desca, typbas, basemo, tinit,&
                  tfin, dtarch, nbsauv, nbchoc, logcho,&
                  dplmod, parcho, noecho, nbrede, dplred,&
                  fonred, nbrevi, dplrev, fonrev, depsto,&
                  vitsto, accsto, passto, iorsto, temsto,&
                  fchost, dchost, vchost, ichost, iredst,&
                  dredst, coefm, liad, inumor, idescf,&
                  nofdep, nofvit, nofacc, nomfon, psidel,&
                  monmot, nbpal, dtsto, vrotat, prdeff,&
                  method, nomres, nbexci, irevst, drevst)
!
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/frqapp.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdacce.h"
#include "asterfort/mdarnl.h"
#include "asterfort/mdfext.h"
#include "asterfort/mdfnli.h"
#include "asterfort/mdinit.h"
#include "asterfort/mdsize.h"
#include "asterfort/preres.h"
#include "asterfort/r8inir.h"
#include "asterfort/recpar.h"
#include "asterfort/resu74.h"
#include "asterfort/sigusr.h"
#include "asterfort/trlds.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/dcopy.h"
    integer :: iorsto(*), iredst(*), descm, descr, desca, nbchoc
    integer :: logcho(nbchoc, *), ichost(*), neqgen
    real(kind=8) :: pulsat(*), pulsa2(*), masgen(*), riggen(*), amogen(*)
    real(kind=8) :: parcho(*), depsto(*), vitsto(*), accsto(*)
    real(kind=8) :: passto(*), temsto(*), fchost(*), dchost(*), vchost(*)
    real(kind=8) :: dredst(*), epsi, dplmod(nbchoc, neqgen, *), dplrev(*)
    real(kind=8) :: dplred(*), drevst(*)
    real(kind=8) :: dti, dtmax
    real(kind=8) :: dtsto, vrotat
    character(len=8) :: basemo, noecho(nbchoc, *), fonred(*), fonrev(*), vvar
    character(len=8) :: nomres, monmot
    character(len=16) :: typbas, method
    logical :: lamor, prdeff
!
    real(kind=8) :: coefm(*), psidel(*)
    integer :: liad(*), inumor(*), idescf(*)
    integer :: nbpal, ibid
    character(len=8) :: nofdep(*), nofvit(*), nofacc(*), nomfon(*)
!
!-----------------------------------------------------------------------
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
!     DIFFERENCES CENTREES AVEC PAS ADAPTATIF
!     ------------------------------------------------------------------
! IN  : DTI    : PAS DE TEMPS INITIAL (ET MAXIMUM)
! IN  : NEQGEN : NOMBRE DE MODES
! IN  : PULSAT : PULSATIONS MODALES
! IN  : PULSA2 : PULSATIONS MODALES AU CARREES
! IN  : MASGEN : MASSES GENERALISEES ( TYPBAS = 'MODE_MECA' )
!                MATRICE DE MASSE GENERALISEE ( TYPBAS = 'BASE_MODA' )
! IN  : DESCM  : DESCRIPTEUR DE LA MATRICE DE MASSE
! IN  : RIGGEN : RAIDEURS GENERALISES ( TYPBAS = 'MODE_MECA' )
!                MATRICE DE RAIDEUR GENERALISE ( TYPBAS = 'BASE_MODA' )
! IN  : DESCR  : DESCRIPTEUR DE LA MATRICE DE RIGIDITE
! IN  : LAMOR  : AMORTISSEMENT SOUS FORME D'UNE LISTE DE REELS
! IN  : AMOGEN : AMORTISSEMENTS REDUITS ( LAMOR = .TRUE. )
!                MATRICE D'AMORTISSEMENT ( LAMOR = .FALSE. )
! IN  : DESCA  : DESCRIPTEUR DE LA MATRICE D'AMORTISSEMENT
! IN  : TYPBAS : TYPE DE LA BASE ('MODE_MECA' 'BASE_MODA' 'MODELE_GENE')
! IN  : BASEMO : NOM K8 DE LA BASE MODALE DE PROJECTION
! IN  : TINIT  : TEMPS INITIAL
! IN  : TFIN   : TEMPS FINAL
! IN  : DTARCH : PAS D'ARCHIVAGE
! IN  : NBSAUV : NOMBRE DE PAS ARCHIVES
! IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
! IN  : LOGCHO : INDICATEUR D'ADHERENCE ET DE FORCE FLUIDE
! IN  : DPLMOD : TABLEAU DES DEPL MODAUX AUX NOEUDS DE CHOC
! IN  : PARCHO : TABLEAU DES PARAMETRES DE CHOC
! IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
! IN  : NBREDE : NOMBRE DE RELATION EFFORT DEPLACEMENT (RED)
! IN  : DPLRED : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DE RED
! IN  : FONRED : TABLEAU DES FONCTIONS AUX NOEUDS DE RED
! IN  : NBREVI : NOMBRE DE RELATION EFFORT VITESSE (REV)
! IN  : DPLREV : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DE REV
! IN  : FONREV : TABLEAU DES FONCTIONS AUX NOEUDS DE REV
! IN  : LIAD   : LISTE DES ADRESSES DES VECTEURS CHARGEMENT
! IN  : NOFDEP : NOM DE LA FONCTION DEPL_IMPO
! IN  : NOFVIT : NOM DE LA FONCTION VITE_IMPO
! IN  : PSIDEL : TABLEAU DE VALEURS DE PSI*DELTA
! IN  : MONMOT : = OUI SI MULTI-APPUIS
! IN  : NBEXCI : NBRE D'EXCITATIONS (SOUS LE MC EXCIT ET EXCIT_RESU)
! ----------------------------------------------------------------------
!
!
!
!
    real(kind=8) :: tps1(4), conv
    real(kind=8) :: valr(3), rint1, rint2, valr2(2)
    integer :: vali(2)
    character(len=8) :: tran, fbid(2), k8bid
    character(len=19) :: mamass, solveu, matpre
!
    integer :: ii
    integer :: palmax
!-----------------------------------------------------------------------
    integer :: if, im, ipas, iret, isto1, isto2
    integer :: isto3, istoav, iv, iveri
    integer :: jcho2, jchor
    integer ::  jmass, jredi
    integer :: jredr, jslvi,  jvint
    integer ::   nbacc, nbexci, nbmod1, nbpasc
    integer :: nbrede, nbrevi, nbsauv, nbscho, ndt, npas
    integer :: nper, nr, nrmax
    integer :: isto4, jrevr, jrevi, irevst(*)
    real(kind=8) :: cdp, cmp, deux, dt1, dt2, dtarch, dtmin
    real(kind=8) :: err, freq, pas1, pas2, r8bid1
    real(kind=8) :: r8val, tarch, tarchi, temp2
    real(kind=8) :: temps, tfin, tinf, tinit, tjob, tmoy, tmp
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    parameter (palmax=20)
    integer :: iadrk, iapp
    integer :: dimnas
    parameter     (dimnas=8)
    character(len=3) :: finpal(palmax)
    character(len=6) :: typal(palmax)
    character(len=8) :: cnpal(palmax)
    character(len=24) :: cpal
    real(kind=8) :: fsauv(palmax, 3)
    real(kind=8), pointer :: acc2(:) => null()
    real(kind=8), pointer :: acce(:) => null()
    real(kind=8), pointer :: dep2(:) => null()
    real(kind=8), pointer :: depl(:) => null()
    real(kind=8), pointer :: fext(:) => null()
    real(kind=8), pointer :: tra1(:) => null()
    real(kind=8), pointer :: vip1(:) => null()
    real(kind=8), pointer :: vip2(:) => null()
    real(kind=8), pointer :: vit2(:) => null()
    real(kind=8), pointer :: vite(:) => null()
    real(kind=8), pointer :: vmin(:) => null()
!
    call jemarq()
!
    fbid = '        '
!
    zero = 0.d0
    deux = 2.d0
    jchor = 1
    jredr = 1
    jredi = 1
    jrevr = 1
    jrevi = 1
    jvint = 1
    ipas = 0
    isto1 = 0
    isto2 = 0
    isto3 = 0
    isto4 = 0
    npas = 0
    nbacc = 0
    conv = 1.d0
    ii = 1
    nbmod1 = neqgen - 1
    nbscho = nbsauv * 3 * nbchoc
    epsi = r8prem()
    do iapp = 1, palmax
        typal(iapp)='      '
        finpal(iapp)='   '
        cnpal(iapp)=' '
    end do
    prdeff = .false.
!
    if (lamor) then
        do im = 1, neqgen
            amogen(im) = deux * amogen(im) * pulsat(im)
        end do
    endif
!
!     --- RECUPERATION DES PARAMETRES D'ADAPTATION DU PAS
!
    AS_ALLOCATE(vr=vmin, size=neqgen)
    call recpar(neqgen, dti, dtmax, vmin, vvar,&
                cmp, cdp, dtmin, nper, nrmax)
!
!     --- FACTORISATION DE LA MATRICE MASSE ---
!
    if (typbas .eq. 'BASE_MODA') then
        call wkvect('&&MDADAP.MASS', 'V V R8', neqgen*neqgen, jmass)
        call dcopy(neqgen*neqgen, masgen, 1, zr(jmass), 1)
        call trlds(zr(jmass), neqgen, neqgen, iret)
        if (iret .ne. 0) then
            call utmess('F', 'ALGORITH5_22')
        endif
        call dcopy(neqgen*neqgen, masgen, 1, zr(jmass), 1)
    else if (typbas.eq.'MODELE_GENE     ') then
        mamass=zk24(zi(descm+1))(1:19)
        call dismoi('SOLVEUR', mamass, 'MATR_ASSE', repk=solveu)
        ASSERT(solveu.eq.'&&OP0074.SOLVEUR')
        matpre='&&OP0074.BIDON'
!
!       ISTOP MIS A 1 POUR NE PAS ARRETER L'EXECUTION EN CAS
!       DE MATRICE SINGULIERE (MODES STATIQUES)
        call jeveuo(solveu//'.SLVI', 'E', jslvi)
        istoav=zi(jslvi-1+3)
        zi(jslvi-1+3)=1
        call preres(solveu, 'V', iret, matpre, mamass,&
                    ibid, -9999)
!       -- ON RETABLIT ISTOP
        zi(jslvi-1+3)=istoav
    else
        call wkvect('&&MDADAP.MASS', 'V V R8', neqgen, jmass)
        call dcopy(neqgen, masgen, 1, zr(jmass), 1)
    endif
!
!     --- VECTEURS DE TRAVAIL ---
!
    AS_ALLOCATE(vr=depl, size=neqgen)
    AS_ALLOCATE(vr=dep2, size=neqgen)
    AS_ALLOCATE(vr=vite, size=neqgen)
    AS_ALLOCATE(vr=vit2, size=neqgen)
    AS_ALLOCATE(vr=vip1, size=neqgen)
    AS_ALLOCATE(vr=vip2, size=neqgen)
    AS_ALLOCATE(vr=acce, size=neqgen)
    AS_ALLOCATE(vr=acc2, size=neqgen)
    AS_ALLOCATE(vr=tra1, size=neqgen)
    AS_ALLOCATE(vr=fext, size=neqgen)
    if (nbchoc .ne. 0 .and. nbpal .eq. 0) then
        call wkvect('&&MDADAP.SCHOR', 'V V R8', nbchoc*14, jchor)
        call wkvect('&&MDADAP.SCHO2', 'V V R8', nbchoc*14, jcho2)
!        INITIALISATION POUR LE FLAMBAGE
        call jeveuo(nomres//'           .VINT', 'E', jvint)
        call r8inir(nbchoc, 0.d0, zr(jvint), 1)
    else
        jchor=1
        jcho2=1
    endif
    if (nbrede .ne. 0) then
        call wkvect('&&MDADAP.SREDR', 'V V R8', nbrede, jredr)
        call wkvect('&&MDADAP.SREDI', 'V V I', nbrede, jredi)
    else
        jredr=1
        jredi=1
    endif
    if (nbrevi .ne. 0) then
        call wkvect('&&MDEUL1.SREVR', 'V V R8', nbrevi, jrevr)
        call wkvect('&&MDEUL1.SREVI', 'V V I', nbrevi, jrevi)
    else
        jrevr=1
        jrevi=1
    endif
!
!
!     --- CONDITIONS INITIALES ---
!
    call mdinit(basemo, neqgen, nbchoc, depl, vite,&
                zr(jvint), iret, tinit)
    if (iret .ne. 0) goto 999
    call dcopy(neqgen, vite, 1, vip1, 1)
    dt2 = dti
    dt1 = zero
    if (nbchoc .gt. 0 .and. nbpal .eq. 0) then
        call dcopy(nbchoc, zr(jvint), 1, zr(jchor+13*nbchoc), 1)
    endif
!
!     --- FORCES EXTERIEURES ---
!
    if (nbexci .ne. 0) then
        call mdfext(tinit, r8bid1, neqgen, nbexci, idescf,&
                    nomfon, coefm, liad, inumor, 1,&
fext)
    endif
!
!    COUPLAGE AVEC EDYOS
!
    if (nbpal .gt. 0) then
        cpal='C_PAL'
!     RECUPERATION DES DONNEES SUR LES PALIERS
!     -------------------------------------------------
        call jeveuo(cpal, 'L', iadrk)
        do iapp = 1, nbpal
            fsauv(iapp,1)= 0.d0
            fsauv(iapp,2)= 0.d0
            fsauv(iapp,3)= 0.d0
            typal(iapp)=zk8(iadrk+(iapp-1))(1:6)
            finpal(iapp)=zk8(iadrk+(iapp-1)+palmax)(1:3)
            cnpal(iapp)=zk8(iadrk+(iapp-1)+2*palmax)(1:dimnas)
        end do
    endif
    if (nbpal .ne. 0) nbchoc = 0
!
!    FIN COUPLAGE AVEC EDYOS
!
!
!       CAS CLASSIQUE
!
    call mdfnli(neqgen, depl, vite, acce, fext,&
                nbchoc, logcho, dplmod, parcho, noecho,&
                zr(jchor), nbrede, dplred, fonred, zr(jredr),&
                zi(jredi), nbrevi, dplrev, fonrev, zr(jrevr),&
                zi(jrevi), tinit, nofdep, nofvit, nofacc,&
                nbexci, psidel, monmot, 0, fbid,&
                fbid, 0.d0, k8bid, 1, nbpal,&
                dt2, dtsto, vrotat, typal, finpal,&
                cnpal, prdeff, conv, fsauv)
!
    if (conv .le. 0.d0) then
        call utmess('I', 'EDYOS_47')
    endif
!
!
!     --- ACCELERATIONS GENERALISEES INITIALES ---
!
    call mdacce(typbas, neqgen, pulsa2, masgen, descm,&
                riggen, descr, fext, lamor, amogen,&
                desca, tra1, depl, vite,acce)
!
!
!     --- ARCHIVAGE DONNEES INITIALES ---
!
    tarchi = tinit
!
    call mdarnl(isto1, 0, tinit, dt2, neqgen,&
                depl, vite, acce, isto2, nbchoc,&
                zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                zi(jredi), isto4, nbrevi, zr(jrevr), zi(jrevi),&
                depsto, vitsto, accsto, passto, iorsto,&
                temsto, fchost, dchost, vchost, ichost,&
                zr(jvint), iredst, dredst, irevst, drevst)
!
    temps = tinit
    tarch = tinit+ dtarch
    call uttcpu('CPU.MDADAP', 'INIT', ' ')
    iveri = 0
    nbpasc = 1
!
!     --- BOUCLE TEMPORELLE ---
!
 30 continue
    if (temps .lt. tfin) then
!       DO 30 WHILE(TEMPS .LT. TFIN)
!
        if (iveri .eq. 0) then
            call uttcpu('CPU.MDADAP', 'DEBUT', ' ')
        else
            if (mod(iveri,nbpasc) .eq. 0) then
                call uttcpu('CPU.MDADAP', 'DEBUT', ' ')
            endif
        endif
!
        err = 100.d0
        nr = 0
!        --- DERNIER PAS DE TEMPS ? ---
!
        if (temps+dt2 .gt. tfin) dt2 = tfin-temps
!         DO 29 WHILE(ERR .GT. 1. .AND. NR .LT. NRMAX)
 29     continue
        if (err .gt. 1.d0 .and. nr .lt. nrmax) then
!
            pas1 = (dt1+dt2)*0.5d0
            pas2 = dt2*0.5d0
!  MODIFICATION POUR ADAPT ORDRE1
            if ((dt1.le.1.d-13) .and. (method.eq.'ADAPT_ORDRE1')) pas1= dt2
!
            do im = 0, nbmod1
!              --- VITESSES GENERALISEES ---
                vit2(im+1) = vite(im+1) + acce(im+1) * pas1
!              --- DEPLACEMENTS GENERALISES ---
                dep2(im+1) = depl(im+1) + ( dt2 * vit2(im+1) )
!              --- PREDICTEUR DE LA VITESSE ---
                if (method .eq. 'ADAPT_ORDRE2') then
                    vip2(im+1) = vit2(im+1) + pas2 * acce(im+1)
                else
!  MODIFICATION POUR ADAPT ORDRE1
                    vip2(im+1) = vit2(im+1)
                endif
            end do
!
!
!        --- FORCES EXTERIEURES ---
!
            do if = 0, neqgen-1
                fext(if+1) = zero
            end do
            if (nbexci .ne. 0) then
                r8val = temps+dt2
                call mdfext(r8val, r8bid1, neqgen, nbexci, idescf,&
                            nomfon, coefm, liad, inumor, 1,&
fext)
            endif
!
!
!             CALCUL CLASSIQUE FORCES NON-LINEAIRES ET ACCELERATIONS
!
!             --- CONTRIBUTION DES FORCES NON LINEAIRES ---
!
            r8val = temps + dt2
            ii = ii + 1
            call mdfnli(neqgen, dep2, vip2, acce, fext,&
                        nbchoc, logcho, dplmod, parcho, noecho,&
                        zr(jcho2), nbrede, dplred, fonred, zr(jredr),&
                        zi(jredi), nbrevi, dplrev, fonrev, zr(jrevr),&
                        zi(jrevi), r8val, nofdep, nofvit, nofacc,&
                        nbexci, psidel, monmot, 0, fbid,&
                        fbid, 0.d0, k8bid, ii, nbpal,&
                        dt2, dtsto, vrotat, typal, finpal,&
                        cnpal, prdeff, conv, fsauv)
!
            if (conv .le. 0.d0) then
                call utmess('I', 'EDYOS_47')
            endif
!
!
!             --- ACCELERATIONS GENERALISEES ---
!
            nbacc = nbacc + 1
            call mdacce(typbas, neqgen, pulsa2, masgen, descm,&
                        riggen, descr, fext, lamor, amogen,&
                        desca, tra1, dep2, vip2,acc2)
!
!
!           --- CALCUL DE L'ERREUR ---
!
            call frqapp(dt2, neqgen, depl, dep2, acce,&
                        acc2, vmin, freq)
            err = nper * freq * dt2
!
            if (method .eq. 'ADAPT_ORDRE1' .and. temps .lt. (tinit+5.d0*dt2)) err = 0.d0
!
!
!           --- REDUCTION DU PAS DE TEMPS ---
!
            if ((err .gt. 1.d0) .or. (conv .le. 0.d0)) then
                dt2 = dt2/cdp
                prdeff = .false.
            else
                prdeff = .true.
            endif
            if (dt2 .le. dtmin .and. abs(tfin-(temps+dt2)) .gt. epsi) then
                call utmess('F', 'ALGORITH5_23')
            endif
!
            nr = nr + 1
! 29         CONTINUE
!           LES DEUX LIGNES SUIVANTES SIMULENT LE WHILE - CONTINUE
            goto 29
        endif
        if (err .gt. 1.d0 .and. nr .eq. nrmax) then
            valr2(1) = temps
            valr2(2) = dt2
            call utmess('A', 'DYNAMIQUE_18', si=nr, nr=2, valr=valr2)
        endif
!
        dt1 = dt2
        temp2 = temps + dt2
!
!           --- AUGMENTATION DU PAS SI ERREUR TROP FAIBLE ---
!
        if ((err .lt. 0.75d0) .and. (conv .gt. 0.d0)) then
            if (npas .eq. 5) then
                dt2 = cmp*dt2
                dt2 = min(dt2,dtmax)
                npas = 4
            endif
            npas = npas + 1
        else
            npas = 0
        endif
        if (temps+dt2 .gt. tfin) dt2 = tfin-temps
!
        ipas = ipas + 1
!
!           --- ARCHIVAGE ---
!
        if (temps .le. tarch .and. temp2 .ge. tarch) then
            isto1 = isto1+1
            if ((temp2-tarch) .le. (tarch-temps)) then
                tarchi = temp2
!
                call mdarnl(isto1, ipas, temp2, dt2, neqgen,&
                            dep2, vip2, acc2, isto2, nbchoc,&
                            zr(jcho2), nbscho, isto3, nbrede, zr(jredr),&
                            zi(jredi), isto4, nbrevi, zr( jrevr), zi(jrevi),&
                            depsto, vitsto, accsto, passto, iorsto,&
                            temsto, fchost, dchost, vchost, ichost,&
                            zr(jvint), iredst, dredst, irevst, drevst)
!
            else
                tarchi = temps
!
                call mdarnl(isto1, ipas-1, temps, dt2, neqgen,&
                            depl, vip1, acce, isto2, nbchoc,&
                            zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                            zi(jredi), isto4, nbrevi, zr( jrevr), zi(jrevi),&
                            depsto, vitsto, accsto, passto, iorsto,&
                            temsto, fchost, dchost, vchost, ichost,&
                            zr(jvint), iredst, dredst, irevst, drevst)
!
!
!
            endif
            tarch = tarch + dtarch
        endif
!
!           --- CALCUL DE VMIN ---
!
        if (vvar(1:4) .eq. 'NORM') then
            tmp = zero
            do iv = 0, nbmod1
                tmp = tmp+vit2(iv+1)**2
            end do
            tmp = sqrt(tmp)*0.01d0
            do iv = 0, nbmod1
                vmin(iv+1) = tmp
            end do
        else if (vvar(1:4) .eq. 'MAXI') then
            do iv = 0, nbmod1
                rint1 = vit2(iv+1)*0.01d0
                rint2 = abs(rint1)
                rint1 = vmin(iv+1)
                vmin(iv+1) = max(rint1,rint2)
            end do
        endif
!
!           --- MISE A JOUR ---
!
        temps = temp2
        call dcopy(neqgen, dep2, 1, depl, 1)
        call dcopy(neqgen, vit2, 1, vite, 1)
        call dcopy(neqgen, vip2, 1, vip1, 1)
        call dcopy(neqgen, acc2, 1, acce, 1)
        if (nbchoc .ne. 0) call dcopy(14, zr(jcho2), 1, zr(jchor), 1)
!
!        --- TEST SI LE TEMPS RESTANT EST SUFFISANT POUR CONTINUER ---
!       ON FIXE UN TEMPS MOYEN PAR PAS A UN MINIMUM DE 0.001 S
!       ACTIF POUR LA VERSION SOLARIS
!
        tinf = 1.d9
        rint1 = 1.d-3
        if (iveri .eq. 0) then
            call uttcpu('CPU.MDADAP', 'FIN', ' ')
            call uttcpr('CPU.MDADAP', 4, tps1)
            tjob = min(tinf,tps1(1))
            tmoy = max(tps1(4),rint1)
            nbpasc = int(1.d-02 * (tjob/tmoy)) + 1
        else
            if (mod(iveri,nbpasc) .eq. 0) then
                call uttcpu('CPU.MDADAP', 'FIN', ' ')
                call uttcpr('CPU.MDADAP', 4, tps1)
                tjob = min(tinf,tps1(1))
                if (tps1(1) .le. max(tjob/100.d0,15.d0)) then
                    goto 31
                endif
                tmoy = max(tps1(4),rint1)
                nbpasc = int(1.d-02 * (tjob/tmoy)) + 1
            endif
        endif
        iveri = iveri + 1
!
        goto 30
    endif
! 30      CONTINUE
!
 31 continue
!
    if (nbsauv .gt. (isto1+1)) then
        isto1 = isto1 + 1
        tarchi = temps
!
        call mdarnl(isto1, ipas, temps, dt2, neqgen,&
                    dep2, vip2, acc2, isto2, nbchoc,&
                    zr(jchor), nbscho, isto3, nbrede, zr( jredr),&
                    zi(jredi), isto4, nbrevi, zr(jrevr), zi(jrevi),&
                    depsto, vitsto, accsto, passto, iorsto,&
                    temsto, fchost, dchost, vchost, ichost,&
                    zr(jvint), iredst, dredst, irevst, drevst)
!
        goto 31
    endif
!
    vali (1) = ipas
    vali (2) = nbacc
    call utmess('I', 'ALGORITH16_1', ni=2, vali=vali)
!
! --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
!
    if (etausr() .eq. 1) then
        call sigusr()
    endif
!
    if (tps1(1) .le. max(tjob/100.d0,15.d0)) then
        if (nomres .eq. '&&OP0074') then
!       --- CAS D'UNE POURSUITE ---
            call getvid('ETAT_INIT', 'RESULTAT', iocc=1, scal=tran, nbret=ndt)
            if (ndt .ne. 0) call resu74(tran, nomres)
        endif
        call mdsize(nomres, isto1, neqgen, nbchoc, nbrede,&
                    nbrevi)
        vali (1) = ipas
        vali (2) = isto1
        valr (1) = tarchi
        valr (2) = tps1(4)
        valr (3) = tps1(1)
        call utmess('Z', 'ALGORITH16_77', ni=2, vali=vali, nr=3,&
                    valr=valr, num_except=28)
    endif
!
999 continue
    AS_DEALLOCATE(vr=depl)
    AS_DEALLOCATE(vr=dep2)
    AS_DEALLOCATE(vr=vite)
    AS_DEALLOCATE(vr=vit2)
    AS_DEALLOCATE(vr=vip1)
    AS_DEALLOCATE(vr=vip2)
    AS_DEALLOCATE(vr=acce)
    AS_DEALLOCATE(vr=acc2)
    AS_DEALLOCATE(vr=tra1)
    AS_DEALLOCATE(vr=fext)
    call jedetr('&&MDADAP.MASS')
    AS_DEALLOCATE(vr=vmin)
    if (nbchoc .ne. 0) then
        call jedetr('&&MDADAP.SCHOR')
        call jedetr('&&MDADAP.SCHO2')
    endif
    if (nbrede .ne. 0) then
        call jedetr('&&MDADAP.SREDR')
        call jedetr('&&MDADAP.SREDI')
    endif
    if (iret .ne. 0) then
        call utmess('F', 'ALGORITH5_24')
    endif
!
    call jedema()
end subroutine

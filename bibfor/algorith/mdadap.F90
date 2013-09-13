subroutine mdadap(dti, dtmax, neqgen, pulsat, pulsa2,&
                  masgen, descm, riggen, descr, lamor,&
                  amogen, desca, typbas, basemo, tinit,&
                  tfin, dtarch, nbsauv, itemax, prec,&
                  xlambd, lflu, nbchoc, logcho, dplmod,&
                  parcho, noecho, nbrede, dplred, fonred,&
                  nbrevi, dplrev, fonrev, depsto, vitsto,&
                  accsto, passto, iorsto, temsto, fchost,&
                  dchost, vchost, ichost, iredst, dredst,&
                  coefm, liad, inumor, idescf, nofdep,&
                  nofvit, nofacc, nomfon, psidel, monmot,&
                  nbpal, dtsto, vrotat, prdeff, method,&
                  nomres, nbexci, irevst, drevst)
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
#include "asterfort/utexcm.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: iorsto(*), iredst(*), itemax, descm, descr, desca, nbchoc
    integer :: logcho(nbchoc, *), ichost(*), neqgen
    real(kind=8) :: pulsat(*), pulsa2(*), masgen(*), riggen(*), amogen(*)
    real(kind=8) :: parcho(*), depsto(*), vitsto(*), accsto(*)
    real(kind=8) :: passto(*), temsto(*), fchost(*), dchost(*), vchost(*)
    real(kind=8) :: dredst(*), prec, epsi, dplmod(nbchoc, neqgen, *), dplrev(*)
    real(kind=8) :: dplred(*), drevst(*)
    real(kind=8) :: dti, dtmax
    real(kind=8) :: dtsto, vrotat
    character(len=8) :: basemo, noecho(nbchoc, *), fonred(*), fonrev(*), vvar
    character(len=8) :: nomres, monmot
    character(len=16) :: typbas, method
    logical :: lamor, lflu, prdeff
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
! IN  : ITEMAX : NOMBRE D'ITERATIONS MAXIMUM POUR TROUVER L'ACCELERATION
! IN  : PREC   : RESIDU RELATIF POUR TESTER LA CONVERGENCE DE L'ACCE.
! IN  : XLAMBD : MULTIPLICATEUR POUR RENDRE CONTRACTANTES LES ITERATIONS
! IN  : LFLU   : LOGIQUE INDIQUANT LA PRESENCE DE FORCES DE LAME FLUIDE
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
    integer :: icho, if, im, ipas, iret, isto1, isto2
    integer :: isto3, istoav, iter, iv, iveri, jacc2, jacce
    integer :: jacgi1, jacgi2, jamogi, jcho2, jchor, jdep2, jdepl
    integer :: jfext, jfexti, jm, jmass, jphi2, jpuls, jredi
    integer :: jredr, jslvi, jtra1, jvint, jvip1, jvip2, jvit2
    integer :: jvite, jvmin, nbacc, nbexci, nbmod1, nbpasc
    integer :: nbrede, nbrevi, nbsauv, nbscho, ndt, npas
    integer :: nper, nr, nrmax
    integer :: isto4, jrevr, jrevi, irevst(*)
    real(kind=8) :: cdp, cmp, deux, dt1, dt2, dtarch, dtmin
    real(kind=8) :: err, freq, pas1, pas2, r8bid1, r8b(1)
    real(kind=8) :: r8val, tarch, tarchi, temp2
    real(kind=8) :: temps, tfin, tinf, tinit, tjob, tmoy, tmp
    real(kind=8) :: xlambd, xnorm, xref, xx, zero
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
    do 111 iapp = 1, palmax
        typal(iapp)='      '
        finpal(iapp)='   '
        cnpal(iapp)=' '
111  end do
    prdeff = .false.
!
    if (lamor) then
        do 100 im = 1, neqgen
            amogen(im) = deux * amogen(im) * pulsat(im)
100      continue
    endif
!
!     --- RECUPERATION DES PARAMETRES D'ADAPTATION DU PAS
!
    call wkvect('&&MDADAP.VMIN', 'V V R8', neqgen, jvmin)
    call recpar(neqgen, dti, dtmax, zr(jvmin), vvar,&
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
        call dismoi('F', 'SOLVEUR', mamass, 'MATR_ASSE', ibid,&
                    solveu, ibid)
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
        if (nbchoc .ne. 0) then
            if (lflu) then
!
!        CALCUL DE LA MATRICE DIAGONALE POUR LES NOEUDS DE LAME FLUIDE
!
                call wkvect('&&MDADAP.PHI2', 'V V R8', neqgen*nbchoc, jphi2)
!
!        CALCUL DES MATRICES M' PAR NOEUD DE CHOC FLUIDE
!
                do 51 icho = 1, nbchoc
                    do 51 im = 1, neqgen
                        zr(jphi2+im-1+(icho-1)*neqgen) = 0.d0
                        if (logcho(icho,2) .eq. 1) then
                            if (noecho(icho,9)(1:2) .eq. 'BI') then
                                do 52 jm = 1, 3
                                    xx = dplmod(icho,im,jm) - dplmod( icho,im,jm+3)
                                    zr(jphi2+im-1+(icho-1)*neqgen) =&
                                    zr(jphi2+im-1+(icho-1)*neqgen) +&
                                    xlambd*xx**2
52                              continue
                            else
                                do 50 jm = 1, 3
                                    zr(jphi2+im-1+(icho-1)*neqgen) =&
                                    zr(jphi2+im-1+(icho-1)*neqgen) +&
                                    xlambd*dplmod(icho,im,jm)**2
50                              continue
                            endif
                        endif
51                  continue
            endif
        endif
    endif
!
!     --- VECTEURS DE TRAVAIL ---
!
    call wkvect('&&MDADAP.DEPL', 'V V R8', neqgen, jdepl)
    call wkvect('&&MDADAP.DEP2', 'V V R8', neqgen, jdep2)
    call wkvect('&&MDADAP.VITE', 'V V R8', neqgen, jvite)
    call wkvect('&&MDADAP.VIT2', 'V V R8', neqgen, jvit2)
    call wkvect('&&MDADAP.VIP1', 'V V R8', neqgen, jvip1)
    call wkvect('&&MDADAP.VIP2', 'V V R8', neqgen, jvip2)
    call wkvect('&&MDADAP.ACCE', 'V V R8', neqgen, jacce)
    call wkvect('&&MDADAP.ACC2', 'V V R8', neqgen, jacc2)
    call wkvect('&&MDADAP.TRA1', 'V V R8', neqgen, jtra1)
    call wkvect('&&MDADAP.FEXT', 'V V R8', neqgen, jfext)
    if (lflu) then
        call wkvect('&&MDADAP.FEXTI', 'V V R8', neqgen, jfexti)
        call wkvect('&&MDADAP.ACCGEN1', 'V V R8', neqgen, jacgi1)
        call wkvect('&&MDADAP.ACCGEN2', 'V V R8', neqgen, jacgi2)
        call wkvect('&&MDADAP.PULSAI', 'V V R8', neqgen, jpuls)
        call wkvect('&&MDADAP.AMOGEI', 'V V R8', neqgen, jamogi)
        call dcopy(neqgen, pulsa2, 1, zr(jpuls), 1)
        call dcopy(neqgen, amogen, 1, zr(jamogi), 1)
    endif
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
    call mdinit(basemo, neqgen, nbchoc, zr(jdepl), zr(jvite),&
                zr(jvint), iret, tinit)
    if (iret .ne. 0) goto 9999
    call dcopy(neqgen, zr(jvite), 1, zr(jvip1), 1)
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
                    zr(jfext))
    endif
!
!    COUPLAGE AVEC EDYOS
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
    if (nbpal .ne. 0) nbchoc = 0
!
!    FIN COUPLAGE AVEC EDYOS
!
    if (lflu) then
!
!     --- CONTRIBUTION DES FORCES NON LINEAIRES ---
!         CAS DES FORCES DE LAME FLUIDE
!
        call mdfnli(neqgen, zr(jdepl), zr(jvite), zr(jacce), zr(jfext),&
                    zr(jmass), zr(jphi2), zr(jpuls), zr(jamogi), nbchoc,&
                    logcho, dplmod, parcho, noecho, zr(jchor),&
                    nbrede, dplred, fonred, zr(jredr), zi(jredi),&
                    nbrevi, dplrev, fonrev, zr(jrevr), zi(jrevi),&
                    tinit, nofdep, nofvit, nofacc, nbexci,&
                    psidel, monmot, 0, fbid, fbid,&
                    0.d0, k8bid, 1, 0, dt2,&
                    dtsto, vrotat, typal, finpal, cnpal,&
                    prdeff, conv, fsauv)
!
        if (conv .le. 0.d0) then
            call utmess('I', 'EDYOS_47')
        endif
!
!     --- ACCELERATIONS GENERALISEES INITIALES ---
!
        call mdacce(typbas, neqgen, zr(jpuls), zr(jmass), descm,&
                    riggen, descr, zr(jfext), lamor, zr(jamogi),&
                    desca, zr(jtra1), zr(jdepl), zr(jvite), zr(jacce))
    else
!
!       CAS CLASSIQUE
!
        call mdfnli(neqgen, zr(jdepl), zr(jvite), zr(jacce), zr(jfext),&
                    masgen, r8b, pulsa2, amogen, nbchoc,&
                    logcho, dplmod, parcho, noecho, zr(jchor),&
                    nbrede, dplred, fonred, zr(jredr), zi(jredi),&
                    nbrevi, dplrev, fonrev, zr(jrevr), zi(jrevi),&
                    tinit, nofdep, nofvit, nofacc, nbexci,&
                    psidel, monmot, 0, fbid, fbid,&
                    0.d0, k8bid, 1, nbpal, dt2,&
                    dtsto, vrotat, typal, finpal, cnpal,&
                    prdeff, conv, fsauv)
!
        if (conv .le. 0.d0) then
            call utmess('I', 'EDYOS_47')
        endif
!
!
!     --- ACCELERATIONS GENERALISEES INITIALES ---
!
        call mdacce(typbas, neqgen, pulsa2, masgen, descm,&
                    riggen, descr, zr(jfext), lamor, amogen,&
                    desca, zr(jtra1), zr(jdepl), zr(jvite), zr(jacce))
!
    endif
!
!     --- ARCHIVAGE DONNEES INITIALES ---
!
    tarchi = tinit
!
    call mdarnl(isto1, 0, tinit, dt2, neqgen,&
                zr(jdepl), zr(jvite), zr(jacce), isto2, nbchoc,&
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
30  continue
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
29      continue
        if (err .gt. 1.d0 .and. nr .lt. nrmax) then
!
            pas1 = (dt1+dt2)*0.5d0
            pas2 = dt2*0.5d0
!  MODIFICATION POUR ADAPT ORDRE1
            if ((dt1.le.1.d-13) .and. (method.eq.'ADAPT_ORDRE1')) pas1= dt2
!
            do 40 im = 0, nbmod1
!              --- VITESSES GENERALISEES ---
                zr(jvit2+im) = zr(jvite+im) + zr(jacce+im) * pas1
!              --- DEPLACEMENTS GENERALISES ---
                zr(jdep2+im) = zr(jdepl+im) + ( dt2 * zr(jvit2+im) )
!              --- PREDICTEUR DE LA VITESSE ---
                if (method .eq. 'ADAPT_ORDRE2') then
                    zr(jvip2+im) = zr(jvit2+im) + pas2 * zr(jacce+im)
                else
!  MODIFICATION POUR ADAPT ORDRE1
                    zr(jvip2+im) = zr(jvit2+im)
                endif
40          continue
!
!
!        --- FORCES EXTERIEURES ---
!
            do 20 if = 0, neqgen-1
                zr(jfext+if) = zero
20          continue
            if (nbexci .ne. 0) then
                r8val = temps+dt2
                call mdfext(r8val, r8bid1, neqgen, nbexci, idescf,&
                            nomfon, coefm, liad, inumor, 1,&
                            zr(jfext))
            endif
!
            if (lflu) then
!           ------------------------------------------------------
!           ITERATIONS IMPLICITES POUR OBTENIR L'ACCELERATION DANS
!           LE CAS DE FORCE DE LAME FLUIDE
!           ------------------------------------------------------
                xnorm = 0.d0
                xref = 0.d0
                call dcopy(neqgen, zr(jacce), 1, zr(jacgi1), 1)
                nbacc = nbacc + 1
                r8val = temps + dt2
                do 5 iter = 1, itemax
!
!             REMISE A JOUR DE LA MASSE, PULSATION CARRE
!             DE L'AMORTISSEMENT MODAL ET DE LA FORCE EXT
!
                    call dcopy(neqgen, masgen, 1, zr(jmass), 1)
                    call dcopy(neqgen, pulsa2, 1, zr(jpuls), 1)
                    call dcopy(neqgen, amogen, 1, zr(jamogi), 1)
                    call dcopy(neqgen, zr(jfext), 1, zr(jfexti), 1)
!
!           --- CONTRIBUTION DES FORCES NON LINEAIRES ---
!
                    ii = ii + 1
                    call mdfnli(neqgen, zr(jdep2), zr(jvip2), zr(jacgi1), zr(jfexti),&
                                zr(jmass), zr(jphi2), zr(jpuls), zr(jamogi), nbchoc,&
                                logcho, dplmod, parcho, noecho, zr(jcho2),&
                                nbrede, dplred, fonred, zr(jredr), zi(jredi),&
                                nbrevi, dplrev, fonrev, zr(jrevr), zi(jrevi),&
                                r8val, nofdep, nofvit, nofacc, nbexci,&
                                psidel, monmot, 0, fbid, fbid,&
                                0.d0, k8bid, ii, nbpal, dt2,&
                                dtsto, vrotat, typal, finpal, cnpal,&
                                prdeff, conv, fsauv)
!
                    if (conv .le. 0.d0) then
                        call utmess('I', 'EDYOS_47')
                    endif
!
!
!           --- ACCELERATIONS GENERALISEES ---
!
                    call mdacce(typbas, neqgen, zr(jpuls), zr(jmass), descm,&
                                riggen, descr, zr(jfexti), lamor, zr(jamogi),&
                                desca, zr(jtra1), zr(jdep2), zr(jvip2), zr(jacgi2))
                    xnorm = 0.d0
                    xref = 0.d0
                    do 15 im = 1, neqgen
                        xnorm = xnorm + (zr(jacgi2+im-1)-zr(jacgi1+im- 1))**2
                        xref = xref + zr(jacgi2+im-1)**2
15                  continue
                    call dcopy(neqgen, zr(jacgi2), 1, zr(jacgi1), 1)
!             TEST DE CONVERGENCE
                    if (xnorm .le. prec*xref) goto 25
 5              continue
!
!           NON CONVERGENCE
!
                vali (1) = itemax
                valr (1) = xnorm/xref
                valr (2) = temps
                call utmess('F', 'ALGORITH15_99', si=vali(1), nr=2, valr=valr)
!
25              continue
                call dcopy(neqgen, zr(jacgi2), 1, zr(jacc2), 1)
!
            else
!
!             CALCUL CLASSIQUE FORCES NON-LINEAIRES ET ACCELERATIONS
!
!
!             --- CONTRIBUTION DES FORCES NON LINEAIRES ---
!
                r8val = temps + dt2
                ii = ii + 1
                call mdfnli(neqgen, zr(jdep2), zr(jvip2), zr(jacce), zr( jfext),&
                            r8b, r8b, r8b, r8b, nbchoc,&
                            logcho, dplmod, parcho, noecho, zr(jcho2),&
                            nbrede, dplred, fonred, zr(jredr), zi(jredi),&
                            nbrevi, dplrev, fonrev, zr(jrevr), zi(jrevi),&
                            r8val, nofdep, nofvit, nofacc, nbexci,&
                            psidel, monmot, 0, fbid, fbid,&
                            0.d0, k8bid, ii, nbpal, dt2,&
                            dtsto, vrotat, typal, finpal, cnpal,&
                            prdeff, conv, fsauv)
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
                            riggen, descr, zr(jfext), lamor, amogen,&
                            desca, zr(jtra1), zr(jdep2), zr(jvip2), zr(jacc2))
!
            endif
!
!           --- CALCUL DE L'ERREUR ---
!
            call frqapp(dt2, neqgen, zr(jdepl), zr(jdep2), zr(jacce),&
                        zr( jacc2), zr(jvmin), freq)
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
                            zr(jdep2), zr(jvip2), zr(jacc2), isto2, nbchoc,&
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
                            zr(jdepl), zr(jvip1), zr(jacce), isto2, nbchoc,&
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
            do 28 iv = 0, nbmod1
                tmp = tmp+zr(jvit2+iv)**2
28          continue
            tmp = sqrt(tmp)*0.01d0
            do 27 iv = 0, nbmod1
                zr(jvmin+iv) = tmp
27          continue
        else if (vvar(1:4) .eq. 'MAXI') then
            do 26 iv = 0, nbmod1
                rint1 = zr(jvit2+iv)*0.01d0
                rint2 = abs(rint1)
                rint1 = zr(jvmin+iv)
                zr(jvmin+iv) = max(rint1,rint2)
26          continue
        endif
!
!           --- MISE A JOUR ---
!
        temps = temp2
        call dcopy(neqgen, zr(jdep2), 1, zr(jdepl), 1)
        call dcopy(neqgen, zr(jvit2), 1, zr(jvite), 1)
        call dcopy(neqgen, zr(jvip2), 1, zr(jvip1), 1)
        call dcopy(neqgen, zr(jacc2), 1, zr(jacce), 1)
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
31  continue
!
    if (nbsauv .gt. (isto1+1)) then
        isto1 = isto1 + 1
        tarchi = temps
!
        call mdarnl(isto1, ipas, temps, dt2, neqgen,&
                    zr(jdep2), zr(jvip2), zr(jacc2), isto2, nbchoc,&
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
        call utexcm(28, 'ALGORITH16_77', 0, ' ', 2,&
                    vali, 3, valr)
    endif
!
9999  continue
    call jedetr('&&MDADAP.DEPL')
    call jedetr('&&MDADAP.DEP2')
    call jedetr('&&MDADAP.VITE')
    call jedetr('&&MDADAP.VIT2')
    call jedetr('&&MDADAP.VIP1')
    call jedetr('&&MDADAP.VIP2')
    call jedetr('&&MDADAP.ACCE')
    call jedetr('&&MDADAP.ACC2')
    call jedetr('&&MDADAP.TRA1')
    call jedetr('&&MDADAP.FEXT')
    call jedetr('&&MDADAP.MASS')
    call jedetr('&&MDADAP.VMIN')
    if (lflu) then
        call jedetr('&&MDADAP.FEXTI')
        call jedetr('&&MDADAP.ACCGEN1')
        call jedetr('&&MDADAP.ACCGEN2')
        call jedetr('&&MDADAP.PULSAI')
        call jedetr('&&MDADAP.AMOGEI')
        call jedetr('&&MDADAP.PHI2')
    endif
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

subroutine mdeul1(nbpas, dt, neqgen, pulsat, pulsa2,&
                  masgen, descmm, riggen, descmr, rgygen,&
                  lamor, amogen, descma, gyogen, foncv,&
                  fonca, typbas, basemo, tinit, iparch,&
                  nbsauv, itemax, prec, xlambd, lflu,&
                  nbchoc, logcho, dplmod, parcho, noecho,&
                  nbrede, dplred, fonred, nbrevi, dplrev,&
                  fonrev, depsto, vitsto, accsto, iorsto,&
                  temsto, fchost, dchost, vchost, ichost,&
                  iredst, dredst, irevst, drevst, coefm,&
                  liad, inumor, idescf, nofdep, nofvit,&
                  nofacc, nomfon, psidel, monmot, nbrfis,&
                  fk, dfk, angini, foncp, nbpal,&
                  dtsto, vrotat, prdeff, nomres, nbexci,&
                  passto)
!
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
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
#include "asterfort/resu74.h"
#include "asterfort/sigusr.h"
#include "asterfort/trlds.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/utexcm.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: nbchoc, neqgen
    integer :: iorsto(*), iredst(*), itemax, descmm, descmr, descma, iparch(*)
    integer :: logcho(nbchoc, *), ichost(*), ibid, irevst(*)
    real(kind=8) :: pulsat(*), pulsa2(*), masgen(*), riggen(*), amogen(*)
    real(kind=8) :: gyogen(*), rgygen(*), parcho(*), depsto(*)
    real(kind=8) :: vitsto(*), accsto(*), temsto(*), fchost(*), dchost(*)
    real(kind=8) :: vchost(*), dredst(*), drevst(*), prec
    real(kind=8) :: dplmod(nbchoc, neqgen, *), dplred(*), dplrev(*), passto(*)
    real(kind=8) :: dt, dtsto, vrotat, angini
    character(len=8) :: basemo, noecho(nbchoc, *), fonred(*), fonrev(*)
    character(len=8) :: nomres, monmot
    character(len=16) :: typbas
    logical :: lamor, lflu, prdeff
!
    real(kind=8) :: coefm(*), psidel(*)
    integer :: liad(*), inumor(*), idescf(*)
    integer :: nbpal, nbrfis
    character(len=8) :: nofdep(*), nofvit(*), nofacc(*), nomfon(*)
    character(len=8) :: fk(2), dfk(2), foncv, fonca, foncp
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
!     ALGORITHME EULER D'ORDRE 1 OPTION LAME FLUIDE
!     ------------------------------------------------------------------
! IN  : NBPAS  : NOMBRE DE PAS
! IN  : DT     : PAS DE TEMPS
! IN  : NEQGEN : NOMBRE DE MODES
! IN  : PULSAT : PULSATIONS MODALES
! IN  : PULSA2 : PULSATIONS MODALES AU CARREES
! IN  : MASGEN : MASSES GENERALISEES ( TYPBAS = 'MODE_MECA' )
!                MATRICE DE MASSE GENERALISEE ( TYPBAS = 'BASE_MODA' )
! IN  : DESCMM : DESCRIPTEUR DE LA MATRICE DE MASSE
! IN  : RIGGEN : RAIDEURS GENERALISES ( TYPBAS = 'MODE_MECA' )
!                MATRICE DE RAIDEUR GENERALISE ( TYPBAS = 'BASE_MODA' )
! IN  : DESCMR : DESCRIPTEUR DE LA MATRICE DE RIGIDITE
! IN  : LAMOR  : AMORTISSEMENT SOUS FORME D'UNE LISTE DE REELS
! IN  : AMOGEN : AMORTISSEMENTS REDUITS ( LAMOR = .TRUE. )
!                MATRICE D'AMORTISSEMENT ( LAMOR = .FALSE. )
! IN  : DESCMA : DESCRIPTEUR DE LA MATRICE D'AMORTISSEMENT
! IN  : TYPBAS : TYPE DE LA BASE ('MODE_MECA' 'BASE_MODA' 'MODELE_GENE')
! IN  : BASEMO : NOM K8 DE LA BASE MODALE DE PROJECTION SI C'EST UN
!                MODE MECA K8BID LORS D'UN CALCUL PAR SOUS_STUCTURATION
! IN  : TINIT  : TEMPS INITIAL
! IN  : IPARCH : VECTEUR DES PAS D'ARCHIVAGE
! IN  : NBSAUV : NOMBRE DE PAS ARCHIVE
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
    real(kind=8) :: tps1(4), rint1, rint2, conv
    real(kind=8) :: valr(3)
    integer :: vali(2), nbconv, nbmxcv, n1
    character(len=8) :: tran, k8b, vvar
    character(len=19) :: matpre, matasm
!     ------------------------------------------------------------------
!
    integer :: palmax
!-----------------------------------------------------------------------
    integer :: i, iarchi, icho, ifor, im, iret, isto1, ier, ind
    integer :: isto2, isto3, iter, jacce, jaccgi, jamogi, jchor
    integer :: jdepl, jfext, jfexti, jm, jmass, jphi2, jpuls
    integer :: jredi, jredr, jtra1, jvint, jvite, n100
    integer :: nbexci, nbmod1, nbpas, nbrede, nbrevi, nbsauv, nbscho
    integer :: ndt, jamgy, jrigy, jrevr, jrevi, isto4
    real(kind=8) :: deux, r8bid1, tarchi, r8b(1)
    real(kind=8) :: temps, tinit, xlambd, xnorm, xref, xx, zero
!
!-----------------------------------------------------------------------
    parameter (palmax=20)
    integer :: iadrk, iapp
    integer :: dimnas
    parameter     (dimnas=8)
    character(len=3) :: finpal(palmax)
    character(len=6) :: typal(palmax)
    character(len=8) :: cnpal(palmax)
    character(len=24) :: cpal
    integer :: iarg
    real(kind=8) :: fsauv(palmax, 3), vrot, arot, vrotin, arotin
!
!
    call jemarq()
    zero = 0.d0
    deux = 2.d0
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
    nbmod1 = neqgen - 1
    nbscho = nbsauv * 3 * nbchoc
    vvar = 'NON'
!  COUPLAGE EDYOS : CONVERGENCE EDYOS :
    conv = 1.d0
    nbconv = 0
!  COUPLAGE EDYOS : NOMBRE MAXIMAL DE TENTATIVES DE REPRISE DES DONNEES
!  PRECEDENTES EN CAS DE NON-CONVERGENCE EDYOS :
    nbmxcv = 10
!
    do 111 iapp = 1, palmax
        typal(iapp)='      '
        finpal(iapp)='   '
        cnpal(iapp)=' '
111  continue
!
    call wkvect('&&MDEUL1.AMOGYR', 'V V R8', neqgen*neqgen, jamgy)
    call wkvect('&&MDEUL1.RIGGYR', 'V V R8', neqgen*neqgen, jrigy)
    if (lamor) then
        do 100 im = 1, neqgen
            amogen(im) = deux * amogen(im) * pulsat(im)
100      continue
    else
        call getvtx(' ', 'VITESSE_VARIABLE', nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvtx(' ', 'VITESSE_VARIABLE', scal=vvar, nbret=n1)
        endif
        vrotin = 0.d0
        arotin = 0.d0
        if (vvar .eq. 'OUI') then
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
        call wkvect('&&MDEUL1.MASS', 'V V R8', neqgen*neqgen, jmass)
        call dcopy(neqgen*neqgen, masgen, 1, zr(jmass), 1)
        call trlds(zr(jmass), neqgen, neqgen, iret)
        if (iret .ne. 0) then
            call u2mess('F', 'ALGORITH5_22')
        endif
        call dcopy(neqgen*neqgen, masgen, 1, zr(jmass), 1)
    else if (typbas.eq.'MODELE_GENE     ') then
        matpre='&&MDEUL1.MATPRE'
        matasm=zk24(zi(descmm+1))(1:19)
        call preres(' ', 'V', iret, matpre, matasm,&
                    ibid, -9999)
    else
        call wkvect('&&MDEUL1.MASS', 'V V R8', neqgen, jmass)
        call dcopy(neqgen, masgen, 1, zr(jmass), 1)
        if (nbchoc .ne. 0) then
            if (lflu) then
!
!     CALCUL DE LA MATRICE DIAGONALE POUR LES NOEUDS DE LAME FLUIDE
!
                call wkvect('&&MDEUL1.PHI2', 'V V R8', neqgen*nbchoc, jphi2)
!
!     CALCUL DES MATRICES M' PAR NOEUD DE CHOC FLUIDE
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
    call wkvect('&&MDEUL1.DEPL', 'V V R8', neqgen, jdepl)
    call wkvect('&&MDEUL1.VITE', 'V V R8', neqgen, jvite)
    call wkvect('&&MDEUL1.ACCE', 'V V R8', neqgen, jacce)
    call wkvect('&&MDEUL1.TRA1', 'V V R8', neqgen, jtra1)
    call wkvect('&&MDEUL1.FEXT', 'V V R8', neqgen, jfext)
    if (lflu) then
        call wkvect('&&MDEUL1.FEXTI', 'V V R8', neqgen, jfexti)
        call wkvect('&&MDEUL1.ACCGENI', 'V V R8', neqgen, jaccgi)
        call wkvect('&&MDEUL1.PULSAI', 'V V R8', neqgen, jpuls)
        call wkvect('&&MDEUL1.AMOGEI', 'V V R8', neqgen, jamogi)
        call dcopy(neqgen, pulsa2, 1, zr(jpuls), 1)
        call dcopy(neqgen, amogen, 1, zr(jamogi), 1)
    endif
    if (nbchoc .ne. 0 .and. nbpal .eq. 0) then
!      IF (NBCHOC.NE.0  ) THEN
        call wkvect('&&MDEUL1.SCHOR', 'V V R8', nbchoc*14, jchor)
!        INITIALISATION POUR LE FLAMBAGE
        call jeveuo(nomres//'           .VINT', 'E', jvint)
        call r8inir(nbchoc, 0.d0, zr(jvint), 1)
    endif
    if (nbrede .ne. 0) then
        call wkvect('&&MDEUL1.SREDR', 'V V R8', nbrede, jredr)
        call wkvect('&&MDEUL1.SREDI', 'V V I', nbrede, jredi)
    endif
    if (nbrevi .ne. 0) then
        call wkvect('&&MDEUL1.SREVR', 'V V R8', nbrevi, jrevr)
        call wkvect('&&MDEUL1.SREVI', 'V V I', nbrevi, jrevi)
    endif
!
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
    if (lflu) then
!
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
                    psidel, monmot, nbrfis, fk, dfk,&
                    angini, foncp, 1, 0, dt,&
                    dtsto, vrotat, typal, finpal, cnpal,&
                    prdeff, conv, fsauv)
        if ((conv.le.0.d0) .and. (nbconv.gt.nbmxcv)) then
            call u2mess('F', 'EDYOS_46')
        else if ((conv.le.0.d0) .and. (nbconv.le.nbmxcv)) then
            nbconv = nbconv + 1
        endif
!
!
!     --- ACCELERATIONS GENERALISEES INITIALES ---
!
        call mdacce(typbas, neqgen, zr(jpuls), zr(jmass), descmm,&
                    riggen, descmr, zr(jfext), lamor, zr(jamogi),&
                    descma, zr(jtra1), zr(jdepl), zr(jvite), zr(jacce))
    else
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
21          continue
        endif
!  FIN COUPLAGE AVEC EDYOS
!
!       CAS CLASSIQUE
!
        if (nbpal .ne. 0) nbchoc = 0
        call mdfnli(neqgen, zr(jdepl), zr(jvite), zr(jacce), zr(jfext),&
                    masgen, r8b, pulsa2, zr(jamgy), nbchoc,&
                    logcho, dplmod, parcho, noecho, zr(jchor),&
                    nbrede, dplred, fonred, zr(jredr), zi(jredi),&
                    nbrevi, dplrev, fonrev, zr(jrevr), zi(jrevi),&
                    tinit, nofdep, nofvit, nofacc, nbexci,&
                    psidel, monmot, nbrfis, fk, dfk,&
                    angini, foncp, 1, nbpal, dt,&
                    dtsto, vrotat, typal, finpal, cnpal,&
                    prdeff, conv, fsauv)
!
        if ((conv.le.0.d0) .and. (nbconv.gt.nbmxcv)) then
            call u2mess('F', 'EDYOS_46')
        else if ((conv.le.0.d0) .and. (nbconv.le.nbmxcv)) then
            nbconv = nbconv + 1
        endif
!
!
!     --- ACCELERATIONS GENERALISEES INITIALES ---
!
        call mdacce(typbas, neqgen, pulsa2, masgen, descmm,&
                    riggen, descmr, zr(jfext), lamor, zr(jamgy),&
                    descma, zr(jtra1), zr(jdepl), zr(jvite), zr(jacce))
!
    endif
!
!     --- ARCHIVAGE DONNEES INITIALES ---
!
    tarchi = tinit
!
    call mdarnl(isto1, 0, tinit, dt, neqgen,&
                zr(jdepl), zr(jvite), zr(jacce), isto2, nbchoc,&
                zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                zi(jredi), isto4, nbrevi, zr(jrevr), zi(jrevi),&
                depsto, vitsto, accsto, passto, iorsto,&
                temsto, fchost, dchost, vchost, ichost,&
                zr(jvint), iredst, dredst, irevst, drevst)
!
    temps = tinit + dt
    call uttcpu('CPU.MDEUL1', 'INIT', ' ')
    n100 = nbpas/100 + 1
!
!     --- BOUCLE TEMPORELLE ---
!
    do 30 i = 1, nbpas
!
        if (mod(i,n100) .eq. 0) call uttcpu('CPU.MDEUL1', 'DEBUT', ' ')
!
        if (lamor) then
            do 110 im = 1, neqgen
                do 121 jm = 1, neqgen
                    ind = jm + neqgen*(im-1)
!              --- LAMOR = .TRUE. ALORS COPIER LA LISTE DES AMORTISS.
!                  (TERMES DIAGONAUX) DANS LE VECTEUR DE TRAVAIL
                    zr(jamgy+ind-1) = amogen(jm)
121              continue
110          continue
        else
            vrot = 0.d0
            arot = 0.d0
            if (vvar .eq. 'OUI') then
                call fointe('F ', foncv, 1, 'INST', temps,&
                            vrot, ier)
                call fointe('F ', fonca, 1, 'INST', temps,&
                            arot, ier)
                do 115 im = 1, neqgen
                    do 116 jm = 1, neqgen
                        ind = jm + neqgen*(im-1)
                        zr(jamgy+ind-1) = amogen(ind) + vrot * gyogen( ind)
                        zr(jrigy+ind-1) = riggen(ind) + arot * rgygen( ind)
116                  continue
115              continue
            else
                do 119 im = 1, neqgen
                    do 120 jm = 1, neqgen
                        ind = jm + neqgen*(im-1)
                        zr(jamgy+ind-1) = amogen(ind)
                        zr(jrigy+ind-1) = riggen(ind)
120                  continue
119              continue
            endif
        endif
!
        do 40 im = 0, nbmod1
!           --- VITESSES GENERALISEES ---
            zr(jvite+im) = zr(jvite+im) + ( dt * zr(jacce+im) )
!           --- DEPLACEMENTS GENERALISES ---
            zr(jdepl+im) = zr(jdepl+im) + ( dt * zr(jvite+im) )
40      continue
!
!        --- FORCES EXTERIEURES ---
!
        do 20 ifor = 0, neqgen-1
            zr(jfext+ifor) = zero
20      continue
        if (nbexci .ne. 0) then
            call mdfext(temps, r8bid1, neqgen, nbexci, idescf,&
                        nomfon, coefm, liad, inumor, 1,&
                        zr(jfext))
        endif
!
        if (lflu) then
!        ------------------------------------------------------
!        ITERATIONS IMPLICITES POUR OBTENIR L'ACCELERATION DANS
!        LE CAS DE FORCE DE LAME FLUIDE
!        ------------------------------------------------------
            xnorm = 0.d0
            xref = 0.d0
            do 5 iter = 1, itemax
!
!           REMISE A JOUR DE LA MASSE, PULSATION CARRE
!           DE L'AMORTISSEMENT MODAL ET DE LA FORCE EXT
!
                call dcopy(neqgen, masgen, 1, zr(jmass), 1)
                call dcopy(neqgen, pulsa2, 1, zr(jpuls), 1)
                call dcopy(neqgen, amogen, 1, zr(jamogi), 1)
                call dcopy(neqgen, zr(jfext), 1, zr(jfexti), 1)
!
!         --- CONTRIBUTION DES FORCES NON LINEAIRES ---
!
                call mdfnli(neqgen, zr(jdepl), zr(jvite), zr(jacce), zr(jfexti),&
                            zr(jmass), zr(jphi2), zr(jpuls), zr(jamogi), nbchoc,&
                            logcho, dplmod, parcho, noecho, zr(jchor),&
                            nbrede, dplred, fonred, zr(jredr), zi(jredi),&
                            nbrevi, dplrev, fonrev, zr(jrevr), zi(jrevi),&
                            temps, nofdep, nofvit, nofacc, nbexci,&
                            psidel, monmot, nbrfis, fk, dfk,&
                            angini, foncp, (i+1), nbpal, dt,&
                            dtsto, vrotat, typal, finpal, cnpal,&
                            prdeff, conv, fsauv)
!
                if ((conv.le.0.d0) .and. (nbconv.gt.nbmxcv)) then
                    call u2mess('F', 'EDYOS_46')
                else if ((conv.le.0.d0) .and. (nbconv.le.nbmxcv)) then
                    nbconv = nbconv + 1
                endif
!
!           --- ACCELERATIONS GENERALISEES ---
!
                call mdacce(typbas, neqgen, zr(jpuls), zr(jmass), descmm,&
                            riggen, descmr, zr(jfexti), lamor, zr(jamogi),&
                            descma, zr(jtra1), zr(jdepl), zr(jvite), zr(jaccgi))
                xnorm = 0.d0
                xref = 0.d0
                do 15 im = 1, neqgen
                    xnorm = xnorm + (zr(jaccgi+im-1)-zr(jacce+im-1))** 2
                    xref = xref + zr(jaccgi+im-1)**2
15              continue
                call dcopy(neqgen, zr(jaccgi), 1, zr(jacce), 1)
!           TEST DE CONVERGENCE
                if (xnorm .le. prec*xref) goto 25
 5          continue
!
!        NON CONVERGENCE
!
            vali (1) = itemax
            valr (1) = xnorm/xref
            call u2mesg('F', 'ALGORITH16_11', 0, ' ', 1,&
                        vali, 1, valr)
!
25          continue
        else
!
!        CALCUL CLASSIQUE FORCES NON-LINEAIRES ET ACCELERATIONS
!
!
!        --- CONTRIBUTION DES FORCES NON LINEAIRES ---
!
            call mdfnli(neqgen, zr(jdepl), zr(jvite), zr(jacce), zr(jfext),&
                        r8b, r8b, r8b, r8b, nbchoc,&
                        logcho, dplmod, parcho, noecho, zr(jchor),&
                        nbrede, dplred, fonred, zr(jredr), zi(jredi),&
                        nbrevi, dplrev, fonrev, zr(jrevr), zi(jrevi),&
                        temps, nofdep, nofvit, nofacc, nbexci,&
                        psidel, monmot, nbrfis, fk, dfk,&
                        angini, foncp, (i+1), nbpal, dt,&
                        dtsto, vrotat, typal, finpal, cnpal,&
                        prdeff, conv, fsauv)
!
            if ((conv.le.0.d0) .and. (nbconv.gt.nbmxcv)) then
                call u2mess('F', 'EDYOS_46')
            else if ((conv.le.0.d0) .and. (nbconv.le.nbmxcv)) then
                nbconv = nbconv + 1
            endif
!
!        --- ACCELERATIONS GENERALISEES ---
!
            call mdacce(typbas, neqgen, pulsa2, masgen, descmm,&
                        riggen, descmr, zr(jfext), lamor, zr(jamgy),&
                        descma, zr(jtra1), zr( jdepl), zr(jvite), zr(jacce))
!
        endif
!
!        --- ARCHIVAGE ---
!
        if (iparch(i) .eq. 1) then
            iarchi = i
            tarchi = temps
            isto1 = isto1 + 1
!
            call mdarnl(isto1, iarchi, temps, dt, neqgen,&
                        zr(jdepl), zr( jvite), zr(jacce), isto2, nbchoc,&
                        zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                        zi(jredi), isto4, nbrevi, zr(jrevr), zi( jrevi),&
                        depsto, vitsto, accsto, passto, iorsto,&
                        temsto, fchost, dchost, vchost, ichost,&
                        zr(jvint), iredst, dredst, irevst, drevst)
!
        endif
!
!        --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1 ---
!
        if (etausr() .eq. 1) then
            call sigusr()
        endif
!
!        --- TEST SI LE TEMPS RESTANT EST SUFFISANT POUR CONTINUER ---
!
        if (mod(i,n100) .eq. 0) then
            call uttcpu('CPU.MDEUL1', 'FIN', ' ')
            call uttcpr('CPU.MDEUL1', 4, tps1)
            rint1 = 5.d0
            rint2 = 0.90d0
            if (max(rint1,n100*tps1(4)) .gt. (rint2*tps1(1))) then
                call mdsize(nomres, isto1, neqgen, nbchoc, nbrede,&
                            nbrevi)
                if (nomres .eq. '&&OP0074') then
!          --- CAS D'UNE POURSUITE ---
                    call getvid('ETAT_INIT', 'RESULTAT', iocc=1, scal=tran, nbret=ndt)
                    if (ndt .ne. 0) call resu74(tran, nomres)
                endif
                vali (1) = i
                vali (2) = isto1
                valr (1) = tarchi
                valr (2) = tps1(4)
                valr (3) = tps1(1)
                call utexcm(28, 'ALGORITH16_77', 0, ' ', 2,&
                            vali, 3, valr)
                goto 9999
            endif
        endif
        temps = temps + dt
30  end do
!
9999  continue
    call jedetr('&&MDEUL1.DEPL')
    call jedetr('&&MDEUL1.VITE')
    call jedetr('&&MDEUL1.ACCE')
    call jedetr('&&MDEUL1.TRA1')
    call jedetr('&&MDEUL1.FEXT')
    call jedetr('&&MDEUL1.MASS')
    if (lflu) then
        call jedetr('&&MDEUL1.FEXTI')
        call jedetr('&&MDEUL1.ACCGENI')
        call jedetr('&&MDEUL1.PULSAI')
        call jedetr('&&MDEUL1.AMOGEI')
        call jedetr('&&MDEUL1.PHI2')
    endif
    if (nbchoc .ne. 0) then
        call jedetr('&&MDEUL1.SCHOR')
    endif
    if (nbrede .ne. 0) then
        call jedetr('&&MDEUL1.SREDR')
        call jedetr('&&MDEUL1.SREDI')
    endif
    if (nbrevi .ne. 0) then
        call jedetr('&&MDEUL1.SREVR')
        call jedetr('&&MDEUL1.SREVI')
    endif
    if (iret .ne. 0) call u2mess('F', 'ALGORITH5_24')
!
    call jedema()
end subroutine

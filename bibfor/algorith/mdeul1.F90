subroutine mdeul1(nbpas, dt, neqgen, pulsat, pulsa2,&
                  masgen, descmm, riggen, descmr, rgygen,&
                  lamor, amogen, descma, gyogen, foncv,&
                  fonca, typbas, basemo, tinit, iparch,&
                  nbsauv, nbchoc, logcho, dplmod, parcho,&
                  noecho, nbrede, dplred, fonred, nbrevi,&
                  dplrev, fonrev, depsto, vitsto, accsto,&
                  iorsto, temsto, fchost, dchost, vchost,&
                  ichost, iredst, dredst, irevst, drevst,&
                  coefm, liad, inumor, idescf, nofdep,&
                  nofvit, nofacc, nomfon, psidel, monmot,&
                  nbrfis, fk, dfk, angini, foncp,&
                  nbpal, dtsto, vrotat, prdeff, nomres,&
                  nbexci, passto, intitu)
!
! aslint: disable=W1504
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterfort/amgene.h"
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
#include "asterfort/mdtr74grd.h"
#include "asterfort/preres.h"
#include "asterfort/r8inir.h"
#include "asterfort/resu74.h"
#include "asterfort/rigene.h"
#include "asterfort/sigusr.h"
#include "asterfort/trlds.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/dcopy.h"
    integer :: nbchoc, neqgen
    integer :: iorsto(*), iredst(*), descmm, descmr, descma, iparch(*)
    integer :: logcho(nbchoc, *), ichost(*), ibid, irevst(*)
    real(kind=8) :: pulsat(*), pulsa2(*), masgen(*), riggen(*), amogen(*)
    real(kind=8) :: gyogen(*), rgygen(*), parcho(*), depsto(*)
    real(kind=8) :: vitsto(*), accsto(*), temsto(*), fchost(*), dchost(*)
    real(kind=8) :: vchost(*), dredst(*), drevst(*)
    real(kind=8) :: dplmod(nbchoc, neqgen, *), dplred(*), dplrev(*), passto(*)
    real(kind=8) :: dt, dtsto, vrotat, angini
    character(len=8) :: basemo, noecho(nbchoc, *), fonred(*), fonrev(*)
    character(len=8) :: nomres, monmot
    character(len=16) :: typbas
    aster_logical :: lamor, prdeff, condrepri
!
    real(kind=8) :: coefm(*), psidel(*)
    integer :: liad(*), inumor(*), idescf(*)
    integer :: nbpal, nbrfis
    character(len=8) :: nofdep(*), nofvit(*), nofacc(*), nomfon(*), intitu(*)
    character(len=8) :: fk(2), dfk(2), foncv, fonca, foncp
!
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     ALGORITHME EULER D'ORDRE 1
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
! IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
! IN  : LOGCHO : INDICATEUR D'ADHERENCE
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
    character(len=8) :: tran, vvar
    character(len=19) :: matpre, matasm
!     ------------------------------------------------------------------
!
    integer :: palmax
!-----------------------------------------------------------------------
    integer :: i, iarchi, ifor, im, iret, isto1, ier, ind
    integer :: isto2, isto3, jchor
    integer :: jdepl, jm, jmass
    integer :: jredi, jredr, jvint, jvite, n100
    integer :: nbexci, nbmod1, nbpas, nbrede, nbrevi, nbsauv, nbscho
    integer :: ndt, jamgy, jrigy, jrevr, jrevi, isto4
    real(kind=8) :: deux, r8bid1, tarchi
    real(kind=8) :: temps, tinit, zero
!
!-----------------------------------------------------------------------
    parameter (palmax=20)
    integer :: iadrk, iapp, nbschor, nbvint
    integer :: dimnas
    parameter     (dimnas=8)
    character(len=3) :: finpal(palmax)
    character(len=6) :: typal(palmax)
    character(len=8) :: cnpal(palmax)
    character(len=19) :: solveu
    character(len=24) :: cpal
    real(kind=8) :: fsauv(palmax, 3), vrot, arot, vrotin, arotin
    real(kind=8), pointer :: acce(:) => null()
    real(kind=8), pointer :: fext(:) => null()
    real(kind=8), pointer :: tra1(:) => null()
!
!   ------------------------------------------------------------------------------------
!   Definition of statement functions giving the appropriate (i,j) term in the mass,
!   rigidity and damping matrices
#define rgen(row,col) rigene(row, col, riggen, neqgen, typbas, 'EULER')
#define agen(row,col) amgene(row, col, amogen, neqgen, typbas, 'EULER', lamor)
!   ------------------------------------------------------------------------------------
!
    call jemarq()
    solveu='&&OP0074.SOLVEUR'
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
    do iapp = 1, palmax
        typal(iapp)='      '
        finpal(iapp)='   '
        cnpal(iapp)=' '
    end do
!
    call wkvect('&&MDEUL1.AMOGYR', 'V V R8', neqgen*neqgen, jamgy)
    call wkvect('&&MDEUL1.RIGGYR', 'V V R8', neqgen*neqgen, jrigy)
    if (lamor) then
        do im = 1, neqgen
            amogen(im) = deux * amogen(im) * pulsat(im)
        enddo
    else
        call getvtx(' ', 'VITESSE_VARIABLE', nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvtx(' ', 'VITESSE_VARIABLE', scal=vvar, nbret=n1)
        endif
        vrotin = 0.d0
        arotin = 0.d0
        if (vvar .eq. 'OUI') then
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
!     --- FACTORISATION DE LA MATRICE MASSE ---
!
    if (typbas .eq. 'BASE_MODA') then
        call wkvect('&&MDEUL1.MASS', 'V V R8', neqgen*neqgen, jmass)
        call dcopy(neqgen*neqgen, masgen, 1, zr(jmass), 1)
        call trlds(zr(jmass), neqgen, neqgen, iret)
        if (iret .ne. 0) then
            call utmess('F', 'ALGORITH5_22')
        endif
        call dcopy(neqgen*neqgen, masgen, 1, zr(jmass), 1)
    else if (typbas.eq.'MODELE_GENE     ') then
        matpre='&&MDEUL1.MATPRE'
        matasm=zk24(zi(descmm+1))(1:19)
        call preres(solveu, 'V', iret, matpre, matasm,&
                    ibid, -9999)
    else
        call wkvect('&&MDEUL1.MASS', 'V V R8', neqgen, jmass)
        call dcopy(neqgen, masgen, 1, zr(jmass), 1)
    endif
!
!     --- VECTEURS DE TRAVAIL ---
!
    call wkvect('&&MDEUL1.DEPL', 'V V R8', neqgen, jdepl)
    call wkvect('&&MDEUL1.VITE', 'V V R8', neqgen, jvite)
    AS_ALLOCATE(vr=acce, size=neqgen)
    AS_ALLOCATE(vr=tra1, size=neqgen)
    AS_ALLOCATE(vr=fext, size=neqgen)
    if (nbchoc .ne. 0 .and. nbpal .eq. 0) then
        nbschor = nbchoc*(mdtr74grd('SCHOR')+mdtr74grd('MAXVINT'))
        call wkvect('&&MDEUL1.SCHOR', 'V V R8', nbschor, jchor)
!       initialisation variables internes
        call jeveuo(nomres//'           .VINT', 'E', jvint)
        nbvint = nbsauv*nbchoc*mdtr74grd('MAXVINT')
        call r8inir(nbvint, 0.d0, zr(jvint), 1)
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
!   conditions initiales
    call mdinit(basemo, neqgen, nbchoc, zr(jdepl), zr(jvite),&
                zr(jvint), iret, tinit, intitu=intitu, noecho=noecho,&
                reprise=condrepri, accgen=acce)
    if (iret .ne. 0) goto 9999
    if (nbchoc .gt. 0 .and. nbpal .eq. 0) then
        nbvint = nbchoc*mdtr74grd('MAXVINT')
        call dcopy(nbvint, zr(jvint), 1, zr(jchor+mdtr74grd('SCHOR')*nbchoc), 1)
    endif
!   forces exterieures
    if (nbexci .ne. 0) then
        call mdfext(tinit, r8bid1, neqgen, nbexci, idescf,&
                    nomfon, coefm, liad, inumor, 1,&
                    fext)
    endif
!
!   couplage avec edyos
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
        enddo
    endif
!  fin couplage avec edyos
!
!   cas classique
    if (nbpal .ne. 0) nbchoc = 0
!   Si ce n'est pas une reprise : on calcule l'état initial
    if (.not. condrepri) then
        call mdfnli(neqgen, zr(jdepl), zr(jvite), acce, fext,&
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
                    riggen, descmr, fext, lamor, zr(jamgy),&
                    descma, tra1, zr(jdepl), zr(jvite), acce)
    endif
!
!   archivage donnees initiales
    tarchi = tinit
    call mdarnl(isto1, 0, tinit, dt, neqgen,&
                zr(jdepl), zr(jvite), acce, isto2, nbchoc,&
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
!   boucle temporelle
    do i = 1, nbpas
!
        if (mod(i,n100) .eq. 0) call uttcpu('CPU.MDEUL1', 'DEBUT', ' ')
!
        if (lamor) then
            do im = 1, neqgen
                do jm = 1, neqgen
                    ind = jm + neqgen*(im-1)
!              --- LAMOR = .TRUE. ALORS COPIER LA LISTE DES AMORTISS.
!                  (TERMES DIAGONAUX) DANS LE VECTEUR DE TRAVAIL
                    zr(jamgy+ind-1) = amogen(jm)
                enddo
            enddo
        else
            vrot = 0.d0
            arot = 0.d0
            if (vvar .eq. 'OUI') then
                call fointe('F ', foncv, 1, ['INST'], [temps],&
                            vrot, ier)
                call fointe('F ', fonca, 1, ['INST'], [temps],&
                            arot, ier)
                do im = 1, neqgen
                    do jm = 1, neqgen
                        ind = jm + neqgen*(im-1)
                        zr(jamgy+ind-1) = amogen(ind) + vrot * gyogen( ind)
                        zr(jrigy+ind-1) = riggen(ind) + arot * rgygen( ind)
                    enddo
                enddo
            else
                do im = 1, neqgen
                    do jm = 1, neqgen
                        ind = jm + neqgen*(im-1)
                        zr(jamgy+ind-1) = amogen(ind)
                        zr(jrigy+ind-1) = riggen(ind)
                    enddo
                enddo
            endif
        endif
!
        do im = 0, nbmod1
!           --- VITESSES GENERALISEES ---
            zr(jvite+im) = zr(jvite+im) + ( dt * acce(im+1) )
!           --- DEPLACEMENTS GENERALISES ---
            zr(jdepl+im) = zr(jdepl+im) + ( dt * zr(jvite+im) )
        enddo
!
!        --- FORCES EXTERIEURES ---
!
        do ifor = 0, neqgen-1
            fext(ifor+1) = zero
        enddo
        if (nbexci .ne. 0) then
            call mdfext(temps, r8bid1, neqgen, nbexci, idescf,&
                        nomfon, coefm, liad, inumor, 1,&
                        fext)
        endif
!
!       CALCUL CLASSIQUE FORCES NON-LINEAIRES ET ACCELERATIONS
!       CONTRIBUTION DES FORCES NON LINEAIRES
        call mdfnli(neqgen, zr(jdepl), zr(jvite), acce, fext,&
                    nbchoc, logcho, dplmod, parcho, noecho,&
                    zr(jchor), nbrede, dplred, fonred, zr(jredr),&
                    zi(jredi), nbrevi, dplrev, fonrev, zr(jrevr),&
                    zi(jrevi), temps, nofdep, nofvit, nofacc,&
                    nbexci, psidel, monmot, nbrfis, fk,&
                    dfk, angini, foncp, (i+1), nbpal,&
                    dt, dtsto, vrotat, typal, finpal,&
                    cnpal, prdeff, conv, fsauv)
!
        if ((conv.le.0.d0) .and. (nbconv.gt.nbmxcv)) then
            call utmess('F', 'EDYOS_46')
        else if ((conv.le.0.d0) .and. (nbconv.le.nbmxcv)) then
            nbconv = nbconv + 1
        endif
!
!       ACCELERATIONS GENERALISEES ---
        call mdacce(typbas, neqgen, pulsa2, masgen, descmm,&
                    riggen, descmr, fext, lamor, zr(jamgy),&
                    descma, tra1, zr(jdepl), zr(jvite), acce)
!
!        --- ARCHIVAGE ---
!
        if (iparch(i) .eq. 1) then
            iarchi = i
            tarchi = temps
            isto1 = isto1 + 1
!
            call mdarnl(isto1, iarchi, temps, dt, neqgen,&
                        zr(jdepl), zr(jvite), acce, isto2, nbchoc,&
                        zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                        zi(jredi), isto4, nbrevi, zr(jrevr), zi(jrevi),&
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
                call utmess('Z', 'ALGORITH16_77', ni=2, vali=vali, nr=3,&
                            valr=valr, num_except=28)
                goto 9999
            endif
        endif
        temps = temps + dt
    enddo
!
9999 continue
    call jedetr('&&MDEUL1.DEPL')
    call jedetr('&&MDEUL1.VITE')
    AS_DEALLOCATE(vr=acce)
    AS_DEALLOCATE(vr=tra1)
    AS_DEALLOCATE(vr=fext)
    call jedetr('&&MDEUL1.MASS')
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
    if (iret .ne. 0) then
        call utmess('F', 'ALGORITH5_24')
    endif
!
    call jedema()
end subroutine

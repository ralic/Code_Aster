subroutine mddevo(nbpas, dt, nbmode, pulsat, pulsa2,&
                  masgen, amogen, basemo, tinit, iparch,&
                  nbsauv, nbchoc, logcho, dplmod, parcho,&
                  noecho, nbrede, dplred, fonred, nbrevi,&
                  dplrev, fonrev, depsto, vitsto, accsto,&
                  iorsto, temsto, fchost, dchost, vchost,&
                  ichost, iredst, dredst, coefm, liad,&
                  inumor, idescf, nofdep, nofvit, nofacc,&
                  nomfon, psidel, monmot, nomres, nbexci,&
                  passto, irevst, drevst)
!
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdarnl.h"
#include "asterfort/mdfext.h"
#include "asterfort/mdfnli.h"
#include "asterfort/mdinit.h"
#include "asterfort/mdsize.h"
#include "asterfort/r8inir.h"
#include "asterfort/resu74.h"
#include "asterfort/sigusr.h"
#include "asterfort/u2mess.h"
#include "asterfort/utexcm.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: logcho(*), iorsto(*), iredst(*), iparch(*), ichost(*), irevst(*)
    integer :: nbmode, nbchoc
    real(kind=8) :: pulsat(*), pulsa2(*), masgen(*), amogen(*), parcho(*)
    real(kind=8) :: depsto(*), vitsto(*), accsto(*), temsto(*)
    real(kind=8) :: fchost(*), dchost(*), vchost(*), dredst(*), dplred(*)
    real(kind=8) :: dplrev(*), passto(*), dplmod(nbchoc, nbmode, *), drevst(*)
    character(len=8) :: basemo, noecho(*), fonred(*), fonrev(*), nomres, monmot
    character(len=8) :: fbid(2), k8b
!
    real(kind=8) :: coefm(*), psidel(*)
    integer :: liad(*), inumor(*), idescf(*)
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
!     ALGORITHME DE DEVOGELAERE
!     ------------------------------------------------------------------
! IN  : NBPAS  : NOMBRE DE PAS
! IN  : DT     : PAS DE TEMPS
! IN  : NBMODE : NOMBRE DE MODES
! IN  : PULSAT : PULSATIONS MODALES
! IN  : PULSA2 : PULSATIONS MODALES AU CARREES
! IN  : MASGEN : MASSES GENERALISEES
!                MATRICE DE MASSE GENERALISEE
! IN  : AMOGEN : AMORTISSEMENTS REDUITS
!                MATRICE D'AMORTISSEMENT
! IN  : BASEMO : NOM K8 DE LA BASE MODALE DE PROJECTION SI C'EST UN
!                MODE MECA K8BID LORS D'UN CALCUL PAR SOUS_STUCTURATION
! IN  : TINIT  : TEMPS INITIAL
! IN  : IPARCH : VECTEUR DES PAS D'ARCHIVAGE
! IN  : NBSAUV : NOMBRE DE PAS ARCHIVE
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
!     ------------------------------------------------------------------
!
    logical :: prdeff
    integer :: vali(2)
    real(kind=8) :: r8bid2, r8bid3, r8bid4, r8bid5, r8b2, tps1(4), valr(3)
    real(kind=8) :: rint1, rint2
    character(len=8) :: tran
    integer :: palmax
!-----------------------------------------------------------------------
    integer :: i, iarchi, if, im, im1, iret, isto1
    integer :: isto2, isto3, jacce, jbid1, jchor, jdep1, jdep2
    integer :: jdep3, jdep4, jdepl, jfex1, jfex2, jfex3, jfex4
    integer :: jfext, jredi, jredr, jtra1, jvint, jvit1, jvit2
    integer :: jvit3, jvit4, jvite, n100, nbexci, nbmod1
    integer :: isto4, jrevr, jrevi
    integer :: nbpas, nbrede, nbrevi, nbsauv, nbscho, ndt
!
    real(kind=8) :: deux, dt, dt1, dt2, dt3, dt4, dt5, r8b(1)
    real(kind=8) :: dt6, g1, g2, g3, g4, quatre, six
    real(kind=8) :: tarchi, temps, tinit, x1, x2, zero
!-----------------------------------------------------------------------
    parameter (palmax=20)
    character(len=6) :: typal(palmax)
    character(len=3) :: finpal(palmax)
    character(len=8) :: cnpal(palmax)
    real(kind=8) :: fsauv(palmax, 3)
!
    call jemarq()
    zero = 0.d0
    jvint = 1
    call wkvect('&&MDDEVO.BID', 'V V R8', nbmode, jbid1)
    do 10 i = 1, nbmode
        zr(jbid1+i-1)=zero
10  end do
    r8bid2=zero
    r8bid3=zero
    r8bid4=zero
    r8bid5=zero
    deux = 2.d0
    quatre = 4.d0
    six = 6.d0
    dt1 = dt / deux
    dt2 = dt / quatre
    dt3 = dt / six
    dt4 = dt * dt / 24.d0
    dt5 = dt * dt / six
    dt6 = dt * dt / 8.d0
    isto1 = 0
    isto2 = 0
    isto3 = 0
    nbmod1 = nbmode - 1
    nbscho = nbsauv * 3 * nbchoc
    prdeff = .false.
!
    fbid(1) = '        '
    fbid(2) = '        '
!
!     --- VECTEURS DE TRAVAIL ---
    call wkvect('&&MDDEVO.DEPL', 'V V R8', 4*nbmode, jdepl)
    call wkvect('&&MDDEVO.VITE', 'V V R8', 4*nbmode, jvite)
    call wkvect('&&MDDEVO.ACCE', 'V V R8', nbmode, jacce)
    call wkvect('&&MDDEVO.TRA1', 'V V R8', nbmode, jtra1)
    call wkvect('&&MDDEVO.FEXT', 'V V R8', 4*nbmode, jfext)
    if (nbchoc .ne. 0) then
        call wkvect('&&MDDEVO.SCHOR', 'V V R8', nbchoc*14, jchor)
!        INITIALISATION POUR LE FLAMBAGE
        call jeveuo(nomres//'           .VINT', 'E', jvint)
        call r8inir(nbchoc, 0.d0, zr(jvint), 1)
    else
        jchor=1
    endif
    if (nbrede .ne. 0) then
        call wkvect('&&MDDEVO.SREDR', 'V V R8', nbrede, jredr)
        call wkvect('&&MDDEVO.SREDI', 'V V I', nbrede, jredi)
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
    jdep1 = jdepl
    jdep2 = jdep1 + nbmode
    jdep3 = jdep2 + nbmode
    jdep4 = jdep3 + nbmode
    jvit1 = jvite
    jvit2 = jvit1 + nbmode
    jvit3 = jvit2 + nbmode
    jvit4 = jvit3 + nbmode
    jfex1 = jfext
    jfex2 = jfex1 + nbmode
    jfex3 = jfex2 + nbmode
    jfex4 = jfex3 + nbmode
!
!     --- CONDITIONS INITIALES ---
    call mdinit(basemo, nbmode, nbchoc, zr(jdep2), zr(jvit2),&
                zr(jvint), iret, tinit)
    if (iret .ne. 0) goto 9999
    if (nbchoc .gt. 0) then
        call dcopy(nbchoc, zr(jvint), 1, zr(jchor+13*nbchoc), 1)
    endif
!
!     --- FORCES EXTERIEURES ---
    if (nbexci .ne. 0) then
        call mdfext(tinit, r8bid2, nbmode, nbexci, idescf,&
                    nomfon, coefm, liad, inumor, 1,&
                    zr(jfex2))
    endif
!
!     --- CONTRIBUTION DES FORCES NON LINEAIRES ---
    call mdfnli(nbmode, zr(jdep2), zr(jvit2), zr(jbid1), zr(jfex2),&
                r8b, r8b, r8b, r8b, nbchoc,&
                logcho, dplmod, parcho, noecho, zr(jchor),&
                nbrede, dplred, fonred, zr(jredr), zi(jredi),&
                nbrevi, dplrev, fonrev, zr(jrevr), zi(jrevi),&
                tinit, nofdep, nofvit, nofacc, nbexci,&
                psidel, monmot, 0, fbid, fbid,&
                0.d0, k8b, 0, 0, 0.d0,&
                0.d0, 0.d0, typal, finpal, cnpal,&
                prdeff, r8b2, fsauv)
!
!     --- INITIALISATION DE L'ALGORITHME ---
    do 100 im = 0, nbmod1
        im1 = im + 1
        amogen(im1) = deux * pulsat(im1) * amogen(im1)
        zr(jfex1+im) = zr(jfex2+im)
        g2 = ( zr(jfex2+im) / masgen(im1) ) - pulsa2(im1)*zr(jdep2+im)
        zr(jdep1+im) = zr(jdep2+im) - dt1*zr(jvit2+im) + dt6 * ( g2 - amogen(im1)*zr(jvit2+im) )
        g1 = (zr(jfex1+im) / masgen(im1)) - pulsa2(im1)*zr(jdep1+im)
        zr(jvit1+im) = (&
                       1.d0 / (&
                       4.d0 - dt * amogen(im1) ) ) * ( zr(jvit2+im)*( 4.d0 + dt*amogen(im1) ) - d&
                       &t * ( g1 + g2&
                       )&
                       )
        zr(jacce+im) = g2 - amogen(im1)*zr(jvit2+im)
!
100  end do
!
!     --- ARCHIVAGE DONNEES INITIALES ---
    tarchi = tinit
!
    call mdarnl(isto1, 0, tinit, dt, nbmode,&
                zr(jdep2), zr(jvit2), zr(jacce), isto2, nbchoc,&
                zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                zi(jredi), isto4, nbrevi, zr(jrevr), zi(jrevi),&
                depsto, vitsto, accsto, passto, iorsto,&
                temsto, fchost, dchost, vchost, ichost,&
                zr(jvint), iredst, dredst, irevst, drevst)
!
!
    temps = tinit + dt1
    call uttcpu('CPU.MDDEVO', 'INIT', ' ')
    n100 = nbpas/100 + 1
!
!     --- BOUCLE TEMPORELLE ---
!
    do 30 i = 1, nbpas
!
        if (i .eq. 1 .or. mod(i,n100) .eq. 0) call uttcpu('CPU.MDDEVO', 'DEBUT', ' ')
!
        do 32 im = 0, nbmod1
            im1 = im + 1
            g1 = (zr(jfex1+im) / masgen(im1)) - pulsa2(im1)*zr(jdep1+ im)
            g2 = (zr(jfex2+im) / masgen(im1)) - pulsa2(im1)*zr(jdep2+ im)
!
!              --- DEPLACEMENTS GENERALISES AU DEMI PAS ---
            x2 = quatre*g2 - g1 - amogen(im1) * ( quatre*zr(jvit2+im) - zr(jvit1+im) )
            zr(jdep3+im) = zr(jdep2+im) + dt1*zr(jvit2+im) + dt4*x2
            zr(jvit3+im) = ( zr(jdep3+im) - zr(jdep2+im) ) / dt1
32      continue
!
!     --- FORCES EXTERIEURES ---
        do 20 if = 0, nbmode-1
            zr(jfex3+if) = zero
20      continue
        if (nbexci .ne. 0) then
            call mdfext(temps, r8bid2, nbmode, nbexci, idescf,&
                        nomfon, coefm, liad, inumor, 1,&
                        zr(jfex3))
        endif
!
!        --- CONTRIBUTION DES FORCES NON LINEAIRES ---
        call mdfnli(nbmode, zr(jdep3), zr(jvit3), zr(jbid1), zr(jfex3),&
                    r8b, r8b, r8b, r8b, nbchoc,&
                    logcho, dplmod, parcho, noecho, zr(jchor),&
                    nbrede, dplred, fonred, zr(jredr), zi(jredi),&
                    nbrevi, dplrev, fonrev, zr(jrevr), zi(jrevi),&
                    temps, nofdep, nofvit, nofacc, nbexci,&
                    psidel, monmot, 0, fbid, fbid,&
                    0.d0, k8b, 0, 0, 0.d0,&
                    0.d0, 0.d0, typal, finpal, cnpal,&
                    prdeff, r8b2, fsauv)
!
        do 34 im = 0, nbmod1
            im1 = im + 1
            g2 = (zr(jfex2+im) / masgen(im1)) - pulsa2(im1)*zr(jdep2+ im)
            g3 = (zr(jfex3+im) / masgen(im1)) - pulsa2(im1)*zr(jdep3+ im)
!
!              --- VITESSES GENERALISEES AU DEMI PAS ---
            x1 = quatre / ( quatre + dt*amogen(im1) )
            x2 = g2 + g3 - amogen(im1)*zr(jvit2+im)
            zr(jvit3+im) = x1 * ( zr(jvit2+im) + dt2*x2 )
!
!           --- DEPLACEMENTS GENERALISES AU PAS ---
            x2 = g2 + deux*g3 - amogen(im1) * ( zr(jvit2+im) + deux* zr(jvit3+im) )
            zr(jdep4+im) = zr(jdep2+im) + dt*zr(jvit2+im) + dt5*x2
            zr(jvit4+im) = ( zr(jdep4+im) - zr(jdep3+im) ) / dt1
34      continue
!
!        --- FORCES EXTERIEURES ---
        do 35 if = 0, nbmode-1
            zr(jfex4+if) = zero
35      continue
        temps = temps + dt1
        if (nbexci .ne. 0) then
            call mdfext(temps, r8bid2, nbmode, nbexci, idescf,&
                        nomfon, coefm, liad, inumor, 1,&
                        zr(jfex4))
        endif
!        --- CONTRIBUTION DES FORCES NON LINEAIRES ---
        call mdfnli(nbmode, zr(jdep4), zr(jvit4), zr(jbid1), zr(jfex4),&
                    r8b, r8b, r8b, r8b, nbchoc,&
                    logcho, dplmod, parcho, noecho, zr(jchor),&
                    nbrede, dplred, fonred, zr(jredr), zi(jredi),&
                    nbrevi, dplrev, fonrev, zr(jrevr), zi(jrevi),&
                    temps, nofdep, nofvit, nofacc, nbexci,&
                    psidel, monmot, 0, fbid, fbid,&
                    0.d0, k8b, 0, 0, 0.d0,&
                    0.d0, 0.d0, typal, finpal, cnpal,&
                    prdeff, r8b2, fsauv)
!
!
        do 36 im = 0, nbmod1
            im1 = im + 1
            g2 = (zr(jfex2+im) / masgen(im1)) - pulsa2(im1)*zr(jdep2+ im)
            g3 = (zr(jfex3+im) / masgen(im1)) - pulsa2(im1)*zr(jdep3+ im)
            g4 = (zr(jfex4+im) / masgen(im1)) - pulsa2(im1)*zr(jdep4+ im)
!
!           --- VITESSES GENERALISEES AU PAS ---
            x1 = six / ( six + dt*amogen(im1) )
            x2 = g4 + quatre*g3 + g2 - ( amogen(im1) * ( quatre*zr( jvit3+im) + zr(jvit2+im) ))
            zr(jvit4+im) = x1 * ( zr(jvit2+im) + dt3*x2 )
!
!           --- ACCELERATIONS GENERALISEES AU PAS ---
            zr(jacce+im) = g4 - amogen(im1)*zr(jvit4+im)
36      continue
!
!        --- ARCHIVAGE ---
        iarchi = i
        if (iparch(iarchi) .eq. 1) then
            isto1 = isto1 + 1
            tarchi = temps
!
            call mdarnl(isto1, iarchi, temps, dt, nbmode,&
                        zr(jdep4), zr( jvit4), zr(jacce), isto2, nbchoc,&
                        zr(jchor), nbscho, isto3, nbrede, zr(jredr),&
                        zi(jredi), isto4, nbrevi, zr(jrevr), zi( jrevi),&
                        depsto, vitsto, accsto, passto, iorsto,&
                        temsto, fchost, dchost, vchost, ichost,&
                        zr(jvint), iredst, dredst, irevst, drevst)
!
        endif
!
        do 40 im = 0, nbmod1
            zr(jdep1+im) = zr(jdep3+im)
            zr(jdep2+im) = zr(jdep4+im)
            zr(jvit1+im) = zr(jvit3+im)
            zr(jvit2+im) = zr(jvit4+im)
            zr(jfex1+im) = zr(jfex3+im)
            zr(jfex2+im) = zr(jfex4+im)
40      continue
!
!
!        --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1 ---
!
        if (etausr() .eq. 1) then
            call sigusr()
        endif
!
!        --- TEST SI LE TEMPS RESTANT EST SUFFISANT POUR CONTINUER ---
!
        if (i .eq. 1 .or. mod(i,n100) .eq. 0) then
            call uttcpu('CPU.MDDEVO', 'FIN', ' ')
            call uttcpr('CPU.MDDEVO', 4, tps1)
            rint1 = 5.d0
            rint2 = 0.90d0
            if (max(rint1,n100*tps1(4)) .gt. (rint2*tps1(1))) then
                call mdsize(nomres, isto1, nbmode, nbchoc, nbrede,&
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
        temps = temps+dt1
30  end do
!
9999  continue
    call jedetr('&&MDDEVO.DEPL')
    call jedetr('&&MDDEVO.VITE')
    call jedetr('&&MDDEVO.ACCE')
    call jedetr('&&MDDEVO.TRA1')
    call jedetr('&&MDDEVO.FEXT')
    if (nbchoc .ne. 0) call jedetr('&&MDDEVO.SCHOR')
    if (nbrede .ne. 0) then
        call jedetr('&&MDDEVO.SREDR')
        call jedetr('&&MDDEVO.SREDI')
    endif
    if (iret .ne. 0) call u2mess('F', 'ALGORITH5_24')
!
    call jedema()
end subroutine

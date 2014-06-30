subroutine op0171()
!
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
    implicit none
! ----------------------------------------------------------------------
!     COMMANDE:  THER_NON_LINE_MO
!
!
!
!
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/cresol.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/gnomsd.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/medith.h"
#include "asterfort/ntdoth.h"
#include "asterfort/nttain.h"
#include "asterfort/nttcmv.h"
#include "asterfort/numero.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rssepa.h"
#include "asterfort/sigusr.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
!
    logical(kind=1) :: matcst, coecst, prem, reasmt, reasvt
    integer :: parcri(9), iifm,   jlagp, jinst
    integer :: ibid, k, neq, iret
    integer :: itmaxl, iterl, ifm, niv, num
    integer :: iocc, n1, n2
    integer :: jtemp, jtempm, jtempp, j2nd, lonch, lglap
    integer :: jlcha,  nchar, jinfc
    integer :: i, jfcha, ialifc
    real(kind=8) :: tpsthe(6), tpsnp1, testn, testr
    real(kind=8) :: tps1(4), tps2(4), tpex
    real(kind=8) :: parcrr(9), testi, epsr, epsl
    real(kind=8) :: r8aux(1)
    character(len=1) :: ci1, ci2, creas, ce1, ce2
    character(len=8) :: k8bid
    character(len=16) :: k16bid, nomcmd, nomcvg
    character(len=19) :: infcha, solveu, maprec, lischa
    character(len=24) :: modele, mate, carele, fomult, charge, infoch
    character(len=24) :: nomch, vtemp, vtempm, vtempp, vec2nd
    character(len=24) :: result, ligrmo, tempev, tempin
    character(len=24) :: time, mediri, matass, noojb, numedd
    character(len=24) :: cndirp, cnchci, cnchtp
    character(len=24) :: chlapm, chlapp, cnresi, noobj
    character(len=76) :: fmt
    integer :: vali(2)
    real(kind=8) :: valr(2)
    integer, pointer :: infc(:) => null()
    character(len=24), pointer :: lcha(:) => null()
    real(kind=8), pointer :: lagpm(:) => null()
    real(kind=8), pointer :: lagpp(:) => null()
!
! ----------------------------------------------------------------------
!
    data infcha                 /'&&OP0171.INFCHA'/
    data solveu                 /'&&OP0171.SOLVEUR'/
    data maprec                 /'&&OP0171.MAPREC'/
    data result  /' '/
    data cndirp,cnchtp          /2*' '/
    data cnchci,cnresi    /2*' '/
    data chlapm,chlapp          /'&&OP0171.CLPM','&&OP0171.CLPP'/
    data vtemp,vec2nd           /'&&OP0171.TH'  ,'&&OP0171.2ND'/
    data vtempm,vtempp          /'&&OP0171.THM' ,'&&OP0171.THP'/
    data mediri                 /' '/
    data matass                 /'&&MTHASS'/
    data fmt                    /'(76(''*''))'/
!
! ======================================================================
!                    RECUPERATION DES OPERANDES
! ======================================================================
    call jemarq()
!
!-----RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infmaj()
!
    call infniv(ifm, niv)
!---------------------------------------------------------------------
    ce1 = ' '
    ce2 = ' '
!
!
!     DETERMINATION DU NOM DE LA SD INFO_CHARGE
!             12345678    90123    45678901234
    noobj ='12345678'//'.1234'//'.EXCIT.INFC'
    call gnomsd(' ', noobj, 10, 13)
    lischa = noobj(1:19)
!
! --- LECTURE DES OPERANDES DE LA COMMANDE
!
! --- NOM UTILISATEUR DU CONCEPT RESULTAT CREE PAR LA COMMANDE
!
    call getres(result, k16bid, nomcmd)
!
! --- DONNEES
!
    call ntdoth(modele, mate, carele, fomult, matcst,&
                coecst, infcha, k8bid, ibid)
    charge = infcha//'.LCHA'
    infoch = infcha//'.INFC'
!
! --- PARAMETRES DONNES APRES LE MOT-CLE FACTEUR SOLVEUR
!
    call cresol(solveu)
!
! --- RECUPERATION DU CRITERE DE CONVERGENCE
!
    nomcvg = 'CONVERGENCE'
    call getfac(nomcvg, iocc)
    if (iocc .eq. 1) then
        call getvr8(nomcvg, 'CRIT_TEMP_RELA', iocc=1, scal=parcrr(4), nbret=parcri(4))
        call getvr8(nomcvg, 'CRIT_ENTH_RELA', iocc=1, scal=parcrr(6), nbret=parcri(6))
!
        call getvis(nomcvg, 'ITER_GLOB_MAXI', iocc=1, scal=parcri(1), nbret=n1)
!
        call getvtx(nomcvg, 'ARRET', iocc=1, scal=k8bid, nbret=n1)
        parcri(9) = 0
        if (n1 .gt. 0) then
            if (k8bid .eq. 'NON') then
                parcri(9) = 1
            endif
        endif
    endif
    itmaxl = parcri(1)
    epsr = parcrr(4)
    epsl = parcrr(6)
!
! ======================================================================
!
    time = result(1:8)//'.CHTPS'
!
! --- NUMEROTATION ET CREATION DU PROFIL DE LA MATRICE
    noojb='12345678.00000.NUME.PRNO'
    call gnomsd(' ', noojb, 10, 14)
    numedd=noojb(1:14)
    call numero(' ', modele, infcha, solveu, 'VG',&
                numedd)
!
    call vtcreb(vtemp, numedd, 'V', 'R', neq)
!
!
    call getvid('ETAT_INIT', 'EVOL_THER', iocc=1, scal=tempev, nbret=n1)
    if (n1 .gt. 0) then
        call getvis('ETAT_INIT', 'NUME_ORDRE', iocc=1, scal=num, nbret=n2)
        if (n2 .le. 0) then
            ASSERT(.false.)
        else
            call rsexch('F', tempev, 'TEMP', num, tempin,&
                        iret)
        endif
        call vtcopy(tempin, vtemp, 'F', iret)
    endif
! ======================================================================
!
    ligrmo = modele(1:8)//'.MODELE'
    r8aux(1) = 0.d0
    call mecact('V', chlapm, 'MODELE', ligrmo, 'NEUT_R',&
                ncmp=1, nomcmp='X1', sr=r8aux(1))
!
    tpsnp1 = 0.d0
    prem = .true.
!
! --- MATRICE DE RIGIDITE ASSOCIEE AUX LAGRANGE
!
    call medith(modele, charge, infoch, mediri)
!
! ======================================================================
!
    call uttcpu('CPU.OP0171.1', 'INIT', ' ')
    call uttcpu('CPU.OP0171.1', 'DEBUT', ' ')
    call uttcpr('CPU.OP0171.1', 4, tps1)
    tpex = tps1(3)
    call uttcpu('CPU.OP0171.2', 'INIT', ' ')
!
    tpsthe(1) = tpsnp1
    tpsthe(2) = 0.d0
    tpsthe(3) = 0.d0
    tpsthe(4) = 0.d0
    tpsthe(5) = 0.d0
    tpsthe(6) = 0.d0
    write (ifm,fmt)
!
! --- DUPLICATION DES STRUCTURES DE DONNEES ET RECUPERATION D'ADRESSES
!
    call copisd('CHAMP_GD', 'V', vtemp(1:19), vtempm(1:19))
    call copisd('CHAMP_GD', 'V', vtemp(1:19), vtempp(1:19))
    call copisd('CHAMP_GD', 'V', vtemp(1:19), vec2nd(1:19))
    call jeveuo(vtemp(1:19 )//'.VALE', 'E', jtemp)
    call jeveuo(vtempm(1:19)//'.VALE', 'E', jtempm)
    call jeveuo(vtempp(1:19)//'.VALE', 'E', jtempp)
    call jeveuo(vec2nd(1:19)//'.VALE', 'E', j2nd)
    call jelira(vec2nd(1:19)//'.VALE', 'LONMAX', lonch)
!
! --- COMPTEUR ET CRITERES D'ARRET
!
    iterl = 0
    testi = 1.d0
    testr = 1.d0
    reasvt = .true.
    reasmt = .true.
    write (ifm,fmt)
    write (ifm,10002)
    10002 format ('*',1x,'ITERATION',1x,'*',1x,'CRIT_TEMPER',1x,'*',1x,&
     &     'VALE_TEST_TEMPER',1x,'*',1x,'CRIT_ENTHAL',1x,'*',1x,&
     &     'VALE_TEST_ENTHAL',1x,'*')
    10001 format ('*',3x,i4,a1,7x,4(1pd11.3,a1,3x),3x,'*')
!
    write (ifm,fmt)
!
! ======================================================================
!        ITERATIONS DU PROBLEME DE TRANSPORT EN THERMIQUE N_LINEAIRE
! ======================================================================
!
2000  continue
    call uttcpu('CPU.OP0171.2', 'DEBUT', ' ')
!
! --- ACTUALISATION EVENTUELLE DES VECTEURS ET DES MATRICES
!
    call nttcmv(modele, mate, carele, fomult, charge,&
                infcha, infoch, numedd, solveu, time,&
                chlapm, tpsthe, tpsnp1, reasvt, reasmt,&
                creas, vtemp, vtempm, vec2nd, matass,&
                maprec, cndirp, cnchci, cnchtp)
    reasmt = .true.
    reasvt = .false.
!
! --- ARRET DES ITERATIONS
!
    if ((testi.gt.epsr .or. testr .gt. epsl) .and. iterl .lt. itmaxl) then
!
! *** ON CONTINUE...
!
        iterl = iterl + 1
!
! - ITERATIONS INTERNES
!
        call nttain(modele, mate, carele, charge, infoch,&
                    numedd, solveu, time, epsr, lonch,&
                    matass, maprec, cnchci, cnresi, vtemp,&
                    vtempm, vtempp, vec2nd, chlapm, chlapp,&
                    ci1, ci2, testi)
!
! - ACTUALISATION DU CHAMP ENTHALPIE
!
        if (prem) then
!
            call jelira(chlapp(1:19)//'.CELV', 'LONUTI', lglap)
            call jeveuo(chlapp(1:19)//'.CELV', 'L', jlagp)
            call copisd('CHAMP_GD', 'V', chlapp(1:19), chlapm(1:19))
            prem = .false.
!
        else
!
            call jeveuo(chlapm(1:19)//'.CELV', 'E', vr=lagpm)
            call jeveuo(chlapp(1:19)//'.CELV', 'L', vr=lagpp)
            testr = 0.d0
            testn = 0.d0
            do 200 k = 1, lglap
                testr = testr + (lagpp(k)-lagpm(k))**2
                testn = testn + lagpp(k)**2
                lagpm(k) = lagpp(k)
200          continue
            testr = sqrt(testr/testn)
!
        endif
!
! - EVALUATION DE LA CONVERGENCE ET AFFICHAGE
!
        iifm = iunifi ('MESSAGE')
        write(iifm,10001) iterl,ce1,epsr,ce2,testi,ce1,epsl,ce2,testr
        call uttcpu('CPU.OP0171.2', 'FIN', ' ')
        call uttcpr('CPU.OP0171.2', 4, tps2)
!
! --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
!
        if (etausr() .eq. 1) then
            call sigusr()
        endif
!
! - Y A-T-IL ASSEZ DE TEMPS POUR REFAIRE UNE ITERATION ?
!
        if (tps2(4) .gt. 0.8d0*tps2(1)-tps2(4)) then
            vali(1) = iterl
            valr(1) = tps2(4)
            valr(2) = tps2(1)
            call utmess('Z', 'DISCRETISATION2_79', si=vali(1), nr=2, valr=valr,&
                        num_except=28)
        endif
!
! - ON VA REFAIRE UNE ITERATION
!
        goto 2000
!
! *** ON S'ARRETE... (CONVERGENCE OU NOMBRE MAX D'ITERATIONS ATTEINT)
!
    else
!
!
        if ((parcri(9).eq.0) .and. (iterl.ge.itmaxl)) then
            write (ifm,fmt)
            call utmess('Z', 'MECANONLINE9_7', num_except=22)
        endif
!
    endif
!
! --- FIN DES ITERATIONS
!
! --- COPIE DE LA SD INFO_CHARGE DANS LA BASE GLOBALE
    call getfac('EXCIT', nchar)
    call jedetr(lischa//'.LCHA')
    call wkvect(lischa//'.LCHA', 'G V K24', nchar, jlcha)
    call jeveuo(infcha//'.LCHA', 'L', vk24=lcha)
    call jedetr(lischa//'.FCHA')
    call wkvect(lischa//'.FCHA', 'G V K24', nchar, jfcha)
    call jeveuo(fomult, 'L', ialifc)
    do 51 i = 1, nchar
        zk24(jlcha+i-1)=lcha(i)
        zk24(jfcha+i-1)=zk24(ialifc+i-1)
51  continue
    call jedetr(lischa//'.INFC')
    call wkvect(lischa//'.INFC', 'G V IS', 2*nchar+1, jinfc)
    call jeveuo(infcha//'.INFC', 'L', vi=infc)
    do 52 i = 1, 2*nchar+1
        zi(jinfc+i-1)=infc(i)
52  continue
!
    call uttcpu('CPU.OP0171.1', 'FIN', ' ')
    call uttcpr('CPU.OP0171.1', 4, tps1)
    write(ifm,fmt)
    write(ifm,'(A,21X,A,1PE10.2,21X,A)')&
     &                                 '*','DUREE:',tps1(3)-tpex,'*'
    write(ifm,fmt)
    write(ifm,'(/)')
!
! ======================================================================
!                   STOCKAGE DU RESULTAT
! ======================================================================
!
    call rscrsd('G', result, 'EVOL_THER', 1)
    call rsexch(' ', result, 'TEMP', 0, nomch,&
                iret)
    call rsadpa(result, 'E', 1, 'INST', 0,&
                0, sjv=jinst, styp=k8bid)
    zr(jinst) = 0.d0
    call copisd('CHAMP_GD', 'G', vtempp(1:19), nomch(1:19))
    call rsnoch(result, 'TEMP', 0)
!
!      ARCHIVAGE DU MODELE, MATERIAU, CARA_ELEM ET DE LA SD CHARGE
!
    call rssepa(result(1:8), 0, modele(1:8), mate(1:8), carele(1:8),&
                lischa)
!
    call titre()
!
! ----------------------------------------------------------------------
    call jedema()
end subroutine

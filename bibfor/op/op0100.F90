subroutine op0100()
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
! ======================================================================
!      OPERATEUR :     CALC_G
!
!      BUT:CALCUL DU TAUX DE RESTITUTION D'ENERGIE PAR LA METHODE THETA
!          CALCUL DES FACTEURS D'INTENSITE DE CONTRAINTES
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
! aslint: disable=W1501
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/cakg2d.h"
#include "asterfort/cakg3d.h"
#include "asterfort/ccbcop.h"
#include "asterfort/cgcrio.h"
#include "asterfort/cgcrtb.h"
#include "asterfort/cglecc.h"
#include "asterfort/cgleco.h"
#include "asterfort/cglect.h"
#include "asterfort/cglemu.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcou2d.h"
#include "asterfort/gcour2.h"
#include "asterfort/gcour3.h"
#include "asterfort/gcouro.h"
#include "asterfort/gver2d.h"
#include "asterfort/gveri3.h"
#include "asterfort/gverig.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerecu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mbilgl.h"
#include "asterfort/mebilg.h"
#include "asterfort/mecagl.h"
#include "asterfort/mecalg.h"
#include "asterfort/medomg.h"
#include "asterfort/memaxg.h"
#include "asterfort/mmaxgl.h"
#include "asterfort/mmaxkl.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsmena.h"
#include "asterfort/rsrusd.h"
#include "asterfort/tbexve.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xcourb.h"
    integer :: nbord, iord, ibid, i, iad, jnord, ivec, iret, nbpara
    integer :: lnoff, jinst, ndeg, nbropt, iadrco, iadrno, j, ipuls, iord0
    integer :: iord1, iord2, nborn, nbco, ibor, ig, lnoeu, labscu, nbval
    integer :: ndimte, ier, ndim, jordr, jopt
    integer :: nxpara, iarg
    parameter (nxpara = 11)
!
    real(kind=8) :: time, timeu, timev, dir(3), rinf, rsup, module, puls
    character(len=6) :: nompro
    parameter  ( nompro = 'OP0100' )
    character(len=8) :: modele, resu, k8bid, calsig, resuc2
    character(len=8) :: nomfis, litypa(nxpara), symech, config
    character(len=8) :: table, noma, thetai, noeud, typfis, typfon
    character(len=16) :: option, typsd, linopa(nxpara), cas, typco
    character(len=16) :: optio2, nomcas, k16bid
    character(len=19) :: lischa, lisopt, vecord, grlt
    character(len=24) :: depla, mate, compor, chvite, chacce
    character(len=24) :: basfon, fonoeu, liss, taillr
    character(len=24) :: chfond, basloc, theta
    character(len=24) :: nomno, coorn, melord
    character(len=24) :: trav1, trav2, trav3, stok4
    character(len=24) :: trav4, courb, depla1, depla2
    parameter  ( resuc2 = '&&MECALG' )
!
    logical :: exitim, thlagr, connex, glagr, milieu, direc
    logical :: thlag2, pair, lncas, lmelas, incr, lmoda
!
!     ==============
!     1. PREALABLES
!     ==============
!
    call jemarq()
    call infmaj()
!
    lischa = '&&'//nompro//'.CHARGES'
    courb = '&&'//nompro//'.COURB'
    trav1 = '&&'//nompro//'.TRAV1'
    trav2 = '&&'//nompro//'.TRAV2'
    trav3 = '&&'//nompro//'.TRAV3'
    trav4 = '&&'//nompro//'.TRAV4'
    stok4 = '&&'//nompro//'.STOK4'
!
!     ========================================
!     2. LECTURE ET VERIFICATION DES OPERANDES
!     ========================================
!
!     CREATION DES OBJETS :
!     - RESU, MODELE, NDIM
!     - OPTION, CAS
!     - TYPFIS, NOMFIS
!     - FONOEU, CHFOND, BASFON, TAILLR
!     - CONFIG
!     - LNOFF
!     - LISS, NDEG
    call cglect(resu, modele, ndim, option, cas,&
                typfis, nomfis, fonoeu, chfond, basfon,&
                taillr, config, lnoff, liss, ndeg)
!
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                noma, iret)
    nomno = noma//'.NOMNOE'
    coorn = noma//'.COORDO    .VALE'
    call jeveuo(coorn, 'L', iadrco)
!
!     RECUPERATION DU CONCEPT DE SORTIE : TABLE
    call getres(table, k16bid, k16bid)
!
!     CREATION DU VECTEUR DES NUME_ORDRE
    vecord = '&&OP0100.VECTORDR'
    call cgcrio(resu, vecord)
    call jeveuo(vecord, 'L', ivec)
    call jelira(vecord, 'LONMAX', nbord, k8bid)
!
!     MODE_MECA OU PAS
    lmoda = .false.
    call gettco(resu, typsd)
    if (typsd .eq. 'MODE_MECA') then
        if (option .eq. 'CALC_K_G') then
            lmoda = .true.
        else
            call u2mess('F', 'RUPTURE0_27')
        endif
    endif
!
!     PREMIER NUME_ORDRE
    iord0 = zi(ivec)
!
!     RECUPERATION MODELE, MATE ET LISCHA
    call medomg(resu, iord0, modele, mate, lischa)
!
!     RECUPERATION DE LA CARTE DE COMPORTEMENT UTILISEE DANS LE CALCUL
!     -> COMPOR, INCR
    call cgleco(resu, modele, iord0, typfis, compor,&
                incr)
!
!     ATTENTION, INCR EST MAL GERE : VOIR MECAGL !!
!
!     LECTURE DES INFORMATIONS RELATIVES AUX MULT_ELAS
    melord = '&&OP100.MULTELAS.NOMCAS'
    call cglemu(resu, vecord, lmelas, lncas, melord)
    if (lncas) then
        call jeveuo(melord, 'L', jnord)
    else
        jnord = 1
    endif
!
!     LECTURE ET VERIFICATION RELATIVE AU MOT-CLE CALCUL_CONTRAINTE
    call cglecc(typfis, resu, vecord, calsig)
!
!      ---------------- DEBUT DU GROS PAQUET A EPURER ---------------
!
!     ON RECHERCHE LA PRESENCE DE SYMETRIE
    symech='NON'
    call getvtx('THETA', 'SYME', 1, iarg, 1,&
                symech, ibid)
    if (typfis .eq. 'FONDFISS') then
        call dismoi('F', 'SYME', nomfis, 'FOND_FISS', ibid,&
                    symech, ier)
    endif
!
!     LE MOT-CLE THETA EST RENSEIGNE : ON CREE L'OBJET THETA (K24)
    if (typfis .eq. 'THETA') then
        call gettco(nomfis, typco)
        call assert(typco.eq.'THETA_GEOM'.or.typco.eq.'CHAM_NO_SDASTER')
        if (typco .eq. 'THETA_GEOM') then
            call rsexch('F', nomfis, 'THETA', 0, theta,&
                        ier)
        else if (typco.eq.'CHAM_NO_SDASTER') then
            theta=nomfis
        endif
    endif
!
!     LE MOT-CLE THETA N'EST PAS RENSEIGNE :
!     IL VA FALLOIR CALCULER UN SEUL CHAMP THETA (2D OU 3D_GLOBAL)
    if (typfis .ne. 'THETA' .and. cas .ne. '3D_LOCAL') then
!
        theta = table//'_CHAM_THETA'
!
!       MOT-CLE A RECUPERER, INDEPENDAMMENT DE NDIM
        call getvr8('THETA', 'DIRECTION', 1, iarg, 3,&
                    dir, iret)
        if (ndim .eq. 2) dir(3)=0.d0
        if (iret .eq. 0) then
            direc=.false.
!         A VIRER !!
            if (typfis .eq. 'FONDFISS' .and. ndim .eq. 2) call u2mess('F', 'RUPTURE0_81')
        else if (iret.lt.0) then
            call assert(.false.)
        else if (iret.gt.0) then
            direc=.true.
        endif
!
!       THETA 2D (COURONNE)
        if (ndim .eq. 2) then
            call gver2d(noma, 1, 'THETA', nomno, noeud,&
                        rinf, rsup, module)
            call gcou2d('V', theta, noma, nomno, noeud,&
                        zr(iadrco), rinf, rsup, module, direc,&
                        dir)
!       THETA 3D
        else if (ndim.eq.3) then
            call jeveuo(fonoeu, 'L', iadrno)
            call gverig(noma, 1, fonoeu, taillr, config,&
                        lnoff, nomno, coorn, trav1, trav2,&
                        trav3, trav4)
            call gcouro('V', theta, noma, nomno, coorn,&
                        lnoff, trav1, trav2, trav3, dir,&
                        zk8(iadrno), nomfis, direc, stok4)
        endif
!
    endif
!
!     BIDOUILLE POUR EVITER DE MODIFIER LES ARGUMENTS DE
!     GVERI3, GCOUR3, COUR2, MBILGL, CAKG3D, MMAXKL, CAKGMO, MECAGL
    if (liss .eq. 'LEGENDRE') then
        glagr =.false.
        thlagr=.false.
        thlag2=.false.
    else if (liss.eq.'MIXTE') then
        glagr =.false.
        thlagr=.true.
        thlag2=.false.
    else if (liss.eq.'LAGRANGE') then
        glagr =.true.
        thlagr=.true.
        thlag2=.false.
    else if (liss.eq.'LAGRANGE_NO_NO') then
        glagr =.true.
        thlagr=.true.
        thlag2=.false.
    else if (liss.eq.'LAGRANGE_REGU') then
        glagr =.false.
        thlagr=.false.
        thlag2=.true.
    endif
!
!     DETERMINATION AUTOMATIQUE DE THETA (CAS 3D LOCAL)
    if (cas .eq. '3D_LOCAL' .and. typfis .eq. 'FISSURE') then
!
        call dismoi('F', 'TYPE_FOND', nomfis, 'FISS_XFEM', ibid,&
                    k16bid, ier)
!       ON A TOUJOURS
        connex = .false.
        thetai = '&&THETA '
        grlt = nomfis//'.GRLTNO'
!
        call gveri3(chfond, taillr, config, lnoff, thlagr,&
                    thlag2, ndeg, trav1, trav2, trav3)
        call gcour3(thetai, noma, coorn, lnoff, trav1,&
                    trav2, trav3, chfond, grlt, thlagr,&
                    thlag2, basfon, ndeg, milieu, pair,&
                    ndimte)
!
    else if (cas.eq.'3D_LOCAL'.and.typfis.eq.'FONDFISS') then
!
!       A FAIRE : DISMOI POUR RECUP CONNEX ET METTRE DANS CGLECT
        call dismoi('F', 'TYPE_FOND', nomfis, 'FOND_FISS', ibid,&
                    typfon, ier)
        if (typfon .eq. 'FERME') then
            connex = .true.
        else
            connex = .false.
        endif
!
        if (liss .eq. 'LEGENDRE' .or. liss .eq. 'MIXTE') then
            if (connex) call u2mess('F', 'RUPTURE0_90')
        endif
!
        thetai = '&&THETA '
!
        call gveri3(chfond, taillr, config, lnoff, thlagr,&
                    thlag2, ndeg, trav1, trav2, trav3)
        call gcour2(thetai, noma, modele, nomno, coorn,&
                    lnoff, trav1, trav2, trav3, fonoeu,&
                    nomfis, connex, stok4, thlagr, thlag2,&
                    ndeg, milieu, ndimte, pair)
!
    endif
!
!     MENAGE
    if (ndim .eq. 3) then
        call jeexin(trav1, iret)
        if (iret .ne. 0) call jedetr(trav1)
        call jeexin(trav2, iret)
        if (iret .ne. 0) call jedetr(trav2)
        call jeexin(trav3, iret)
        if (iret .ne. 0) call jedetr(trav3)
        call jeexin(stok4, iret)
        if (iret .ne. 0) call jedetr(stok4)
    endif
!
!      ---------------- FIN DU GROS PAQUET A EPURER -----------------
!
!     =======================
!     3. CALCUL DE L'OPTION
!     =======================
!
!     CREATION DE LA TABLE
!
    call cgcrtb(table, option, lmelas, cas, typfis,&
                lmoda, nbpara, linopa, litypa)
!
    if (option(1:6) .eq. 'G_BILI' .or. option(1:5) .eq. 'G_MAX') then
!
!       --------------------------------------------------------------
!       3.1. ==> CALCUL DE LA FORME BILINEAIRE DU TAUX DE RESTITUTION
!       --------------------------------------------------------------
!
        do 311 i = 1, nbord
!
            iord1 = zi(ivec-1+i)
            call medomg(resu, iord1, modele, mate, lischa)
            call rsexch(' ', resu, 'DEPL', iord1, depla1,&
                        iret)
!
            if (lmelas) then
                if (lncas) then
                    if (.not.zl(jnord+i-1)) goto 311
                endif
                exitim = .false.
                timeu=0.d0
                timev=0.d0
                call rsadpa(resu, 'L', 1, 'NOM_CAS', iord0,&
                            0, iad, k8bid)
                nomcas=zk16(iad)
            endif
!
            do 312 j = 1, i
                call jemarq()
                call jerecu('V')
                if (nbord .eq. 1) then
                    iord2 = iord1
                    depla2 = depla1
                else
                    iord2 = zi(ivec-1+j)
                    call rsexch('F', resu, 'DEPL', iord2, depla2,&
                                iret)
                endif
!
                if (.not.lmelas) then
                    call rsadpa(resu, 'L', 1, 'INST', iord1,&
                                0, jinst, k8bid)
                    timeu = zr(jinst)
                    call rsadpa(resu, 'L', 1, 'INST', iord2,&
                                0, jinst, k8bid)
                    timev = zr(jinst)
                    exitim = .true.
                endif
!
                optio2 = 'G_BILI'
                if (cas .eq. '3D_LOCAL') then
                    call mbilgl(optio2, table, modele, depla1, depla2,&
                                thetai, mate, lischa, symech, chfond, &
                                lnoff,ndeg, thlagr, glagr, thlag2,&
                                milieu,ndimte, pair, exitim, timeu, &
                                timev,i, j, nbpara, linopa, &
                                lmelas,nomcas, fonoeu)
                else
                    call mebilg(optio2, table, modele, depla1, depla2,&
                                theta, mate, lischa, symech,  timeu, &
                                timev, i, j, nbpara, linopa)
                endif
!
                call jedema()
!
312          continue
!
311      continue
!
!
        if (option(1:5) .eq. 'G_MAX') then
!
!         ----------------------------------------------------
!         3.2. ==> MAXIMISATION DU G SOUS CONTRAINTES BORNES
!         ----------------------------------------------------
!
            call getfac('BORNES', nborn)
            if (nborn .ne. 0) then
                nbco = 2*nborn
                call wkvect('&&'//nompro//'.COUPLES_BORNES', 'V V R8', nbco, ibor)
                do 313 i = 1, nborn
                    call getvis('BORNES', 'NUME_ORDRE', i, iarg, 1,&
                                iord0, ier)
                    call getvr8('BORNES', 'VALE_MIN', i, iarg, 1,&
                                zr(ibor+ 2*(iord0-1)), ier)
                    call getvr8('BORNES', 'VALE_MAX', i, iarg, 1,&
                                zr(ibor+ 2*(iord0-1)+1), ier)
313              continue
!
                if (cas .eq. '3D_LOCAL') then
                    call tbexve(table, 'G_BILI_LOCAL', '&&'//nompro// '.GBILIN', 'V', nbval,&
                                k8bid)
                    call jeveuo('&&'//nompro//'.GBILIN', 'L', ig)
                    call tbexve(table, 'NOEUD', '&&'//nompro//'.NOEUD', 'V', nbval,&
                                k8bid)
                    call jeveuo('&&'//nompro//'.NOEUD', 'L', lnoeu)
                    call tbexve(table, 'ABSC_CURV', '&&'//nompro// '.ABSCUR', 'V', nbval,&
                                k8bid)
                    call jeveuo('&&'//nompro//'.ABSCUR', 'L', labscu)
!
                    call detrsd('TABLE', table)
                    call mmaxgl(nbco, zr(ibor), zr(ig), zk8(lnoeu), zr(labscu),&
                                nbord, lnoff, table)
                else
                    call tbexve(table, 'G_BILIN', '&&'//nompro// '.GBILIN', 'V', nbval,&
                                k8bid)
                    call jeveuo('&&'//nompro//'.GBILIN', 'L', ig)
                    call detrsd('TABLE', table)
                    call memaxg(nbco, zr(ibor), zr(ig), nbord, table)
                endif
!
            else
                call u2mesk('F', 'RUPTURE0_92', 1, option)
            endif
        endif
!
!
    else if (cas.eq.'3D_LOCAL'.and.option.eq.'CALC_K_G') then
!
!       -------------------------------
!       3.3. ==> CALCUL DE KG (3D LOC)
!       -------------------------------
!
        basloc=nomfis//'.BASLOC'
        call xcourb(basloc, noma, modele, courb)
!
        do 33 i = 1, nbord
            iord = zi(ivec-1+i)
            call medomg(resu, iord, modele, mate, lischa)
            call rsexch('F', resu, 'DEPL', iord, depla,&
                        iret)
!
            if (lmelas) then
                if (lncas) then
                    if (.not.zl(jnord+i-1)) goto 33
                endif
                exitim = .false.
                time=0.d0
                call rsadpa(resu, 'L', 1, 'NOM_CAS', iord,&
                            0, iad, k8bid)
                nomcas=zk16(iad)
            else if (lmoda) then
                call rsadpa(resu, 'L', 1, 'OMEGA2', iord,&
                            0, ipuls, k8bid)
                puls = zr(ipuls)
                puls = sqrt(puls)
                time = 0.d0
            else
                call rsadpa(resu, 'L', 1, 'INST', iord,&
                            0, jinst, k8bid)
                time = zr(jinst)
                exitim = .true.
            endif
!
!
            call cakg3d(option, table, modele, depla, thetai,&
                        mate, compor, lischa, symech, chfond,&
                        lnoff, basloc, courb, iord, ndeg,&
                        thlagr, glagr, thlag2, pair, ndimte,&
                        exitim, time, nbpara, linopa, nomfis,&
                        lmelas, nomcas, lmoda, puls, milieu,&
                        connex)
!
33      continue
!
!
    else if (option .eq.'CALC_K_MAX') then
!
!       ------------------------
!       3.3.2. ==>OPTION CALC_K_MAX
!       -----------------------
!
        basloc=nomfis//'.BASLOC'
        call xcourb(basloc, noma, modele, courb)
!
        call mmaxkl(table, modele, thetai, mate, compor,&
                    symech, chfond, lnoff, basloc, courb,&
                    ndeg, thlagr, glagr, thlag2, pair,&
                    ndimte, nbpara, linopa, nomfis, nbord,&
                    ivec, resu, lmelas, lncas,zl( jnord),&
                    milieu, connex, lischa)
!
!     -------------------------------
!     3.5. ==> CALCUL DE G, K_G (2D)
!     -------------------------------
!
    else
!
        if (incr) then
            lisopt = '&&OP0100.LISOPT'
            nbropt = 2
!
            call wkvect(lisopt, 'V V K16', nbropt, jopt)
            zk16(jopt) = 'VARI_ELNO'
            zk16(jopt+1) = 'EPSP_ELNO'
!
            call ccbcop(resu, resuc2, vecord, nbord, lisopt,&
                        nbropt)
        endif
!
        do 35 i = 1, nbord
            call jemarq()
            call jerecu('V')
            iord = zi(ivec-1+i)
            call medomg(resu, iord, modele, mate, lischa)
!
            call rsexch('F', resu, 'DEPL', iord, depla,&
                        iret)
            call rsexch(' ', resu, 'VITE', iord, chvite,&
                        iret)
            if (iret .ne. 0) then
                chvite = ' '
            else
                call rsexch(' ', resu, 'ACCE', iord, chacce,&
                            iret)
            endif
!
            if (lmelas) then
                if (lncas) then
                    if (.not.zl(jnord+i-1)) goto 34
                endif
                call rsadpa(resu, 'L', 1, 'NOM_CAS', iord,&
                            0, iad, k8bid)
                nomcas=zk16(iad)
                exitim = .false.
                time = 0.d0
            else if (lmoda) then
                call rsadpa(resu, 'L', 1, 'OMEGA2', iord,&
                            0, ipuls, k8bid)
                puls = zr(ipuls)
                puls = sqrt(puls)
                time = 0.d0
            else
                call rsadpa(resu, 'L', 1, 'INST', iord,&
                            0, jinst, k8bid)
                time = zr(jinst)
                exitim = .true.
            endif
!
            if ((option(1:6).eq.'CALC_G'.and.cas.eq.'2D') .or. option .eq. 'CALC_G_GLOB') then
!
                call mecalg(option, table, modele, depla, theta,&
                            mate, lischa,symech, compor, incr, &
                            time,iord, nbpara, linopa, chvite, &
                            chacce,lmelas, nomcas, calsig)
!
            else if (option(1:6).eq.'CALC_G'.and.cas.eq.'3D_LOCAL') then
!
                call mecagl(option, table, modele, depla, thetai,&
                            mate, compor, lischa,symech, chfond, &
                            lnoff,iord, ndeg, thlagr, glagr, &
                            thlag2,milieu, ndimte, pair, exitim, &
                            time,nbpara, linopa, chvite, chacce, &
                            lmelas,nomcas, calsig, fonoeu)
!
            else if (option(1:6).eq.'CALC_K'.and.cas.eq.'2D') then
!
                call cakg2d(option, table, modele, depla, theta,&
                            mate, lischa, symech, nomfis, noeud,&
                            time, iord, nbpara, linopa, lmelas,&
                            nomcas, lmoda, puls, compor)
!
            else
                call assert(.false.)
            endif
!
34          continue
!
            call jedema()
35      continue
!
        if (incr) then
            call jeexin(resuc2//'           .ORDR', iret)
            if (iret .ne. 0) then
                call jeveuo(resuc2//'           .ORDR', 'L', jordr)
                call rsrusd(resuc2, zi(jordr))
                call detrsd('RESULTAT', resuc2)
            endif
!
            call jedetr('&&MECALCG.VECTORDR')
            call jedetr('&&MECALG')
            call rsmena(resu)
        endif
!
    endif
!
    call titre()
!
    call detrsd('CARTE', '&&NMDORC.COMPOR')
!
    call jedema()
!
end subroutine

subroutine op0100()
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/gettco.h"
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
#include "asterfort/deprecated_algom.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcou2d.h"
#include "asterfort/gcour2.h"
#include "asterfort/gcour3.h"
#include "asterfort/gcouro.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
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
#include "asterfort/mecagl.h"
#include "asterfort/mecalg.h"
#include "asterfort/medomg.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsmena.h"
#include "asterfort/rsrusd.h"
#include "asterfort/tbexve.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xcourb.h"
    integer :: nbord, iord, i, iad, jnord, ivec, iret, nbpara
    integer :: lnoff, jinst, ndeg, nbropt, iadrco, iadrno, ipuls, iord0
    integer :: iadfis, iadnoe
    integer :: ndimte, ndim, jopt
    integer :: nxpara
    parameter (nxpara = 15)
!
    real(kind=8) :: time, dir(3), rinf, rsup, module, puls
    character(len=6) :: nompro
    parameter  ( nompro = 'OP0100' )
    character(len=8) :: modele, resu, k8bid, calsig, resuc2
    character(len=8) :: nomfis, litypa(nxpara), symech, config
    character(len=8) :: table, noma, thetai, noeud, typfis, typfon
    character(len=16) :: option, typsd, linopa(nxpara), cas
    character(len=16) :: nomcas, k16bid, typdis
    character(len=19) :: lischa, lisopt, vecord, grlt
    character(len=24) :: depla, mate, compor, chvite, chacce
    character(len=24) :: basfon, fonoeu, liss, taillr
    character(len=24) :: chfond, basloc, theta
    character(len=24) :: nomno, coorn, melord
    character(len=24) :: trav1, trav2, trav3, stok4
    character(len=24) :: trav4, courb
    character(len=24) :: norfon
    parameter  ( resuc2 = '&&MECALG' )
!
    aster_logical :: exitim, connex, milieu, direc
    aster_logical :: lncas, lmelas, incr, lmoda
    integer, pointer :: ordr(:) => null()
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
!     - TYPDIS
    call cglect(resu, modele, ndim, option, cas,&
                typfis, nomfis, fonoeu, chfond, basfon,&
                taillr, config, lnoff, liss, ndeg, typdis)
!
    call deprecated_algom(option)
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
    nomno = noma//'.NOMNOE'
    coorn = noma//'.COORDO    .VALE'
    call jeveuo(coorn, 'L', iadrco)
!     RECUPERATION DES COORDONNEES POINTS FOND DE FISSURE ET ABSC CURV
    if (typfis.ne.'THETA') then
        call jeveuo(chfond, 'L', iadfis)
    else
        iadfis=0
    endif
!     RECUPERATION DU NOM DES NOEUDS DU FOND DE FISSURE
    if (typfis .eq. 'FONDFISS') then
        call jeveuo(fonoeu, 'L', iadnoe)
    endif
!
!     RECUPERATION DU CONCEPT DE SORTIE : TABLE
    call getres(table, k16bid, k16bid)
!
!     CREATION DU VECTEUR DES NUME_ORDRE
    vecord = '&&OP0100.VECTORDR'
    call cgcrio(resu, vecord)
    call jeveuo(vecord, 'L', ivec)
    call jelira(vecord, 'LONMAX', nbord)
!
!     MODE_MECA OU PAS
    lmoda = .false.
    call gettco(resu, typsd)
    if (typsd .eq. 'MODE_MECA') then
        if (option .eq. 'CALC_K_G') then
            lmoda = .true.
        else
            call utmess('F', 'RUPTURE0_27')
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
    call cgleco(resu, modele, mate, iord0, compor(1:19),&
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
    if (typfis .eq. 'FONDFISS') then
        call dismoi('SYME', nomfis, 'FOND_FISS', repk=symech)
    endif
!
!     CALCUL DU CHAMP THETA (2D OU 3D_GLOBAL)
    if (cas .ne. '3D_LOCAL') then
!
        theta = table//'_CHAM_THETA'
!
!       MOT-CLE A RECUPERER, INDEPENDAMMENT DE NDIM
        call getvr8('THETA', 'DIRECTION', iocc=1, nbval=3, vect=dir,&
                    nbret=iret)
        if (ndim .eq. 2) dir(3)=0.d0
        if (iret .eq. 0) then
            direc=.false.
        else if (iret.lt.0) then
            ASSERT(.false.)
        else if (iret.gt.0) then
            direc=.true.
        endif
!
!       THETA 2D (COURONNE)
        if (ndim .eq. 2) then
            call gver2d(1, noeud,&
                        rinf, rsup, module)
            call gcou2d('V', theta, noma, nomno, noeud,&
                        zr(iadrco), rinf, rsup, module, direc,&
                        dir)
!       THETA 3D
        else if (ndim.eq.3) then
            call jeveuo(fonoeu, 'L', iadrno)
            call gverig(1, fonoeu, taillr, config,&
                        lnoff, nomno, coorn, trav1, trav2,&
                        trav3, trav4)
            call gcouro('V', theta, noma, nomno, coorn,&
                        lnoff, trav1, trav2, trav3, dir,&
                        zk8(iadrno), nomfis, direc, stok4)
        endif
!
    endif
!
!     DETERMINATION AUTOMATIQUE DE THETA (CAS 3D LOCAL)
    if (cas .eq. '3D_LOCAL' .and. typfis .eq. 'FISSURE') then
!
        call dismoi('TYPE_FOND', nomfis, 'FISS_XFEM', repk=typfon)
!
        if (typfon .eq. 'FERME') then
            connex = .true.
        else
            connex = .false.
        endif
!
        if (liss .eq. 'LEGENDRE' .or. liss .eq. 'MIXTE') then
            if (connex) call utmess('F', 'RUPTURE0_90')
        endif
!
        thetai = '&&THETA '
        grlt = nomfis//'.GRLTNO'
!
        call gveri3(chfond, taillr, config, lnoff,&
                    liss, ndeg, trav1, trav2, trav3, typdis)
        call gcour3(thetai, noma, coorn, lnoff, trav1,&
                    trav2, trav3, chfond, connex, grlt,&
                    liss, basfon, ndeg, milieu,&
                    ndimte, typdis, nomfis)
!
    else if (cas.eq.'3D_LOCAL'.and.typfis.eq.'FONDFISS') then
!
!       A FAIRE : DISMOI POUR RECUP CONNEX ET METTRE DANS CGLECT
        call dismoi('TYPE_FOND', nomfis, 'FOND_FISS', repk=typfon)
        if (typfon .eq. 'FERME') then
            connex = .true.
        else
            connex = .false.
        endif
!
        if (liss .eq. 'LEGENDRE' .or. liss .eq. 'MIXTE') then
            if (connex) then
                call utmess('F', 'RUPTURE0_90')
            endif
        endif
!
        thetai = '&&THETA '
!
        call gveri3(chfond, taillr, config, lnoff, liss,&
                    ndeg, trav1, trav2, trav3, option)
        call gcour2(thetai, noma, modele, nomno, coorn,&
                    lnoff, trav1, trav2, trav3, fonoeu, chfond, basfon,&
                    nomfis, connex, stok4, liss,&
                    ndeg, milieu, ndimte, norfon)
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
    call cgcrtb(table, option, lmelas, cas, typfis, nxpara,&
                lmoda, nbpara, linopa, litypa)
!
!!    ARRET POUR CONTROLE DEVELOPPEMENT DANS CGCRTB
!    ASSERT(.false.)
!
!
    if (cas.eq.'3D_LOCAL'.and.option.eq.'CALC_K_G') then
!
!       -------------------------------
!       3.3. ==> CALCUL DE KG (3D LOC)
!       -------------------------------
!
        basloc=nomfis//'.BASLOC'
        call xcourb(basloc, noma, modele, courb)
!
        do i = 1, nbord
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
                            0, sjv=iad, styp=k8bid)
                nomcas=zk16(iad)
            else if (lmoda) then
                call rsadpa(resu, 'L', 1, 'OMEGA2', iord,&
                            0, sjv=ipuls, styp=k8bid)
                puls = zr(ipuls)
                puls = sqrt(puls)
                time = 0.d0
            else
                call rsadpa(resu, 'L', 1, 'INST', iord,&
                            0, sjv=jinst, styp=k8bid)
                time = zr(jinst)
                exitim = .true.
            endif
!
!
            call cakg3d(option, table, modele, depla, thetai,&
                        mate, compor, lischa, symech, chfond,&
                        lnoff, basloc, courb, iord, ndeg,&
                        liss, ndimte,&
                        exitim, time, nbpara, linopa, nomfis,&
                        lmelas, nomcas, lmoda, puls, milieu,&
                        connex, iadfis, iadnoe, typdis)
!
 33         continue
        end do
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
        do i = 1, nbord
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
                            0, sjv=iad, styp=k8bid)
                nomcas=zk16(iad)
                exitim = .false.
                time = 0.d0
            else if (lmoda) then
                call rsadpa(resu, 'L', 1, 'OMEGA2', iord,&
                            0, sjv=ipuls, styp=k8bid)
                puls = zr(ipuls)
                puls = sqrt(puls)
                time = 0.d0
            else
                call rsadpa(resu, 'L', 1, 'INST', iord,&
                            0, sjv=jinst, styp=k8bid)
                time = zr(jinst)
                exitim = .true.
            endif
!
            if ((option(1:6).eq.'CALC_G'.and.cas.eq.'2D') .or. option .eq. 'CALC_G_GLOB') then
!
                call mecalg(option, table, modele, depla, theta,&
                            mate, lischa, symech, compor, incr,&
                            time, iord, nbpara, linopa, chvite,&
                            chacce, lmelas, nomcas, calsig, iadfis, iadnoe)
!
            else if (option(1:6).eq.'CALC_G'.and.cas.eq.'3D_LOCAL') then
!
                call mecagl(option, table, modele, depla, thetai,&
                            mate, compor, lischa, symech, chfond,&
                            lnoff, iord, ndeg, liss,&
                            milieu, ndimte, exitim,&
                            time, nbpara, linopa, chvite, chacce,&
                            lmelas, nomcas, calsig, fonoeu, incr, iadfis, &
                            norfon, connex)
!
            else if (option(1:6).eq.'CALC_K'.and.cas.eq.'2D') then
!
                call cakg2d(option, table, modele, depla, theta,&
                            mate, lischa, symech, nomfis, noeud,&
                            time, iord, nbpara, linopa, lmelas,&
                            nomcas, lmoda, puls, compor)
!
            else
                ASSERT(.false.)
            endif
!
 34         continue
!
            call jedema()
        end do
!
        if (incr) then
            call jeexin(resuc2//'           .ORDR', iret)
            if (iret .ne. 0) then
                call jeveuo(resuc2//'           .ORDR', 'L', vi=ordr)
                call rsrusd(resuc2, ordr(1))
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

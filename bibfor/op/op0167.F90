subroutine op0167()
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
!     OPERATEUR CREA_MAILLAGE
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/cargeo.h"
#include "asterfort/chckma.h"
#include "asterfort/chcoma.h"
#include "asterfort/chcomb.h"
#include "asterfort/cm1518.h"
#include "asterfort/cm2027.h"
#include "asterfort/cmcovo.h"
#include "asterfort/cmcrea.h"
#include "asterfort/cmdgma.h"
#include "asterfort/cmlqlq.h"
#include "asterfort/cmmoma.h"
#include "asterfort/cmqlql.h"
#include "asterfort/cmqutr.h"
#include "asterfort/cocali.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/cpclma.h"
#include "asterfort/dismoi.h"
#include "asterfort/eclpgm.h"
#include "asterfort/exlima.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/infoma.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/palim2.h"
#include "asterfort/palim3.h"
#include "asterfort/rdtmai.h"
#include "asterfort/reliem.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: i, lgno, lgnu, nbecla, nbmc, iret, iad, nbma, nbmst, iqtr, nbvolu
    integer :: n1, numma, nbjoin, nbrest, n1a, n1b
!
    parameter(nbmc=5)
    real(kind=8) :: epais
    character(len=4) :: cdim, repk
    character(len=8) :: nomain, nomaou, newmai, prefix, mo, geofi
    character(len=8) :: nomori, knume, prfno, prfma, plan, trans
    character(len=16) :: typcon, nomcmd, option
    character(len=16) :: motfac, tymocl(nbmc), motcle(nbmc)
    character(len=19) :: table, ligrel, cham1
    character(len=24) :: nommai, grpmai, typmai, connex, nodime, grpnoe, nomnoe
    character(len=24) :: cooval, coodsc, cooref, nomjv
    character(len=24) :: nommav, grpmav, typmav, connev, nodimv, grpnov, nomnov
    character(len=24) :: coovav, coodsv, coorev
    character(len=24) :: momanu, momano, crmanu, crmano, crgrnu, crgrno, lisi
    character(len=24) :: lisk
    character(len=24) :: nomg, valk(2), nogma, gpptnm, gpptnn
    character(len=24) :: prfn1, prfn2, nume2, iadr, nume1, momoto, momuto, prfn
    integer :: nn1, iaa, iagma, iatyma, ierd, ii, ima, in, ino, inumol, j, nfi
    integer :: jcrgno, jcrgnu, jcrmno, jcrmnu, jgg, jlii, jlik, jmail, jmomto
    integer :: jmomtu, jnoeu, jnono, jnpt, jopt, jtom, jtrno, jvale, jvg, kvale
    integer :: nbcrma, nbcrp1, nbdgma, nbgma, nbgrma, nbgrmn, nbgrmt, nbgrmv
    integer :: nbgrno, nbmain, nbmaj1, nbmaj2, nbmaj3, nbno, nbnot
    integer :: nbpt, nbptt, nori, nrep, ntab, ntpoi
    integer :: ibid, icham, ifm, iocc, jdime, jiad, jlima, jma, jmomno, jmomnu
    integer :: jnommc, jnu2, jnum, joccmc, jpr2, jpro, jrefe, jtypmv
    integer :: nbmaiv, nbmoma, nbnoaj, nbnoev, nch, ndinit, niv, k, jgeofi
    integer :: jnnoma, jnnomb, jadrjv, jnonum, dimcon, decala, iocct
    real(kind=8) :: shrink, lonmin
    logical :: lpb
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(nomaou, typcon, nomcmd)
!
! ----------------------------------------------------------------------
!               TRAITEMENT DU MOT CLE "ECLA_PG"
! ----------------------------------------------------------------------
    call getfac('ECLA_PG', nbecla)
    if (nbecla .gt. 0) then
        call getvid('ECLA_PG', 'MODELE', iocc=1, scal=mo, nbret=ibid)
        ASSERT(ibid.eq.1)
        call getvr8('ECLA_PG', 'SHRINK', iocc=1, scal=shrink, nbret=ibid)
        call getvr8('ECLA_PG', 'TAILLE_MIN', iocc=1, scal=lonmin, nbret=ibid)
        call getvtx('ECLA_PG', 'NOM_CHAM', iocc=1, nbval=0, nbret=nch)
        if (nch .lt. 0) then
            nch=-nch
            call wkvect('&&OP0167.NOMCHAMP', 'V V K16', nch, icham)
            call getvtx('ECLA_PG', 'NOM_CHAM', iocc=1, nbval=nch, vect=zk16(icham))
        else
            icham=1
        endif
        call exlima('ECLA_PG', 1, 'V', mo, ligrel)
        cham1=' '
        call eclpgm(nomaou, mo, cham1, ligrel, shrink,&
                    lonmin, nch, zk16( icham))
        goto 350
    endif
!
!
! ----------------------------------------------------------------------
!          TRAITEMENT DU MOT CLE "GEOM_FIBRE"
! ----------------------------------------------------------------------
    call getvid(' ', 'GEOM_FIBRE', scal=geofi, nbret=nfi)
    if (nfi .ne. 0) then
        call jeveuo(geofi//'.GFMA', 'L', jgeofi)
        call copisd('MAILLAGE', 'G', zk8(jgeofi), nomaou)
        goto 350
    endif
!
!
    call getvid(' ', 'MAILLAGE', scal=nomain, nbret=nn1)
    if (nn1 .eq. 0) then
        call utmess('F', 'CALCULEL5_10')
    endif
!
! ----------------------------------------------------------------------
!          TRAITEMENT DU MOT CLE "CREA_FISS"
! ----------------------------------------------------------------------
    call getfac('CREA_FISS', nbjoin)
    if (nbjoin .ne. 0) then
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE2_89')
        endif
        call wkvect('&&OP0167.NOMMC', 'V V K16', nbjoin, jnommc)
        call wkvect('&&OP0167.OCCMC', 'V V I', nbjoin, joccmc)
        do 10 i = 1, nbjoin
            zk16(jnommc-1+i)='CREA_FISS'
            zi(joccmc-1+i)=i
10      continue
!
        call cmcrea(nomain, nomaou, nbjoin, zk16(jnommc), zi(joccmc))
        goto 350
!
    endif
!
!
! ----------------------------------------------------------------------
!          TRAITEMENT DU MOT CLE "LINE_QUAD"
! ----------------------------------------------------------------------
!
    call getfac('LINE_QUAD', nbmoma)
    if (nbmoma .gt. 0) then
        ASSERT(nbmoma.eq.1)
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE2_90')
        endif
!
        call getvtx('LINE_QUAD', 'PREF_NOEUD', iocc=1, scal=prefix, nbret=n1)
        call getvis('LINE_QUAD', 'PREF_NUME', iocc=1, scal=ndinit, nbret=n1)
!
        call getvtx('LINE_QUAD', 'MAILLE', iocc=1, nbval=0, nbret=n1a)
        call getvtx('LINE_QUAD', 'GROUP_MA', iocc=1, nbval=0, nbret=n1b)
        if (n1a+n1b .lt. 0) then
            call utmess('A', 'MODELISA4_1', sk='LINE_QUAD')
        endif
!
        motcle(1)='MAILLE'
        motcle(2)='GROUP_MA'
        motcle(3)='TOUT'
        nomjv='&&OP0167.LISTE_MA'
        call reliem(' ', nomain, 'NU_MAILLE', 'LINE_QUAD', 1,&
                    3, motcle, motcle, nomjv, nbma)
        call jeveuo(nomjv, 'L', jlima)
        call jeexin(nomain//'.NOMACR', iret)
        if (iret .ne. 0) then
            call utmess('F', 'ALGELINE2_91')
        endif
        call jeexin(nomain//'.ABS_CURV', iret)
        if (iret .ne. 0) then
            call utmess('F', 'ALGELINE2_92')
        endif
!
        call cmlqlq(nomain, nomaou, nbma, zi(jlima), prefix,&
                    ndinit)
!
        goto 350
!
    endif
!
! ----------------------------------------------------------------------
!          TRAITEMENT DES MOTS CLES "PENTA15_18","HEXA20_27"
! ----------------------------------------------------------------------
!
    do 77, k=1,2
    if (k .eq. 1) motfac='HEXA20_27'
    if (k .eq. 2) motfac='PENTA15_18'
    call getfac(motfac, nbmoma)
    if (nbmoma .gt. 0) then
        if (nn1 .eq. 0) then
            call utmess('F', 'MAIL0_14', sk=motfac)
        endif
!
        call getvtx(motfac, 'MAILLE', iocc=1, nbval=0, nbret=n1a)
        call getvtx(motfac, 'GROUP_MA', iocc=1, nbval=0, nbret=n1b)
        if (n1a+n1b .lt. 0) then
            call utmess('A', 'MODELISA4_1', sk=motfac)
        endif
!
        lpb=.false.
        if (motfac .eq. 'HEXA20_27') then
            call dismoi('F', 'EXI_PENTA15', nomain, 'MAILLAGE', ibid,&
                        repk, ierd)
            if (repk .eq. 'OUI') lpb=.true.
            call dismoi('F', 'EXI_PYRAM13', nomain, 'MAILLAGE', ibid,&
                        repk, ierd)
            if (repk .eq. 'OUI') lpb=.true.
        else if (motfac.eq.'PENTA15_18') then
            call dismoi('F', 'EXI_HEXA20', nomain, 'MAILLAGE', ibid,&
                        repk, ierd)
            if (repk .eq. 'OUI') lpb=.true.
            call dismoi('F', 'EXI_PYRAM13', nomain, 'MAILLAGE', ibid,&
                        repk, ierd)
            if (repk .eq. 'OUI') lpb=.true.
        endif
        if (lpb) then
            call utmess('A', 'MODELISA4_11', sk=motfac)
        endif
!
        call getvtx(motfac, 'PREF_NOEUD', iocc=1, scal=prefix, nbret=n1)
        call getvis(motfac, 'PREF_NUME', iocc=1, scal=ndinit, nbret=n1)
!
        motcle(1)='MAILLE'
        motcle(2)='GROUP_MA'
        motcle(3)='TOUT'
        nomjv='&&OP0167.LISTE_MA'
        call reliem(' ', nomain, 'NU_MAILLE', motfac, 1,&
                    3, motcle, motcle, nomjv, nbma)
        call jeveuo(nomjv, 'L', jlima)
!
        if (motfac .eq. 'HEXA20_27') then
            call cm2027(nomain, nomaou, nbma, zi(jlima), prefix,&
                        ndinit)
        else if (motfac.eq.'PENTA15_18') then
            call cm1518(nomain, nomaou, nbma, zi(jlima), prefix,&
                        ndinit)
        endif
        goto 350
    endif
    77 end do
!
! ----------------------------------------------------------------------
!          TRAITEMENT DU MOT CLE "QUAD_LINE"
! ----------------------------------------------------------------------
!
    call getfac('QUAD_LINE', nbmoma)
    if (nbmoma .gt. 0) then
        ASSERT(nbmoma.eq.1)
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE2_93')
        endif
!
        call getvtx('QUAD_LINE', 'MAILLE', iocc=1, nbval=0, nbret=n1a)
        call getvtx('QUAD_LINE', 'GROUP_MA', iocc=1, nbval=0, nbret=n1b)
        if (n1a+n1b .lt. 0) then
            call utmess('A', 'MODELISA4_1', sk='QUAD_LINE')
        endif
!
        motcle(1)='MAILLE'
        motcle(2)='GROUP_MA'
        motcle(3)='TOUT'
        nomjv='&&OP0167.LISTE_MA'
        call reliem(' ', nomain, 'NU_MAILLE', 'QUAD_LINE', 1,&
                    3, motcle, motcle, nomjv, nbma)
        call jeveuo(nomjv, 'L', jlima)
        call jeexin(nomain//'.NOMACR', iret)
        if (iret .ne. 0) then
            call utmess('F', 'ALGELINE2_94')
        endif
        call jeexin(nomain//'.ABS_CURV', iret)
        if (iret .ne. 0) then
            call utmess('F', 'ALGELINE2_95')
        endif
!
        call cmqlql(nomain, nomaou, nbma, zi(jlima))
!
        goto 350
!
    endif
!
! ----------------------------------------------------------------------
!          TRAITEMENT DU MOT CLE "MODI_MAILLE", OPTION "QUAD_TRIA3"
! ----------------------------------------------------------------------
!
    call getfac('MODI_MAILLE', nbmoma)
    if (nbmoma .gt. 0) then
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE2_96')
        endif
!
        iqtr=0
        do 20 iocc = 1, nbmoma
            call getvtx('MODI_MAILLE', 'OPTION', iocc=iocc, scal=option, nbret=n1)
            if (option .eq. 'QUAD_TRIA3') then
                iqtr=iqtr+1
                iocct=iocc
            endif
20      continue
!
        if (iqtr .eq. 0) then
            goto 30
        else if (iqtr.gt.1) then
            call utmess('F', 'ALGELINE2_97')
        else
            call getvtx('MODI_MAILLE', 'MAILLE', iocc=iocct, nbval=0, nbret=n1a)
            call getvtx('MODI_MAILLE', 'GROUP_MA', iocc=iocct, nbval=0, nbret=n1b)
            if (n1a+n1b .lt. 0) then
                call utmess('A', 'MODELISA4_1', sk='QUAD_TRIA3')
            endif
            call dismoi('F', 'EXI_TRIA6', nomain, 'MAILLAGE', ibid,&
                        repk, ierd)
            if (repk .eq. 'OUI') then
                call utmess('A', 'MODELISA4_2')
            endif
        endif
!
        call getvtx('MODI_MAILLE', 'PREF_MAILLE', iocc=1, scal=prefix, nbret=n1)
        call getvis('MODI_MAILLE', 'PREF_NUME', iocc=1, scal=ndinit, nbret=n1)
!
        motcle(1)='MAILLE'
        motcle(2)='GROUP_MA'
        motcle(3)='TOUT'
        nomjv='&&OP0167.LISTE_MA'
        call reliem(' ', nomain, 'NU_MAILLE', 'MODI_MAILLE', iocct,&
                    3, motcle, motcle, nomjv, nbma)
        call jeveuo(nomjv, 'L', jlima)
!
        call cmqutr('G', nomain, nomaou, nbma, zi(jlima),&
                    prefix, ndinit)
!
        goto 350
!
    endif
30  continue
!
! ----------------------------------------------------------------------
!                 TRAITEMENT DU MOT CLE "COQU_VOLU"
! ----------------------------------------------------------------------
!
    call getfac('COQU_VOLU', nbvolu)
    if (nbvolu .ne. 0) then
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE2_98')
        endif
!
        call getvr8('COQU_VOLU', 'EPAIS', iocc=1, scal=epais, nbret=n1)
        call getvtx('COQU_VOLU', 'PREF_NOEUD', iocc=1, scal=prfno, nbret=n1)
        call getvtx('COQU_VOLU', 'PREF_MAILLE', iocc=1, scal=prfma, nbret=n1)
        call getvis('COQU_VOLU', 'PREF_NUME', iocc=1, scal=numma, nbret=n1)
        call getvtx('COQU_VOLU', 'PLAN', iocc=1, scal=plan, nbret=n1)
!
        if (plan .eq. 'MOY') then
            trans='INF'
            call getvtx('COQU_VOLU', 'TRANSLATION', iocc=1, scal=trans, nbret=n1)
        endif
!
        nomjv='&&OP0167.LISTE_MAV'
        call reliem(' ', nomain, 'NU_MAILLE', 'COQU_VOLU', 1,&
                    1, 'GROUP_MA', 'GROUP_MA', nomjv, nbma)
        call jeveuo(nomjv, 'L', jma)
!
        call cmcovo(nomain, nomaou, nbma, zi(jma), prfno,&
                    prfma, numma, epais, plan, trans)
!
!
        goto 350
!
    endif
!
! ----------------------------------------------------------------------
!                 TRAITEMENT DU MOT CLE "RESTREINT"
! ----------------------------------------------------------------------
    call getfac('RESTREINT', nbrest)
    if (nbrest .ne. 0) then
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE2_98')
        endif
        call rdtmai(nomain, nomaou, 'G', nomaou//'.CRNO', nomaou// '.CRMA',&
                    'G', 0, 0)
! ---    VERIFICATIONS DU MAILLAGE
        call chckma(nomaou, 1.0d-03)
        goto 350
!
    endif
!
! ----------------------------------------------------------------------
!               AURES MOTS CLES :
! ----------------------------------------------------------------------
!
    nommav=nomain//'.NOMMAI         '
    nomnov=nomain//'.NOMNOE         '
    typmav=nomain//'.TYPMAIL        '
    connev=nomain//'.CONNEX         '
    grpmav=nomain//'.GROUPEMA       '
    grpnov=nomain//'.GROUPENO       '
    nodimv=nomain//'.DIME           '
    coovav=nomain//'.COORDO    .VALE'
    coodsv=nomain//'.COORDO    .DESC'
    coorev=nomain//'.COORDO    .REFE'
!
    nommai=nomaou//'.NOMMAI         '
    nomnoe=nomaou//'.NOMNOE         '
    typmai=nomaou//'.TYPMAIL        '
    connex=nomaou//'.CONNEX         '
    grpmai=nomaou//'.GROUPEMA       '
    grpnoe=nomaou//'.GROUPENO       '
    nodime=nomaou//'.DIME           '
    cooval=nomaou//'.COORDO    .VALE'
    coodsc=nomaou//'.COORDO    .DESC'
    cooref=nomaou//'.COORDO    .REFE'
    gpptnm=nomaou//'.PTRNOMMAI'
    gpptnn=nomaou//'.PTRNOMNOE'
!
!
    call jedupo(nodimv, 'G', nodime, .false.)
    call jedupo(coodsv, 'G', coodsc, .false.)
    call jedupo(coorev, 'G', cooref, .false.)
    call jedupo(nomain//'.NOMACR', 'G', nomaou//'.NOMACR', .false.)
    call jedupo(nomain//'.PARA_R', 'G', nomaou//'.PARA_R', .false.)
    call jedupo(nomain//'.SUPMAIL', 'G', nomaou//'.SUPMAIL', .false.)
    call jedupo(nomain//'.TYPL', 'G', nomaou//'.TYPL', .false.)
    call jedupo(nomain//'.ABS_CURV', 'G', nomaou//'.ABS_CURV', .false.)
!
    call jeveuo(cooref, 'E', jrefe)
    zk24(jrefe)=nomaou
!
    call jeveuo(nodime, 'E', jdime)
    nbnoev=zi(jdime)
    nbmaiv=zi(jdime+3-1)
!
    call jeveuo(typmav, 'L', jtypmv)
!
! ----------------------------------------------------------------------
!               TRAITEMENT DU MOT CLE "MODI_MAILLE"
! ----------------------------------------------------------------------
!
    call getfac('MODI_MAILLE', nbmoma)
    nbnoaj=0
!
    if (nbmoma .ne. 0) then
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE2_96')
        endif
        momanu='&&OP0167.MO_MA.NUM'
        momano='&&OP0167.MO_MA.NOM'
!
        momuto='&&OP0167.MO_TO.NUM'
        momoto='&&OP0167.MO_TO.NOM'
!
        lisi='&&OP0167.LISI'
        lisk='&&OP0167.LISK'
!
        iadr='&&OP0167.IADR'
        prfn='&&OP0167.PRFN'
        nume1='&&OP0167.NUME'
        prfn2='&&OP0167.PRFN2'
        nume2='&&OP0167.NUME2'
!
        call wkvect(momanu, 'V V I', nbmaiv, jmomnu)
        call wkvect(momano, 'V V K8', nbmaiv, jmomno)
!
        call wkvect(iadr, 'V V I', nbmoma, jiad)
        call wkvect(prfn, 'V V K8', nbmoma, jpro)
        call wkvect(nume1, 'V V I', nbmoma, jnum)
        call wkvect(prfn2, 'V V K8', nbmaiv, jpr2)
        call wkvect(nume2, 'V V I', nbmaiv, jnu2)
!
        iad=1
        do 60 iocc = 1, nbmoma
            call getvtx('MODI_MAILLE', 'OPTION', iocc=iocc, scal=option, nbret=n1)
            zi(jiad+iocc-1)=1
            call getvtx('MODI_MAILLE', 'PREF_NOEUD', iocc=iocc, nbval=0, nbret=n1)
            if (n1 .ne. 0) then
                call getvtx('MODI_MAILLE', 'PREF_NOEUD', iocc=iocc, scal=zk8(jpro+iocc-1),&
                            nbret=n1)
                lgno=lxlgut(zk8(jpro+iocc-1))
            endif
            call getvis('MODI_MAILLE', 'PREF_NUME', iocc=iocc, nbval=0, nbret=n1)
            if (n1 .ne. 0) then
                call getvis('MODI_MAILLE', 'PREF_NUME', iocc=iocc, scal=zi(jnum+iocc-1),&
                            nbret=n1)
            endif
            call palim2('MODI_MAILLE', iocc, nomain, momanu, momano,&
                        zi(jiad+iocc-1))
            if (zi(jiad+iocc-1)-1 .le. 0) then
                call utmess('A', 'MODELISA3_32', sk=option, si=iocc)
                goto 60
            endif
!
            call wkvect(lisi, 'V V I', zi(jiad+iocc-1)-1, jlii)
            call wkvect(lisk, 'V V K8', zi(jiad+iocc-1)-1, jlik)
!
            do 40 ii = 1, zi(jiad+iocc-1)-1
                zi(jlii+ii-1)=zi(jmomnu+ii-1)
                zk8(jlik+ii-1)=zk8(jmomno+ii-1)
40          continue
            call cocali(momuto, lisi, 'I')
            call cocali(momoto, lisk, 'K8')
            iaa=iad
            iad=iad+zi(jiad+iocc-1)-1
!
! LE PREFIXE EST LE MEME POUR TOUS LES NOEUDS ENTRE
! L'ANCIENNE ET LA NOUVELLE ADRESSE
!
            do 50 ii = iaa, iad-1
                zk8(jpr2+ii-1)=zk8(jpro+iocc-1)
50          continue
!
! LE PREF_NUME EST A DEFINIR POUR LE PREMIER NOEUD
! LES AUTRES SE TROUVENT EN INCREMENTANT
!
            zi(jnu2+iaa-1)=zi(jnum+iocc-1)
            call jedetr(lisi)
            call jedetr(lisk)
!
            if (niv .ge. 1) then
                write (ifm,9000)iocc
                if (option .eq. 'TRIA6_7') then
                    write (ifm,9010)zi(jiad+iocc-1)-1,'TRIA6','TRIA7'
                else if (option.eq.'QUAD8_9') then
                    write (ifm,9010)zi(jiad+iocc-1)-1,'QUAD8','QUAD9'
                else if (option.eq.'SEG3_4') then
                    write (ifm,9010)zi(jiad+iocc-1)-1,'SEG3','SEG4'
                endif
            endif
60      continue
!
        call jeveuo(momuto, 'L', jmomtu)
        call jeveuo(momoto, 'L', jmomto)
        nbnoaj=iad-1
        if (nbnoaj .eq. 0) then
            call utmess('F', 'ALGELINE2_99')
        endif
    endif
!
! ----------------------------------------------------------------------
!                TRAITEMENT DU MOT CLE "CREA_MAILLE"
! ----------------------------------------------------------------------
!
    call getfac('CREA_MAILLE', nbcrma)
    nbmaj1=0
    if (nbcrma .ne. 0) then
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE3_1')
        endif
        crmanu='&&OP0167.CR_MA.NUM'
        crmano='&&OP0167.CR_MA.NOM'
        call wkvect(crmanu, 'V V I', nbmaiv, jcrmnu)
        call wkvect(crmano, 'V V K8', nbmaiv, jcrmno)
        nbmaj1=0
        do 70 iocc = 1, nbcrma
            nbmst=nbmaj1
            call palim3('CREA_MAILLE', iocc, nomain, crmanu, crmano,&
                        nbmaj1)
            if (niv .ge. 1) then
                write (ifm,9040)iocc
                write (ifm,9050)nbmaj1-nbmst
            endif
70      continue
        call jeveuo(crmanu, 'L', jcrmnu)
        call jeveuo(crmano, 'L', jcrmno)
    endif
!
! ----------------------------------------------------------------------
!                 TRAITEMENT DU MOT CLE "CREA_GROUP_MA"
! ----------------------------------------------------------------------
!
    call getfac('CREA_GROUP_MA', nbgrma)
    nbmaj2=0
    if (nbgrma .ne. 0) then
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE3_2')
        endif
        crgrnu='&&OP0167.CR_GR.NUM'
        crgrno='&&OP0167.CR_GR.NOM'
        call wkvect(crgrnu, 'V V I', nbmaiv, jcrgnu)
        call wkvect(crgrno, 'V V K8', nbmaiv, jcrgno)
        nbmaj2=0
        do 80 iocc = 1, nbgrma
            call palim3('CREA_GROUP_MA', iocc, nomain, crgrnu, crgrno,&
                        nbmaj2)
80      continue
        call jeveuo(crgrnu, 'L', jcrgnu)
        call jeveuo(crgrno, 'L', jcrgno)
    endif
!
! ----------------------------------------------------------------------
!                TRAITEMENT DU MOT CLE "CREA_POI1"
! ----------------------------------------------------------------------
!
    call getfac('CREA_POI1', nbcrp1)
    nbmaj3=0
    if (nbcrp1 .ne. 0) then
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE3_3')
        endif
        call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), ntpoi)
!
!        -- RECUPERATION DE LA LISTE DES NOEUD :
        nomjv='&&OP0167.LISTE_NO'
        motfac='CREA_POI1'
        motcle(1)='NOEUD'
        tymocl(1)='NOEUD'
        motcle(2)='GROUP_NO'
        tymocl(2)='GROUP_NO'
        motcle(3)='MAILLE'
        tymocl(3)='MAILLE'
        motcle(4)='GROUP_MA'
        tymocl(4)='GROUP_MA'
        motcle(5)='TOUT'
        tymocl(5)='TOUT'
!
        call wkvect('&&OP0167.IND_NOEUD', 'V V I', nbnoev, jtrno)
        call wkvect('&&OP0167.NOM_NOEUD', 'V V K8', nbnoev, jnono)
!
        do 100 iocc = 1, nbcrp1
            call reliem(' ', nomain, 'NO_NOEUD', motfac, iocc,&
                        nbmc, motcle, tymocl, nomjv, nbno)
            call jeveuo(nomjv, 'L', jnoeu)
            do 90 i = 0, nbno-1
                call jenonu(jexnom(nomnov, zk8(jnoeu+i)), ino)
                zi(jtrno-1+ino)=1
90          continue
100      continue
!
!        --- VERIFICATION QUE LE NOM N'EXISTE PAS ET COMPTAGE---
        do 110 ima = 1, nbnoev
            if (zi(jtrno+ima-1) .eq. 0) goto 110
            call jenuno(jexnum(nomnov, ima), newmai)
            call jenonu(jexnom(nommav, newmai), ibid)
            if (ibid .eq. 0) then
                nbmaj3=nbmaj3+1
                zk8(jnono-1+nbmaj3)=newmai
            else
                valk(1)=newmai
                valk(2)=newmai
                call utmess('A', 'ALGELINE4_43', nk=2, valk=valk)
            endif
110      continue
    endif
!
! ----------------------------------------------------------------------
!          ON AGRANDIT LE '.NOMNOE' ET LE '.COORDO    .VALE'
! ----------------------------------------------------------------------
!
    if (nbnoaj .ne. 0) then
        nbnot=nbnoev+nbnoaj
        zi(jdime)=nbnot
!
        call jecreo(nomnoe, 'G N K8')
        call jeecra(nomnoe, 'NOMMAX', nbnot, ' ')
        do 120 ino = 1, nbnoev
            call jenuno(jexnum(nomnov, ino), nomg)
            call jeexin(jexnom(nomnoe, nomg), iret)
            if (iret .eq. 0) then
                call jecroc(jexnom(nomnoe, nomg))
            else
                valk(1)=nomg
                call utmess('F', 'ALGELINE4_5', sk=valk(1))
            endif
120      continue
        do 130 ino = nbnoev+1, nbnot
! TRAITEMENT DES NOEUDS AJOUTES
! ON CODE LE NUMERO DU NOEUD COURANT
            call codent(zi(jnu2+ino-nbnoev-1), 'G', knume)
!
! SI LE PREFIXE COURANT EST LE MEME QUE LE SUIVANT ALORS
! LE NUME EST INCREMENTE
            if (zk8(jpr2+ino-nbnoev-1) .eq. zk8(jpr2+ino-nbnoev)) then
                zi(jnu2+ino-nbnoev)=zi(jnu2+ino-nbnoev-1)+1
            endif
!
            lgnu=lxlgut(knume)
            prfn1=zk8(jpr2+ino-nbnoev-1)
            lgno=lxlgut(prfn1)
            if (lgnu+lgno .gt. 8) then
                call utmess('F', 'ALGELINE_16')
            endif
            nomg=prfn1(1:lgno)//knume
            call jeexin(jexnom(nomnoe, nomg), iret)
            if (iret .eq. 0) then
                call jecroc(jexnom(nomnoe, nomg))
            else
                valk(1)=nomg
                call utmess('F', 'ALGELINE4_5', sk=valk(1))
            endif
130      continue
!
        call jeveuo(coovav, 'L', jvale)
        call wkvect(cooval, 'G V R8', 3*nbnot, kvale)
        do 140 i = 0, 3*nbnoev-1
            zr(kvale+i)=zr(jvale+i)
140      continue
        call jelira(coovav, 'DOCU', cval=cdim)
        call jeecra(cooval, 'DOCU', cval=cdim)
    else
        call jedupo(nomnov, 'G', nomnoe, .false.)
        call jedupo(coovav, 'G', cooval, .false.)
    endif
!
! --- CAS OU L'ON FOURNIT UNE TABLE.
! --- IL S'AGIT DE DEFINIR LES COORDONNEES DES NOEUDS DU MAILLAGE
! --- EN SORTIE DANS UN NOUVEAU REPERE.
! --- CETTE FONCTIONNALITE SERT DANS LE CAS OU L'ON CALCULE LES
! --- CARACTERISTIQUES DE CISAILLEMENT D'UNE POUTRE A PARTIR DE LA
! --- DONNEE D'UNE SECTION DE CETTE POUTRE MAILLEE AVEC DES ELEMENTS
! --- MASSIFS 2D.
! --- LA TABLE OBTENUE PAR POST_ELEM (OPTION : CARA_GEOM)  CONTIENT
! --- LES COORDONNEES DE LA NOUVELLE ORIGINE  (I.E. LE CENTRE DE
! --- GRAVITE) ET L'ANGLE FORME PAR LES AXES PRINCIPAUX D'INERTIE
! --- (LES NOUVEAUX AXES) AVEC LES AXES GLOBAUX :
! --- ON DEFINIT LE MAILLAGE EN SORTIE DANS CE NOUVEAU REPERE
! --- POUR LE CALCUL DU CENTRE DE CISAILLEMENT TORSION ET DES
! --- COEFFICIENTS DE CISAILLEMENT.
! --- DANS LE CAS OU L'ON DONNE LE MOT-CLE ORIG_TORSION
! --- LA TABLE CONTIENT LES COORDONNEES DU CENTRE DE CISAILLEMENT-
! --- TORSION ET ON DEFINIT LE NOUVEAU MAILLAGE EN PRENANT COMME
! --- ORIGINE CE POINT. CETTE OPTION EST UTILISEE POUR LE CALCUL
! --- DE L'INERTIE DE GAUCHISSEMENT :
!     -----------------------------
    call getfac('REPERE', nrep)
    if (nrep .ne. 0) then
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE3_4')
        endif
        call getvid('REPERE', 'TABLE', iocc=1, nbval=0, nbret=ntab)
        if (ntab .ne. 0) then
            call getvid('REPERE', 'TABLE', iocc=1, scal=table, nbret=ntab)
            call getvtx('REPERE', 'NOM_ORIG', iocc=1, nbval=0, nbret=nori)
            if (nori .ne. 0) then
                call getvtx('REPERE', 'NOM_ORIG', iocc=1, scal=nomori, nbret=nori)
                if (nomori .eq. 'CDG') then
                    call chcoma(table, nomaou)
                else if (nomori.eq.'TORSION') then
                    call chcomb(table, nomaou)
                else
                    call utmess('F', 'ALGELINE3_5')
                endif
            endif
        endif
    endif
!
! ----------------------------------------------------------------------
!         ON AGRANDIT LE '.NOMMAI' ET LE '.CONNEX'
! ----------------------------------------------------------------------
!
    nbmain=nbmaiv+nbmaj1+nbmaj2+nbmaj3
!
    zi(jdime+3-1)=nbmain
    call jecreo(nommai, 'G N K8')
    call jeecra(nommai, 'NOMMAX', nbmain, ' ')
!
    call wkvect(typmai, 'G V I', nbmain, iatyma)
!
    call jecrec(connex, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmain)
!
    call wkvect('&&OP0167.NBNOMA', 'V V I', nbmain, jnnoma)
    call wkvect('&&OP0167.NBNOMB', 'V V I', nbmaiv, jnnomb)
    call wkvect('&&OP0167.ADRJVX', 'V V I', nbmain, jadrjv)
    call wkvect('&&OP0167.NOMNUM', 'V V I', nbmain, jnonum)
    dimcon = 0
    decala = 0
    do 180 ima = 1, nbmaiv
        call jenuno(jexnum(nommav, ima), nomg)
        call jeexin(jexnom(nommai, nomg), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(nommai, nomg))
        else
            valk(1)=nomg
            call utmess('F', 'ALGELINE4_7', sk=valk(1))
        endif
!
        call jenonu(jexnom(nommav, nomg), ibid)
        jtom=jtypmv-1+ibid
        call jenonu(jexnom(nommai, nomg), ibid)
        zi(iatyma-1+ibid)=zi(jtom)
!
        call jenonu(jexnom(nommav, nomg), ibid)
        call jelira(jexnum(connev, ibid), 'LONMAX', nbpt)
        call jeveuo(jexnum(connev, ibid), 'L', jopt)
        nbptt=nbpt
        do 150 in = 1, nbnoaj
            if (ima .eq. zi(jmomtu+in-1)) then
                nbptt=nbpt+1
                goto 160
!
            endif
150      continue
160      continue
        call jenonu(jexnom(nommai, nomg), ibid)
        dimcon = dimcon+nbptt
        zi(jnnoma+ima-1) = nbptt
        zi(jnnomb+ima-1) = nbpt
        zi(jadrjv+ima-1) = jopt
        zi(jnonum+ima-1) = ibid
180  end do
!
    decala = decala + nbmaiv
!
    do 200 ima = 1, nbmaj1
        newmai=zk8(jcrmno+ima-1)
        inumol=zi(jcrmnu+ima-1)
        call jeexin(jexnom(nommai, newmai), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(nommai, newmai))
        else
            valk(1)=newmai
            call utmess('F', 'ALGELINE4_7', sk=valk(1))
        endif
!
        jtom=jtypmv-1+inumol
        call jenonu(jexnom(nommai, newmai), ibid)
        if (ibid .eq. 0) then
            call utmess('F', 'ALGELINE3_6', sk=newmai)
        endif
        zi(iatyma-1+ibid)=zi(jtom)
!
        call jelira(jexnum(connev, inumol), 'LONMAX', nbpt)
        call jeveuo(jexnum(connev, inumol), 'L', jopt)
        dimcon = dimcon+nbpt
        zi(jnnoma+decala+ima-1) = nbpt
        zi(jadrjv+decala+ima-1) = jopt
        zi(jnonum+decala+ima-1) = ibid
200  end do
!
    decala = decala + nbmaj1
!
    do 220 ima = 1, nbmaj2
        newmai=zk8(jcrgno+ima-1)
        inumol=zi(jcrgnu+ima-1)
        call jeexin(jexnom(nommai, newmai), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(nommai, newmai))
        else
            valk(1)=newmai
            call utmess('F', 'ALGELINE4_7', sk=valk(1))
        endif
!
        jtom=jtypmv-1+inumol
        call jenonu(jexnom(nommai, newmai), ibid)
        if (ibid .eq. 0) then
            call utmess('F', 'ALGELINE3_6', sk=newmai)
        endif
        zi(iatyma-1+ibid)=zi(jtom)
!
        call jelira(jexnum(connev, inumol), 'LONMAX', nbpt)
        call jeveuo(jexnum(connev, inumol), 'L', jopt)
        dimcon = dimcon+nbpt
        zi(jnnoma+decala+ima-1) = nbpt
        zi(jadrjv+decala+ima-1) = jopt
        zi(jnonum+decala+ima-1) = ibid
220  end do
!
    dimcon = dimcon+nbmaj3
    call jeecra(connex, 'LONT', dimcon)
!
    decala = 0
    do 500 ima = 1, nbmaiv
        nbptt = zi(jnnoma+decala+ima-1)
        nbpt = zi(jnnomb+decala+ima-1)
        jopt = zi(jadrjv+decala+ima-1)
        ibid = zi(jnonum+decala+ima-1)
        call jeecra(jexnum(connex, ibid), 'LONMAX', nbptt)
        call jeveuo(jexnum(connex, ibid), 'E', jnpt)
        do 510 ino = 0, nbpt-1
            zi(jnpt+ino)=zi(jopt+ino)
510      continue
500  end do
!
    decala = decala + nbmaiv
!
    do 520 ima = 1, nbmaj1
        nbpt = zi(jnnoma+decala+ima-1)
        jopt = zi(jadrjv+decala+ima-1)
        ibid = zi(jnonum+decala+ima-1)
        call jeecra(jexnum(connex, ibid), 'LONMAX', nbpt)
        call jeveuo(jexnum(connex, ibid), 'E', jnpt)
        do 530 ino = 0, nbpt-1
            zi(jnpt+ino)=zi(jopt+ino)
530      continue
520  end do
!
    decala = decala + nbmaj1
!
    do 540 ima = 1, nbmaj2
        nbpt = zi(jnnoma+decala+ima-1)
        jopt = zi(jadrjv+decala+ima-1)
        ibid = zi(jnonum+decala+ima-1)
        call jeecra(jexnum(connex, ibid), 'LONMAX', nbpt)
        call jeveuo(jexnum(connex, ibid), 'E', jnpt)
        do 550 ino = 0, nbpt-1
            zi(jnpt+ino)=zi(jopt+ino)
550      continue
540  end do
!
    do 230 ima = 1, nbmaj3
        newmai=zk8(jnono+ima-1)
        call jenonu(jexnom(nommai, newmai), ibid)
        if (ibid .ne. 0) goto 230
        call jeexin(jexnom(nommai, newmai), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(nommai, newmai))
        else
            valk(1)=newmai
            call utmess('F', 'ALGELINE4_7', sk=valk(1))
        endif
!
        call jenonu(jexnom(nommai, newmai), ibid)
        if (ibid .eq. 0) then
            call utmess('F', 'ALGELINE3_6', sk=newmai)
        endif
        zi(iatyma-1+ibid)=ntpoi
!
        call jeecra(jexnum(connex, ibid), 'LONMAX', 1)
        call jeveuo(jexnum(connex, ibid), 'E', jnpt)
        call jenonu(jexnom(nomnoe, newmai), zi(jnpt))
230  end do
    call jedetr('&&OP0167.NBNOMA')
    call jedetr('&&OP0167.NBNOMB')
    call jedetr('&&OP0167.ADRJVX')
    call jedetr('&&OP0167.NOMNUM')
! ----------------------------------------------------------------------
!
    call jeexin(grpmav, iret)
    if (iret .eq. 0) then
        nbgrmv=0
    else
        call jelira(grpmav, 'NOMUTI', nbgrmv)
    endif
    nbgrmn=nbgrmv+nbgrma
    if (nbgrmn .ne. 0) then
        call jecreo(gpptnm, 'G N K24')
        call jeecra(gpptnm, 'NOMMAX', nbgrmn, ' ')
        call jecrec(grpmai, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                    nbgrmn)
        do 250 i = 1, nbgrmv
            call jenuno(jexnum(grpmav, i), nomg)
            call jeexin(jexnom(grpmai, nomg), iret)
            if (iret .eq. 0) then
                call jecroc(jexnom(grpmai, nomg))
            else
                valk(1)=nomg
                call utmess('F', 'ALGELINE4_9', sk=valk(1))
            endif
            call jeveuo(jexnum(grpmav, i), 'L', jvg)
            call jelira(jexnum(grpmav, i), 'LONMAX', nbma)
            call jeecra(jexnom(grpmai, nomg), 'LONMAX', max(nbma, 1))
            call jelira(jexnum(grpmav, i), 'LONUTI', nbma)
            call jeecra(jexnom(grpmai, nomg), 'LONUTI', nbma)
            call jeveuo(jexnom(grpmai, nomg), 'E', jgg)
            do 240 j = 0, nbma-1
                zi(jgg+j)=zi(jvg+j)
240          continue
250      continue
        do 270 i = 1, nbgrma
            call getvtx('CREA_GROUP_MA', 'NOM', iocc=i, scal=nomg, nbret=n1)
            ASSERT(n1.eq.1)
            call jeexin(jexnom(grpmai, nomg), iret)
            if (iret .eq. 0) then
                call jecroc(jexnom(grpmai, nomg))
            else
                valk(1)=nomg
                call utmess('F', 'ALGELINE4_9', sk=valk(1))
            endif
            nbmaj2=0
            call palim3('CREA_GROUP_MA', i, nomain, crgrnu, crgrno,&
                        nbmaj2)
            call jeveuo(crgrno, 'L', jcrgno)
            call jeecra(jexnom(grpmai, nomg), 'LONMAX', max(nbmaj2, 1))
            call jeecra(jexnom(grpmai, nomg), 'LONUTI', nbmaj2)
            call jeveuo(jexnom(grpmai, nomg), 'E', iagma)
            do 260 ima = 0, nbmaj2-1
                call jenonu(jexnom(nommai, zk8(jcrgno+ima)), zi(iagma+ ima))
260          continue
270      continue
    endif
!
! ----------------------------------------------------------------------
!
    call jeexin(grpnov, iret)
    if (iret .eq. 0) then
        nbgrno=0
    else
        call jelira(grpnov, 'NOMUTI', nbgrno)
        call jecreo(gpptnn, 'G N K24')
        call jeecra(gpptnn, 'NOMMAX', nbgrno, ' ')
        call jecrec(grpnoe, 'G V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE',&
                    nbgrno)
        do 290 i = 1, nbgrno
            call jenuno(jexnum(grpnov, i), nomg)
            call jeveuo(jexnum(grpnov, i), 'L', jvg)
            call jelira(jexnum(grpnov, i), 'LONUTI', nbno)
            call jeexin(jexnom(grpnoe, nomg), iret)
            if (iret .eq. 0) then
                call jecroc(jexnom(grpnoe, nomg))
            else
                valk(1)=nomg
                call utmess('F', 'ALGELINE4_11', sk=valk(1))
            endif
            call jeecra(jexnom(grpnoe, nomg), 'LONMAX', max(nbno, 1))
            call jeecra(jexnom(grpnoe, nomg), 'LONUTI', nbno)
            call jeveuo(jexnom(grpnoe, nomg), 'E', jgg)
            do 280 j = 0, nbno-1
                zi(jgg+j)=zi(jvg+j)
280          continue
290      continue
    endif
!
    if (nbmoma .ne. 0) call cmmoma(nomaou, momuto, nbnoev, nbnoaj)
!
!
! ----------------------------------------------------------------------
!         CREATION DES GROUP_MA ASSOCIE AU MOT CLE "CREA_POI1"
! ----------------------------------------------------------------------
!
    if (nbcrp1 .ne. 0) then
        nbgrma=0
        do 300 iocc = 1, nbcrp1
            call getvtx('CREA_POI1', 'NOM_GROUP_MA', iocc=iocc, nbval=0, nbret=n1)
            if (n1 .ne. 0) nbgrma=nbgrma+1
300      continue
        if (nbgrma .ne. 0) then
            call jeexin(grpmai, iret)
            if (iret .eq. 0) then
                call jecreo(gpptnm, 'G N K24')
                call jeecra(gpptnm, 'NOMMAX', nbgrma, ' ')
                call jecrec(grpmai, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                            nbgrma)
            else
                grpmav='&&OP0167.GROUPEMA'
                call jelira(grpmai, 'NOMUTI', nbgma)
                nbgrmt=nbgma+nbgrma
                call cpclma(nomaou, '&&OP0167', 'GROUPEMA', 'V')
                call jedetr(grpmai)
                call jedetr(gpptnm)
                call jecreo(gpptnm, 'G N K24')
                call jeecra(gpptnm, 'NOMMAX', nbgrmt, ' ')
                call jecrec(grpmai, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                            nbgrmt)
                do 320 i = 1, nbgma
                    call jenuno(jexnum(grpmav, i), nomg)
                    call jeexin(jexnom(grpmai, nomg), iret)
                    if (iret .eq. 0) then
                        call jecroc(jexnom(grpmai, nomg))
                    else
                        valk(1)=nomg
                        call utmess('F', 'ALGELINE4_9', sk=valk(1))
                    endif
                    call jeveuo(jexnum(grpmav, i), 'L', jvg)
                    call jelira(jexnum(grpmav, i), 'LONMAX', nbma)
                    call jeecra(jexnom(grpmai, nomg), 'LONMAX', max(1, nbma))
                    call jelira(jexnum(grpmav, i), 'LONUTI', nbma)
                    call jeecra(jexnom(grpmai, nomg), 'LONUTI', nbma)
                    call jeveuo(jexnom(grpmai, nomg), 'E', jgg)
                    do 310 j = 0, nbma-1
                        zi(jgg+j)=zi(jvg+j)
310                  continue
320              continue
            endif
            do 340 iocc = 1, nbcrp1
                call getvtx('CREA_POI1', 'NOM_GROUP_MA', iocc=iocc, nbval=0, nbret=n1)
                if (n1 .ne. 0) then
                    call getvtx('CREA_POI1', 'NOM_GROUP_MA', iocc=iocc, scal=nogma, nbret=n1)
                    call jenonu(jexnom(grpmai, nogma), ibid)
                    if (ibid .gt. 0) then
                        call utmess('F', 'ALGELINE3_7', sk=nogma)
                    endif
                    call reliem(' ', nomain, 'NO_NOEUD', motfac, iocc,&
                                nbmc, motcle, tymocl, nomjv, nbma)
                    call jeveuo(nomjv, 'L', jmail)
!
                    call jeexin(jexnom(grpmai, nogma), iret)
                    if (iret .eq. 0) then
                        call jecroc(jexnom(grpmai, nogma))
                    else
                        valk(1)=nogma
                        call utmess('F', 'ALGELINE4_9', sk=valk(1))
                    endif
                    call jeecra(jexnom(grpmai, nogma), 'LONMAX', max(nbma, 1))
                    call jeecra(jexnom(grpmai, nogma), 'LONUTI', nbma)
                    call jeveuo(jexnom(grpmai, nogma), 'E', iagma)
                    do 330,ima=0,nbma-1
                    call jenonu(jexnom(nommai, zk8(jmail+ima)), zi( iagma+ima))
330                  continue
                    if (niv .ge. 1) then
                        write (ifm,9020)iocc
                        write (ifm,9030)nogma,nbma
                    endif
                endif
340          continue
        endif
    endif
! ----------------------------------------------------------------------
!              TRAITEMENT DU MOT CLE DETR_GROUP_MA
! ----------------------------------------------------------------------
!
    call getfac('DETR_GROUP_MA', nbdgma)
    if (nbdgma .eq. 1) then
        if (nn1 .eq. 0) then
            call utmess('F', 'ALGELINE3_8')
        endif
        call cmdgma(nomaou)
    endif
350  continue
!
    call titre()
!
    call cargeo(nomaou)
!
!     IMPRESSIONS DU MOT CLE INFO :
!     -----------------------------
    call infoma(nomaou)
!
!
    call jedema()
!
    9000 format ('MOT CLE FACTEUR "MODI_MAILLE", OCCURRENCE ',i4)
    9010 format ('  MODIFICATION DE ',i6,' MAILLES ',a8,' EN ',a8)
    9020 format ('MOT CLE FACTEUR "CREA_POI1", OCCURRENCE ',i4)
    9030 format ('  CREATION DU GROUP_MA ',a8,' DE ',i6,' MAILLES POI1')
    9040 format ('MOT CLE FACTEUR "CREA_MAILLE", OCCURRENCE ',i4)
    9050 format ('  CREATION DE ',i6,' MAILLES')
end subroutine

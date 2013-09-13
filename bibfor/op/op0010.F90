subroutine op0010()
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: patrick.massin at edf.fr
!
! aslint: disable=W1501
    implicit none
!
! ----------------------------------------------------------------------
!
! OPERATEUR PROPA_XFEM
!
! CALCUL DE LA FISSURE APRES PROPAGATION AU PAS DE TEMPS SUIVANT
!
! ----------------------------------------------------------------------
!
!
!
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/cncinv.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdbg.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mesr.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xajuls.h"
#include "asterfort/xbaslo.h"
#include "asterfort/xenrch.h"
#include "asterfort/xlenri.h"
#include "asterfort/xpraju.h"
#include "asterfort/xprdis.h"
#include "asterfort/xprdom.h"
#include "asterfort/xprgeo.h"
#include "asterfort/xprini.h"
#include "asterfort/xprls.h"
#include "asterfort/xprmil.h"
#include "asterfort/xprpls.h"
#include "asterfort/xprrei.h"
#include "asterfort/xprreo.h"
#include "asterfort/xprtor.h"
#include "asterfort/xprupw.h"
#include "asterfort/xprvit.h"
    integer :: ifm, niv, ibid, ndim, iret, jcaraf, clsm, jconx1, jconx2, nbma, i
    integer :: j
    integer :: iadrma
    real(kind=8) :: lcmin, deltat
    character(len=8) :: k8bid, noma, nomo, fiss, fispre, method, fisini, ncrack
    character(len=16) :: k16bid, typdis
    character(len=19) :: cnsvt, cnsvn, grlt, grln, cnslt, cnsln, cnsen, cnsenr
    character(len=19) :: noesom, isozro, noresi, cnxinv, cnsbl, cnsdis, cnslj
    character(len=19) :: vpoint, delta
    character(len=24) :: lismae, lisnoe, vcn, grlr, vcnt, grlrt
    real(kind=8) :: meserr(3)
    character(len=8) :: test, msgout(2)
!     MESSAGES
!
!     CRACK ADVANCEMENT
    real(kind=8) :: damax, dttot, vmax, rayon, dafiss, bmax
    character(len=24) :: vvit, vbeta
    character(len=19) :: cnsbet, listp
    integer :: crack, jbeta, jvit, jfiss, nbval, nfiss
    integer :: numfis
!
!     LEVELSET AUXILIARY MESH
    character(len=8) :: unomo, unoma, griaux
    integer :: jlisno
    character(len=19) :: ucnslt, ucnsln, ugrlt, ugrln, ucnxin, disfr, nodtor
    character(len=19) :: eletor, liggrd
    logical :: grille, locdom
!
!     DUMMY MODEL
    character(len=8) :: dnoma, dnomo
    character(len=19) :: dcnslt, dcnsln, dgrlt, dgrln, dcnxin
!
!     FIELD PROJECTION
    real(kind=8) :: radtor
    character(len=16) :: corres
    character(len=19) :: ndomp, edomg
!
!     TEST_MAIL
    real(kind=8) :: dist, distol
!
!     DOMAINE LOCALISATION
    integer :: nbno, jgltp, jglnp, jglt, jgln, jgltl, jglnl
    character(len=19) :: grltc, grlnc
    logical :: ldpre
    real(kind=8) :: radimp, radlim
!
!     FRONT SUR LA GRILLE
    logical :: goinop
    character(len=19) :: cnseg, cnseng, cnsljg
    character(len=24) :: lismag, lisnog
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infdbg('XFEM', ifm, niv)
!
! --- NOM DU CONCEPT FISSURE
!
    call getres(fiss, k16bid, k16bid)
!
!
! --- RETRIEVE THE NAME OF THE CRACK THAT MUST BE ELABORATED
!
    call getvid(' ', 'FISS_PROP', scal=fispre, nbret=ibid)
!
!     VERIFICATION QUE L'ON TRAITE UNE FISSURE ET NON UNE INTERFACE
    call dismoi('F', 'TYPE_DISCONTINUITE', fispre, 'FISS_XFEM', ibid,&
                typdis, iret)
    if (typdis .ne. 'FISSURE') call u2mess('F', 'XFEM2_1')
!
!
! --- NOM DU MODELE
!
    call getvid(' ', 'MODELE', scal=nomo, nbret=ibid)
!
!     CHECK THAT A CRACK HAS BEEN DEFINED ON THE MODEL
!     AND RETRIEVE THE NUMBER OF CRACKS IN THE MODEL
    call dismoi('F', 'NB_FISS_XFEM', nomo, 'MODELE', nfiss,&
                k8bid, iret)
    if (nfiss .eq. 0) call u2mesk('F', 'XFEM2_93', 1, nomo)
!
!     RETRIEVE THE NAME OF THE DATA STRUCTURE CONTAINING EACH CRACK
    call jeveuo(nomo//'.FISS', 'L', jfiss)
!
    numfis=0
!     SEARCH FOR THE CRACK THAT MUST BE PROPAGATED
    do 1 crack = 1, nfiss
!
        ncrack = zk8(jfiss-1+crack)
        if (ncrack .eq. fispre) numfis=crack
!
 1  end do
!
    if (numfis .eq. 0) then
        msgout(1) = fispre
        msgout(2) = nomo
        call u2mesk('F', 'XFEM2_89', 2, msgout)
    endif
!
! --- RETRIEVE THE NAME OF THE MODEL THAT SHOULD BE USED AS AN AUXILIARY
!     GRID FOR THE EVALUATION OF THE LEVELSETS.
!
    call jeexin(fispre//'.GRI.MODELE', ibid)
    if (ibid .eq. 0) then
!        NO AUXILIARY GRID USED
        griaux=' '
        grille=.false.
    else
!        AUXILIARY GRID USED
        call jeveuo(fispre//'.GRI.MODELE', 'L', ibid)
        griaux=zk8(ibid-1+1)
        grille=.true.
    endif
!
!     WRITE A WARNING IF THE CRACK HAS BEEN DEFINED GIVING DIRECTLY THE
!     TWO LEVEL SET FIELDS
    call jeexin(fispre//'.CHAMPS.LVS', ibid)
    if ((ibid.gt.0) .and. (.not.grille)) then
        call jeveuo(fispre//'.CHAMPS.LVS', 'L', ibid)
        if (zl(ibid)) call u2mess('A', 'XFEM_69')
    endif
!
!     CHECK IF THE LOCALIZATION OF THE DOMAIN SHOULD BE ACTIVATED
    locdom=.false.
    call getvtx(' ', 'ZONE_MAJ', scal=k8bid, nbret=ibid)
    radimp=0.d0
    if (k8bid(1:4) .eq. 'TORE') then
!        OK, THE LOCALIZATION MUST BE ACTIVATED
        locdom = .true.
!        CHECK IF THE USER HAS SPECIFIED THE RADIUS OF THE TORUS
        call getvr8(' ', 'RAYON_TORE', scal=radimp, nbret=ibid)
        if (ibid .eq. 0) then
!           THE USER HAS NOT SPECIFIED THE RADIUS OF THE TORUS
            radimp = -1.d0
        else
            radimp = radimp**2
        endif
    else
!        THE WHOLE GRID MUST BE USED
        locdom = .false.
    endif
!
!
! --- NOM DU MAILLAGE ATTACHE AU MODELE
!
    call jeveuo(nomo(1:8)//'.MODELE    .LGRF', 'L', iadrma)
    noma = zk8(iadrma)
!
! --- DIMENSION DU PROBLEME
!
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8bid, iret)
    if ((ndim.lt.2) .or. (ndim.gt.3)) then
        call u2mess('F', 'XFEM_18')
    endif
!
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8bid, ibid)
    call jeveuo(noma(1:8)//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma(1:8)//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
! --- CONNECTIVITE INVERSEE
!
!
    cnxinv = '&&XPRREO.CNCINV'
    call cncinv(noma, ibid, 0, 'V', cnxinv)
!
!     RETRIEVE THE MAXIMUM ADVANCEMENT OF THE CRACK FRONT
    call getvr8(' ', 'DA_MAX', scal=damax, nbret=ibid)
!
!     RETRIEVE THE VALUE FOR THE "TEST_MAIL" PARAMETER
    call getvtx(' ', 'TEST_MAIL', scal=test, nbret=ibid)
!
!     ISSUE AN ALARM FOR THE USER
    if (test(1:3) .eq. 'OUI') then
        if (ndim .eq. 2) call u2mess('F', 'XFEM2_87')
    endif
!
!     RECUPERATION DES VITESSES DE PROPAGATION, DES ANGLES
!     DE BIFURCATION ET DE L'AVANCEE MAXIMALE DE LA FISSURE
!     A PROPAGER
!
    vvit = '&&OP0010.VVIT'
    vbeta = '&&OP0010.VBETA'
!
    call getvr8(' ', 'VITESSE', nbval=0, nbret=nbval)
!
    call wkvect(vbeta, 'V V R8', -nbval, jbeta)
    call wkvect(vvit, 'V V R8', -nbval, jvit)
!
    call getvr8(' ', 'ANGLE', nbval=-nbval, vect=zr(jbeta), nbret=ibid)
    call getvr8(' ', 'VITESSE', nbval=-nbval, vect=zr(jvit), nbret=ibid)
!
    call getvr8(' ', 'DA_FISS', scal=dafiss, nbret=ibid)
!
!     RECUPERATION DU NOMBRE DE CYCLES
    call getvr8(' ', 'NB_CYCLES', scal=dttot, nbret=ibid)
!
! --- RECUPERATION DES LEVEL SETS ET GRADIENTS
!
    cnslt = '&&OP0010.CNSLT'
    cnsln = '&&OP0010.CNSLN'
    grlt = '&&OP0010.GRLT'
    grln = '&&OP0010.GRLN'
    call cnocns(fispre//'.LTNO', 'V', cnslt)
    call cnocns(fispre//'.LNNO', 'V', cnsln)
    call cnocns(fispre//'.GRLTNO', 'V', grlt)
    call cnocns(fispre//'.GRLNNO', 'V', grln)
!
! --- DUPLICATION DES GROUP_MA_ENRI ET GROUP_NO_ENRI
!
    lismae = fiss//'.GROUP_MA_ENRI'
    lisnoe = fiss//'.GROUP_NO_ENRI'
    call jedupo(fispre//'.GROUP_MA_ENRI', 'G', lismae, .false.)
    call jedupo(fispre//'.GROUP_NO_ENRI', 'G', lisnoe, .false.)
!
! --- DUPLICATION DE INFO ET MODELE (LA NOUVELLE FISSURE RESTE
!     ATTACHE AU MODELE SAIN INITIAL
!
    call jedupo(fispre//'.INFO', 'G', fiss//'.INFO', .false.)
    call jedupo(fispre//'.MODELE', 'G', fiss//'.MODELE', .false.)
!
! --- RECUPERATION DES CARACTERISTIQUES DU FOND DE FISSURE
!
    call jedupo(fispre//'.CARAFOND', 'G', fiss//'.CARAFOND', .false.)
    call jeveuo(fiss//'.CARAFOND', 'L', jcaraf)
!
!   RECUPERATION DE LA METHODE DE REINITIALISATION A EMPLOYER
    call getvtx(' ', 'METHODE', scal=method, nbret=ibid)
!
!   RETRIEVE THE RADIUS THAT MUST BE USED TO ASSESS THE LOCAL RESIDUAL
    call getvr8(' ', 'RAYON', scal=rayon, nbret=ibid)
!
!     SET THE DEFAULT VALUES FOR THE DOMAIN RESTRICTION FLAG
    ldpre = .false.
!
!-----------------------------------------------------------------------
!     RETRIEVE THE AUXILIARY GRID FOR THE LEVELSETS, IF THIS IS THE CASE
!-----------------------------------------------------------------------
    if (grille) then
!
        unomo = griaux
!
!        RETRIEVE THE NAME OF THE MAILLAGE
        call jeveuo(unomo//'.MODELE    .LGRF', 'L', iadrma)
        unoma = zk8(iadrma)
!
!        RETRIEVE THE LEVELSETS AND THEIR GRADIENTS ON THE AUXILIARY
!        GRID
        ucnslt = '&&OP0010.UCNSLT'
        ucnsln = '&&OP0010.UCNSLN'
        ugrlt = '&&OP0010.UGRLT'
        ugrln = '&&OP0010.UGRLN'
        call cnocns(fispre//'.GRI.LTNO', 'V', ucnslt)
        call cnocns(fispre//'.GRI.LNNO', 'V', ucnsln)
        call cnocns(fispre//'.GRI.GRLTNO', 'V', ugrlt)
        call cnocns(fispre//'.GRI.GRLNNO', 'V', ugrln)
!
!        CREATE THE INVERSE CONNECTIVITY
        ucnxin = '&&OP0010.UCNCINV'
        call cncinv(unoma, ibid, 0, 'V', ucnxin)
!
!        CREATE A TEMPORARY JEVEUO OBJECT TO STORE THE "CONNECTION"
!        BETWEEN THE PHYSICAL AND AUXILIARY MESH USED IN THE PROJECTION
        corres = '&&OP0010.CORRES'
!
!        WRITE SOME INFORMATIONS ABOUT THE MODELS USED IN THE
!        PROPAGATION
        if (niv .ge. 0) then
            write(ifm,*)'UNE GRILLE AUXILIAIRE EST UTILISEE POUR LA'//&
            ' PROPAGATION:'
            write(ifm,*)'   MODELE PHYSIQUE  : ',nomo
            write(ifm,*)'   MODELE GRILLE AUXILIAIRE: ',unomo
        endif
!
    else
!
!        NO PROJECTION REQUIRED
        corres = ' '
!
!        WRITE SOME INFORMATIONS ABOUT THE MODELS USED IN THE
!        PROPAGATION
        if (niv .ge. 0) then
            write(ifm,*)'LA PROPAGATION EST CALCULEE SUR LE MODELE '//&
            'DE LA STRUCTURE.'
            write(ifm,*)'AUCUNE GRILLE AUXILIAIRE N''EST UTILISEE.'
            write(ifm,*)'   MODELE STRUCTURE: ',nomo
        endif
!
    endif
!
!-----------------------------------------------------------------------
!     CHECK FOR THE COHERENCE OF THE USE OF THE AUXILIARY GRID AND
!     DOMAIN LOCALISATION BETWEEN THE PREVIOUS AND THE ACTUAL STEP
!-----------------------------------------------------------------------
!
!     CHECK THE CONDITION ON THE VALUE OF DAMAX IF THE DOMAIN
!     LOCALISATION HAS BEEN REQUESTED (ONLY FOR 3D MESHES)
    if (locdom .and. (ndim.eq.3)) then
        call jeveuo(vbeta, 'L', jbeta)
        call jelira(vbeta, 'LONMAX', j)
        bmax = 0.d0
        do 500 i = 1, j
            if (abs(zr(jbeta-1+i)) .gt. bmax) bmax=abs(zr(jbeta-1+i))
500      continue
!        THE CHECK IS MADE ONLY IF THE ANGLE IS GREATER THAN 3 DEGREES
!        AND LOWER OF 90 DEGREES
        if ((bmax.lt.1.57d0) .and. (bmax.gt.5.2d-2)) then
            if (dafiss .lt. (rayon/cos(bmax))) then
                meserr(1)=damax
                meserr(2)=dafiss
                meserr(3)=rayon/cos(bmax)*damax/dafiss
                call u2mesr('A', 'XFEM2_94', 3, meserr)
            endif
        endif
    endif
!
    call jeexin(fispre//'.PRO.RAYON_TORE', ibid)
    if (ibid .ne. 0) then
        ldpre=.true.
    else
        ldpre=.false.
    endif
!
!     IF THE DOMAIN LOCALISATION HAS BEEN USED PREVIOUSLY, IT MUST BE
!     USED ALSO IN THIS STEP
    if (ldpre .and. (.not.locdom)) call u2mess('F', 'XFEM2_97')
!
!     IF AN AUXILIARY GRID IS USED IN THIS STEP, STORE ITS NAME FOR THE
!     NEW CRACK
    if (grille) then
        call wkvect(fiss//'.GRI.MODELE', 'G V K8', 1, ibid)
        zk8(ibid) = griaux
    endif
!
!-----------------------------------------------------------------------
!     SET THE CORRECT VALUE OF THE WORKING MODEL IN ORDER TO ASSESS
!     CORRECTLY THE CASE IN WHICH THE USER WANTS TO USE ONLY ONE MODEL
!     AND THE CASE IN WHICH HE OR SHE WANTS TO USE TWO DIFFERENT MODELS.
!     ALL THE FOLLOWING SUBROUTINES REFER TO A DUMMY MODEL AND ALL THE
!     VARIABLES REFERRED TO THIS MODEL BEGIN WITH THE LETTER "D".
!-----------------------------------------------------------------------
!
    if (grille) then
!
        dnoma = unoma
        dnomo = unomo
        dcnslt = ucnslt
        dcnsln = ucnsln
        dgrlt = ugrlt
        dgrln = ugrln
        dcnxin = ucnxin
!
    else
!
        dnoma = noma
        dnomo = nomo
        dcnslt = cnslt
        dcnsln = cnsln
        dgrlt = grlt
        dgrln = grln
        dcnxin = cnxinv
!
    endif
!
!-----------------------------------------------------------------------
!     INITIALISE THE SIMPLEXE OR THE UPWIND SCHEME
!-----------------------------------------------------------------------
!
    noesom = '&&OP0010.NOESOM'
    noresi = '&&OP0010.NORESI'
    if (.not.grille) then
        vcn = '&&OP0010.VCN'
        grlr = '&&OP0010.GRLR'
    else
        vcn = unoma//'.GRLI'
        grlr = unoma//'.GRLR'
    endif
!
    if (grille) then
!        RETREIVE THE LENGTH OF THE SHORTEST EDGE IN THE GRID FROM THE
!        SD_GRILLE
        call jeveuo(unoma//'.GRLR', 'L', ibid)
        lcmin=zr(ibid)
    endif
!
!     FISPRE AND FISS ARE NOT USED BY THE UPWIND SCHEMA. HOWEVER THEY
!     ARE PASSED TO THE SUBROUTINE IN ORDER TO USE THE SAME SUBROUTINE
!     FOR THE SIMPLEXE AND UPWIND SCHEMA.
    call xprini(dnomo, dnoma, dcnxin, grille, fispre,&
                fiss, dcnsln, dcnslt, dgrlt, noesom,&
                noresi, vcn, grlr, lcmin)
!
!-----------------------------------------------------------------------
!     CALCUL DES POINTS DU FOND DE FISSURE SUR LA GRILLE
!     DANS LE CADRE DE L'UTILISATION D'UN FOND VIRTUEL
!-----------------------------------------------------------------------
!
    goinop=.false.
    if ((grille) .and. (ndim.eq.3) .and. (method.ne.'GEOMETRI')) then
        lismag = '&&OP0010.LISTE_MA_ENRICH'
        lisnog = '&&OP0010.LISTE_NO_ENRICH'
        goinop=.true.
        call xlenri(dnoma, fispre, goinop, lismag, lisnog)
!
        cnsljg = '&&OP0010.CNSLJG'
        cnseg='&&OP0010.CNSEG'
        cnseng='&&OP0010.CNSENG'
        call xenrch(dnomo, dnoma, dcnslt, dcnsln, cnsljg,&
                    cnseg, cnseng, ndim, fispre, goinop,&
                    lismag, lisnog)
!
        call jedetr(cnsljg)
        call jedetr(cnseg)
        call jedetr(cnseng)
    endif
!
!-----------------------------------------------------------------------
!     CALCUL DES CHAM_NO_S DES VITESSES DE PROPAGATION
!-----------------------------------------------------------------------
!
    if (locdom) then
        if (radimp .le. 0.d0) then
            radtor=(rayon+damax)**2
        endif
    endif
!
    if (niv .ge. 0) then
        write(ifm,*)
        write(ifm,*)'OP0010-1) CALCUL DU CHAMP DE VITESSE AUX NOEUDS'
        write(ifm,901)
    endif
!
    cnsvt='&&OP0010.CNSVT'
    cnsvn='&&OP0010.CNSVN'
    vpoint='&&OP0010.VPOINT'
    cnsbet='&&OP0010.CNSBET'
    listp='&&OP0010.LISTP'
    disfr='&&OP0010.DISFR'
    cnsbl='&&OP0010.CNSBL'
    cnsdis='&&OP0010.CNSDIS'
    delta='&&OP0010.DELTA'
!
    call xprvit(dnoma, fispre, ndim, vvit, vbeta,&
                lcmin, cnsvt, cnsvn, vpoint, cnsbl,&
                cnsdis, disfr, cnsbet, listp, damax,&
                locdom, radimp, radtor, delta, ucnslt,&
                ucnsln)
!
!
!
!-----------------------------------------------------------------------
!     DOMAINS USED FOR THE RESTRICTION AND FOR THE PROJECTION
!-----------------------------------------------------------------------
!
    if (niv .ge. 0) then
        write(ifm,*)
        write(ifm,*)'OP0010-2) DOMAINE DE CALCUL'
        write(ifm,901)
    endif
!
    if (locdom) then
        vcnt = '&&OP0010.VCNT'
        grlrt = '&&OP0010.GRLRT'
    else
        vcnt = vcn
        grlrt = grlr
    endif
!
!     DEFINE THE PROJECTION DOMAINS FOR THE PHYSICAL AND LEVEL SET
!     MESHES (IF THE AUXILIARY GRID IS USED)
    if (grille) then
        ndomp = '&&OP0010.NDOMP'
        edomg = '&&OP0010.EDOMG'
        call xprdom(dnoma, dcnxin, disfr, noma, cnxinv,&
                    fispre, damax, ndomp, edomg, radtor)
    else
!        IF THE PROJECTION HAS NOT BEEN SELECTED, THE ESTIMATION OF THE
!        RADIUS OF THE TORUS DEFINING THE LOCAL DOMAIN TO BE USED FOR
!        THE LEVEL SET UPDATE CALCULATIONS MUST BE ESTIMATED HERE
        radtor = (rayon+damax)**2
    endif
!
!     RETREIVE THE RADIUS OF THE TORUS TO BE IMPOSED
    if (locdom) then
!        THE USER HAS NOT SPECIFIED THE RADIUS. THE RADIUS USED IN
!        THE PREVIOUS PROPAGATION SHOULD BE RETREIVED, IF ANY
        if ((radimp.lt.0.d0) .and. ldpre) then
            call jeveuo(fispre//'.PRO.RAYON_TORE', 'L', ibid)
            radimp = zr(ibid)
        endif
    endif
!
!     DEFINE THE DOMAIN USED FOR THE LEVEL SET COMPUTATION (ONLY IF
!     THE LOCALISATION HAS BEEN SELECTED)
    nodtor='&&OP0010.NODTOR'
    eletor='&&OP0010.ELETOR'
    liggrd='&&OP0010.LIGGRD'
!
    call xprtor(method, dnomo, dnoma, dcnxin, fispre,&
                fiss, vcn, grlr, dcnsln, dgrln,&
                dcnslt, dgrlt, locdom, radtor, radimp,&
                cnsdis, disfr, cnsbl, nodtor, eletor,&
                liggrd, vcnt, grlrt)
!
!     CHECK IF THE RADIUS OF THE TORUS IS GREATER THAN THE CRITICAL
!     VALUE
    if (ldpre) then
        call jeveuo(fispre//'.PRO.RAYON_TORE', 'L', ibid)
!
!        CALCULATE THE CRITICAL VALUE OF THE RADIUS
        radlim = damax**2+zr(ibid)**2
!
        if (radlim .lt. radtor) then
            meserr(1)=sqrt(radtor)
            meserr(2)=sqrt(radlim)
            call u2mesr('A', 'XFEM2_88', 2, meserr)
        endif
!
    endif
!
    if (locdom .and. (niv.ge.0)) then
        write(ifm,*)'   LE DOMAINE DE CALCUL EST LOCALISE AUTOUR DU'//&
     &               ' FOND DE LA FISSURE:'
        write(ifm,*)'      RAYON DU TORE DE LOCALISATION = ',&
     &                sqrt(radtor)
!
        call jelira(nodtor, 'LONMAX', i)
        write(ifm,*)'      NOMBRE DE NOEUDS DU DOMAINE   = ',i
    endif
!
    if ((.not.locdom) .and. (niv.ge.0)) then
        if (griaux .eq. ' ') then
            write(ifm,*)'   LE DOMAINE DE CALCUL COINCIDE AVEC LE'//&
     &                  ' MODELE PHYSIQUE ',nomo
        else
            write(ifm,*)'   LE DOMAINE DE CALCUL COINCIDE AVEC LE'//&
     &                  ' MODELE GRILLE AUXILIAIRE ',griaux
        endif
    endif
!
!     MAKE SOME CHECKS
!
!     THE VALUE OF RAYON MUST BE GREATER THAN THE SHORTEST EDGE IN THE
!     MESH IN ORDER TO BE ABLE TO CALCULATE THE LOCAL RESIDUAL
    if (rayon .lt. lcmin) then
        meserr(1)=rayon
        meserr(2)=lcmin
        call u2mesr('F', 'XFEM2_64', 2, meserr)
    endif
!
!     THE VALUE OF DAMAX SHOULD BE GREATER THAN THE SHORTEST EDGE IN THE
!     MESH. IF THIS IS NOT TRUE, THE MESH COULD FAIL TO CORRECTLY
!     REPRESENT THE LEVEL SETS. THIS IS NOT A FATAL ERROR AND A WARNING
!     MESSAGE IS ISSUED.
    if (lcmin .gt. damax) then
        meserr(1)=damax
        meserr(2)=lcmin
        call u2mesr('A', 'XFEM2_63', 2, meserr)
    endif
!
!-----------------------------------------------------------------------
!     AJUSTEMENT DE VT
!-----------------------------------------------------------------------
!
    if (method .ne. 'GEOMETRI') then
!
        if (niv .ge. 0) then
            write(ifm,*)
            write(ifm,*)'OP0010-3) AJUSTEMENT DU CHAMP DES VITESSES VN'
            write(ifm,903)
        endif
!
        call xpraju(dnoma, fiss, dcnslt, cnsvt, cnsvn,&
                    dttot, vmax)
!
    endif
!
!-----------------------------------------------------------------------
!     PROPAGATION DES LEVEL SETS
!-----------------------------------------------------------------------
    if (niv .ge. 0) then
        write(ifm,*)
        if (method .eq. 'GEOMETRI') then
            write(ifm,*)'OP0010-3) MISE A JOUR DES LEVEL SETS'
        else
            write(ifm,*)'OP0010-4) PROPAGATION DES LEVEL SETS'
        endif
        write(ifm,904)
!
!        WRITE SOME INFORMATIONS
        write(ifm,*)'   AVANCEE MAXIMALE DU FOND DE FISSURE    = '&
     &               ,dafiss
        write(ifm,*)'   NOMBRE DE CYCLES DE FATIGUE            = '&
     &               ,dttot
    endif
!
    if (method .eq. 'GEOMETRI') then
        write(ifm,*)'   '
        write(ifm,*)'   UTILISATION DE LA METHODE GEOMETRIQUE.'
        call xprgeo(dnoma, dcnsln, dcnslt, dgrln, dgrlt,&
                    vpoint, cnsbl, dttot, nodtor, liggrd,&
                    cnsbet, listp)
        goto 1000
    endif
!
    call xprls(dnoma, dcnsln, dcnslt, dgrln, dgrlt,&
               cnsvn, cnsvt, cnsbl, dttot, nodtor,&
               eletor, liggrd, delta)
!
    call jedetr(cnsvt)
    call jedetr(cnsvn)
    call jedetr(cnsbl)
!
!-----------------------------------------------------------------------
!     REINITIALISATION DE LSN
!-----------------------------------------------------------------------
!
    if (niv .ge. 0) then
        write(ifm,*)
        write(ifm,*)'OP0010-5) REINITIALISATION DE LSN'
        write(ifm,905)
    endif
!
    deltat = lcmin*0.45d0
    isozro = '&&OP0010.ISOZRO'
!
    if (method .eq. 'SIMPLEXE') then
        call xprrei(dnoma, fiss, fispre, noesom, noresi,&
                    dcnsln, dcnslt, dgrln, deltat, lcmin,&
                    'LN', isozro, dcnxin, nodtor, eletor,&
                    liggrd)
    endif
!
! "ELSE" AVOIDED IN ORDER TO LEAVE ROOM FOR A FUTURE METHOD
    if (method .eq. 'UPWIND') then
        call xprupw('REINITLN', dnoma, fispre, vcnt, grlrt,&
                    noesom, lcmin, dcnsln, dgrln, dcnslt,&
                    dgrlt, deltat, noresi, isozro, nodtor,&
                    eletor, liggrd)
    endif
!
!-----------------------------------------------------------------------
!     REORTHOGONALISATION DE LST
!-----------------------------------------------------------------------
    if (niv .ge. 0) then
        write(ifm,*)
        write(ifm,*)'OP0010-6) REORTHOGONALISATION DE LST'
        write(ifm,906)
    endif
!
    if (method .eq. 'SIMPLEXE') then
        call xprreo(dnoma, fiss, noesom, noresi, dcnsln,&
                    dcnslt, dgrln, dgrlt, deltat, isozro,&
                    dcnxin, nodtor, eletor, liggrd)
    endif
!
! "ELSE" AVOIDED IN ORDER TO LEAVE ROOM FOR A FUTURE METHOD
    if (method .eq. 'UPWIND') then
        call xprupw('REORTHOG', dnoma, fispre, vcnt, grlrt,&
                    noesom, lcmin, dcnsln, dgrln, dcnslt,&
                    dgrlt, deltat, noresi, isozro, nodtor,&
                    eletor, liggrd)
    endif
!
    call jedetr(isozro)
!
!-----------------------------------------------------------------------
!     REINITIALISATION DE  LST
!-----------------------------------------------------------------------
!
    if (niv .ge. 0) then
        write(ifm,*)
        write(ifm,*)'OP0010-7) REINITIALISATION DE LST'
        write(ifm,907)
    endif
!
    if (method .eq. 'SIMPLEXE') then
        call xprrei(dnoma, fiss, fispre, noesom, noresi,&
                    dcnsln, dcnslt, dgrlt, deltat, lcmin,&
                    'LT', isozro, dcnxin, nodtor, eletor,&
                    liggrd)
    endif
!
! "ELSE" AVOIDED IN ORDER TO LEAVE ROOM FOR A FUTURE METHOD
    if (method .eq. 'UPWIND') then
        call xprupw('REINITLT', dnoma, fispre, vcnt, grlrt,&
                    noesom, lcmin, dcnsln, dgrln, dcnslt,&
                    dgrlt, deltat, noresi, isozro, nodtor,&
                    eletor, liggrd)
    endif
!
    call jedetr(isozro)
1000  continue
    call jedetr(vvit)
    call jedetr(vbeta)
    call jedetr(noesom)
    if (method(1:6) .eq. 'UPWIND') then
        if (.not.grille) then
            call jedetr(vcn)
            call jedetr(grlr)
        endif
        if (locdom) then
            call jedetr(vcnt)
            call jedetr(grlrt)
        endif
    endif
    call jedetr(cnsdis)
    call jedetr(nodtor)
    call jedetr(eletor)
    call jedetr(liggrd)
    call jedetr(vpoint)
    call jedetr(cnsbet)
    call jedetr(listp)
!
!-----------------------------------------------------------------------
!     THE NEW VALUES OF THE LEVELSETS FOR THE AUXILIARY MESH (TWO GRIDS
!     CASE ONLY) ARE STORED. AFTER THAT THESE VALUES MUST BE PROJECTED
!     TO THE PHYSICAL MESH FOR THE FRACTURE MECHANICS COMPUTATIONS.
!-----------------------------------------------------------------------
!
    if (grille) then
!
!       CREATE THE CHAMP_NO WITH THE NEW VALUES OF THE LEVELSETS AND
!       THEIR GRADIENTS. THE EXISTING CHAMP_NO ARE AUTOMATICALLY
!       DESTROYED BY THE SUBROUTINE "CNSCNO"
        call cnscno(dcnslt, ' ', 'OUI', 'G', fiss//'.GRI.LTNO',&
                    'F', ibid)
        call cnscno(dcnsln, ' ', 'OUI', 'G', fiss//'.GRI.LNNO',&
                    'F', ibid)
        call cnscno(dgrlt, ' ', 'OUI', 'G', fiss//'.GRI.GRLTNO',&
                    'F', ibid)
        call cnscno(dgrln, ' ', 'OUI', 'G', fiss//'.GRI.GRLNNO',&
                    'F', ibid)
!
!       PROJECT THE LEVEL SETS
        call xprpls(dnomo, dcnsln, dcnslt, nomo, noma,&
                    cnsln, cnslt, grln, grlt, corres,&
                    ndim, ndomp, edomg)
!
!       STORE THE LIST OF THE NODES OF THE STRUCTURAL MESH WHERE THE
!       PROJECTION HAS BEEN CARRIED OUT
        call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', j,&
                    k8bid, iret)
        call wkvect(fiss//'.PRO.NOEUD_PROJ', 'G V L', j, iret)
!
        call jeveuo(ndomp, 'L', ibid)
        call jelira(ndomp, 'LONMAX', j)
!
        do 1001 i = 1, j
            zl(iret-1+zi(ibid-1+i)) = .true.
1001      continue
!
    endif
!
    call jedetr(disfr)
!
!     NOW I CAN WORK ON THE PHYSICAL MESH
!
!-----------------------------------------------------------------------
!     REAJUSTEMENT DES LEVEL SETS TROP PROCHES DE 0
!-----------------------------------------------------------------------
    if (niv .ge. 0) then
        write(ifm,*)
        if (method .eq. 'GEOMETRI') then
            write(ifm,*)'OP0010-4) ENRICHISSEMENT DE LA SD FISS_XFEM'
        else
            write(ifm,*)'OP0010-8) ENRICHISSEMENT DE LA SD FISS_XFEM'
        endif
        write(ifm,908)
    endif
!
    call xajuls(noma, nbma, cnslt, cnsln, jconx1,&
                jconx2, clsm)
!
    if (niv .ge. 0) then
        write(ifm,*)'NOMBRE DE LEVEL SET REAJUSTEES APRES CONTROLE:',&
        clsm
    endif
!
!-----------------------------------------------------------------------
!     EXTENSION DES LEVEL SETS AUX NOEUDS MILIEUX
!-----------------------------------------------------------------------
!
    call xprmil(noma, cnslt, cnsln)
!
!     IF THE DOMAINE LOCALISATION HAS BEEN USED ON THE PHYSICAL MODEL,
!     THE VALUES OF THE LEVEL SET GRADIENTS OUTSIDE THE DOMAINE MUST
!     BE COPIED FROM THE ORIGINAL VALUES (NOW THEY ARE EQUAL TO ZERO!)
    if ((.not.grille) .and. locdom) then
!        RETRIEVE THE LIST OF THE NODES USED IN THE LAST LOCALIZATION
        call jeveuo(fiss//'.PRO.NOEUD_TORE', 'E', jlisno)
        call jelira(fiss//'.PRO.NOEUD_TORE', 'LONMAX', nbno)
!
        grltc = '&&OP0010.GRLTC'
        grlnc = '&&OP0010.GRLNC'
!
        call cnocns(fispre//'.GRLTNO', 'V', grltc)
        call cnocns(fispre//'.GRLNNO', 'V', grlnc)
!
        call jeveuo(grltc//'.CNSV', 'L', jgltp)
        call jeveuo(grlnc//'.CNSV', 'L', jglnp)
!
        call jeveuo(grlt//'.CNSV', 'E', jglt)
        call jeveuo(grlt//'.CNSL', 'E', jgltl)
        call jeveuo(grln//'.CNSV', 'E', jgln)
        call jeveuo(grln//'.CNSL', 'E', jglnl)
!
        do 2000 i = 1, nbno
!
            if (.not.zl(jlisno-1+i)) then
                zr(jglt-1+ndim*(i-1)+1) = zr(jgltp-1+ndim*(i-1)+1)
                zl(jgltl-1+ndim*(i-1)+1) = .true.
                zr(jglt-1+ndim*(i-1)+2) = zr(jgltp-1+ndim*(i-1)+2)
                zl(jgltl-1+ndim*(i-1)+2) = .true.
!
                zr(jgln-1+ndim*(i-1)+1) = zr(jglnp-1+ndim*(i-1)+1)
                zl(jglnl-1+ndim*(i-1)+1) = .true.
                zr(jgln-1+ndim*(i-1)+2) = zr(jglnp-1+ndim*(i-1)+2)
                zl(jglnl-1+ndim*(i-1)+2) = .true.
!
                if (ndim .eq. 3) then
                    zr(jglt-1+ndim*(i-1)+3) = zr(jgltp-1+ndim*(i-1)+3)
                    zl(jgltl-1+ndim*(i-1)+3) = .true.
!
                    zr(jgln-1+ndim*(i-1)+3) = zr(jglnp-1+ndim*(i-1)+3)
                    zl(jglnl-1+ndim*(i-1)+3) = .true.
                endif
!
            endif
!
2000      continue
!
        call jedetr(grltc)
        call jedetr(grlnc)
!
    endif
!
    call cnscno(cnslt, ' ', 'NON', 'G', fiss//'.LTNO',&
                'F', ibid)
    call cnscno(cnsln, ' ', 'NON', 'G', fiss//'.LNNO',&
                'F', ibid)
    call cnscno(grlt, ' ', 'NON', 'G', fiss//'.GRLTNO',&
                'F', ibid)
    call cnscno(grln, ' ', 'NON', 'G', fiss//'.GRLNNO',&
                'F', ibid)
!
!     IF THE DOMAIN LOCALISATION HAS NOT BEEN USED, THE BOOLEAN LIST
!     OF THE NODES IN THE TORE MUST BE DESTROYED
    if (.not.locdom) then
        call jedetr(fiss//'.PRO.NOEUD_TORE')
    endif
!
!     IF THE DOMAIN LOCALISATION HAS BEEN USED, THE RADIUS OF THE TORUS
!     USED IN THE LOCALISATION MUST BE STORED
    if (locdom) then
        call wkvect(fiss//'.PRO.RAYON_TORE', 'G V R', 1, ibid)
!        VALUE OF THE RADIUS USED IN THE ACTUAL PROPAGATION
        zr(ibid) = radtor
    endif
!
    call jedetr(delta)
!----------------------------------------------------------------------+
!                 FIN DE LA PARTIE PROPAGATION :                       |
!                 ----------------------------                         |
!    LA SD FISS_XFEM EST ENRICHIE COMME DANS OP0041 : DEFI_FISS_XFEM   |
!   ( TOUTE MODIF. AFFECTANT OP0041 DOIT ETRE REPERCUTEE PLUS BAS,     |
!     EXCEPTE L'APPEL A SDCONX )                                       |
!----------------------------------------------------------------------+
!
!-----------------------------------------------------------------------
!     CALCUL DE L'ENRICHISSEMENT ET DES POINTS DU FOND DE FISSURE
!-----------------------------------------------------------------------
!
    cnslj = '&&OP0010.CNSLJ'
    cnsen='&&OP0010.CNSEN'
    cnsenr='&&OP0010.CNSENR'
    goinop=.false.
    call xenrch(nomo, noma, cnslt, cnsln, cnslj,&
                cnsen, cnsenr, ndim, fiss, goinop,&
                lismae, lisnoe)
!
    call cnscno(cnsenr, ' ', 'NON', 'G', fiss//'.STNOR',&
                'F', ibid)
    call cnscno(cnsen, ' ', 'NON', 'G', fiss//'.STNO',&
                'F', ibid)
!
!-----------------------------------------------------------------------
!     CALCUL DE LA BASE LOCALE AU FOND DE FISSURE
!-----------------------------------------------------------------------
!
    call xbaslo(noma, fiss, grlt, grln, ndim)
!
!-----------------------------------------------------------------------
!     ELABORATE THE OPTION "TEST_MAIL"
!-----------------------------------------------------------------------
!
!     CHECK THE MESH, IF THIS IS THE CASE
    if (test(1:3) .eq. 'OUI') then
!        RETREIVE THE DISTANCE BETWEEN THE PROPAGATED CRACK AND THE
!        INITIAL FRONT
        call getvr8(' ', 'DISTANCE', scal=dist, nbret=ibid)
!        RETREIVE THE VALUE OF THE TOLERANCE
        call getvr8(' ', 'TOLERANCE', scal=distol, nbret=ibid)
!        RETREIVE THE INITIAL CRACK
        call getvid(' ', 'FISS_INITIALE', scal=fisini, nbret=ibid)
!        CHECK THE CRACK FRONT
        call xprdis(fisini, fiss, dist, distol, lcmin)
!
    endif
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedetr(cnxinv)
!
    901 format (10x,37('-'))
    903 format (10x,35('-'))
    904 format (10x,26('-'))
    905 format (10x,23('-'))
    906 format (10x,26('-'))
    907 format (10x,23('-'))
    908 format (10x,33('-'))
!
    call jedema()
end subroutine

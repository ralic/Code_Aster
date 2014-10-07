subroutine ndlect(modele, mate, carele, lischa, sddyna)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/mxmoam.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmamab.h"
#include "asterfort/nmcsol.h"
#include "asterfort/nmimpe.h"
#include "asterfort/nmmoam.h"
#include "asterfort/nmmuap.h"
#include "asterfort/nmondp.h"
#include "asterfort/utmess.h"
    character(len=19) :: sddyna
    character(len=24) :: modele, mate, carele
    character(len=19) :: lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! LECTURE DES OPERANDES DYNAMIQUES ET REMPLISSAGE DE SDDYNA
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  MATE   : NOM DU CHAM_MATER
! IN  CARELE : NOM DU CARA_ELEM
! IN  LISCHA : SD L_CHARGES
! IN  SDDYNA : SD DYNAMIQUE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: undemi, un, quatre
    parameter     (undemi = 0.5d0,un   = 1.d0)
    parameter     (quatre = 4.d0 )
!
    integer :: nmodam, nreavi, nondp
    integer :: nbmods, nbmoda, nbmodp
    integer :: iret
    integer :: n1, n2, nbmg, nrv
    integer :: nbexci, nbgene
    character(len=24) :: tsch, psch, losd, nosd, tfor
    integer :: jtsch, jpsch, jlosd, jnosd, jtfor
    character(len=24) :: tcha, ncha, veol, vaol
    integer :: jtcha, jncha, jveol, jvaol
    character(len=24) :: vecent, vecabs
    integer :: jvecen, jvecab
    character(len=8) :: k8bid, licmp(3), rep
    character(len=8) :: rep1, rep2, rep3, rep4, rigiam
    character(len=16) :: schema, kform, k16bid, nomcmd
    character(len=24) :: texte
    character(len=19) :: sdammo, stadyn
    character(len=15) :: sdmuap, sdprmo, sdexso
    character(len=24) :: chondp
    integer :: iform
    integer :: ifm, niv
    real(kind=8) :: alpha, beta, gamma, theta, phi, kappa
    real(kind=8) :: rcmp(3), shima
    aster_logical :: lmuap, lammo, lshima, lviss, lamra
    aster_logical :: lamor, lktan, londe, limped, ldyna, lexpl
!
    character(len=19) :: vefsdo, vefint, vedido, vesstf
    character(len=19) :: vefedo, veondp, vedidi, velapl
!
    character(len=19) :: cnfedo, cnfsdo, cndidi, cnfint
    character(len=19) :: cndido, cncine
    character(len=19) :: cnondp, cnlapl
    character(len=19) :: cnsstf, cnviss
!
    character(len=19) :: depent, vitent, accent
    character(len=19) :: depabs, vitabs, accabs
!
    data cnfedo,cnfsdo    /'&&NDLECT.CNFEDO','&&NDLECT.CNFSDO'/
    data cndido,cndidi    /'&&NDLECT.CNDIDO','&&NDLECT.CNDIDI'/
    data cnfint,cnviss    /'&&NDLECT.CNFINT','&&NDLECT.CNVISS'/
    data cnondp,cnlapl    /'&&NDLECT.CNONDP','&&NDLECT.CNLAPL'/
    data cncine,cnsstf    /'&&NDLECT.CNCINE','&&NDLECT.CNSSTF'/
!
    data vefedo,vefsdo    /'&&NDLECT.VEFEDO','&&NDLECT.VEFSDO'/
    data vedido,vedidi    /'&&NDLECT.VEDIDO','&&NDLECT.VEDIDI'/
    data vefint           /'&&NDLECT.VEFINT'/
    data veondp,velapl    /'&&NDLECT.VEONDP','&&NDLECT.VELAPL'/
    data vesstf           /'&&NDLECT.VESSTF'/
!
    data depent           /'&&NDLECT.DEPENT'/
    data vitent           /'&&NDLECT.VITENT'/
    data accent           /'&&NDLECT.ACCENT'/
!
    data depabs           /'&&NDLECT.DEPABS'/
    data vitabs           /'&&NDLECT.VITABS'/
    data accabs           /'&&NDLECT.ACCABS'/
!
    data stadyn           /'&&NDLECT.STADYN'/
    data sdprmo           /'&&NDLECT.SDPRMO'/
    data sdmuap           /'&&NDLECT.SDMUAP'/
    data sdammo           /'&&NDLECT.SDAMMO'/
    data sdexso           /'&&NDLECT.SDEXSO'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- OPERATEUR APPELANT (STATIQUE OU DYNAMIQUE)
!
    call getres(k8bid, k16bid, nomcmd)
!
! --- INITIALISATIONS
!
    beta = 0.d0
    gamma = 0.d0
    phi = 0.d0
    theta = 0.d0
    kappa = 0.d0
!
! --- LECTURE DONNEES DYNAMIQUE
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    if (ldyna) then
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ... REMPLISSAGE SD DYNAMIQUE'
        endif
    else
        goto 999
    endif
!
! --- ACCES AUX OBJETS DE LA SD SDDYNA
!
    tsch = sddyna(1:15)//'.TYPE_SCH'
    tfor = sddyna(1:15)//'.TYPE_FOR'
    psch = sddyna(1:15)//'.PARA_SCH'
    losd = sddyna(1:15)//'.INFO_SD'
    nosd = sddyna(1:15)//'.NOM_SD'
    tcha = sddyna(1:15)//'.TYPE_CHA'
    ncha = sddyna(1:15)//'.NBRE_CHA'
    veol = sddyna(1:15)//'.VEEL_OLD'
    vaol = sddyna(1:15)//'.VEAS_OLD'
    vecent = sddyna(1:15)//'.VECENT'
    vecabs = sddyna(1:15)//'.VECABS'
    call jeveuo(tsch, 'E', jtsch)
    call jeveuo(tfor, 'E', jtfor)
    call jeveuo(psch, 'E', jpsch)
    call jeveuo(losd, 'E', jlosd)
    call jeveuo(nosd, 'E', jnosd)
    call jeveuo(tcha, 'E', jtcha)
    call jeveuo(ncha, 'E', jncha)
    call jeveuo(veol, 'E', jveol)
    call jeveuo(vaol, 'E', jvaol)
    call jeveuo(vecent, 'E', jvecen)
    call jeveuo(vecabs, 'E', jvecab)
!
! --- EXISTENCE D'AMORTISSEMENT DE RAYLEIGH
!
    lamor = .false.
    lktan = .false.
    call dismoi('EXI_AMOR_ALPHA', mate, 'CHAM_MATER', repk=rep1)
    call dismoi('EXI_AMOR_BETA', mate, 'CHAM_MATER', repk=rep2)
    call dismoi('EXI_AMOR_NOR', mate, 'CHAM_MATER', repk=rep3)
    call dismoi('EXI_AMOR_TAN', mate, 'CHAM_MATER', repk=rep4) 
    if ((rep1(1:3).eq.'OUI') .or. (rep2(1:3).eq.'OUI') .or. (rep3(1:3).eq.'OUI') .or.&
        (rep4(1:3).eq.'OUI')) then
        lamor = .true.
        call getvtx(' ', 'AMOR_RAYL_RIGI', scal=rigiam, nbret=iret)
        if (rigiam .eq. 'TANGENTE') lktan = .true.
    endif

    lamra = lamor
!
    if ((rep1(1:3).eq.'OUI') .or. (rep2(1:3).eq.'OUI')) then
        call utmess('I', 'MECANONLINE5_7')
    endif
    if (.not.lamor) call nmamab(modele, carele, lamor)
    zl(jlosd+1-1) = lamor
    zl(jlosd+13-1) = lktan
    zl(jlosd+16-1) = lamra
!
! --- PARAMETRES DU SCHEMA TEMPS
!
!
    beta = 0.d0
    gamma = 0.d0
    phi = 0.d0
    theta = 0.d0
    kappa = 0.d0
    alpha = 0.d0
    call getvtx('SCHEMA_TEMPS', 'SCHEMA', iocc=1, scal=schema, nbret=iret)
!
    if (schema(1:9) .eq. 'DIFF_CENT') then
        beta = 0.d0
        gamma = 0.5d0
        phi = 0.5d0
        zk16(jtsch+7-1) = 'DIFF_CENTREE'
    else if (schema(1:7).eq.'TCHAMWA') then
        beta = 0.d0
        gamma = 0.5d0
        call getvr8('SCHEMA_TEMPS', 'PHI', iocc=1, scal=phi, nbret=n1)
        zk16(jtsch+8-1) = 'TCHAMWA'
    else if (schema(1:7).eq.'NEWMARK') then
        call getvr8('SCHEMA_TEMPS', 'BETA', iocc=1, scal=beta, nbret=n1)
        call getvr8('SCHEMA_TEMPS', 'GAMMA', iocc=1, scal=gamma, nbret=n1)
        phi = 0.5d0
        zk16(jtsch+2-1) = 'NEWMARK'
    else if (schema(1:13).eq.'THETA_METHODE') then
        call getvr8('SCHEMA_TEMPS', 'THETA', iocc=1, scal=theta, nbret=n2)
        zk16(jtsch+4-1) = 'THETA_METHODE'
        phi = 0.5d0
    else if (schema(1:3).eq.'HHT') then
        call getvr8('SCHEMA_TEMPS', 'ALPHA', iocc=1, scal=alpha, nbret=n1)
        call getvtx('SCHEMA_TEMPS', 'MODI_EQUI', iocc=1, scal=rep, nbret=n1)
        if (rep(1:3) .eq. 'NON') then
            zk16(jtsch+3-1) = 'HHT'
        else
            zk16(jtsch+5-1) = 'HHT_COMPLET'
        endif
        phi = undemi
        beta = (un-alpha)* (un-alpha)/quatre
        gamma = undemi - alpha
    else if (schema(1:5).eq.'KRENK') then
        call getvr8('SCHEMA_TEMPS', 'KAPPA', iocc=1, scal=kappa, nbret=n2)
        zk16(jtsch+9-1) = 'KRENK'
        phi = 0.5d0
    else
        ASSERT(.false.)
    endif
!
    zr(jpsch+1-1) = beta
    zr(jpsch+2-1) = gamma
    zr(jpsch+3-1) = phi
    zr(jpsch+4-1) = theta
    zr(jpsch+5-1) = kappa
    zr(jpsch+7-1) = alpha
!
! --- TYPE DE SCHEMA
!
    lexpl = ndynlo(sddyna,'EXPLICITE')
!
! --- NOM DE QUELQUES SD
!
    zk24(jnosd+3-1) = sdprmo
    zk24(jnosd+4-1) = stadyn
    zk24(jnosd+2-1) = sdammo
    zk24(jnosd+1-1) = sdmuap
    zk24(jnosd+5-1) = sdexso
!
! --- DECALAGE MASSE
!
    call getvr8('SCHEMA_TEMPS', 'COEF_MASS_SHIFT', iocc=1, scal=shima, nbret=n1)
    if (abs(shima) .gt. r8prem()) then
        lshima = .true.
    else
        lshima = .false.
    endif
    zr(jpsch+6-1) = shima
    zl(jlosd+14-1) = lshima
!
! --- TYPE DE FORMULATION
!
    call getvtx('SCHEMA_TEMPS', 'FORMULATION', iocc=1, scal=kform, nbret=n1)
    if (kform(1:11) .eq. 'DEPLACEMENT') then
        iform = 1
    else if (kform(1:7).eq.'VITESSE') then
        iform = 2
    else if (kform(1:12).eq.'ACCELERATION') then
        iform = 3
    endif
    zi(jtfor+1-1) = iform
!
! --- INCOMPATIBILITES SCHEMA/FORMULATION/PARAMETRES
!
    if ((ndynlo(sddyna,'NEWMARK')) .or. (ndynlo(sddyna,'HHT_COMPLET')) .or.&
        (ndynlo(sddyna,'HHT'))) then
        if (beta .eq. 0.d0) then
            call utmess('F', 'MECANONLINE5_9')
        endif
        if (iform .eq. 2) then
            call utmess('F', 'MECANONLINE5_11')
        endif
    endif
    if (ndynlo(sddyna,'THETA_METHODE')) then
        if (iform .eq. 3) then
            call utmess('F', 'MECANONLINE5_12')
        endif
    endif
    if (lexpl) then
        if (iform .ne. 3) then
            call utmess('F', 'MECANONLINE5_10')
        endif
    endif
!
! --- VERIFICATION DE LA PRESENCE D'ELEMENTS AVEC 'IMPE_ABSO'
!
    call nmimpe(modele, limped)
    zl(jlosd+6-1) = limped
!
! --- NOMBRE DE CHARGEMENTS
!
    call getfac('EXCIT', nbexci)
    zi(jncha+1-1) = nbexci
    call getfac('EXCIT_GENE', nbgene)
    zi(jncha+3-1) = nbgene
!
! --- TEST DE LA PRESENCE DE CHARGES DE TYPE 'ONDE_PLANE'
!
    call nmondp(lischa, londe, chondp, nondp)
    zl(jlosd+7-1) = londe
    zi(jncha+2-1) = nondp
    zk24(jtcha+1-1) = chondp
!
! --- MULTI-APPUI - VECTEURS DE DEPL/VITE/ACCE D'ENTRAINEMENT
!
    zk24(jvecen+1-1) = depent
    zk24(jvecen+2-1) = vitent
    zk24(jvecen+3-1) = accent
!
! --- MULTI-APPUI - VECTEURS DE DEPL/VITE/ACCE ABSOLUS
!
    zk24(jvecab+1-1) = depabs
    zk24(jvecab+2-1) = vitabs
    zk24(jvecab+3-1) = accabs
!
! --- MASSE DIAGONALE POUR SCHEMAS EXPLICITES
!
    zl(jlosd+4-1) = .false.
    call getvtx(' ', 'MASS_DIAG', scal=texte, nbret=n1)
    if (n1 .gt. 0) then
        if (texte(1:3) .eq. 'OUI') then
            if (lexpl) then
                zl(jlosd+4-1) = .true.
            else
                call utmess('F', 'MECANONLINE5_13')
            endif
        endif
    endif
!
! --- PROJECTION MODALE POUR SCHEMAS EXPLICITES
!
    zl(jlosd+5-1) = .false.
    zl(jlosd+9-1) = .false.
    if (lexpl) then
        call getfac('PROJ_MODAL', iret)
        if (iret .gt. 0) then
            zl(jlosd+5-1) = .true.
            call mxmoam(sddyna, nbmodp)
            call getvid('PROJ_MODAL', 'MASS_GENE', iocc=1, scal=k8bid, nbret=nbmg)
            zl(jlosd+9-1) = nbmg.ne.0
            zi(jncha+5-1) = nbmodp
        endif
    endif
!
! --- SCHEMA MULTIPAS: VECT_* SAUVEGARDES PAS PRECEDENT
!
    if ((zk16(jtsch+5-1)(1:11).eq.'HHT_COMPLET') .or.&
        (zk16(jtsch+4-1)(1:13).eq.'THETA_METHODE') .or. (zk16(jtsch+9-1)(1:5).eq.'KRENK')) then
        zk24(jveol+1-1) = vefedo
        zk24(jveol+2-1) = vefsdo
        zk24(jveol+3-1) = vedido
        zk24(jveol+4-1) = vedidi
        zk24(jveol+5-1) = vefint
        zk24(jveol+6-1) = veondp
        zk24(jveol+7-1) = velapl
        zk24(jveol+8-1) = vesstf
        zk24(jvaol+1-1) = cnfedo
        zk24(jvaol+2-1) = cnfsdo
        zk24(jvaol+3-1) = cndido
        zk24(jvaol+4-1) = cndidi
        zk24(jvaol+5-1) = cnfint
        zk24(jvaol+6-1) = cnondp
        zk24(jvaol+7-1) = cnlapl
        zk24(jvaol+8-1) = cnsstf
        zk24(jvaol+9-1) = cncine
        zk24(jvaol+10-1) = cnviss
    endif
!
! --- CARTE STADYN POUR POUTRES
!
    licmp(1) = 'STAOUDYN'
    licmp(2) = 'ALFNMK'
    licmp(3) = 'DELNMK'
    rcmp(1) = un
    rcmp(2) = beta
    rcmp(3) = gamma
    call jedetr(stadyn)
    call mecact('V', stadyn, 'MODELE', modele(1:8)//'.MODELE', 'STAOUDYN',&
                ncmp=3, lnomcmp=licmp, vr=rcmp)
!
! --- MODE MULTI-APPUI
!
    call getvid(' ', 'MODE_STAT', scal=k8bid, nbret=nbmods)
    lmuap = nbmods.gt.0
    if (lmuap) then
        call nmmuap(sddyna)
    endif
    zl(jlosd+2-1) = lmuap
!
! --- AMORTISSEMENT MODAL
!
    call getfac('AMOR_MODAL', nmodam)
    lammo = nmodam.gt.0
    if (lammo) then
        call nmmoam(sdammo, nbmoda)
        nreavi = 0
!
! --- REACTUALISATION DE L'AMORT A CHAQUE ITERATION ?
!
        call getvtx('AMOR_MODAL', 'REAC_VITE', iocc=1, scal=k8bid, nbret=nrv)
        if (k8bid .eq. 'OUI') nreavi = 1
    else
        nreavi = 0
        nbmoda = 0
    endif
    zl(jlosd+3-1) = lammo
    zl(jlosd+12-1) = nreavi.gt.0
    zi(jncha+4-1) = nbmoda
!
! --- VECT ISS
!
    call nmcsol(lischa, sddyna, lviss)
    zl(jlosd+15-1) = lviss
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... '//&
     &                'FONCTIONNALITES ACTIVEES EN DYNAMIQUE '
!
        if (ndynlo(sddyna,'IMPLICITE')) then
            write (ifm,*) '<MECANONLINE> ...... SCHEMA IMPLICITE'
        endif
        if (ndynlo(sddyna,'EXPLICITE')) then
            write (ifm,*) '<MECANONLINE> ...... SCHEMA EXPLICITE'
        endif
!
        if (ndynlo(sddyna,'MAT_AMORT')) then
            write (ifm,*) '<MECANONLINE> ...... MATRICE AMORTISSEMENT'
        endif
        if (ndynlo(sddyna,'MULTI_APPUI')) then
            write (ifm,*) '<MECANONLINE> ...... MULTI APPUI'
        endif
        if (ndynlo(sddyna,'AMOR_MODAL')) then
            write (ifm,*) '<MECANONLINE> ...... AMORTISSEMENT MODAL'
        endif
        if (ndynlo(sddyna,'MASS_DIAG')) then
            write (ifm,*) '<MECANONLINE> ...... MATRICE MASSE DIAGONALE'
        endif
        if (ndynlo(sddyna,'PROJ_MODAL')) then
            write (ifm,*) '<MECANONLINE> ...... PROJECTION MODALE'
        endif
        if (ndynlo(sddyna,'IMPE_ABSO')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS D''IMPEDANCE'
        endif
        if (ndynlo(sddyna,'ONDE_PLANE')) then
            write (ifm,*) '<MECANONLINE> ...... CHARGEMENT ONDES PLANES'
        endif
        if (ndynlo(sddyna,'EXPL_GENE')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL EXPLICITE EN MODAL'
        endif
        if (ndynlo(sddyna,'NREAVI')) then
            write (ifm,*) '<MECANONLINE> ...... REAC. VITE'
        endif
        if (ndynlo(sddyna,'COEF_MASS_SHIFT')) then
            write (ifm,*) '<MECANONLINE> ...... COEF. MASS. SHIFT'
        endif
        if (ndynlo(sddyna,'VECT_ISS')) then
            write (ifm,*) '<MECANONLINE> ...... FORCE SOL'
        endif
    endif
!
999 continue
!
    call jedema()
!
end subroutine

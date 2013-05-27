subroutine op0180()
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!-----------------------------------------------------------------------
!  DESCRIPTION :
!  -----------       O P E R A T E U R    D E F I _ C A B L E _ B P
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
!
! VARIABLES LOCALES
! -----------------
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/alcart.h'
    include 'asterfort/caelca.h'
    include 'asterfort/cncinv.h'
    include 'asterfort/crelrl.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/etenca.h'
    include 'asterfort/gromab.h'
    include 'asterfort/immeca.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/ltcrsd.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/nocart.h'
    include 'asterfort/projca.h'
    include 'asterfort/reliem.h'
    include 'asterfort/sigmca.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    include 'asterfort/tensca.h'
    include 'asterfort/titre.h'
    include 'asterfort/tomabe.h'
    include 'asterfort/topoca.h'
    include 'asterfort/trajca.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/voisca.h'
    include 'asterfort/wkvect.h'
    integer :: ibid, icabl, icmp, irana1, iret, jcaba, jnbno, jncmp, jsief
    integer :: jvalv, n1, n2, nbancr, nbcabl, nbf0, nbmama, nbnobe, nbnoma
    integer :: ncaba, nsief, nbmabe, jlimab, nbnoca
    real(kind=8) :: delta, ea, f0, frco, frli, mu0, rh1000, sa, fprg, xflu, xret
    real(kind=8) :: trelax
    logical :: mail2d, relax
    character(len=1) :: k1b
    character(len=3) :: k3b
    character(len=8) :: caelem, chmat, k8b, mailla, modele, noancr(2), nomu
    character(len=8) :: typanc(2)
    character(len=16) :: cmd, concep
    character(len=19) :: carsig, carte, ligrmo, lirela, numaca, nunobe, xnoca
    character(len=19) :: ynoca, znoca, nomt19, nunobi
    character(len=24) :: cadesc, ncncin, nmabet, comima, gromai
    character(len=8) :: aire
    integer :: nbpar, nbnobi
    parameter    (nbpar=13)
    character(len=3) :: typpar(nbpar)
    character(len=24) :: nompar(nbpar), typrel
    character(len=4) :: regl
    integer :: iarg
!
    data          aire  /'A1      '/
    data          typpar /'I ','K8','R ','R ','R ',&
     &                      'K8','K8','I ','I ','R ','K24','K24','K24'/
    data          nompar /'NUME_CABLE              ',&
     &                      'NOEUD_CABLE             ',&
     &                      'ABSC_CURV               ',&
     &                      'ALPHA                   ',&
     &                      'TENSION                 ',&
     &                      'MAILLE_BETON_VOISINE    ',&
     &                      'NOEUD_BETON_VOISIN      ',&
     &                      'INDICE_IMMERSION        ',&
     &                      'INDICE_PROJECTION       ',&
     &                      'EXCENTRICITE            ',&
     &                      'NOM_CABLE               ',&
     &                      'NOM_ANCRAGE1            ',&
     &                      'NOM_ANCRAGE2            '/
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
    call infmaj()
!
    call getres(nomu, concep, cmd)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   SAISIE DES ARGUMENTS POUR VERIFICATION AVANT EXECUTION
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    call getfac('DEFI_CABLE', nbcabl)
    nbcabl = abs(nbcabl)
    call getvr8(' ', 'TENSION_INIT', 0, iarg, 1,&
                f0, ibid)
    call getvr8(' ', 'RECUL_ANCRAGE', 0, iarg, 1,&
                delta, ibid)
    call getvtx(' ', 'TYPE_RELAX', 1, iarg, 1,&
                typrel, ibid)
    if (typrel .eq. 'BPEL') then
        relax = .true.
        call getvr8(' ', 'R_J', 0, iarg, 1,&
                    trelax, ibid)
    else if (typrel(1:4).eq.'ETCC') then
        relax = .true.
        call getvr8(' ', 'NBH_RELAX', 0, iarg, 1,&
                    trelax, ibid)
    else
        relax = .false.
    endif
!     RECUPERATION DES PERTES ELASTIQUES
!     CALL GETVTX(' ','PERT_ELAS'   ,1,IARG,1,PELAS,IBID)
!      IF (PELAS.EQ.'OUI') THEN
!        PERTEL = .TRUE.
!        CALL GETVR8(' ','EP_BETON',0,IARG,1,EPBET,IBID)
!        CALL GETVR8(' ','ESP_CABLE',0,IARG,1,ESPCAB,IBID)
!      ELSE
!        PERTEL = .FALSE.
!      ENDIF
!
!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   VERIFICATION DES ARGUMENTS AVANT EXECUTION
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    do 10 icabl = 1, nbcabl
!
        call getvtx('DEFI_CABLE', 'NOEUD_ANCRAGE', icabl, iarg, 0,&
                    k8b, n1)
        call getvtx('DEFI_CABLE', 'GROUP_NO_ANCRAGE', icabl, iarg, 0,&
                    k8b, n2)
        nbancr = n1 + n2
        if (abs(nbancr) .ne. 2) then
            write(k3b,'(I3)') icabl
            if (n1 .ne. 0) then
                call u2mesk('F', 'MODELISA5_83', 1, k3b)
            else
                call u2mesk('F', 'MODELISA5_84', 1, k3b)
            endif
        else
            if (n1 .ne. 0) then
                call getvtx('DEFI_CABLE', 'NOEUD_ANCRAGE', icabl, iarg, 2,&
                            noancr(1), ibid)
                if (noancr(1) .eq. noancr(2)) then
                    write(k3b,'(I3)') icabl
                    call u2mesk('F', 'MODELISA5_85', 1, k3b)
                endif
            else
                call getvtx('DEFI_CABLE', 'GROUP_NO_ANCRAGE', icabl, iarg, 2,&
                            noancr(1), ibid)
                if (noancr(1) .eq. noancr(2)) then
                    write(k3b,'(I3)') icabl
                    call u2mesk('F', 'MODELISA5_86', 1, k3b)
                endif
            endif
        endif
!
! TEST DU TYPE D'ANCRAGE
!    LE CATALOGUE ASSURE QU'IL Y A DEUX OCCURENCES DE CE MOT-CLE
        call getvtx(' ', 'TYPE_ANCRAGE', icabl, iarg, 2,&
                    typanc(1), ibid)
!
!    SI TYPES D'ANCRAGE SONT TOUS LES DEUX PASSIFS
        if ((typanc(1).eq.'PASSIF') .and. (typanc(2).eq.'PASSIF')) then
            write(k3b,'(I3)') icabl
!    SI LA TENSION EST NULLE : SIMPLE ALARME
            if (f0 .eq. 0.d0) then
                call u2mesk('A', 'MODELISA5_87', 1, k3b)
            else
                call u2mesk('F', 'MODELISA5_88', 1, k3b)
!
            endif
        endif
!    SI LA TENSION EST NON-NULLE : ARRET FATAL
!
!
10  end do
!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 3   SAISIE DES ARGUMENTS A L'EXECUTION
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    call getvid(' ', 'MODELE', 0, iarg, 1,&
                modele, ibid)
    call getvid(' ', 'CHAM_MATER', 0, iarg, 1,&
                chmat, ibid)
    call getvid(' ', 'CARA_ELEM', 0, iarg, 1,&
                caelem, ibid)
    call titre()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 4   EXECUTION DES OPERATIONS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 4.1 RECUPERATION DU NOM DU CONCEPT MAILLAGE
! ---
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                mailla, iret)
    call dismoi('F', 'NB_MA_MAILLA', mailla, 'MAILLAGE', nbmama,&
                k8b, iret)
    call dismoi('F', 'NB_NO_MAILLA', mailla, 'MAILLAGE', nbnoma,&
                k8b, iret)
!
! 4.2 CREATION DES OBJETS DE TRAVAIL
! ---
    call wkvect('&&OP0180.NBNO_CABLE', 'V V I', nbcabl, jnbno)
!
    nunobe = '&&OP0180.NUMNOE_BET'
    call jecreo(nunobe, 'V V I')
    call jeecra(nunobe, 'LONMAX', nbnoma, ' ')
!
    numaca = '&&OP0180.NUMAIL_CAB'
    call jecreo(numaca, 'V V I')
    call jeecra(numaca, 'LONMAX', nbmama, ' ')
!
    xnoca = '&&OP0180.X_NOEU_CAB'
    call jecreo(xnoca, 'V V R')
    call jeecra(xnoca, 'LONMAX', nbnoma, ' ')
    ynoca = '&&OP0180.Y_NOEU_CAB'
    call jecreo(ynoca, 'V V R')
    call jeecra(ynoca, 'LONMAX', nbnoma, ' ')
    znoca = '&&OP0180.Z_NOEU_CAB'
    call jecreo(znoca, 'V V R')
    call jeecra(znoca, 'LONMAX', nbnoma, ' ')
!
! 4.3 EXTENSION DES CARTES ELEMENTAIRES : CREATION DE VECTEURS
! --- D'ADRESSES DES CARACTERISTIQUES POINTES PAR LE NUMERO DE
!     MAILLE
!
    ligrmo = modele//'.MODELE    '
!
    carte = chmat//'.CHAMP_MAT '
    cadesc = carte//'.DESC'
    call jeexin(cadesc, iret)
    if (iret .eq. 0) call u2mess('F', 'MODELISA5_89')
    call etenca(carte, ligrmo, iret)
    if (iret .ne. 0) call u2mesk('F', 'MODELISA3_37', 1, carte)
!
    carte = caelem//'.CARGENBA  '
    cadesc = carte//'.DESC'
    call jeexin(cadesc, iret)
    if (iret .eq. 0) call u2mess('F', 'MODELISA5_90')
    call etenca(carte, ligrmo, iret)
    if (iret .ne. 0) call u2mesk('F', 'MODELISA3_37', 1, carte)
!
!.... DETERMINATION DU RANG DE LA COMPOSANTE <A1>
!.... DE LA GRANDEUR <CAGNBA>
!
    call jelira(jexnom('&CATA.GD.NOMCMP', 'CAGNBA'), 'LONMAX', ncaba, k1b)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', 'CAGNBA'), 'L', jcaba)
    irana1 = 0
    do 20 icmp = 1, ncaba
        if (zk8(jcaba+icmp-1) .eq. aire) then
            irana1 = icmp
            goto 21
        endif
20  end do
21  continue
    if (irana1 .eq. 0) call u2mess('F', 'MODELISA5_91')
!
! 4.4 CREATION DE LA SD TABLE RESULTAT
! ---
    call jeexin(nomu//'           .LTNT', iret)
    if (iret .eq. 0) call ltcrsd(nomu, 'G')
    nomt19 = ' '
    call ltnotb(nomu, 'CABLE_BP', nomt19)
    call jeexin(nomt19//'.TBBA', iret)
    if (iret .eq. 0) call tbcrsd(nomt19, 'G')
    call tbajpa(nomt19, nbpar, nompar, typpar)
!
!
!
! 4.5 CREATION ET INITIALISATION DE LA CARTE ELEMENTAIRE (= 0)
! --- DES CONTRAINTES INITIALES
!
    carsig = nomu//'.CHME.SIGIN'
    call alcart('G', carsig, mailla, 'SIEF_R')
!
    call jelira(jexnom('&CATA.GD.NOMCMP', 'SIEF_R'), 'LONMAX', nsief, k1b)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', 'SIEF_R'), 'L', jsief)
    call jeveuo(carsig//'.NCMP', 'E', jncmp)
    call jeveuo(carsig//'.VALV', 'E', jvalv)
    do 30 icmp = 1, nsief
        zk8(jncmp+icmp-1) = zk8(jsief+icmp-1)
        zr(jvalv+icmp-1) = 0.0d0
30  end do
    call nocart(carsig, 1, ' ', 'NOM', 0,&
                ' ', 0, ligrmo, nsief)
!
! 4.6 CREATION DE LA SD DE TYPE LISTE_DE_RELATIONS
! ---
    lirela = nomu//'.LIRELA    '
    call crelrl('REEL', 'REEL', 'G', lirela)
!
! 4.7 CARACTERISATION DE LA TOPOLOGIE DE LA STRUCTURE BETON
! --- ET RECUPERATION DES CARACTERISTIQUES DU MATERIAU CONSTITUTIF
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 3   CREATION DE LA CONNECTIVITE INVERSE LIMITEE AU GROUP_MA BETON
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    ncncin = '&&OP0180.CONNECINVBETON '
    nmabet = '&&OP0180.MABETON '
    comima = '&&OP0180.COOR_MIN_MAX'
    nunobi = '&&OP0180.NU_BET_ICA'
    gromai = '&&OP0180.DIM_MAX_MABET'
!
    call wkvect(comima, 'V V R', 6, ibid)
    call wkvect(gromai, 'V V R', 3, ibid)
!
    call reliem(' ', mailla, 'NU_MAILLE', ' ', 0,&
                1, 'GROUP_MA_BETON', 'GROUP_MA', nmabet, nbmabe)
!
    call jeexin(ncncin, n2)
    call jeveuo(nmabet, 'L', jlimab)
!
    if (n2 .eq. 0) call cncinv(mailla, zi(jlimab), nbmabe, 'V', ncncin)
!
    call tomabe(chmat, nmabet, nbmabe, mailla, nbnoma,&
                mail2d, nbnobe, nunobe, xflu, xret,&
                regl)
!
    call gromab(mailla, nmabet, nbmabe, mail2d, caelem,&
                gromai)
!
    call wkvect(nunobi, 'V V I', nbnobe, ibid)
!
! 4.8 BOUCLE SUR LE NOMBRE DE CABLES
! ---
    do 40 icabl = 1, nbcabl
!
! 4.8.1  CARACTERISATION DE LA TOPOLOGIE DU CABLE
! .....
        call topoca(nomt19, mailla, icabl, nbf0, zi(jnbno),&
                    numaca)
!
! 4.8.2  RECUPERATION DES CARACTERISTIQUES ELEMENTAIRES DU CABLE
! .....
        call caelca(modele, chmat, caelem, irana1, icabl,&
                    zi(jnbno), numaca, regl, relax, ea,&
                    rh1000, mu0, fprg, frco, frli,&
                    sa)
!
! 4.8.3  INTERPOLATION DE LA TRAJECTOIRE DU CABLE
! .....
        call trajca(nomt19, mailla, icabl, zi(jnbno), xnoca,&
                    ynoca, znoca, comima, gromai)
!
! 4.8.X   SELECTION D'UNE PARTIE DES NOEUDS DE BETON
! .....
        call voisca(mailla, nbnobe, nunobe, comima, nbnobi,&
                    nunobi)
!
! 4.8.4  CALCUL DE LA TENSION LE LONG DU CABLE
! .....
        nbnoca= zi(jnbno+icabl-1)
        call tensca(nomt19, icabl, nbnoca, nbf0, f0,&
                    delta, typrel, trelax, xflu, xret,&
                    ea, rh1000, mu0, fprg, frco,&
                    frli, sa, regl)
!
! 4.8.5  MISE A JOUR DE LA CARTE ELEMENTAIRE DES CONTRAINTES INITIALES
! .....
        call sigmca(nomt19, carsig, icabl, zi(jnbno), numaca)
!
! 4.8.6  DETERMINATION DES RELATIONS CINEMATIQUES ENTRE LES DDL DES
! .....  NOEUDS DU CABLE ET CEUX DES NOEUDS DE LA STRUCTURE BETON
!
!......  REPRESENTATION DE LA STRUCTURE BETON PAR DES MAILLES 2D :
!......  PROJECTION DU CABLE SUR LE MAILLAGE BETON
!
        if (mail2d) then
            call projca(nomt19, lirela, nmabet, nbmabe, mailla,&
                        nbnobi, nunobi, icabl, zi(jnbno), xnoca,&
                        ynoca, znoca)
!
!......  REPRESENTATION DE LA STRUCTURE BETON PAR DES MAILLES 3D :
!......  IMMERSION DU CABLE DANS LE MAILLAGE BETON
!
        else
            call immeca(nomt19, lirela, mailla, nbnobi, nunobi,&
                        icabl, zi( jnbno), xnoca, ynoca, znoca,&
                        ncncin, nmabet)
        endif
!
40  end do
!
    call jedema()
!
! --- FIN DE OP0180.
end subroutine

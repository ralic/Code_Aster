subroutine op0070()
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
! TOLE CRP_20
!
    implicit none
!
! ----------------------------------------------------------------------
!
! COMMANDE:  STAT_NON_LINE ET DYNA_NON_LINE
!
! ----------------------------------------------------------------------
!
!
! --- PARAMETRES DE MECA_NON_LINE
!
    include 'asterc/getres.h'
    include 'asterfort/assert.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/inidbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jerecu.h'
    include 'asterfort/ndexpl.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nmactp.h'
    include 'asterfort/nmaffi.h'
    include 'asterfort/nmarch.h'
    include 'asterfort/nmcvgc.h'
    include 'asterfort/nmcvgp.h'
    include 'asterfort/nmdata.h'
    include 'asterfort/nmeceb.h'
    include 'asterfort/nmerro.h'
    include 'asterfort/nmevdt.h'
    include 'asterfort/nmfpas.h'
    include 'asterfort/nmini0.h'
    include 'asterfort/nminit.h'
    include 'asterfort/nmleeb.h'
    include 'asterfort/nmlost.h'
    include 'asterfort/nmmeng.h'
    include 'asterfort/nmnewt.h'
    include 'asterfort/nmpost.h'
    include 'asterfort/nmrinc.h'
    include 'asterfort/nmstat.h'
    include 'asterfort/nmtime.h'
    include 'asterfort/onerrf.h'
    include 'asterfort/titre.h'
    integer :: zpmet, zpcri, zconv
    integer :: zpcon, znmeth
    parameter   (zpmet  = 9 ,zpcri  = 7 ,zconv = 4)
    parameter   (zpcon  = 10,znmeth = 7 )
    real(kind=8) :: parmet(zpmet), parcri(zpcri), conv(zconv)
    real(kind=8) :: parcon(zpcon)
    character(len=16) :: method(znmeth)
    integer :: fonact(100)
    integer :: zmeelm, zmeass, zveelm, zveass
    parameter    (zmeelm=9 ,zmeass=4 ,zveelm=21,zveass=32)
    integer :: zsolal, zvalin
    parameter    (zsolal=17,zvalin=28)
!
! --- GESTION BOUCLES
!
    integer :: numins, nbiter
    character(len=4) :: etfixe, etinst, etcalc
!
! --- GESTION ERREUR
!
    integer :: lenout
    character(len=16) :: compex
!
    integer :: ibid
!
    real(kind=8) :: eta
!
    character(len=8) :: result, mailla
!
    character(len=16) :: k16bid
    character(len=19) :: lischa, lisch2
    character(len=19) :: solveu, maprec, matass
    character(len=24) :: modele, mate, carele, compor
    character(len=24) :: numedd, numfix
    character(len=24) :: carcri, comref, codere
!
! --- FONCTIONNALITES ACTIVEES
!
    logical :: lexpl, limpl, lstat
!
! --- STRUCTURES DE DONNEES
!
    character(len=24) :: sdimpr, sdtime, sderro, sdieto
    character(len=24) :: sdstat, sdconv, sdsuiv, sdcriq
    character(len=19) :: sdpilo, sdnume, sddyna, sddisc, sdcrit
    character(len=19) :: sdobse, sdpost, sdener
    character(len=24) :: defico, resoco, deficu, resocu
!
! --- VARIABLES CHAPEAUX
!
    character(len=19) :: valinc(zvalin), solalg(zsolal)
!
! --- MATR_ELEM, VECT_ELEM ET MATR_ASSE
!
    character(len=19) :: meelem(zmeelm), veelem(zveelm)
    character(len=19) :: measse(zmeass), veasse(zveass)
!
! ----------------------------------------------------------------------
!
    data sdpilo, sdobse    /'&&OP0070.PILO.','&&OP0070.OBSE.'/
    data sdimpr, sdsuiv    /'&&OP0070.IMPR.','&&OP0070.SUIV.'/
    data sdpost, sdcriq    /'&&OP0070.POST.','&&OP0070.CRIQ.'/
    data sdtime, sderro    /'&&OP0070.TIME.','&&OP0070.ERRE.'/
    data sdieto, sdstat    /'&&OP0070.IETO.','&&OP0070.STAT.'/
    data sdnume            /'&&OP0070.NUME.ROTAT'/
    data sddisc, sdconv    /'&&OP0070.DISC.','&&OP0070.CONV.'/
    data sdcrit            /'&&OP0070.CRIT.'/
    data lischa            /'&&OP0070.LISCHA'/
    data carcri            /'&&OP0070.PARA_LDC'/
    data solveu            /'&&OP0070.SOLVEUR'/
    data defico, resoco    /'&&OP0070.DEFIC','&&OP0070.RESOC'/
    data deficu, resocu    /'&&OP0070.DEFUC', '&&OP0070.RESUC'/
    data comref            /'&&OP0070.COREF'/
    data maprec            /'&&OP0070.MAPREC'/
    data codere            /'&&OP0070.CODERE'/
    data sdener            /'&&OP0070.SDENER'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! -- TITRE
!
    call titre()
    call infmaj()
    call inidbg()
!
! ======================================================================
!     RECUPERATION DES OPERANDES ET INITIALISATION
! ======================================================================
!
! --- ON STOCKE LE COMPORTEMENT EN CAS D'ERREUR AVANT MNL : COMPEX
! --- PUIS ON PASSE DANS LE MODE "VALIDATION DU CONCEPT EN CAS D'ERREUR"
!
    call onerrf(' ', compex, lenout)
    call onerrf('EXCEPTION+VALID', k16bid, ibid)
!
! --- NOM DE LA SD RESULTAT
!
    call getres(result, k16bid, k16bid)
!
! --- PREMIERES INITALISATIONS
!
    call nmini0(zpmet, zpcri, zconv, zpcon, znmeth,&
                fonact, parmet, parcri, conv, parcon,&
                method, eta, numins, matass, zmeelm,&
                zmeass, zveelm, zveass, zsolal, zvalin,&
                sdimpr)
!
! --- LECTURE DES OPERANDES DE LA COMMANDE
!
    call nmdata(result, modele, mate, carele, compor,&
                lischa, solveu, method, parmet, parcri,&
                parcon, carcri, sddyna, sdpost, sderro,&
                sdener, sdcriq, sdimpr)
!
! --- ETAT INITIAL ET CREATION DES STRUCTURES DE DONNEES
!
    call nminit(result, modele, numedd, numfix, mate,&
                compor, carele, parmet, lischa, maprec,&
                solveu, carcri, numins, sdstat, sddisc,&
                sdnume, defico, sdcrit, comref, fonact,&
                parcon, parcri, method, lisch2, mailla,&
                sdpilo, sddyna, sdimpr, sdsuiv, sdobse,&
                sdtime, sderro, sdpost, sdieto, sdener,&
                sdconv, sdcriq, deficu, resocu, resoco,&
                valinc, solalg, measse, veelem, meelem,&
                veasse, codere)
!
! --- PREMIER INSTANT
!
    numins = 1
!
! --- QUELQUES FONCTIONNALITES ACTIVEES
!
    limpl = ndynlo(sddyna,'IMPLICITE')
    lexpl = ndynlo(sddyna,'EXPLICITE')
    lstat = ndynlo(sddyna,'STATIQUE' )
!
! ======================================================================
!  DEBUT DU PAS DE TEMPS
! ======================================================================
!
200  continue
!
! --- AUCUNE BOUCLE N'EST CONVERGE
!
    call nmeceb(sderro, 'RESI', 'CONT')
    call nmeceb(sderro, 'NEWT', 'CONT')
    call nmeceb(sderro, 'FIXE', 'CONT')
    call nmeceb(sderro, 'INST', 'CONT')
!
    call jerecu('V')
    call nmtime(sdtime, 'RUN', 'PAS')
!
! --- REALISATION DU PAS DE TEMPS
!
    if (lexpl) then
        call ndexpl(modele, numedd, numfix, mate, carele,&
                    comref, compor, lischa, method, fonact,&
                    carcri, parcon, sdimpr, sdstat, sdnume,&
                    sddyna, sddisc, sdtime, sderro, valinc,&
                    numins, solalg, solveu, matass, maprec,&
                    meelem, measse, veelem, veasse, nbiter)
    else if (lstat.or.limpl) then
        call nmnewt(mailla, modele, numins, numedd, numfix,&
                    mate, carele, comref, compor, lischa,&
                    method, fonact, carcri, parcon, conv,&
                    parmet, parcri, sdstat, sdieto, sdtime,&
                    sderro, sdimpr, sdnume, sddyna, sddisc,&
                    sdcrit, sdsuiv, sdpilo, sdconv, solveu,&
                    maprec, matass, valinc, solalg, meelem,&
                    measse, veelem, veasse, defico, resoco,&
                    deficu, resocu, eta, nbiter)
    else
        call assert(.false.)
    endif
!
! --- TEMPS CPU PAS DE TEMPS
!
    call nmtime(sdtime, 'END', 'PAS')
    call nmrinc(sdstat, 'PAS')
!
! ======================================================================
!  FIN DU PAS DE TEMPS
! ======================================================================
!
!
! --- TEMPS PERDU SI DECOUPE
!
    call nmleeb(sderro, 'FIXE', etfixe)
    if (etfixe .eq. 'ERRE') call nmlost(sdtime)
!
! --- POST-TRAITEMENTS
!
    call nmpost(modele, mailla, numedd, numfix, carele,&
                compor, solveu, numins, mate, comref,&
                lischa, defico, resoco, resocu, parmet,&
                parcon, fonact, carcri, sdimpr, sdstat,&
                sddisc, sdtime, sdobse, sderro, sdieto,&
                sddyna, sdpost, valinc, solalg, meelem,&
                measse, veelem, veasse, sdener, sdcriq,&
                eta)
!
! --- ETAT DE LA CONVERGENCE DU PAS DE TEMPS
!
    call nmcvgp(sddisc, numins, sderro, valinc, fonact,&
                defico, resoco)
!
! --- AFFICHAGES PENDANT LA BOUCLE DES PAS DE TEMPS
!
    call nmaffi(fonact, sdconv, sdimpr, sderro, sddisc,&
                'INST')
!
! --- STATISTIQUES SUR PAS DE TEMPS
!
    if (.not.lexpl) then
        call nmstat('P', fonact, sdstat, sdtime, sdimpr,&
                    defico)
    endif
!
! --- GESTION DES ACTIONS A LA FIN D'UN PAS DE TEMPS
!
    call nmactp(sdimpr, sddisc, sderro, defico, resoco,&
                solveu, parcri, nbiter, numins)
!
! --- INSTANT SUIVANT
!
    call nmleeb(sderro, 'INST', etinst)
    if (etinst .eq. 'ERRE') then
        goto 200
    else if (etinst.eq.'STOP') then
        goto 1000
    endif
!
! --- VERIFICATION DU DECLENCHEMENT DES ERREURS FATALES
!
    call nmevdt(sdtime, sderro, 'PAS')
!
! --- EVALUATION DE LA CONVERGENCE DU CALCUL
!
    call nmcvgc(sddisc, sderro, numins, fonact)
!
! --- ARCHIVAGE DES RESULTATS
!
    call onerrf(compex, k16bid, ibid)
    call nmarch(result, numins, modele, mate, carele,&
                fonact, carcri, sdimpr, sddisc, sdpost,&
                sdcrit, sdtime, sderro, sddyna, sdpilo,&
                sdener, sdieto, sdcriq, lisch2)
    call onerrf('EXCEPTION+VALID', k16bid, ibid)
!
! --- ETAT DU CALCUL
!
    call nmleeb(sderro, 'CALC', etcalc)
    if ((etcalc.eq.'ERRE') .or. (etcalc.eq.'STOP')) then
        goto 1000
    else if (etcalc.eq.'CONV') then
        goto 900
    endif
!
! --- MISE A JOUR DES INFORMATIONS POUR UN NOUVEAU PAS DE TEMPS
!
    call assert(etcalc.eq.'CONT')
    call nmfpas(fonact, sddyna, sdpilo, sddisc, nbiter,&
                numins, eta, valinc, solalg, veasse)
    numins = numins + 1
!
    goto 200
!
! ======================================================================
!     GESTION DES ERREURS
! ======================================================================
!
1000  continue
!
! --- ON COMMENCE PAR ARCHIVER LE PAS DE TEMPS PRECEDENT
!
    if (numins .ne. 1) then
        call nmarch(result, numins-1, modele, mate, carele,&
                    fonact, carcri, sdimpr, sddisc, sdpost,&
                    sdcrit, sdtime, sderro, sddyna, sdpilo,&
                    sdener, sdieto, sdcriq, lisch2)
    endif
!
! --- GESTION DES ERREURS ET EXCEPTIONS
!
    call nmerro(sderro, sdtime, numins)
!
! ======================================================================
!     SORTIE
! ======================================================================
!
900  continue
!
! --- IMPRESSION STATISTIQUES FINALES
!
    if (.not.lexpl) then
        call nmstat('T', fonact, sdstat, sdtime, sdimpr,&
                    defico)
    endif
!
! --- ON REMET LE MECANISME D'EXCEPTION A SA VALEUR INITIALE
!
    call onerrf(compex, k16bid, ibid)
!
! --- MENAGE
!
    call nmmeng(fonact)
!
    call jedema()
!
end subroutine

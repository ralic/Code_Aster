subroutine dldiff(result, force1, lcrea, lamort, neq,&
                  imat, masse, rigid, amort, dep0,&
                  vit0, acc0, fexte, famor, fliai,&
                  t0, nchar, nveca, liad, lifo,&
                  modele, mate, carele, charge, infoch,&
                  fomult, numedd, nume, solveu, numrep)
!     ------------------------------------------------------------------
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
! TOLE CRP_21
!     ------------------------------------------------------------------
!     CALCUL MECANIQUE TRANSITOIRE PAR INTEGRATION DIRECTE
!     AVEC  METHODE EXPLICITE :  DIFFERENCES CENTREES
!
!     ------------------------------------------------------------------
!
!  HYPOTHESES :                                                "
!  ----------   SYSTEME CONSERVATIF DE LA FORME  K.U    +    M.U = F
!           OU                                           '     "
!               SYSTEME DISSIPATIF  DE LA FORME  K.U + C.U + M.U = F
!
!     ------------------------------------------------------------------
!  IN  : LCREA     : LOGIQUE INDIQUANT SI IL Y A REPRISE
!  IN  : LAMORT    : LOGIQUE INDIQUANT SI IL Y A AMORTISSEMENT
!  IN  : NEQ       : NOMBRE D'EQUATIONS
!  IN  : IMAT      : TABLEAU D'ADRESSES POUR LES MATRICES
!  IN  : MASSE     : MATRICE DE MASSE
!  IN  : RIGID     : MATRICE DE RIGIDITE
!  IN  : AMORT     : MATRICE D'AMORTISSEMENT
!  IN  : NCHAR     : NOMBRE D'OCCURENCES DU MOT CLE CHARGE
!  IN  : NVECA     : NOMBRE D'OCCURENCES DU MOT CLE VECT_ASSE
!  IN  : LIAD      : LISTE DES ADRESSES DES VECTEURS CHARGEMENT (NVECT)
!  IN  : LIFO      : LISTE DES NOMS DES FONCTIONS EVOLUTION (NVECT)
!  IN  : T0        : INSTANT DE CALCUL INITIAL
!  IN  : MODELE    : NOM DU MODELE
!  IN  : MATE      : NOM DU CHAMP DE MATERIAU
!  IN  : CARELE    : CARACTERISTIQUES DES POUTRES ET COQUES
!  IN  : CHARGE    : LISTE DES CHARGES
!  IN  : INFOCH    : INFO SUR LES CHARGES
!  IN  : FOMULT    : LISTE DES FONC_MULT ASSOCIES A DES CHARGES
!  IN  : NUMEDD    : NUME_DDL DE LA MATR_ASSE RIGID
!  IN  : NUME      : NUMERO D'ORDRE DE REPRISE
!  VAR : DEP0      : TABLEAU DES DEPLACEMENTS A L'INSTANT N
!  VAR : VIT0      : TABLEAU DES VITESSES A L'INSTANT N
!  VAR : ACC0      : TABLEAU DES ACCELERATIONS A L'INSTANT N
! IN  NUMREP : NUMERO DE REUSE POUR LA TABLE PARA_CALC
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
    include 'asterc/etausr.h'
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/r8depi.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/dlarch.h'
    include 'asterfort/dldif0.h'
    include 'asterfort/dltcrr.h'
    include 'asterfort/dltins.h'
    include 'asterfort/dyarch.h'
    include 'asterfort/extdia.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetc.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/sigusr.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utexcm.h'
    include 'asterfort/uttcpr.h'
    include 'asterfort/uttcpu.h'
    include 'asterfort/vtcreb.h'
    include 'asterfort/wkvect.h'
    integer :: neq, imat(*), liad(*), nchar, nveca, nume, numrep
    character(len=8) :: masse, rigid, amort
    character(len=24) :: modele, carele, charge, fomult, mate, numedd
    character(len=24) :: infoch, lifo(*)
    character(len=19) :: solveu
    character(len=8) :: result
    character(len=19) :: force1
!
    real(kind=8) :: dep0(*), vit0(*), acc0(*), t0
    real(kind=8) :: fexte(*), famor(*), fliai(*)
!
    logical :: lamort, lcrea
!
!
    integer :: nbtyar
    parameter ( nbtyar = 6 )
    integer :: iwk0, iwk1, iwk2
    integer :: ifm, niv, ne
    integer :: ieq, iexcl
    integer :: idepl1, ivite1, ivite2, iacce1, iarchi
    integer :: ibid
    integer :: alarm, archiv
    integer :: ipepa, igrpa
    integer :: ipas, istop, istoc, jstoc
    integer :: jnbpa, jbint, jlpas
    integer :: npatot, nbgrpa, nbptpa
    integer :: nbexcl, nbordr
    character(len=4) :: typ1(nbtyar)
    character(len=8) :: nomres
    character(len=16) :: typres, nomcmd, typear(nbtyar)
    character(len=19) :: lisarc
    character(len=24) :: lisins, lispas, libint, linbpa
    character(len=24) :: sop
    real(kind=8) :: tps1(4), tps2(4)
    real(kind=8) :: dt, dtm, dtmax, temps, dt1, tf
    real(kind=8) :: omeg, deuxpi
    real(kind=8) :: r8bid
    character(len=8) :: valk
    integer :: vali(2)
    real(kind=8) :: valr(2)
    logical :: ener
!
!
!     -----------------------------------------------------------------
    call jemarq()
!
!====
! 1. LES DONNEES DU CALCUL
!====
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, niv)
!
! 1.2. ==> NOM DES STRUCTURES
!     --- RECUPERATION NOM DE LA COMMANDE ---
!
    call getres(nomres, typres, nomcmd)
!
! 1.3. ==> VECTEURS DE TRAVAIL SUR BASE VOLATILE ---
!
    call wkvect('&&DLDIFF.F0', 'V V R', neq, iwk0)
    call wkvect('&&DLDIFF.F1', 'V V R', neq, iwk1)
    call wkvect('&&DLDIFF.F2', 'V V R', neq, iwk2)
    call vtcreb('&&DLDIFF.DEPL1', numedd, 'V', 'R', neq)
    call jeveuo('&&DLDIFF.DEPL1     '//'.VALE', 'E', idepl1)
    call wkvect('&&DLDIFF.VITE1', 'V V R', neq, ivite1)
    call wkvect('&&DLDIFF.VITE2', 'V V R', neq, ivite2)
    call wkvect('&&DLDIFF.ACCE1', 'V V R', neq, iacce1)
!
    deuxpi = r8depi()
    iarchi = nume
    ener=.false.
    call getfac('ENERGIE', ne)
    if (ne .ne. 0) then
        ener=.true.
    endif
!
! 1.4. ==> PARAMETRES D'INTEGRATION
!
    call dltins(nbgrpa, lispas, libint, linbpa, npatot,&
                t0, lisins)
    call jeveuo(lispas, 'L', jlpas)
    call jeveuo(libint, 'L', jbint)
    call jeveuo(linbpa, 'L', jnbpa)
!
! 1.5. ==> EXTRACTION DIAGONALE M ET CALCUL VITESSE INITIALE
!
    call dismoi('C', 'SUR_OPTION', masse, 'MATR_ASSE', ibid,&
                sop, ibid)
    if (sop .eq. 'MASS_MECA_DIAG') then
        call extdia(masse, numedd, 2, zr(iwk1))
    else
        call u2mess('F', 'ALGORITH3_13')
    endif
!
    dt1 = zr(jlpas)
    r8bid = dt1/2.d0
    do 15,  ieq = 1, neq
    if (zr(iwk1+ieq-1) .ne. 0.d0) then
        zr(iwk1+ieq-1)=1.0d0/zr(iwk1+ieq-1)
    endif
    vit0(1+ieq) = vit0(1+ieq) - r8bid*acc0(1+ieq)
    15 end do
!
! 1.6. ==> --- ARCHIVAGE ---
!
    lisarc = '&&DLDIFF.ARCHIVAGE'
    call dyarch(npatot, lisins, lisarc, nbordr, 1,&
                nbexcl, typ1)
    call jeveuo(lisarc, 'E', jstoc)
!
    typear(1) = 'DEPL'
    typear(2) = 'VITE'
    typear(3) = 'ACCE'
    if (ener) then
        typear(4) = 'FORC_EXTE'
        typear(5) = 'FORC_AMOR'
        typear(6) = 'FORC_LIAI'
    else
        typear(4) = '         '
        typear(5) = '         '
        typear(6) = '         '
    endif
    if (nbexcl .eq. nbtyar) then
        call u2mess('F', 'ALGORITH3_14')
    endif
    do 16 , iexcl = 1,nbexcl
    if (typ1(iexcl) .eq. 'DEPL') then
        typear(1) = '    '
    else if (typ1(iexcl).eq.'VITE') then
        typear(2) = '    '
    else if (typ1(iexcl).eq.'ACCE') then
        typear(3) = '    '
    endif
    16 end do
!
! 1.7. ==> --- AFFICHAGE DE MESSAGES SUR LE CALCUL ---
!
    write(ifm,*) '-------------------------------------------------'
    write(ifm,*) '--- CALCUL PAR INTEGRATION TEMPORELLE DIRECTE ---'
    write(ifm,*) '! LA MATRICE DE MASSE EST         : ',masse
    write(ifm,*) '! LA MATRICE DE RIGIDITE EST      : ',rigid
    if (lamort) write(ifm,*)'! LA MATRICE D''AMORTISSEMENT EST : ',amort
    write(ifm,*) '! LE NB D''EQUATIONS EST          : ',neq
    if (nume .ne. 0) write(ifm,*)'! REPRISE A PARTIR DU NUME_ORDRE  : ',nume
    do 17 , igrpa = 1,nbgrpa
    dt = zr(jlpas-1+igrpa)
    nbptpa = zi(jnbpa-1+igrpa)
    t0 = zr(jbint-1+igrpa)
    tf = t0 + nbptpa*dt
    write(ifm,*)'! L''INSTANT INITIAL EST        : ',t0
    write(ifm,*)'! L''INSTANT FINAL EST          : ',tf
    write(ifm,*)'! LE PAS DE TEMPS DU CALCUL EST : ',dt
    write(ifm,*)'! LE NB DE PAS DE CALCUL EST    : ',nbptpa
    17 end do
    write(ifm,*) '----------------------------------------------',' '
!
!====
! 2. CREATION DU CONCEPT RESULTAT
!====
!
    t0 = zr(jbint)
    call dltcrr(result, neq, nbordr, iarchi, ' ',&
                ifm, t0, lcrea, typres, masse,&
                rigid, amort, dep0, vit0, acc0,&
                fexte, famor, fliai, numedd, nume,&
                nbtyar, typear)
!
!
    call titre()
!
!
!
!====
! 3. CALCUL
!====
!
! 3.1. ==> BOUCLE SUR LES GROUPES DE PAS DE TEMPS
    istop = 0
    ipas = 0
!
    call uttcpu('CPU.DLDIFF.1', 'INIT', ' ')
    call uttcpu('CPU.DLDIFF.2', 'INIT', ' ')
!
    do 31 , igrpa = 1,nbgrpa
!
! 3.1.1. ==> PREALABLES
!
    call uttcpu('CPU.DLDIFF.1', 'DEBUT', ' ')
    dt = zr(jlpas-1+igrpa)
    nbptpa = zi(jnbpa-1+igrpa)
    t0 = zr(jbint-1+igrpa)
    tf = zr(jbint+igrpa)
!
! 3.1.2. ==> VERIFICATION DU PAS DE TEMPS
!
    call extdia(rigid, numedd, 2, zr(iwk2))
    ibid=0
    dtmax=dt
    do 312 ieq = 1, neq
        if (zr(iwk1+ieq-1) .ne. 0.d0) then
            omeg = sqrt( zr(iwk2+ieq-1) * zr(iwk1+ieq-1) )
            dtm = 5.d-02*deuxpi/omeg
            if (dtmax .gt. dtm) then
                dtmax=dtm
                ibid=1
            endif
        endif
312  continue
!
    if (ibid .eq. 1) then
        vali(1) = nint((tf-t0)/dtmax)
        vali(2) = igrpa
        valr(1) = dt
        valr(2) = dtmax
        call u2mesg('F', 'DYNAMIQUE_12', 0, valk, 2,&
                    vali, 2, valr)
    endif
! ==> FIN DE VERIFICATION
!
!
! 3.1.3. ==> BOUCLE SUR LES NBPTPA "PETITS" PAS DE TEMPS
!
    do 313 , ipepa = 1 , nbptpa
    ipas = ipas+1
    if (ipas .gt. npatot) goto 3900
    istoc = 0
    temps = t0 + dt*ipepa
    call uttcpu('CPU.DLDIFF.2', 'DEBUT', ' ')
    archiv = zi(jstoc+ipas-1)
!
    call dldif0(result, force1, neq, istoc, iarchi,&
                ifm, lamort, imat, masse, rigid,&
                amort, dep0, vit0, acc0, zr(idepl1),&
                zr(ivite1), zr(iacce1), zr(ivite2), fexte(1), famor(1),&
                fliai(1), nchar, nveca, liad, lifo,&
                modele, ener, solveu, mate, carele,&
                charge, infoch, fomult, numedd, dt,&
                temps, zr(iwk0), zr(iwk1), archiv, nbtyar,&
                typear, numrep)
!
! 3.5. ==> VERIFICATION DU TEMPS DE CALCUL RESTANT
!
    call uttcpu('CPU.DLDIFF.2', 'FIN', ' ')
    call uttcpr('CPU.DLDIFF.2', 4, tps2)
    if (tps2(1) .lt. 5.d0 .or. tps2(4) .gt. tps2(1)) then
        if (ipepa .ne. npatot) then
            istop = 1
            vali(1) = igrpa
            vali(2) = ipepa
            valr(1) = tps2(4)
            valr(2) = tps2(1)
            goto 3900
        endif
    endif
!
! ---------- FIN DE LA BOUCLE SUR LES NBPTPA "PETITS" PAS DE TEMPS
    313     end do
    call uttcpu('CPU.DLDIFF.1', 'FIN', ' ')
    call uttcpr('CPU.DLDIFF.1', 4, tps1)
    if (tps1(1) .lt. 5.d0 .and. igrpa .ne. nbgrpa) then
        istop = 1
        vali(1) = igrpa
        vali(2) = ipepa
        valr(1) = tps1(4)
        valr(2) = tps1(1)
        goto 3900
    endif
! ------- FIN BOUCLE SUR LES GROUPES DE PAS DE TEMPS
    31 end do
!
3900  continue
!
!====
! 4. ARCHIVAGE DU DERNIER INSTANT DE CALCUL POUR LES CHAMPS QUI ONT
!    ETE EXCLUS DE L'ARCHIVAGE AU FIL DES PAS DE TEMPS
!====
!
    if (nbexcl .ne. 0) then
!
        do 41 , iexcl = 1,nbexcl
        typear(iexcl) = typ1(iexcl)
41      continue
!
        alarm = 0
!
        call dlarch(result, neq, istoc, iarchi, ' ',&
                    alarm, ifm, temps, nbexcl, typear,&
                    masse, zr(idepl1), zr(ivite1), zr( iacce1), fexte(1+neq),&
                    famor(1+neq), fliai(1+neq))
!
    endif
!
!====
! 5. LA FIN
!====
!
! --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
!
    if (etausr() .eq. 1) then
        call sigusr()
    endif
!
    if (istop .eq. 1) then
        call utexcm(28, 'DYNAMIQUE_10', 0, valk, 2,&
                    vali, 2, valr)
    endif
!
!     --- DESTRUCTION DES OBJETS DE TRAVAIL ---
!
    call jedetc('V', '.CODI', 20)
    call jedetc('V', '.MATE_CODE', 9)
!
    call jedema()
!
end subroutine

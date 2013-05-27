subroutine te0497(option, nomte)
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sebastien.meunier at edf.fr
! TOLE CRP_20
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DE L'ESTIMATEUR D'ERREUR EN RESIDU
!      SUR UN ELEMENT ISOPARAMETRIQUE 2D, VIA L'OPTION 'ERME_ELEM'
!      POUR LES MODELISATIONS HM SATUREES
!   -------------------------------------------------------------------
!
! REMARQUE : LES PROGRAMMES SUIVANTS DOIVENT RESTER TRES SIMILAIRES
!            TE0368, TE0375, TE0377, TE0378, TE0382, TE0497
!
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS :
!       03/07/06 (SM): CREATION EN S'INSPIRANT DE TE0003.F ET DE
!                      TE0377.F .
!                      CALCUL INDICATEURS EN STATIONNAIRE
!       01/05/07 (SM): ADIMENSIONNEMENT DES INDICATEURS EN
!                      STATIONNAIRE .
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterc/r8miem.h'
    include 'asterfort/assert.h'
    include 'asterfort/caethm.h'
    include 'asterfort/calnor.h'
    include 'asterfort/erhmb2.h'
    include 'asterfort/erhms2.h'
    include 'asterfort/erhmv2.h'
    include 'asterfort/fointe.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/resrot.h'
    include 'asterfort/tecach.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/uthk.h'
    include 'asterfort/utjac.h'
    character(len=16) :: option, nomte
!
! DECLARATION VARIABLES LOCALES
!
    integer :: ifm, niv, typvf
    integer :: ibid, iaux, iret, itab(7)
    integer :: igeom
    integer :: ierr, ivois
    integer :: ierrm
    integer :: imate
    integer :: ifovr, ifovf
    integer :: ipes, irot
    integer :: iref1, iref2
    integer :: ndim
    integer :: nno, nnos, npg, ipoids, ivf, idfde, jgano
    integer :: ipoid2, ivf2, idfde2
    integer :: nbcmp
    integer :: ipg
    integer :: ifa, tyv
    integer :: nbs, kpg, spt
    integer :: isienp, isienm, ideplp, ideplm, jkp, nbna
    integer :: iagd, iatyma, typ
    integer :: iacmp
    integer :: iade2, iava2, iaptm2, igd2, ncmpm2
    integer :: iade3, iava3, iaptm3, igd3, ncmpm3
    integer :: igrdca
    integer :: dimdep, dimdef, dimcon
    integer :: nmec, npi, np1, np2, nnom, nddls, nddlm
    integer :: mecani(5), press1(7), press2(7), tempe(5), dimuel
    integer :: adsip
    integer :: yamec, addeme, adcome, yate, addete
    integer :: yap1, addep1, adcp11
    integer :: yap2, addep2
    integer :: ii
!
    real(kind=8) :: ovfl
    real(kind=8) :: r8bid3(2)
    real(kind=8) :: valres(1)
    real(kind=8) :: orien, nx(3), ny(3), tx(3), ty(3), hf
    real(kind=8) :: fpx, fpy
    real(kind=8) :: frx(9), fry(9)
    real(kind=8) :: fovo(2)
    real(kind=8) :: instpm(2)
    real(kind=8) :: biot, rholiq, unsurk, unsurm, jaco(9)
    real(kind=8) :: hk, deltat, theta
    real(kind=8) :: cyoung, rhohom, permin, viscli, porosi, poisso
    real(kind=8) :: tm2h1v(3), tm2h1b(3), tm2h1s(3)
    real(kind=8) :: tsivom, tdevom, tsivoh, tsibom, tdebom, tsibsh, tsibbh
    real(kind=8) :: tsisam, tdesam, tsissh, tsisbh, denomi
    real(kind=8) :: longc, presc, admec, adhy0, adhy1, adv1h, adhymd
!
    logical :: laxi, perman, vf
!
    character(len=2) :: form, noeu
    character(len=3) :: modint
    character(len=4) :: nompar(1)
    character(len=8) :: typema, typmav
    character(len=8) :: typmod(2), fami, poum
!
    integer :: nbre1, nbre2, nbre3, nbre4
    parameter ( nbre1 = 2 , nbre2 = 2, nbre3 = 1 , nbre4 = 2 )
!
    integer :: nbr11, nbr12
    parameter ( nbr11 = 1 , nbr12 = 3 )
!
    real(kind=8) :: valre1(nbre1), valre2(nbre2), valre3(nbre3), valre4(nbre4)
    real(kind=8) :: valr11(nbr11), valr12(nbr12)
!
    integer :: codme1(nbre1), codme2(nbre2), codme3(nbre3), codme4(nbre4)
    integer :: codm11(nbr11), codm12(nbr12)
!
    character(len=8) :: nomre1(nbre1), nomre2(nbre2), nomre3(nbre3)
    character(len=8) :: nomre4(nbre4), nomr11(nbr11), nomr12(nbr12)
    character(len=8) :: valk(2)
!
    logical :: yapr, yaro
!
    data nomre1 / 'RHO','BIOT_COE' /
    data nomr11 / 'PERM_IN' /
    data nomr12 / 'PERMIN_X','PERMIN_Y','PERMIN_Z' /
    data nomre2 / 'RHO','VISC' /
    data nomre3 / 'PORO'       /
    data nomre4 / 'E', 'NU'    /
!
! ----------------------------------------------------------------------
    1000 format(a,' :',(6(1x,1pe17.10)))
! 2000 FORMAT(A,10I8)
! ----------------------------------------------------------------------
! 1 -------------- GESTION DES DONNEES ---------------------------------
! ----------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
! ------------------------------------------------------------------
!
    ovfl = r8miem()
!
! =====================================================================
! A. --- RECUPERATION D'INFORMATIONS SUR L'ELEMENT THM ----------------
! =====================================================================
    ibid = 0
    typvf = 0
    vf = .false.
    call caethm(nomte, laxi, perman, vf, typvf,&
                typmod, modint, mecani, press1, press2,&
                tempe, dimdep, dimdef, dimcon, nmec,&
                np1, np2, ndim, nno, nnos,&
                nnom, ibid, npi, npg, nddls,&
                nddlm, ibid, ibid, dimuel, ipoids,&
                ivf, idfde, ipoid2, ivf2, idfde2,&
                ibid, jgano)
!
! =====================================================================
! B. --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU ----------
! =====================================================================
    yamec = mecani(1)
    addeme = mecani(2)
    adcome = mecani(3)
    yap1 = press1(1)
    addep1 = press1(3)
    adcp11 = press1(4)
    yap2 = press2(1)
    addep2 = press2(3)
    yate = tempe(1)
    addete = tempe(2)
    adsip = adcp11 - adcome
!
! =====================================================================
! C. --- RECUPERATION DES DONNEES NECESSAIRES AU CALCUL ---------------
! =====================================================================
!--------------------------------------------------------------------
! 1. EVENTUELS PARAMETRES TEMPORELS
!--------------------------------------------------------------------
    call tecach('ONN', 'PTEMPSR', 'L', 1, itab,&
                iret)
    if (iret .eq. 0) then
        instpm(1) = zr(itab(1))
        if (.not.perman) then
            deltat = zr(itab(1)+1)
            theta = zr(itab(1)+2)
            instpm(2) = instpm(1)-deltat
        endif
    else
        call u2mess('F', 'INDICATEUR_11')
    endif
!--------------------------------------------------------------------
! 2. RECUPERATION DE LA GEOMETRIE, DU MATERIAU ET DES CHAMPS LOCAUX
!--------------------------------------------------------------------
!
! 2.1. GEOMETRIE (IGEOM), MATERIAU (IMATE) :
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
! 2.2. LES DEPLACEMENTS A L'INSTANT COURANT
!      1. TOUJOURS A L'INSTANT COURANT --> IDEPLP
!      2. SI TRANSITOIRE, A L'INSTANT PRECEDENT --> IDEPLM
!
    call jevech('PDEPLAR', 'L', ideplp)
!
    if (.not. perman) then
        call jevech('PDEPLMR', 'L', ideplm)
    else
        ideplm=1
    endif
!
! 2.3. CONTRAINTES AUX NOEUDS PAR ELEMENTS A L'INSTANT ACTUEL
!      1. TOUJOURS A L'INSTANT ACTUEL --> ISIENP
!      2. SI TRANSITOIRE, A L'INSTANT PRECEDENT --> ISIENM
!
    call tecach('ONN', 'PCONTNO', 'L', 3, itab,&
                iret)
    isienp = itab(1)
    nbcmp = itab(2)/nno
    if (.not. perman) then
        call tecach('ONN', 'PCONTNM', 'L', 3, itab,&
                    iret)
        isienm = itab(1)
    else
        isienm = 1
    endif
!
! 2.4. CARTES DE PESANTEUR ET ROTATION
!
    call tecach('ONN', 'PPESANR', 'L', 1, itab,&
                iret)
    if (itab(1) .ne. 0) then
        call jevech('PPESANR', 'L', ipes)
        yapr = .true.
    else
        yapr = .false.
    endif
    call tecach('ONN', 'PROTATR', 'L', 1, itab,&
                iret)
    if (itab(1) .ne. 0) then
        call jevech('PROTATR', 'L', irot)
        yaro = .true.
    else
        yaro = .false.
    endif
!
! 2.5. LES FORCES VOLUMIQUES EVENTUELLES :
!          VALEURS REELLES ?
    call tecach('ONN', 'PFRVOLU', 'L', 1, ifovr,&
                iret)
!          OU FONCTIONS ?
    if (ifovr .eq. 0) then
        call tecach('ONN', 'PFFVOLU', 'L', 1, ifovf,&
                    iret)
    else
        ifovf = 0
    endif
!GN      WRITE(IFM,2000) 'IFOVR', IFOVR
!GN      WRITE(IFM,2000) 'IFOVF', IFOVF
!--------------------------------------------------------------------
! 3. RECHERCHE DES VALEURS NECESSAIRES AU CALCUL DE L'INDICATEUR
!     . COEFFICIENT DE BIOT
!     . MASSE VOLUMIQUE HOMOGENEISEE RHOHOM
!     . MASSE VOLUMIQUE DU LIQUIDE RHOLIQ
!     . CONDUCTIVITE HYDRAULIQUE
!--------------------------------------------------------------------
    nompar(1) = 'INST'
    valres(1) = instpm(1)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THM_DIFFU', 1, nompar, valres,&
                nbre1, nomre1, valre1, codme1, 1)
!
    if (codme1(1) .eq. 0 .and. codme1(2) .eq. 0) then
        rhohom = valre1(1)
        biot = valre1(2)
    else
        call u2mesk('F', 'ELEMENTS4_78', 1, nomre1(1)//nomre1(2))
    endif
!
! ON RECUPERE LA PERMEABILITE INTRINSEQUE
!
! => PERMIN SI ISOTROPE
! => PERMIN_X,PERMIN_Y ET PERMIN_Z SINON
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THM_DIFFU', 1, nompar, valres,&
                nbr11, nomr11, valr11, codm11, 0)
!
    if (codm11(1) .eq. 0) then
        permin = valr11(1)
    else if (codm11(1).eq.1) then
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'THM_DIFFU', 1, nompar, valres,&
                    nbr12, nomr12, valr12, codm12, 0)
        if (( codm12(1).eq.0 ) .and. ( codm12(2).eq.0 ) .and. ( codm12(3) .eq.0 )) then
            permin = sqrt(valr12(1)**2+valr12(2)**2+valr12(3)**2)
        endif
    else
        call u2mesk('F', 'ELEMENTS4_78', 1, nomr11(1))
    endif
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THM_LIQU', 1, nompar, valres,&
                nbre2, nomre2, valre2, codme2, 1)
!
    if (( codme2(1).eq.0 ) .and. ( codme2(2).eq.0 )) then
        rholiq = valre2(1)
        viscli = valre2(2)
    else
        call u2mesk('F', 'ELEMENTS4_69', 1, nomre2(1)//nomre2(2))
    endif
!
    if (permin .gt. ovfl) then
        unsurk = viscli/permin
    else
        call u2mess('F', 'INDICATEUR_20')
    endif
!
!--------------------------------------------------------------------
! 4. SI INSTATIONNAIRE, ON RECUPERE DES COEFFICIENTS SUPPLEMENTAIRES
!     . MODULE DE BIOT
!     . MODULE DE YOUNG
!--------------------------------------------------------------------
!
    if (.not. perman) then
!
! 4.1. RECHERCHE DE LA POROSITE INITIALE
!
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'THM_INIT', 1, nompar, valres,&
                    nbre3, nomre3, valre3, codme3, 1)
!
        if (codme3(1) .eq. 0) then
            porosi = valre3(1)
        else
            call u2mesk('F', 'ELEMENTS4_70', 1, nomre3(1))
        endif
!
! 4.2. RECHERCHE DU COEFFICIENT DE POISSON ET DU MODULE DE YOUNG
!
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'ELAS', 1, nompar, valres,&
                    nbre4, nomre4, valre4, codme4, 1)
!
        if (( codme4(1).eq.0 ) .and. ( codme4(2).eq.0 )) then
            cyoung = valre4(1)
            poisso = valre4(2)
        else
            call u2mesk('F', 'ELEMENTS4_71', 1, nomre4(1)//nomre4(2))
        endif
!
! 4.4. ON CALCULE L'INVERSE DU MODULE DE BIOT
!
        if (cyoung .gt. ovfl) then
            unsurm = 3.d0*(biot-porosi)*(1.d0-biot)*(1.d0-2*poisso)/ cyoung
!
        else
            call u2mess('F', 'ELEMENTS4_67')
        endif
!
    endif
!
!--------------------------------------------------------------------
! 5. RECUPERATION DES GRANDEURS CARACTERISTIQUES
!--------------------------------------------------------------------
    call jevech('PGRDCA', 'L', igrdca)
    longc = zr(igrdca)
    presc = zr(igrdca+1)
!
    iaux = 0
    if (presc .le. ovfl) then
        iaux = 1
        valk(1) = 'pression'
    else if (longc.le.ovfl) then
        iaux = 1
        valk(1) = 'longueur'
    endif
!
    if (iaux .ne. 0) then
        call u2mesk('F', 'INDICATEUR_21', 1, valk)
    endif
!
! =====================================================================
! D. --- CALCUL DES INDICATEURS ---------------------------------------
! =====================================================================
!
!------------------------------------------------------------------
! 1. -------------- CALCUL DU PREMIER TERME DE L'ERREUR -----------
!------------------------------------------------------------------
!
! 2.1. --- CALCUL DU DIAMETRE HK DE LA MAILLE ----
!
    call uthk(nomte, zr(igeom), hk, ndim, itab,&
              ibid, ibid, ibid, niv, ifm)
!
! 2.2. --- CALCUL DE LA FORCE DE PESANTEUR ---
!
    if (yapr) then
        fpx = rhohom * zr(ipes) * zr(ipes+1)
        fpy = rhohom * zr(ipes) * zr(ipes+2)
    else
        fpx = 0.d0
        fpy = 0.d0
    endif
!GN      WRITE(IFM,1000) 'P',FPX,FPY,FPZ
!
! 2.3. --- CALCUL DE LA FORCE DE ROTATION ---
!
    if (yaro) then
        call resrot(zr(irot), zr(igeom), zr(ivf), rhohom, nno,&
                    npg, frx, fry)
    else
!
        do 52 , ipg = 1 , npg
        frx(ipg) = 0.d0
        fry(ipg) = 0.d0
52      continue
!
    endif
!
! 2.4. --- CALCUL DE LA FORCE VOLUMIQUE EVENTUELLE ---
!
    if (ifovr .ne. 0) then
        fovo(1) = zr(ifovr )
        fovo(2) = zr(ifovr+1)
!
    else if (ifovf.ne.0) then
        nompar(1) = 'INST'
        r8bid3(1) = instpm(1)
!       SI UNE COMPOSANTE N'A PAS ETE DECRITE, ASTER AURA MIS PAR
!       DEFAUT LA FONCTION NULLE &FOZERO. ON LE REPERE POUR
!       IMPOSER LA VALEUR 0 SANS FAIRE DE CALCULS INUTILES
        do 24 , ibid = 1 , ndim
        if (zk8(ifovf+ibid-1)(1:7) .eq. '&FOZERO') then
            fovo(ibid) = 0.d0
        else
            call fointe('FM', zk8(ifovf+ibid-1), 1, nompar, r8bid3,&
                        fovo(ibid), iret)
        endif
24      continue
!GN        WRITE(IFM,*) 'F X : ',ZK8(IFOVF),FOVO(1)
!GN        WRITE(IFM,*) 'F Y : ',ZK8(IFOVF+1),FOVO(2)
!
    else
        fovo(1) = 0.d0
        fovo(2) = 0.d0
    endif
!
! 2.3. --- TERME VOLUMIQUE ---
!
    call erhmv2(laxi, perman, deltat, dimdep, dimdef,&
                nmec, np1, np2, ndim, nno,&
                nnos, nnom, npg, nddls, nddlm,&
                dimuel, ipoids, ivf, idfde, ipoid2,&
                ivf2, idfde2, zr(igeom), fovo, zr(ideplp),&
                zr(ideplm), zr(isienp), zr(isienm), nbcmp, biot,&
                unsurm, fpx, fpy, frx, fry,&
                yamec, addeme, yap1, addep1, yap2,&
                addep2, yate, addete, tm2h1v)
!
! ON ADIMENSIONNE LES INDICATEURS VOLUMIQUES
!
    admec = 1.d0/(presc**2*longc**ndim)
    tsivom = hk**2 * admec * tm2h1v(1)
!
    if (.not. perman) then
!
        tdevom = hk**2 * admec * tm2h1v(2)
!
        adv1h = cyoung*unsurk*admec
        tsivoh = deltat * hk**2 * adv1h * tm2h1v(3)
!
    endif
!
!------------------------------------------------------------------
! 2. CALCUL DES TERMES SURFACIQUES
!------------------------------------------------------------------
! 2.1. PHASE DE PREPARATION : ON RECUPERE LES ADRESSES NECESSAIRES
!                             AUX CALCULS
! -----------------------------------------------------------------
!
! ON RECUPERE LES ADRESSES
!    1. VOISINS
!    2. CHARGEMENTS DE TYPE FORCE_FACE
!    3. CHARGEMENTS DE TYPE PRES_REP
    call jevech('PVOISIN', 'L', ivois)
    call jevech('PFORCE', 'L', iref1)
    call jevech('PPRESS', 'L', iref2)
!
! RECHERCHE DES ADRESSES POUR LES CHARGES SUR LES SEGMENTS
!
    iagd = zi(iref1+4)
    iacmp = zi(iref1+5)
!
    iade2 = zi(iref2+4)
    iava2 = zi(iref2+5)
    iaptm2 = zi(iref2+6)
    if (iade2 .ne. 0) then
        igd2 = zi(iade2)
        ncmpm2 = zi(iacmp-1+igd2)
    endif
!
    iade3 = zi(iref2+8)
    iava3 = zi(iref2+9)
    iaptm3 = zi(iref2+10)
    if (iade3 .ne. 0) then
        igd3 = zi(iade3)
        ncmpm3 = zi(iacmp-1+igd3)
    endif
!
!------------------------------------------------------------------
! 2.2. CARACTERISATIONS DE LA MAILLE COURANTE
! -----------------------------------------------------------------
!
! TYPE DE LA MAILLE COURANTE
!
    typ = zi(ivois+7)
!
! ADRESSE DU VECTEUR TYPE MAILLE
!
    iatyma = zi(iref1+3)
    typema = zk8(iatyma-1+typ)
    form = typema(1:2)
!
! NOMBRE DE NOEUDS SOMMETS ET NOMBRE DE NOEUDS DES ARETES
!
    if (form .eq. 'TR') then
        nbs = 3
    else
        nbs = 4
    endif
!
    noeu = typema(5:5)
!
! EN THM, ON EST TOUJOURS SUR DES MAILLAGES QUADRATIQUES.
! (TRIA6 OU QUAD8 EN 2D)
! ON A DONC FORCEMENT NBNA = 3
!
    if (noeu .eq. '6' .or. noeu .eq. '8') then
        nbna = 3
    else
        call assert(.false.)
    endif
!
! CALCUL DE L'ORIENTATION DE LA MAILLE 2D
!     REMARQUE : ON APPELLE LE PROGRAMME GENERIQUE POUR LE PREMIER POINT
!                DE GAUSS, SACHANT QUE L'ORIENTATION NE DOIT PAS CHANGER
!
    jkp = 1
    call utjac(.true., zr(igeom), jkp, idfde, 0,&
               ibid, nno, orien)
!
!------------------------------------------------------------------
! 2.3. CALCUL DES TERMES LIES AUX ARETES
!------------------------------------------------------------------
! ON INITIALISE LES TERMES DE SAUT ET LES TERMES PROVENANT DES
! CONDITIONS AUX LIMITES (HYDRAULIQUE + MECANIQUE)
!
    do 11 , ii = 1 , 3
!
    tm2h1b(ii) = 0.d0
    tm2h1s(ii) = 0.d0
!
    11 end do
!
! BOUCLE SUR LES ARETES : IMPLICITEMENT, ON NUMEROTE LOCALEMENT LES
!                         ARETES COMME LES NOEUDS SOMMETS
! . DONC LE PREMIER NOEUD DE L'ARETE A LE MEME NUMERO QUE L'ARETE : IFA
! . LE NOEUD SUIVANT EST IFA+1, SAUF SI ON EST SUR LA DERNIERE ARETE ;
!   LE NOEUD SUIVANT EST ALORS LE PREMIER, 1.
! . L'EVENTUEL NOEUD MILIEU EST LE 1ER NOEUD, DECALE DU NOMBRE DE NOEUDS
!   SOMMETS : IFA + NBS
!
    do 20 , ifa = 1,nbs
!
! ------TEST DU TYPE DE VOISIN -----------------------------------------
!
    tyv=zi(ivois+7+ifa)
!
    if (tyv .ne. 0) then
!
! ------- RECUPERATION DU TYPE DE LA MAILLE VOISINE
!
        call jenuno(jexnum('&CATA.TM.NOMTM', tyv), typmav)
!
! --- CALCUL DES NORMALES, TANGENTES ET JACOBIENS AUX POINTS DE L'ARETE
!
        iaux = ifa
        call calnor('2D', zr(igeom), iaux, nbs, nbna,&
                    orien, ibid, ibid, itab, ibid,&
                    ibid, ibid, jaco, nx, ny,&
                    r8bid3, tx, ty, hf)
!
! ------- SI L'ARRETE N'EST PAS SUR LA FRONTIERE DE LA STRUCTURE...
!         CALCUL DES TERMES DE SAUT A TRAVERS LES FACES INTERIEURES
!         DE LA MAILLE
!
        if (typmav(1:4) .eq. 'TRIA' .or. typmav(1:4) .eq. 'QUAD') then
!
            call erhms2(perman, ifa, nbs, theta, jaco,&
                        nx, ny, zr(isienp), adsip, zr(isienm),&
                        nbcmp, typmav, zi(iref1), zi(iref2), ivois,&
                        tm2h1s)
!
! ------- SI L'ARRETE EST SUR LA FRONTIERE DE LA STRUCTURE...
!         CALCUL DES TERMES DE VERIFICATION DES CONDITIONS DE BORD
!
        else if (typmav(1:2).eq.'SE') then
!
            call erhmb2(perman, ifa, nbs, ndim, theta,&
                        instpm, jaco, nx, ny, tx,&
                        ty, nbcmp, zr(igeom), ivois, zr(isienp),&
                        zr(isienm), adsip, iagd, zi(iref2), iade2,&
                        iava2, ncmpm2, iaptm2, iade3, iava3,&
                        ncmpm3, iaptm3, tm2h1b)
! ----------------------------------------------------------------
!
! ----------------------------------------------------------------------
! --------------- CURIEUX ----------------------------------------------
! ----------------------------------------------------------------------
!
        else
!
            valk(1)=typmav(1:4)
            call u2mesk('F', 'INDICATEUR_10', 1, valk)
!
        endif
!
        if (niv .ge. 2) then
            write(ifm,1003) ifa, zi(ivois+ifa), typmav
            1003 format (i2,'-EME FACE DE NUMERO',i10,' ==> TYPMAV = ', a)
            write(ifm,1000) 'TM2H1B', tm2h1b
            write(ifm,1000) 'TM2H1S', tm2h1s
        endif
!
    endif
!
    20 end do
!
! =====================================================================
! E. --- MISE EN FORME DES INDICATEURS --------------------------------
! =====================================================================
!
! ON ADIMENSIONNE LES INDICATEURS
!
    denomi = presc**2*longc**(ndim-2)*rholiq**2
    adhy0 = unsurk**2/denomi
    adhy1 = adhy0/(longc**2)
!
    tsisam = hk * admec * tm2h1s(1)
    tsibom = hk * admec * tm2h1b(1)
!
    if (perman) then
!
        tsibsh = hk * adhy0 * tm2h1b(3)
        tsibbh = hk**3 * adhy1 * tm2h1b(3)
!
        tsissh = hk * adhy0 * tm2h1s(3)
        tsisbh = hk**3 * adhy1 * tm2h1s(3)
!
    else
!
        call jevech('PERREM', 'L', ierrm)
!
        tdebom = hk * admec * tm2h1b(2)
        tdesam = hk * admec * tm2h1s(2)
!
        adhymd = deltat * hk * cyoung * permin/viscli * adhy1
        tsibsh = adhymd * tm2h1b(3)
        tsissh = adhymd * tm2h1s(3)
!
    endif
!
! ON STOCKE LES INDICATEURS
!
    call jevech('PERREUR', 'E', ierr)
!
    if (perman) then
!
        zr(ierr ) = sqrt(tsivom + tsibom + tsisam) + sqrt(tsibsh + tsissh)
        zr(ierr+1) = sqrt(tsivom + tsibom + tsisam) + sqrt(tsibbh + tsisbh)
        zr(ierr+2) = tsibsh + tsissh
        zr(ierr+3) = tsivom + tsibom + tsisam
        zr(ierr+4) = tsibbh + tsisbh
!
    else
!
        zr(ierr+1) = tsivom + tsisam + tsibom
        zr(ierr+2) = tdevom + tdebom + tdesam
        zr(ierr+3) = tsivoh + tsibsh + tsissh
!
        zr(ierr+4) = max(zr(ierrm+4),sqrt(zr(ierr+1)))
        zr(ierr+5) = zr(ierrm+5) + sqrt(zr(ierr+2))
        zr(ierr+6) = sqrt(zr(ierrm+6)**2+zr(ierr+3))
        zr(ierr) = zr(ierr+4)+zr(ierr+5)+zr(ierr+6)
!
    endif
!
    call jedema()
!
end subroutine

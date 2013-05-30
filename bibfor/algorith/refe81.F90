subroutine refe81(nomres, basmod, raidf, massf, amorf,&
                  mailla)
    implicit none
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
!***********************************************************************
!  P. RICHARD     DATE 13/07/90
!-----------------------------------------------------------------------
!  BUT : < CREATION DU REFE ET DU DESC POUR OP0081 >
!
!        - RECUPERER LES NOMS UTILISATEUR DES CONCEPTS ASSOCIES AUX
!          MATRICES ASSEMBLEES ET BASE MODALE CONSIDEREES
!        - EFFECTUER QUELQUES CONTROLES ET DETERMINER
!          OPTION DE CALCUL MATRICES PROJETEES
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM UTILISATEUR DU RESULTAT
! BASMOD /O/ : NOM UT DE LA BASE MODALE DE PROJECTION
! RAIDF  /O/ : NOM UT DE LA MATRICE RAIDEUR A PROJETER
! MASSEF /O/ : NOM UT DE LA MATRICE DE MASSE A PROJETER
! AMORF  /O/ : NOM UT DE LA MATRICE D'AMORTISSEMENT A PROJETER
! MAILLA /O/ : NOM UT DU MAILLAGE EN AMONT
!
!
!
!
    include 'jeveux.h'
!
    include 'asterc/getvid.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: valk(2), typbas
    character(len=8) :: nomres, mailla, basmod, maillb, amor, blanc, lintf
    character(len=8) :: k8bid
    character(len=14) :: numddl, numbis, numter
    character(len=19) :: raid, raidb, mass, massb, massf, raidf, amorf, amorb
    integer :: iarg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadref, ibid, ioc, iret, l, lddesc, lldesc
    integer :: llref, llrefb, nbval
!-----------------------------------------------------------------------
    data blanc         /'        '/
!-----------------------------------------------------------------------
!
! --- RECUPERATION EVENTUELLE MATRICE RAIDEUR EN ARGUMENT
!
    call jemarq()
    call getvid(blanc, 'MATR_RIGI', 1, iarg, 0,&
                k8bid, ioc)
    ioc=-ioc
    if (ioc .eq. 0) then
        raid=blanc
    else if (ioc.eq.1) then
        call getvid(blanc, 'MATR_RIGI', 1, iarg, 1,&
                    raid, l)
    else
        call u2mesg('F', 'ALGORITH14_14', 0, ' ', 0,&
                    0, 0, 0.d0)
    endif
!
! --- RECUPERATION EVENTUELLE MATRICE MASSE EN ARGUMENT
!
    call getvid(blanc, 'MATR_MASS', 1, iarg, 0,&
                k8bid, ioc)
    ioc=-ioc
    if (ioc .eq. 0) then
        mass=blanc
    else if (ioc.eq.1) then
        call getvid(blanc, 'MATR_MASS', 1, iarg, 1,&
                    mass, l)
    else
        call u2mesg('F', 'ALGORITH14_15', 0, ' ', 0,&
                    0, 0, 0.d0)
    endif
!
! --- RECUPERATION EVENTUELLE MATRICE AMORTISSEMENT EN ARGUMENT
!
    call getvid(blanc, 'MATR_AMOR', 1, iarg, 0,&
                k8bid, ioc)
    ioc=-ioc
    if (ioc .eq. 0) then
        amor=blanc
    else if (ioc.eq.1) then
        call getvid(blanc, 'MATR_AMOR', 1, iarg, 1,&
                    amor, l)
    else
        call u2mesg('F', 'ALGORITH14_16', 0, ' ', 0,&
                    0, 0, 0.d0)
    endif
!
! --- RECUPERATION BASE MODALE OBLIGATOIRE
!
    raidb=blanc
    massb=blanc
    amorb=blanc
    call getvid(blanc, 'BASE_MODALE', 1, iarg, 1,&
                basmod, nbval)
    call jeveuo(basmod//'           .REFD', 'L', llrefb)
    raidb=zk24(llrefb)(1:8)
    massb=zk24(llrefb+1)(1:8)
    amorb=zk24(llrefb+2)(1:8)
    numddl=zk24(llrefb+3)(1:14)
    lintf=zk24(llrefb+4)(1:8)
    typbas=zk24(llrefb+6)
!
!
! --- RECUPERATION MAILLAGE
!
    if (lintf .ne. blanc) then
        call jeveuo(lintf//'.IDC_REFE', 'L', llref)
        mailla=zk24(llref)
    else
        call dismoi('F', 'NOM_MAILLA', numddl, 'NUME_DDL', ibid,&
                    mailla, iret)
    endif
!
! --- RECUPERATION DU TYPE DE BASE MODALE
!
! --- CAS DE LA DONNEE DES MATRICE DE LA BASE
!
    if (typbas(1:4) .ne. 'RITZ') then
        if (mass .eq. massb) mass=blanc
        if (raid .eq. raidb) raid=blanc
        if (amor .eq. amorb) amor=blanc
    endif
!
!
! --- CAS BASE MODALE CLASSIQUE
!
    if ((typbas(1:9).eq.'CLASSIQUE') .or. (typbas(1:9).eq.'DIAG_MASS') .or.&
        (typbas(1:9).eq.'         ')) then
!
        if (mass .ne. blanc) then
            massf=mass
        else
            massf=massb
        endif
        if (raid .ne. blanc) then
            raidf=raid
        else
            raidf=raidb
        endif
        if (amor .ne. blanc) then
            amorf=amor
        else
            amorf=amorb
        endif
    endif
!
! --- CAS BASE MODALE RITZ
    if ((typbas(1:4).eq.'RITZ')) then
!
! --- TRAITEMENT DU NOM FINAL MASSE ET RAIDEUR
!
        massf=mass
        raidf=raid
        amorf=amor
    endif
!
! --- TRAITEMENT COHERENCE MATRICE ASSEMBLEES
!     SI MASSF ET RAIDF NON BLANC
!
!
    if (raidf .eq. blanc .or. massf .eq. blanc) goto 10
!
    call dismoi('F', 'NOM_NUME_DDL', raidf, 'MATR_ASSE', ibid,&
                numddl, iret)
!
    call dismoi('F', 'NOM_MAILLA', numddl, 'NUME_DDL', ibid,&
                maillb, iret)
!
    call dismoi('F', 'NOM_NUME_DDL', massf, 'MATR_ASSE', ibid,&
                numbis, iret)
!
    if (amorf .ne. blanc) then
        call dismoi('F', 'NOM_NUME_DDL', amorf, 'MATR_ASSE', ibid,&
                    numter, iret)
    endif
!
! --- CONTROLE DE LA COHERENCE DES MATRICES ASSEMBLEES
!
    if (numddl .ne. numbis) then
        valk (1) = mass
        valk (2) = raid
        call u2mesg('F', 'ALGORITH14_21', 2, valk, 0,&
                    0, 0, 0.d0)
    endif
!
    if (amor .ne. blanc) then
        if (numddl .ne. numter) then
            valk (1) = amor
            valk (2) = raid
            call u2mesg('F', 'ALGORITH14_22', 2, valk, 0,&
                        0, 0, 0.d0)
        endif
    endif
!
    if (mailla .ne. maillb) then
        valk (1) = maillb
        valk (2) = mailla
        call u2mesg('F', 'ALGORITH14_23', 2, valk, 0,&
                    0, 0, 0.d0)
    endif
!
10  continue
!
! --- REMPLISSAGE DU .REFE
!
    call wkvect(nomres//'.MAEL_REFE', 'G V K24', 2, iadref)
    zk24(iadref)=basmod
    zk24(iadref+1)=mailla
!
! --- REMPLISSAGE DU .DESC
!
    call wkvect(nomres//'.MAEL_DESC', 'G V I', 3, lddesc)
    if (lintf .ne. blanc) then
        call jeveuo(lintf//'.IDC_DESC', 'L', lldesc)
        zi(lddesc)=zi(lldesc+1)
        zi(lddesc+1)=zi(lldesc+2)
        zi(lddesc+2)=zi(lldesc+3)
    endif
!
    call jedema()
end subroutine

subroutine preres(solvez, base, iret, matpre, matass,&
                  npvneg, istop)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! BUT : FACTORISER UNE MATR_ASSE (LDLT/MULT_FRONT/MUMPS/FETI)
!       OU FABRIQUER UNE MATRICE DE PRECONDITIONNEMENT (GCPC)
!
! SOLVEZ (K19) IN : OBJET SOLVEUR (OU ' ')
! BASE (K1)    IN : BASE SUR LAQUELLE ON CREE LA MATRICE FACTORISEE
!                  (OU LA MATRICE DE PRECONDITIONNEMENT)
! IRET (I)     OUT : CODE_RETOUR :
!             /0 -> OK (PAR DEFAUT AVEC GCPC/PETSC)
!             /2 -> LA FACTORISATION N'A PAS PU SE FAIRE
!                   JUSQU'AU BOUT.
!             /1 -> LA FACTORISATION EST ALLEE AU BOUT
!                   MAIS ON A PERDU BEAUCOUP DE DECIMALES
!             /3 -> LA FACTORISATION EST ALLEE AU BOUT
!                   MAIS ON NE SAIT PAS DIRE SI ON A PERDU DES DECIMALES
!                   (CAS DE FETI OU DE MUMPS SI NPREC<0
!                    ET SI LA FACTO EST OK)
! MATPRE(K19) IN/JXVAR : MATRICE DE PRECONDITIONNEMENT (SI GCPC)
! MATASS(K19) IN/JXVAR : MATRICE A FACTORISER OU A PRECONDITIONNER
! NPVNEG (I) OUT : NBRE DE TERMES DIAGONAUX NEGATIFS DE LA FACTORISEE
!          CE NBRE N'EST LICITE QUE SI LA MATRICE EST REELLE SYMETRIQUE
!          ET N'EST FOURNI QUE PAR UN SOLVEUR DIRECT: LDLT, MF OU MUMPS
! ISTOP (I)  IN: COMPORTEMENT EN CAS DE DETECTION DE SINGULARITE. CE
!                PARAMETRE N'A D'UTILITE QU'AVEC UN SOLVEUR DIRECT
!                  /0 -> SI IRET>0 : ERREUR <F>
!                  /1 -> SI IRET=1 : ALARME <A>
!                        SI IRET=2 : ERREUR <F>
!                  /2 -> LE PROGRAMME NE S'ARRETE PAS
!                        ET N'IMPRIME AUCUN MESSAGE.
!                 /-9999 -> ON PREND LA VALEUR PREVUE DS LA SD_SOLVEUR
!                        POUR STOP_SINGULIER (VALEUR 0 OU 1 SEULEMENT)
!                 /AUTRE --> CALL ASSERT
!-----------------------------------------------------------------------
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterc/cheksd.h'
    include 'asterfort/alfeti.h'
    include 'asterfort/apetsc.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/fetfac.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedbg2.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/pcldlt.h'
    include 'asterfort/pcmump.h'
    include 'asterfort/sdmpic.h'
    include 'asterfort/tldlg3.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/uttcpr.h'
    include 'asterfort/uttcpu.h'
    integer :: npvneg, istop, iret
    character(len=1) :: base
    character(len=*) :: matass, matpre, solvez
!
    integer :: nbsd, idd, idd0, ifetm, ifets, idbgav, ifm, niv, islvk, ibid
    integer :: islvi, lmat, idime, nprec, ndeci, isingu, niremp, nbsdf, iinf
    integer :: ifcpu, ilimpi, istopz, iretgc
    real(kind=8) :: temps(6), rbid
    character(len=24) :: metres, sdfeti, infofe, k24b, opt, precon
    character(len=19) :: matas, maprec, solvsd, matas1, k19b, solveu
    character(len=8) :: renum, kmpic, kmatd
    logical :: lfeti, iddok, lfetic, dbg
!
!----------------------------------------------------------------------
    call jemarq()
    call jedbg2(idbgav, 0)
    call infniv(ifm, niv)
    call uttcpu('CPU.RESO.1', 'DEBUT', ' ')
    call uttcpu('CPU.RESO.4', 'DEBUT', ' ')
!
!     COHERENCE DES VALEURS DE ISTOPZ (NIVEAU DEVELOPPEUR)
    istopz=istop
    if ((istopz.ne.0) .and. (istopz.ne.1) .and. (istopz.ne.2) .and. (istopz.ne.-9999)) &
    call assert(.false.)
    dbg=.true.
    dbg=.false.
!
    matas1=matass
    matas = matass
    maprec = matpre
    npvneg=-9999
!
    solveu=solvez
    if (solveu .eq. ' ') call dismoi('F', 'SOLVEUR', matas, 'MATR_ASSE', ibid,&
                                     solveu, ibid)
    call jeveuo(solveu//'.SLVK', 'L', islvk)
    metres = zk24(islvk)
!
!     -- FETI OR NOT FETI :
    lfeti= (metres(1:4).eq.'FETI')
!
    if (dbg .and. (.not. lfeti)) then
        call cheksd(matas, 'SD_MATR_ASSE', ibid)
        call cheksd(solveu, 'SD_SOLVEUR', ibid)
    endif
!
    call dismoi('F', 'MPI_COMPLET', matas, 'MATR_ASSE', ibid,&
                kmpic, ibid)
    call dismoi('F', 'MATR_DISTR', matas, 'MATR_ASSE', ibid,&
                kmatd, ibid)
    if (kmpic .eq. 'NON') then
        if (metres .eq. 'FETI' .or. metres .eq. 'MUMPS' .or.&
            ( metres.eq.'PETSC'.and.kmatd.eq.'OUI')) then
!          -- SI FETI OU MUMPS, ON PEUT CONTINUER AVEC UNE
!             MATRICE INCOMPLETE :
        else
!          -- SINON, IL FAUT COMPLETER AVANT DE POURSUIVRE
            call sdmpic('MATR_ASSE', matas)
        endif
    endif
!
!
!
!     -- CAS DU SOLVEUR FETI :
!     --------------------------
    lfetic=.false.
    if (lfeti) then
        sdfeti=zk24(islvk+5)
        call jeveuo('&FETI.FINF', 'L', iinf)
        infofe=zk24(iinf)
        if (infofe(11:11) .eq. 'T') lfetic=.true.
        call jeveuo(sdfeti(1:19)//'.FDIM', 'L', idime)
!       NOMBRE DE SOUS-DOMAINES
        nbsd=zi(idime)
        nbsdf=0
        idd0=1
!       ADRESSE JEVEUX DE LA LISTE DES MATR_ASSE ET DE SOLVEURS
!       ASSOCIES AUX SOUS-DOMAINES
        call jeveuo(matas//'.FETM', 'L', ifetm)
        call jeveuo(solveu//'.FETS', 'L', ifets)
        call jeveuo('&FETI.INFO.CPU.FACN', 'E', ifcpu)
!       ADRESSE JEVEUX OBJET FETI & MPI
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
!       NETTOYAGE DES SD FETI SI NECESSAIRE (SUCCESSION DE CALCULS
!       DECOUPLES) ET INITIALISATION NUMERO D'INCREMENT
        opt='NETTOYAGE_SDI'
        call alfeti(opt, k19b, k19b, k19b, k19b,&
                    ibid, rbid, k24b, rbid, ibid,&
                    k24b, k24b, k24b, k24b, ibid,&
                    k24b, k24b, ibid)
    else
        nbsd=0
        idd0=0
        infofe='FFFFFFFFFFFFFFFFFFFFFFFF'
    endif
!
!
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
! IDD=0 --> DOMAINE GLOBAL/ IDD=I --> IEME SOUS-DOMAINE
    do 100 idd = idd0, nbsd
!
! TRAVAIL PREALABLE POUR DETERMINER SI ON EFFECTUE LA BOUCLE SUIVANT
! LE SOLVEUR (FETI OU NON), LE TYPE DE RESOLUTION (PARALLELE OU
! SEQUENTIELLE) ET L'ADEQUATION "RANG DU PROCESSEUR-NUMERO DU SD"
! ATTENTION SI FETI LIBERATION MEMOIRE PREVUE EN FIN DE BOUCLE
        if (.not. lfeti) then
            iddok=.true.
        else
            if (zi(ilimpi+idd) .eq. 1) then
                iddok=.true.
            else
                iddok=.false.
            endif
        endif
!
        if (iddok) then
            if (lfeti) call jemarq()
            if (lfetic) then
                call uttcpu('CPU.PRERES.FETI', 'INIT', ' ')
                call uttcpu('CPU.PRERES.FETI', 'DEBUT', ' ')
            endif
!
            if (idd .gt. 0) then
!           MATR_ASSE ASSOCIEE A CHAQUE SOUS-DOMAINE
                matas=zk24(ifetm+idd-1)(1:19)
                solvsd=zk24(ifets+idd-1)(1:19)
                call jeveuo(solvsd//'.SLVK', 'L', islvk)
                metres = zk24(islvk)
                call jeveuo(solvsd//'.SLVI', 'L', islvi)
            else
                call jeveuo(solveu//'.SLVI', 'L', islvi)
            endif
!
!         ALLOCATION OBJET JEVEUX TEMPORAIRE .&INT/&IN2
            call mtdscr(matas)
            call jeveuo(matas//'.&INT', 'E', lmat)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!             MULTIFRONTALE OU LDLT OU MUMPS               C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            if (metres .eq. 'LDLT' .or. metres .eq. 'MULT_FRONT' .or. metres .eq. 'MUMPS') then
                if (lfeti .and. (metres.eq.'LDLT')) call u2mess('F', 'ALGELINE3_27')
                if (lfeti .and. (metres.eq.'MUMPS')) call u2mess('F', 'ALGELINE3_28')
                nprec = zi(islvi-1+1)
                if (istopz .eq. -9999) istopz = zi(islvi-1+3)
                if (lfeti) then
                    call fetfac(lmat, matas, idd, nprec, nbsd,&
                                matas1, sdfeti, nbsdf, base, infofe)
                    iret=3
                else
                    renum=' '
                    if (metres(1:10) .eq. 'MULT_FRONT') renum=zk24( islvk-1+4)
                    if ((metres(1:5).eq.'MUMPS') .and. (istopz.eq.2) .and. (nprec.lt.0)) &
                    call u2mess('F', 'ALGELINE5_74')
                    call tldlg3(metres, renum, istopz, lmat, 1,&
                                0, nprec, ndeci, isingu, npvneg,&
                                iret, solveu)
                    if ((nprec.lt.0) .and. (iret.ne.2)) iret=3
!
                endif
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                         PETSC                            C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            else if (metres.eq.'PETSC') then
                if (lfeti) call u2mess('F', 'ALGELINE4_2')
                call apetsc('DETR_MAT', ' ', matas, rbid, ' ',&
                            0, ibid, iret)
                call apetsc('PRERES', solveu, matas, rbid, ' ',&
                            0, ibid, iret)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                         GCPC                             C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            else if (metres.eq.'GCPC') then
                if (lfeti) call u2mess('F', 'ALGELINE3_29')
!
                call jeveuo(solveu//'.SLVK', 'L', islvk)
                call jeveuo(solveu//'.SLVI', 'E', islvi)
                precon=zk24(islvk-1+2)
!
                if (precon .eq. 'LDLT_INC') then
                    niremp = zi(islvi-1+4)
                    call pcldlt(maprec, matas, niremp, base)
                else if (precon.eq.'LDLT_SP') then
                    call pcmump(matas, solveu, iretgc)
                    if (iretgc .ne. 0) then
                        call u2mess('F', 'ALGELINE5_76')
                    endif
                endif
                iret=0
            endif
!
            if (lfetic) then
                call uttcpu('CPU.PRERES.FETI', 'FIN', ' ')
                call uttcpr('CPU.PRERES.FETI', 6, temps)
                zr(ifcpu+idd)=temps(5)+temps(6)
            endif
            if (lfeti) call jedema()
!
!========================================
! FIN BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
        endif
100  end do
!
    call jedbg2(ibid, idbgav)
    call uttcpu('CPU.RESO.1', 'FIN', ' ')
    call uttcpu('CPU.RESO.4', 'FIN', ' ')
    call jedema()
end subroutine

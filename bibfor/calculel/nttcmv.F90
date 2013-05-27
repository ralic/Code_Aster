subroutine nttcmv(modele, mate, carele, fomult, charge,&
                  infcha, infoch, numedd, solveu, time,&
                  chlapm, tpsthe, tpsnp1, reasvt, reasmt,&
                  creas, vtemp, vtempm, vec2nd, matass,&
                  maprec, cndirp, cnchci, cnchtp)
!
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
! TOLE CRP_21
!
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/asasve.h'
    include 'asterfort/ascavc.h'
    include 'asterfort/ascova.h'
    include 'asterfort/asmatr.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mecact.h'
    include 'asterfort/medith.h'
    include 'asterfort/mertth.h'
    include 'asterfort/metnth.h'
    include 'asterfort/preres.h'
    include 'asterfort/vechth.h'
    include 'asterfort/vedith.h'
    logical :: reasvt, reasmt
    real(kind=8) :: tpsthe(6), tpsnp1
    character(len=1) :: creas
    character(len=19) :: infcha, solveu, maprec
    character(len=24) :: modele, mate, carele, fomult, charge, infoch
    character(len=24) :: numedd, time, timemo
    character(len=24) :: vtemp, vtempm, vec2nd, chlapm
    character(len=24) :: matass, cndirp, cnchci, cnchtp
!
! ----------------------------------------------------------------------
!
! COMMANDE THER_MOBI_NLINE : ACTUALISATION
!   - DES VECTEURS CONTRIBUANT AU SECOND MEMBRE
!   - DE LA MATRICE ASSEMBLEE (EVENTUELLEMENT)
!
!
!
    integer :: ibid, k, iret, ierr, nbmat, jmet
    integer :: jmer, jmed, j2nd, jdirp, jchtp, lonch
    complex(kind=8) :: cbid
    character(len=1) :: typres
    character(len=8) :: k8bid, nomcmp(6)
    character(len=24) :: ligrmo, merigi, mediri, tlimat(3)
    character(len=24) :: vediri, vechtp, vadirp, vachtp, metrnl
!
    data typres /'R'/
    data nomcmp /'INST    ','DELTAT  ','THETA   ','KHI     ',&
     &             'R       ','RHO     '/
    data merigi        /'&&METRIG           .RELR'/
    data mediri        /'&&METDIR           .RELR'/
    data metrnl        /'&&METNTH           .RELR'/
    data vediri        /'&&VETDIR           .RELR'/
    data vechtp        /'&&VETCHA           .RELR'/
    data timemo        /'&&OP0171.TIMEMO'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    vadirp = '&&VATDIR'
    vachtp = '&&VATCHA'
!
    creas = ' '
!
! ======================================================================
!         VECTEURS (CHARGEMENTS) CONTRIBUANT AU SECOND MEMBRE
! ======================================================================
!
    if (reasvt) then
!
! --- (RE)ACTUALISATION DU CHAMP CONSTANT EN ESPACE : TIME
!
        ligrmo = modele(1:8)//'.MODELE'
        call mecact('V', time, 'MODELE', ligrmo, 'INST_R',&
                    6, nomcmp, ibid, tpsthe, cbid,&
                    k8bid)
!
!       ON CREE CETTE CARTE IDENTIQUE A TIME MAIS AVEC 1-THETA=1
!       A LA PLACE DE THETA POUR PERMETTRE LE CALCUL DE LA CHARGE
!       D'ECHANGE_PAROI
!
        tpsthe(3) = 1.d0
        call mecact('V', timemo, 'MODELE', ligrmo, 'INST_R',&
                    6, nomcmp, ibid, tpsthe, cbid,&
                    k8bid)
        tpsthe(3) = 0.d0
!
! --- TEMPERATURES IMPOSEES                                  ---> CNDIRP
!
        call vedith(modele, charge, infoch, time, vediri)
        call asasve(vediri, numedd, typres, vadirp)
        call ascova('D', vadirp, fomult, 'INST', tpsthe,&
                    typres, cndirp)
        call jeveuo(cndirp(1:19)//'.VALE', 'E', jdirp)
!
! --- CHARGES CINEMATIQUES                                   ---> CNCHCI
!
        cnchci = ' '
        call ascavc(charge, infoch, fomult, numedd, tpsnp1,&
                    cnchci)
!
! --- CHARGEMENTS THERMIQUES                                 ---> CNCHTP
!            RQ : POUR LE CALCUL THERMIQUE, LES ARGUMENTS VTEMPP,
!                 VTEMPD ET THETA SONT INUTILISES.
!
        call vechth(modele, charge, infoch, carele, mate,&
                    time, vtemp, vechtp)
        call asasve(vechtp, numedd, typres, vachtp)
        call ascova('D', vachtp, fomult, 'INST', tpsthe,&
                    typres, cnchtp)
        call jeveuo(cnchtp(1:19)//'.VALE', 'E', jchtp)
        call jelira(cnchtp(1:19)//'.VALE', 'LONMAX', lonch, k8bid)
!
! --- SECOND MEMBRE COMPLET                                  ---> VEC2ND
!
        call jeveuo(vec2nd(1:19)//'.VALE', 'E', j2nd)
        do 120 k = 1, lonch
            zr(j2nd+k-1) = zr(jchtp+k-1) + zr(jdirp+k-1)
120      continue
!
    endif
!
! ======================================================================
!              MATRICE ASSEMBLEE
! ======================================================================
!
    if (reasmt) then
!
! --- (RE)CALCUL DE LA MATRICE DES DIRICHLET POUR L'ASSEMBLER
!
        call medith(modele, charge, infoch, mediri)
        call jeveuo(mediri, 'L', jmed)
!
! --- (RE)ASSEMBLAGE DE LA MATRICE ET CALCUL DES "REACTIONS D'APPUI"
!
        creas = 'M'
        call mertth(modele, charge, infoch, carele, mate,&
                    time, vtemp, vtempm, merigi)
!
        call metnth(modele, charge, carele, mate, time,&
                    vtempm, metrnl)
!
        nbmat = 0
        call jeveuo(merigi, 'L', jmer)
        if (zk24(jmer)(1:8) .ne. '        ') then
            nbmat = nbmat + 1
            tlimat(nbmat) =merigi(1:19)
        endif
!
        call jeexin(metrnl, iret)
        if (iret .gt. 0) then
            call jeveuo(metrnl, 'L', jmet)
            if (zk24(jmet)(1:8) .ne. '        ') then
                nbmat = nbmat + 1
                tlimat(nbmat) =metrnl(1:19)
            endif
        endif
!
        if (zk24(jmed)(1:8) .ne. '        ') then
            nbmat = nbmat + 1
            tlimat(nbmat) =mediri(1:19)
        endif
!
! --- ASSEMBLAGE DE LA MATRICE
!
        call asmatr(nbmat, tlimat, ' ', numedd, solveu,&
                    infcha, 'ZERO', 'V', 1, matass)
!
! --- DECOMPOSITION OU CALCUL DE LA MATRICE DE PRECONDITIONNEMENT
!
        call preres(solveu, 'V', ierr, maprec, matass,&
                    ibid, -9999)
!
    endif
!-----------------------------------------------------------------------
    call jedema()
end subroutine

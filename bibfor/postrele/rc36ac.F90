subroutine rc36ac(noma, ncncin, chindi, chcara, nbma,&
                  listma, chresu)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/limend.h'
    include 'asterfort/rc3601.h'
    include 'asterfort/rc36sa.h'
    include 'asterfort/rc36sn.h'
    include 'asterfort/rc36sp.h'
    include 'asterfort/rcma01.h'
    include 'asterfort/rcmo01.h'
    include 'asterfort/rcvale.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    integer :: nbma, listma(*)
    character(len=8) :: noma
    character(len=24) :: ncncin, chindi, chcara, chresu
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!
!     CALCUL DES AMPLITUDES DE CONTRAINTES
!     CALCUL DU FACTEUR D'USAGE
!
!     Pour chaque noeud de chaque maille:
!
!     pour une situation P, on a 2 états stabilisés
!     pour une situation Q, on a 2 états stabilisés
!
!     Soit 2 états stabilisés I et J appartenant respectivement aux
!     situations P et Q :
!
!     on calcule le SALT(I,J) = 0,5*(EC/E)*Ke*Sn(P,Q)*Sp(I,J)
!
!     avec Sn(P,Q) = Max( Sn(I,J) )
!          Sn(I,J) = Max( Max(Sn(I,J,ThP)), Max(Sn(I,J,ThQ)) )
!
!     avec Sp(I,J) = Max( Max(Sp(I,J,ThP)), Max(Sp(I,J,ThQ)) )
!
!
! Etape 1 : on calcule le SALT qui correspond aux combinaisons de tous
!           les états stabilisés appartenant aux situations d'un groupe
!           donné.
!
! Etape 2 : on calcule le SALT pour les situations non combinables
!
! Etape 3 : traitement des situations de passage
!           on calcule le SALT(I,J)
!              - avec I appartenant au premier groupe
!              - avec J appartenant au deuxieme groupe
!              - on lui associe le nombre d'occurrences de la
!                situation de passage
!
!
! IN  : NCNCIN : CONNECTIVITE INVERSE
! IN  : CHINDI : CHAM_ELEM DES INDICES DE CONTRAINTES
! IN  : CHCARA : CHAM_ELEM DES CARACTERISTIQUES ELEMENTAIRES
! IN  : NBMA   : NOMBRE DE MAILLES D'ANALYSE
! IN  : LISTMA : LISTE DES MAILLES D'ANALYSE
! OUT : CHRESU : CHAM_ELEM RESULTAT
!     ------------------------------------------------------------------
!
    integer :: ig, nbgr, nbsigr, jnsg, is1, ioc1, nocc, numgr, jcombi, jpresa
    integer :: jpresb, jmomea, jmomeb, jnbocc, i1, nbth1, jth1, nbth2, nbcrs
    integer :: nbcin, nbcca, jnsitu, jchmat, jmat, jcesd, jcesv, jcesl, jcinv
    integer :: jcind, jccav, jccad, im, ima, nbpt, decrs, decin, decca, ipt, ino
    integer :: adrm, nbm, icmp, jconx1, jconx2, jfact, jnumgr, jpassa, npass
    integer :: ifm, niv, iocs, iad, jseigr, ioc2, jcinl, jccal, nbp12, nbp23
    integer :: nbp13
    real(kind=8) :: ppi, ppj, snmax, samax, utot, saltij, ug, nadm, mpi(3)
    real(kind=8) :: mpj(3), sm, sn, sp, c(3), k(3), cara(3), matpi(14)
    real(kind=8) :: matpj(14), mse(3), snb, sab, smm, vale(2)
    logical :: seisme, endur
    integer :: icodre
    character(len=8) :: k8b, nommat, noeud, valk(7), kbid
    character(len=24) :: momepi, momepj, nommai, nomnoe, connex, matepi, matepj
    real(kind=8) :: typeke, spmeca, spther
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
    nommai = noma//'.NOMMAI         '
    nomnoe = noma//'.NOMNOE         '
    connex = noma//'.CONNEX         '
    call jeveuo(connex, 'L', jconx1)
    call jeveuo(jexatr(connex, 'LONCUM'), 'L', jconx2)
!
    call jeveuo('&&RC3600.SITU_NUMERO', 'L', jnsitu)
    call jelira('&&RC3600.SITU_NUME_GROUP', 'LONMAX', nbgr, k8b)
    call jeveuo('&&RC3600.SITU_NUME_GROUP', 'L', jnumgr)
    call jeveuo('&&RC3600.SITU_SEISME', 'L', jseigr)
!
    call jeveuo('&&RC3600.SITU_COMBINABLE', 'L', jcombi)
    call jeveuo('&&RC3600.SITU_PRES_A', 'L', jpresa)
    call jeveuo('&&RC3600.SITU_PRES_B', 'L', jpresb)
    call jeveuo('&&RC3600.SITU_MOMENT_A', 'L', jmomea)
    call jeveuo('&&RC3600.SITU_MOMENT_B', 'L', jmomeb)
    call jeveuo('&&RC3600.SITU_NB_OCCUR', 'L', jnbocc)
    call jeveuo('&&RC3600.SITU_PASSAGE', 'L', jpassa)
!
    call jelira('&&RC32SI.PASSAGE_1_2', 'LONUTI', nbp12, k8b)
    call jelira('&&RC32SI.PASSAGE_2_3', 'LONUTI', nbp23, k8b)
    call jelira('&&RC32SI.PASSAGE_1_3', 'LONUTI', nbp13, k8b)
!
    call jeveuo('&&RC3600.MATERIAU', 'L', jchmat)
    call jeveuo('&&RC3600.NOM_MATERIAU', 'L', jmat)
!
! --- LE CHAM_ELEM RESULTAT
!
    call jeveuo(chresu(1:19)//'.CESD', 'L', jcesd)
    call jeveuo(chresu(1:19)//'.CESV', 'E', jcesv)
    call jeveuo(chresu(1:19)//'.CESL', 'E', jcesl)
    nbcrs = zi(jcesd-1+2)
!
! --- LE CHAMP INDICE DE CONTRAINTES
!
    call jeveuo(chindi(1:19)//'.CESV', 'L', jcinv)
    call jeveuo(chindi(1:19)//'.CESD', 'L', jcind)
    call jeveuo(chindi(1:19)//'.CESL', 'L', jcinl)
    nbcin = zi(jcind-1+2)
!
! --- LE CHAMP CARACTERISTIQUES
!
    call jeveuo(chcara(1:19)//'.CESV', 'L', jccav)
    call jeveuo(chcara(1:19)//'.CESD', 'L', jccad)
    call jeveuo(chcara(1:19)//'.CESL', 'L', jccal)
    nbcca = zi(jccad-1+2)
!
    call wkvect('&&RC36AC_TRAVAIL', 'V V R', 4*50, jfact)
!
! --- IL FAUT CALCULER LE FACTEUR D'USAGE EN CHAQUE NOEUD DE CHAQUE
!     MAILLE
!
    do 10 im = 1, nbma
!
        ima = listma(im)
        nommat = zk8(jmat+ima-1)
!
        nbpt = zi(jcesd-1+5+4*(ima-1)+1)
        decrs = zi(jcesd-1+5+4*(ima-1)+4)
        decin = zi(jcind-1+5+4*(ima-1)+4)
        decca = zi(jccad-1+5+4*(ima-1)+4)
!
        do 20 ipt = 1, nbpt
!
! ------- LA CONNECTIVITE INVERSE
!
            ino = zi(jconx1-1+zi(jconx2+ima-1)+ipt-1)
            call jeveuo(jexnum(ncncin, ino), 'L', adrm)
            call jelira(jexnum(ncncin, ino), 'LONMAX', nbm, k8b)
            if (niv .ge. 2) then
                call jenuno(jexnum(nommai, ima), k8b)
                call jenuno(jexnum(nomnoe, ino), noeud)
                write(ifm,1000)'===>> TRAITEMENT DU NOEUD ', noeud,&
                ' APPARTENANT A LA MAILLE ', k8b
            endif
!
! ------- LES INDICES DE CONTRAINTES
!
            do 202 icmp = 1, 3
                iad = decin + (ipt-1)*nbcin + icmp
                if (.not. zl(jcinl-1+iad)) then
                    call jenuno(jexnum(nomnoe, ino), valk(1))
                    call jenuno(jexnum(nommai, ima), valk(2))
                    if (icmp .eq. 1) then
                        valk(3) = 'C1'
                    else if (icmp .eq. 2) then
                        valk(3) = 'C2'
                    else
                        valk(3) = 'C3'
                    endif
                    call u2mesk('F', 'POSTRCCM_9', 3, valk)
                endif
                c(icmp) = zr(jcinv-1+iad)
                iad = decin + (ipt-1)*nbcin + icmp + 3
                if (.not. zl(jcinl-1+iad)) then
                    call jenuno(jexnum(nomnoe, ino), valk(1))
                    call jenuno(jexnum(nommai, ima), valk(2))
                    if (icmp .eq. 1) then
                        valk(3) = 'K1'
                    else if (icmp .eq. 2) then
                        valk(3) = 'K2'
                    else
                        valk(3) = 'K3'
                    endif
                    call u2mesk('F', 'POSTRCCM_9', 3, valk)
                endif
                k(icmp) = zr(jcinv-1+iad)
202          continue
!
! ------- LES CARATERISTIQUES : INERTIE, DIAMETRE, EPAISSEUR
!
            do 204 icmp = 2, 4
                iad = decca + (ipt-1)*nbcca + icmp
                if (.not. zl(jccal-1+iad)) then
                    call jenuno(jexnum(nomnoe, ino), valk(1))
                    call jenuno(jexnum(nommai, ima), valk(2))
                    call u2mesk('F', 'POSTRCCM_8', 2, valk)
                endif
                cara(icmp-1) = zr(jccav-1+iad)
204          continue
!
            sm = 0.d0
            snmax = 0.d0
            samax = 0.d0
            utot = 0.d0
!
! ----------------------------------------------------------------------
!                           E T A P E   1
! ----------------------------------------------------------------------
!
! ------- ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE
!         -----------------------------------------------------
!
            do 100 ig = 1, nbgr
!
                numgr = zi(jnumgr+ig-1)
                if (numgr .lt. 0) goto 100
                iocs = zi(jseigr+ig-1)
!
                npass = 0
                seisme = .false.
!
                if (ig .eq. 1) then
                    if (nbp12 .ne. 0 .or. nbp13 .ne. 0) goto 100
                else if (ig .eq. 2) then
                    if (nbp12 .ne. 0 .or. nbp23 .ne. 0) goto 100
                else if (ig .eq. 3) then
                    if (nbp13 .ne. 0 .or. nbp23 .ne. 0) goto 100
                endif
!
! --------- PASSAGE 1 : PRISE EN COMPTE DU SEISME,
!                       CALCUL DU FACTEUR D'USAGE -> UTOT
!
                if (iocs .ne. 0) then
                    snb = 0.d0
                    sab = 0.d0
                    seisme = .true.
                    call rc3601(numgr, iocs, seisme, npass, ima,&
                                ipt, nbm, zi(adrm), c, k,&
                                cara, nommat, snb, sab, utot,&
                                sm, zr(jfact))
                    seisme = .false.
                endif
!
! --------- PASSAGE 2 : SANS LE SEISME
!                       CALCUL SU SN_MAX
!                       CALCUL SU SALT_MAX
!                       CALCUL DU FACTEUR D'USAGE -> UTOT
!
                call rc3601(numgr, iocs, seisme, npass, ima,&
                            ipt, nbm, zi(adrm), c, k,&
                            cara, nommat, snmax, samax, utot,&
                            sm, zr(jfact))
!
100          continue
!
! ----------------------------------------------------------------------
!                           E T A P E   2
! ----------------------------------------------------------------------
!
            mse(1) = 0.d0
            mse(2) = 0.d0
            mse(3) = 0.d0
!
! ------- ON TRAITE LES SITUATIONS NON COMBINABLES
!         ----------------------------------------
!
            do 200 ig = 1, nbgr
!
                numgr = zi(jnumgr+ig-1)
                if (numgr .lt. 0) goto 200
!
                call jelira(jexnum('&&RC3600.LES_GROUPES', numgr), 'LONMAX', nbsigr, k8b)
                call jeveuo(jexnum('&&RC3600.LES_GROUPES', numgr), 'L', jnsg)
!
                npass = 0
!
                do 210 is1 = 1, nbsigr
                    ioc1 = zi(jnsg+is1-1)
                    if (zl(jcombi+ioc1-1)) goto 210
!
                    nocc = zi(jnbocc+2*ioc1-2)
!
                    ppi = zr(jpresa+ioc1-1)
                    momepi = zk24(jmomea+ioc1-1)
                    call rcmo01(momepi, ima, ipt, mpi)
                    matepi = zk24(jchmat+2*ioc1-1)
                    call rcma01(matepi, ima, ipt, nbm, zi(adrm),&
                                matpi)
!
                    ppj = zr(jpresb+ioc1-1)
                    momepj = zk24(jmomeb+ioc1-1)
                    call rcmo01(momepj, ima, ipt, mpj)
                    matepj = zk24(jchmat+2*ioc1-2)
                    call rcma01(matepj, ima, ipt, nbm, zi(adrm),&
                                matpj)
!
                    call jelira(jexnum('&&RC3600.SITU_THERMIQUE', ioc1), 'LONUTI', nbth1, k8b)
                    if (nbth1 .ne. 0) then
                        call jeveuo(jexnum('&&RC3600.SITU_THERMIQUE', ioc1), 'L', jth1)
                    else
                        jth1 = 1
                    endif
!
                    nbth2 = 0
                    ioc2=0
!
! ----------- CALCUL DU SN
!
                    sn = 0.d0
                    call rc36sn(nbm, zi(adrm), ipt, c, cara,&
                                matpi, ppi, mpi, matpj, ppj,&
                                mpj, mse, nbth1, nbth2, ioc1,&
                                ioc2, sn)
                    snmax = max ( snmax , sn )
!
! ----------- CALCUL DU SP
!
                    typeke=matpi(14)
                    sp = 0.d0
                    spmeca = 0.d0
                    spther = 0.d0
                    call rc36sp(nbm, zi(adrm), ipt, c, k,&
                                cara, matpi, ppi, mpi, matpj,&
                                ppj, mpj, mse, nbth1, nbth2,&
                                ioc1, ioc2, sp, typeke, spmeca,&
                                spther)
!
! ----------- CALCUL DU SALT
!
                    call rc36sa(nommat, matpi, matpj, sn, sp,&
                                typeke, spmeca, spther, saltij, smm)
!
                    if (saltij .gt. samax) then
                        samax = saltij
                        sm = smm
                    endif
!
! ----------- CALCUL DU FACTEUR D'USAGE
!
                    call limend(nommat, saltij, 'WOHLER', kbid, endur)
                    if (endur) then
                        ug=0.d0
                    else
                        call rcvale(nommat, 'FATIGUE', 1, 'SIGM    ', saltij,&
                                    1, 'WOHLER  ', nadm, icodre, 2)
                        if (nadm .lt. 0) then
                            vale(1) = saltij
                            vale(2) = nadm
                            call u2mesg('A', 'POSTRCCM_32', 0, ' ', 0,&
                                        0, 2, vale)
                        endif
                        ug = dble( nocc ) / nadm
                    endif
                    utot = utot + ug
!
210              continue
!
200          continue
!
! ----------------------------------------------------------------------
!                           E T A P E   3
! ----------------------------------------------------------------------
!
! ------- ON TRAITE LES SITUATIONS DE PASSAGE
!         -----------------------------------
!
            do 310 ig = 1, nbgr
!
                numgr = zi(jnumgr+ig-1)
                if (numgr .ge. 0) goto 310
                numgr = -numgr
                iocs = zi(jseigr+ig-1)
                if (iocs .eq. 0) then
                    seisme = .false.
                else
                    seisme = .true.
                endif
!
                call jelira(jexnum('&&RC3600.LES_GROUPES', numgr), 'LONMAX', nbsigr, k8b)
                call jeveuo(jexnum('&&RC3600.LES_GROUPES', numgr), 'L', jnsg)
                if (niv .ge. 2) then
                    write (ifm,3004)
                    write (ifm,3002) (zi(jnsitu+zi(jnsg+i1-1)-1),i1=1,&
                    nbsigr)
                endif
!
                npass = 7
!
                call rc3601(numgr, iocs, seisme, npass, ima,&
                            ipt, nbm, zi(adrm), c, k,&
                            cara, nommat, snmax, samax, utot,&
                            sm, zr(jfact))
!
310          continue
!
! ----------------------------------------------------------------------
!
! ------- ON STOCKE LES RESULTATS DE CALCUL
!         ---------------------------------
!
!         - LE SM
            icmp = 1
            iad = decrs + (ipt-1)*nbcrs + icmp
            zr(jcesv-1+iad) = sm
!         - LE SN
            icmp = 2
            iad = decrs + (ipt-1)*nbcrs + icmp
            zr(jcesv-1+iad) = snmax
!         - LE SN/3SM
            icmp = 3
            iad = decrs + (ipt-1)*nbcrs + icmp
            if (sm .eq. 0.d0) then
                zr(jcesv-1+iad) = 0.d0
            else
                zr(jcesv-1+iad) = snmax / ( 3 * sm )
            endif
!         - LE SALT
            icmp = 4
            iad = decrs + (ipt-1)*nbcrs + icmp
            zr(jcesv-1+iad) = samax
!         - LE U_TOTAL
            icmp = 5
            iad = decrs + (ipt-1)*nbcrs + icmp
            zr(jcesv-1+iad) = utot
!
20      continue
!
10  end do
!
    1000 format(a,a8,a,a8)
    3002 format ('=> LISTE DES NUMEROS DE SITUATION: ',100 (i4,1x))
    3004 format (/,'=> SITUATION DE PASSAGE')
!
    call jedema()
end subroutine

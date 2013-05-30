subroutine nmdocr(carcrz, modele, nbmo1, moclef, iret)
! person_in_charge: jean-michel.proix at edf.fr
! ----------------------------------------------------------------------
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
!     SAISIE ET STOCKAGE DES PARAMETRES LOCAUX DE COMPOREMENT
!
! IN MODELE  : LE MODELE
! IN NBMO1   : NOMBRE DE MOTS-CLES (1 OU 2) COMP_INCR / COMP_ELAS
! IN MOCLEF  : LISTE DES MOTS-CLES (COMP_INCR / COMP_ELAS)
! OUT IRET   : CODE RETOUR
! OUT CARCRI : CARTE DECRIVANT LES CRITERES LOCAUX DE CONVERGENCE
!                     0 : ITER_INTE_MAXI
!                     1 : COMPOSANTE INUTILISEE
!                     2 : RESI_INTE_RELA
!                     3 : THETA (POUR THM)
!                     4 : ITER_INTE_PAS
!                     5 : ALGO_INTE
!                     .............
!                     13 PARM_ALPHA  -> ALPHA DE SUSHI (DEFAUT 1)
! ----------------------------------------------------------------------
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getexm.h'
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/lcalgo.h'
    include 'asterc/lccree.h'
    include 'asterc/lctest.h'
    include 'asterfort/alcart.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exicp.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nmdocv.h'
    include 'asterfort/nmdoki.h'
    include 'asterfort/nocart.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/utlcal.h'
    character(len=1) :: k1bid
    character(len=8) :: noma, k8b, typmcl(2)
    character(len=16) :: tymatg, comp, algo, mocles(2), moclef(2), crirup
    character(len=16) :: texte(3), comcod, method, k16bid, nomcmd
    character(len=19) :: carcri
    character(len=24) :: carcrz
    character(len=24) :: mesmai, modele
    integer :: iret, jncmp, jvalv, numgd, jacmp, nbcrit, icmp, k, jma, nbma
    integer :: irett
    integer :: itepas, i, itdebo, ibid, typtgt, n1, nbmo1, nbocc
    real(kind=8) :: resi, resid, tsampl, tsretu, tseuil, pert, theta, iteint
    real(kind=8) :: alpha, algor, tolrad
    integer :: exits
    logical :: cplan
!
    integer :: dimaki, dimanv
!    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
    parameter (dimaki=9)
!    DIMANV = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
    parameter (dimanv=4)
    integer :: nbkit, nbnvi(dimanv), ncomel, nvmeta, numlc
    character(len=16) :: nomkit(dimaki), lcomel(5), meca, mecaco
!
!
    integer :: iarg
! ----------------------------------------------------------------------
    call jemarq()
    carcri = carcrz
    call dismoi('C', 'NOM_MAILLA', modele(1:8), 'MODELE', i,&
                noma, irett)
    call getres(k8b, k16bid, nomcmd)
!
!
! CARTE DES CRITERES DE CONVERGENCE LOCAUX
    call jeexin(carcri(1:19)//'.VALV', iret)
    if (iret .eq. 0) then
        call alcart('V', carcri, noma, 'CARCRI')
    endif
    call jeveuo(carcri(1:19)//'.NCMP', 'E', jncmp)
    call jeveuo(carcri(1:19)//'.VALV', 'E', jvalv)
    call jenonu(jexnom('&CATA.GD.NOMGD' , 'CARCRI'), numgd)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', numgd), 'L', jacmp)
    call jelira(jexnum('&CATA.GD.NOMCMP', numgd), 'LONMAX', nbcrit, k1bid)
    do 95 icmp = 1, nbcrit
        zk8(jncmp+icmp-1) = zk8(jacmp+icmp-1)
95  end do
! ----------------------------------------------------------------------
!     CARTE PAR DEFAUT SI ON OUBLIE COMP_INCR SUR DES MAILLES
    zr(jvalv-1+1) = 10
    zr(jvalv-1+2) = 0
    zr(jvalv-1+3) = 1.d-6
    zr(jvalv-1+4) = 1.d0
    do 96 i = 5, nbcrit
        zr(jvalv-1+i) = 0.d0
96  end do
    call nocart(carcri, 1, k8b, k8b, 0,&
                k8b, ibid, k8b, nbcrit)
! ----------------------------------------------------------------------
    mocles(1) = 'GROUP_MA'
    mocles(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
    mesmai = '&&NMDOCR'//'.LISTE_MAILLES'
!
!     LECTURE DES PARAMETRES DE CONVERGENCE A STOCKER DANS CARCRI
!     MOTS CLES FACTEUR
    do 160 i = 1, nbmo1
!
        call getfac(moclef(i), nbocc)
!
!       NOMBRE D'OCCURRENCES
        do 150 k = 1, nbocc
!
            ncomel=1
!
            call reliem(modele, noma, 'NU_MAILLE', moclef(i), k,&
                        2, mocles, typmcl, mesmai, nbma)
!
            call getvtx(moclef(i), 'ALGO_INTE', k, iarg, 1,&
                        algo, iret)
            call getvtx(moclef(i), 'RELATION', k, iarg, 1,&
                        comp, n1)
!         CREATION DE L'OBJET COMPORTEMENT A PARTIR DU CATALOGUE
            call lccree(1, comp, comcod)
!
            if (iret .ne. 0) then
!           SI ALGO_INTE EST RENSEIGNE : VERIF QUE CET ALGO EST POSSIBLE
!           AVEC LA LOI DE COMPORTEMENT
                call lctest(comcod, 'ALGO_INTE', algo, irett)
                if (irett .eq. 0) then
                    if ((comp(1:6).eq.'KIT_HM') .or. (comp(1:7) .eq.'KIT_THM')) then
                        call nmdoki(moclef(i), modele, comp, k, dimaki,&
                                    nbkit, nomkit, nbnvi, ncomel, lcomel,&
                                    numlc, nvmeta)
! --- LE COMPORTEMENT MECANIQUE EST STOCKE AU 5EME RANG DE LCOMEL
                        meca = lcomel(ncomel)
                        call lccree(1, meca, mecaco)
                        call lctest(mecaco, 'ALGO_INTE', algo, irett)
                        if (irett .eq. 0) then
                            texte(1)=algo
                            texte(2)='ALGO_INTE'
                            texte(3)=comp
                            call u2mesk('F', 'COMPOR1_45', 3, texte)
                        endif
                    else
                        texte(1)=algo
                        texte(2)='ALGO_INTE'
                        texte(3)=comp
                        call u2mesk('F', 'COMPOR1_45', 3, texte)
                    endif
                endif
            else
!           RECUP DE ALGO_INTE D'APRES LE CATALOGUE DE LA
!           LOI DE COMPORTEMENT (1ERE VALEUR DE LA LISTE)
                call lcalgo(comcod, algo)
            endif
!
!         CERTAINES LOIS EN CPLAN CHANGENT L'ALGO D'INTEGRATION
            cplan = exicp(modele(1:8),mesmai,nbma)
!
            if (cplan) then
                if (comp .eq. 'VMIS_ECMI_LINE' .or. comp .eq. 'VMIS_ECMI_TRAC' .or. comp&
                    .eq. 'VMIS_ISOT_LINE' .or. comp .eq. 'VMIS_ISOT_TRAC') then
                    algo = 'SECANTE'
                endif
            endif
!
!         PASSAGE NOM ALGO -> IDENTIFICATEUR (VALEUR REELLE)
            call utlcal('NOM_VALE', algo, algor)
!
!         RECUP DES DONNEES LIEES A LA CONVERGENCE LOCALE
            call nmdocv(moclef(i), k, algo, 'RESI_INTE_RELA', resi)
            call nmdocv(moclef(i), k, algo, 'ITER_INTE_MAXI', iteint)
!
            itepas = 0
            if (moclef(i) .eq. 'COMP_INCR') then
                call getvis(moclef(i), 'ITER_INTE_PAS', k, iarg, 1,&
                            itepas, iret)
            endif
!
!         CPLAN DEBORST  ET COMP1D DEBORST SEULEMENT EN COMP_INCR
            resid=1.d-6
            pert=0.d0
            itdebo=1
            tseuil=-1.d0
            tsampl=-1.d0
            tsretu=-1.d0
            typtgt = 0
            tymatg=' '
            if (moclef(i) .eq. 'COMP_INCR') then
                call getvis(moclef(i), 'ITER_CPLAN_MAXI', k, iarg, 1,&
                            itdebo, iret)
                call getvr8(moclef(i), 'RESI_CPLAN_MAXI', k, iarg, 1,&
                            resid, iret)
                if (iret .ne. 0) then
                    resid=-resid
                else
                    call getvr8(moclef(i), 'RESI_CPLAN_RELA', k, iarg, 1,&
                                resid, iret)
                endif
                exits = getexm(moclef(i),'TYPE_MATR_TANG')
                if (exits .eq. 1) then
!               DANS ZR(JVALV+1) ON STOCKE LE TYPE DE MATRICE TGTE
                    call getvtx(moclef(i), 'TYPE_MATR_TANG', k, iarg, 1,&
                                tymatg, iret)
                    if (iret .eq. 0) then
                        typtgt = 0
                    else
                        if (tymatg .eq. 'PERTURBATION') then
                            typtgt = 1
                            call getvr8(moclef(i), 'VALE_PERT_RELA', k, iarg, 1,&
                                        pert, iret)
                        else if (tymatg.eq.'VERIFICATION') then
                            typtgt = 2
                            call getvr8(moclef(i), 'VALE_PERT_RELA', k, iarg, 1,&
                                        pert, iret)
                            elseif (tymatg(1:16).eq.'TANGENTE_SECANTE')&
                        then
!                     MATRICE EVOLUTIVE TANGENTE/SECANTE
                            call getvr8(moclef(i), 'SEUIL', k, iarg, 1,&
                                        tseuil, iret)
                            call getvr8(moclef(i), 'AMPLITUDE', k, iarg, 1,&
                                        tsampl, iret)
                            call getvr8(moclef(i), 'TAUX_RETOUR', k, iarg, 1,&
                                        tsretu, iret)
                        endif
!                  VERIF QUE TYMATG EST POSSIBLE POUR COMP
                        call lctest(comcod, 'TYPE_MATR_TANG', tymatg, irett)
                        if (irett .eq. 0) then
                            texte(1)=tymatg
                            texte(2)=comp
                            call u2mesg('F', 'COMPOR1_46', 2, texte, 0,&
                                        0, 0, 0.d0)
                        endif
                    endif
                endif
! GLUTE POUR IMPLEX
                if (nomcmd(1:13) .eq. 'STAT_NON_LINE') then
                    call getvtx(' ', 'METHODE', 0, iarg, 1,&
                                method, iret)
                    if (iret .ne. 0) then
                        if (method .eq. 'IMPLEX') then
                            if ((typtgt.ne.0) .and. (comp.ne.'SANS')) then
                                texte(1)=tymatg
                                texte(2)='IMPLEX'
                                call u2mesg('F', 'COMPOR1_46', 2, texte, 0,&
                                            0, 0, 0.d0)
                            else
                                typtgt=9
                            endif
!                     VERIF QUE TYMATG EST POSSIBLE POUR COMP
                            call lctest(comcod, 'TYPE_MATR_TANG', 'IMPLEX', irett)
                            if ((irett.eq.0) .and. (comp.ne.'SANS')) then
                                texte(1)='IMPLEX'
                                texte(2)=comp
                                call u2mesg('F', 'COMPOR1_46', 2, texte, 0,&
                                            0, 0, 0.d0)
                            endif
                        endif
                    endif
                endif
! FIN GLUTE POUR IMPLEX
!
            endif
!
            if (moclef(i) .eq. 'COMP_INCR') then
                call getvr8(moclef(i), 'PARM_THETA', k, iarg, 1,&
                            theta, iret)
                call getvr8(moclef(i), 'PARM_ALPHA', k, iarg, 1,&
                            alpha, iret)
            else
                theta=1.d0
                alpha=1.d0
            endif
!         TOLERANCE POUR LE CRITERE DE RADIALITE
            if (moclef(i) .eq. 'COMP_INCR') then
                if (typtgt .eq. 0 .and. tymatg(1:16) .ne. 'TANGENTE_SECANTE') then
                    call getvr8(moclef(i), 'RESI_RADI_RELA', k, iarg, 1,&
                                tolrad, iret)
                    if (iret .ne. 0) then
                        tseuil=tolrad
                    else
                        tseuil=-10.d0
                    endif
                endif
            endif
!         CRIT_RUPT
            if (moclef(i) .eq. 'COMP_INCR') then
                if (typtgt .eq. 0 .and. tymatg(1:16) .ne. 'TANGENTE_SECANTE') then
                    call getvtx(moclef(i), 'POST_ITER', k, iarg, 1,&
                                crirup, iret)
                    if (iret .eq. 1) then
!            VERIF QUE CRIRUP EST POSSIBLE POUR COMP
                        tsampl = 1.d0
                    endif
                endif
            endif
!
!         STOCKAGE DE LA CARTE CARCRI
!
            zr(jvalv-1+1) = iteint
            zr(jvalv-1+2) = typtgt
            zr(jvalv-1+3) = resi
            zr(jvalv-1+4) = theta
            zr(jvalv-1+5) = itepas
            zr(jvalv-1+6) = algor
            zr(jvalv-1+7) = pert
            zr(jvalv-1+8) = resid
            zr(jvalv-1+9) = itdebo
            zr(jvalv-1+10) = tseuil
            zr(jvalv-1+11) = tsampl
            zr(jvalv-1+12) = tsretu
            zr(jvalv-1+13) = alpha
!
            if (nbma .ne. 0) then
                call jeveuo(mesmai, 'L', jma)
                call nocart(carcri, 3, k8b, 'NUM', nbma,&
                            k8b, zi(jma), ' ', nbcrit)
                call jedetr(mesmai)
            else
!           PAR DEFAUT C'EST TOUT='OUI'
                call nocart(carcri, 1, k8b, k8b, 0,&
                            k8b, ibid, k8b, nbcrit)
            endif
!
150      continue
!
160  end do
!
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine

subroutine rcstoc(nommat, nomrc, nbobj, valr, valc,&
                  valk, nbr, nbc, nbk)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/getmjm.h'
    include 'asterc/gettco.h'
    include 'asterc/getvc8.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/focain.h'
    include 'asterfort/foverf.h'
    include 'asterfort/gcncon.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/tbexp2.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nbr, nbc, nbk, nbobj
    real(kind=8) :: valr(*)
    complex(kind=8) :: valc(*)
    character(len=8) :: nommat, valk(*)
    character(len=16) :: nomrc
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRS_513
! ----------------------------------------------------------------------
!     BUT: STOCKER DANS LES DEUX TABLEAUX VALR ET VALK LES REELS
!          ET LES K8 CARACTERISANT LA LOI DE COMPORTEMENT DE NOM NOMRC
!
!  IN  NOMMAT : NOM UTILISATEUR DU MATERIAU
!  IN  NOMRC  : NOM DE LA R.C.
!  IN  NBOBJ  : NOMBRE DE MCSIMPS
!  OUT VALR   : VECTEUR DES VALEURS REELLES
!  OUT VALK   : VECTEUR DES K8
!  OUT VALC   : VECTEUR DES COMPLEXES
!  OUT NBR    : NOMBRE DE REELS
!  OUT NBC    : NOMBRE DE COMPLEXES
!  OUT NBK    : NOMBRE DE CONCEPTS (FONCTION, TRC, TABLE, ... )
!
! ----------------------------------------------------------------------
!
!
!
    real(kind=8) :: valr8, e1, ei, precma, valrr(4)
    character(len=8) :: valtx
    character(len=8) :: valch, k8bid, nomcle(5)
    character(len=8) :: mcle8, table
    character(len=19) :: rdep, nomfct, nomint
    character(len=24) :: prol1, prol2, valkk(2)
    character(len=16) :: typeco
    complex(kind=8) :: valc8
    integer :: jtypo, jnomo, ibk, nbmax, vali
    integer :: i, k, ii, jfct, jrpv, jvale, nbcoup, n
    integer :: iret, nbfct, nbpts, jprol, nbptm, lpro1, lpro2
    integer :: iarg
! ----------------------------------------------------------------------
!
    call jemarq()
    call wkvect('&&RCSTOC.TYPOBJ', 'V V K8', nbobj, jtypo)
    call wkvect('&&RCSTOC.NOMOBJ', 'V V K16', nbobj, jnomo)
    call getmjm(nomrc, 1, nbobj, zk16(jnomo), zk8(jtypo),&
                n)
!
!     ON VERIFIE QUE 2 MOTS CLES DIFFERENTS N'ONT PAS LES MEMES
!     8 PREMIERS CARACTERES :
!     -----------------------------------------------------------
    call jecreo('&&RCSTOC.TEMPOR', 'V N K8')
    call jeecra('&&RCSTOC.TEMPOR', 'NOMMAX', nbobj, k8bid)
    do 777,i=1,nbobj
!
!        ON EST OBLIGE DE RECOPIER LA GLUTE ELAS_FLUI :
    if (zk16(jnomo+i-1) .eq. 'PROF_RHO_F_INT') then
        mcle8 = 'RHO_F_IN'
    else if (zk16(jnomo+i-1) .eq. 'PROF_RHO_F_EXT') then
        mcle8 = 'RHO_F_EX'
    else if (zk16(jnomo+i-1) .eq. 'COEF_MASS_AJOU') then
        mcle8 = 'CM'
    else
        mcle8= zk16(jnomo-1+i)(1:8)
    endif
!
    call jeexin(jexnom('&&RCSTOC.TEMPOR', mcle8), iret)
    if (iret .gt. 0) then
        call u2mesk('F', 'MODELISA6_69', 1, zk16(jnomo-1+i))
    else
        call jecroc(jexnom('&&RCSTOC.TEMPOR', mcle8))
    endif
    777 end do
    call jedetr('&&RCSTOC.TEMPOR')
!
    nbr = 0
    nbc = 0
    nbk = 0
!
! --- 0- GLUT META_MECA*, BETON_DOUBLE_DP, RUPT_FRAG ET CZM_LAB_MIX :
! --- ON TRAITE LES TX QU ON CONVERTIT EN REELS
!
    do 50 i = 1, nbobj
        if (zk8(jtypo+i-1)(1:2) .eq. 'TX') then
            if (nomrc(1:9) .eq. 'ELAS_META') then
                call getvtx(nomrc, zk16(jnomo+i-1), 1, iarg, 1,&
                            valtx, n)
                if (n .eq. 1) then
                    if (zk16(jnomo+i-1) .eq. 'PHASE_REFE' .and. valtx .eq. 'CHAUD') then
                        nbr = nbr + 1
                        valr(nbr) = 1.d0
                        valk(nbr) = zk16(jnomo+i-1)
                        elseif( zk16(jnomo+i-1).eq.'PHASE_REFE' .and.&
                    valtx.eq.'FROID') then
                        nbr = nbr + 1
                        valr(nbr) = 0.d0
                        valk(nbr) = zk16(jnomo+i-1)
                    endif
                endif
            else if (nomrc .eq. 'BETON_DOUBLE_DP') then
                call getvtx(nomrc, zk16(jnomo+i-1), 1, iarg, 1,&
                            valtx, n)
                if (n .eq. 1) then
                    if (zk16(jnomo+i-1) .eq. 'ECRO_COMP_P_PIC' .or. zk16(jnomo+i-1) .eq.&
                        'ECRO_TRAC_P_PIC') then
                        nbr = nbr + 1
                        valk(nbr) = zk16(jnomo+i-1)
                        if (valtx .eq. 'LINEAIRE') then
                            valr(nbr) = 0.d0
                        else
                            valr(nbr) = 1.d0
                        endif
                    endif
                endif
                elseif ((nomrc.eq.'RUPT_FRAG') .or.(&
            nomrc.eq.'CZM_LAB_MIX')) then
                call getvtx(nomrc, zk16(jnomo+i-1), 1, iarg, 1,&
                            valtx, n)
                if (n .eq. 1) then
                    if (zk16(jnomo+i-1) .eq. 'CINEMATIQUE') then
                        nbr = nbr + 1
                        valk(nbr) = zk16(jnomo+i-1)
                        if (valtx .eq. 'UNILATER') then
                            valr(nbr) = 0.d0
                        else if (valtx.eq.'GLIS_1D') then
                            valr(nbr) = 1.d0
                        else if (valtx.eq.'GLIS_2D') then
                            valr(nbr) = 2.d0
                        else
                            call assert(.false.)
                        endif
                    else
                        call assert(.false.)
                    endif
                endif
            endif
        endif
50  end do
!
! --- 1- ON TRAITE LES REELS
!
    do 100 i = 1, nbobj
        if (zk8(jtypo+i-1)(1:3) .eq. 'R8 ') then
            call getvr8(nomrc, zk16(jnomo+i-1), 1, iarg, 1,&
                        valr8, n)
            if (n .eq. 1) then
                nbr = nbr + 1
                valr(nbr) = valr8
                valk(nbr) = zk16(jnomo+i-1)
            endif
        endif
100  end do
!
!
! --- 2- ON TRAITE LES COMPLEXES
!
    do 115 i = 1, nbobj
        if (zk8(jtypo+i-1)(1:3) .eq. 'C8 ') then
            call getvc8(nomrc, zk16(jnomo+i-1), 1, iarg, 1,&
                        valc8, n)
            if (n .eq. 1) then
                nbc = nbc + 1
                valc(nbr+nbc) = valc8
                valk(nbr+nbc) = zk16(jnomo+i-1)
            endif
        endif
115  end do
!
!
! --- 3- ON TRAITE ENSUITE LES CONCEPTS
!
    do 110 i = 1, nbobj
        if (zk8(jtypo+i-1)(1:3) .eq. 'CO ') then
            call getvid(nomrc, zk16(jnomo+i-1), 1, iarg, 1,&
                        valch, n)
            if (n .eq. 1) then
                nbk = nbk + 1
                if (zk16(jnomo+i-1) .eq. 'PROF_RHO_F_INT') then
                    valk(nbr+nbc+nbk) = 'RHO_F_IN'
                else if (zk16(jnomo+i-1) .eq. 'PROF_RHO_F_EXT') then
                    valk(nbr+nbc+nbk) = 'RHO_F_EX'
                else if (zk16(jnomo+i-1) .eq. 'COEF_MASS_AJOU') then
                    valk(nbr+nbc+nbk) = 'CM'
                else
                    valk(nbr+nbc+nbk) = zk16(jnomo+i-1)
                endif
            endif
        endif
110  end do
!
    ibk = 0
    do 120 i = 1, nbobj
        if (zk8(jtypo+i-1)(1:3) .eq. 'CO ') then
            call getvid(nomrc, zk16(jnomo+i-1), 1, iarg, 1,&
                        valch, n)
            if (n .eq. 1) then
                call gettco(valch, typeco)
                ibk = ibk + 1
                valk(nbr+nbc+nbk+ibk) = valch
            endif
        endif
120  end do
!
! --- 4- CREATION D'UNE FONCTION POUR STOCKER R(P)
!
    if (( nomrc(1:8) .eq. 'TRACTION' ) .or. ( nomrc(1:13) .eq. 'META_TRACTION' )) then
        if (nomrc(1:8) .eq. 'TRACTION') then
            nomcle(1)(1:4)='SIGM'
        endif
        if (nomrc(1:13) .eq. 'META_TRACTION') then
            nomcle(1)(1:7)='SIGM_F1'
            nomcle(2)(1:7)='SIGM_F2'
            nomcle(3)(1:7)='SIGM_F3'
            nomcle(4)(1:7)='SIGM_F4'
            nomcle(5)(1:7)='SIGM_C '
        endif
        nbmax = 0
        do 149 ii = 1, nbk
            do 150 i = 1, nbk
                if ((valk(nbr+nbc+i)(1:6) .eq. 'SIGM  ') .or.&
                    (valk(nbr+nbc+i)(1:7) .eq. 'SIGM_F1') .or.&
                    (valk(nbr+nbc+i)(1:7) .eq. 'SIGM_F2') .or.&
                    (valk(nbr+nbc+i)(1:7) .eq. 'SIGM_F3') .or.&
                    (valk(nbr+nbc+i)(1:7) .eq. 'SIGM_F4') .or.&
                    (valk(nbr+nbc+i)(1:7) .eq. 'SIGM_C ')) then
                    nomfct = valk(nbr+nbc+nbk+i)
                    goto 151
                endif
150          continue
            call u2mesk('F', 'MODELISA6_70', 1, nomcle(ii))
151          continue
!
            call jeveuo(nomfct//'.PROL', 'L', jfct)
            if (zk24(jfct)(1:1) .eq. 'F') then
                call jelira(nomfct//'.VALE', 'LONMAX', nbptm, k8bid)
                if (nomrc(1:8) .eq. 'TRACTION') then
                    if (nbptm .lt. 4) then
                        call u2mesk('F', 'MODELISA6_71', 1, nomcle(ii))
                    endif
                endif
                if (nomrc(1:13) .eq. 'META_TRACTION') then
                    if (nbptm .lt. 2) then
                        call u2mesk('F', 'MODELISA6_72', 1, nomcle(ii))
                    endif
                endif
                nbcoup = nbptm / 2
                if (nbptm .ge. nbmax) nbmax = nbptm
!
                call jeveuo(nomfct//'.VALE', 'L', jrpv)
                if (zr(jrpv) .le. 0.d0) then
                    valkk (1) = nomcle(ii)
                    valkk (2) = nomfct
                    valrr (1) = zr(jrpv)
                    call u2mesg('F', 'MODELISA9_59', 2, valkk, 0,&
                                0, 1, valrr)
                endif
                if (zr(jrpv+nbptm/2) .le. 0.d0) then
                    valkk (1) = nomcle(ii)
                    valkk (2) = nomfct
                    valrr (1) = zr(jrpv+nbptm/2)
                    call u2mesg('F', 'MODELISA9_60', 2, valkk, 0,&
                                0, 1, valrr)
                endif
!        VERIF ABSCISSES CROISSANTES (AU SENS LARGE)
                iret=2
                call foverf(zr(jrpv), nbcoup, iret)
                iret = 0
                e1 = zr(jrpv+nbcoup) / zr(jrpv)
                precma = 1.d-10
!
                do 200 i = 1, nbcoup-1
                    ei = (zr(jrpv+nbcoup+i) - zr(jrpv+nbcoup+i-1) ) / ( zr(jrpv+i) - zr(jrpv+i-1)&
                         )
                    if (ei .gt. e1) then
                        iret = iret + 1
                        valkk (1) = nomcle(ii)
                        valrr (1) = e1
                        valrr (2) = ei
                        valrr (3) = zr(jrpv+i)
                        call u2mesg('E', 'MODELISA9_61', 1, valkk, 0,&
                                    0, 3, valrr)
                    else if ((e1-ei)/e1 .le. precma) then
                        valkk (1) = nomcle(ii)
                        valrr (1) = e1
                        valrr (2) = ei
                        valrr (3) = precma
                        valrr (4) = zr(jrpv+i)
                        call u2mesg('A', 'MODELISA9_62', 1, valkk, 0,&
                                    0, 4, valrr)
                    endif
200              continue
                if (iret .ne. 0) then
                    call u2mess('F', 'MODELISA6_73')
                endif
!
            else if (zk24(jfct)(1:1) .eq. 'N') then
                call jelira(nomfct//'.VALE', 'NUTIOC', nbfct, k8bid)
                nbptm = 0
                do 160 k = 1, nbfct
                    call jelira(jexnum(nomfct//'.VALE', k), 'LONMAX', nbpts, k8bid)
                    nbcoup = nbpts / 2
                    if (nbpts .ge. nbmax) nbmax = nbpts
                    if (nomrc(1:8) .eq. 'TRACTION') then
                        if (nbpts .lt. 4) then
                            call u2mess('F', 'MODELISA6_74')
                        endif
                    endif
                    if (nomrc(1:13) .eq. 'META_TRACTION') then
                        if (nbpts .lt. 2) then
                            call u2mesk('F', 'MODELISA6_75', 1, nomcle( ii))
                        endif
                    endif
                    call jeveuo(jexnum(nomfct//'.VALE', k), 'L', jrpv)
                    if (zr(jrpv) .le. 0.d0) then
                        vali = k
                        valkk (1) = nomcle(ii)
                        valkk (2) = nomfct
                        valrr (1) = zr(jrpv)
                        call u2mesg('F', 'MODELISA9_63', 2, valkk, 1,&
                                    vali, 1, valrr)
                    endif
                    if (zr(jrpv+nbpts/2) .le. 0.d0) then
                        vali = k
                        valkk (1) = nomcle(ii)
                        valkk (2) = nomfct
                        valrr (1) = zr(jrpv+nbpts/2)
                        call u2mesg('F', 'MODELISA9_64', 2, valkk, 1,&
                                    vali, 1, valrr)
                    endif
!         VERIF ABSCISSES CROISSANTES (AU SENS LARGE)
                    iret=2
                    call foverf(zr(jrpv), nbcoup, iret)
                    iret = 0
                    e1 = zr(jrpv+nbcoup) / zr(jrpv)
                    do 210 i = 1, nbcoup-1
                        ei = (&
                             zr(jrpv+nbcoup+i) - zr(jrpv+nbcoup+i-1) ) / ( zr(jrpv+i) - zr(jrpv+i&
                             &-1)&
                             )
                        if (ei .gt. e1) then
                            iret = iret + 1
                            valkk (1) = nomcle(ii)
                            valrr (1) = e1
                            valrr (2) = ei
                            valrr (3) = zr(jrpv+i)
                            call u2mesg('E', 'MODELISA9_65', 1, valkk, 0,&
                                        0, 3, valrr)
                        endif
210                  continue
                    if (iret .ne. 0) then
                        call u2mess('F', 'MODELISA6_73')
                    endif
160              continue
!
            else
                call u2mess('F', 'MODELISA6_76')
            endif
149      continue
!
        rdep = nommat//'.&&RDEP'
        call wkvect(rdep//'.PROL', 'G V K24', 6, jprol)
        zk24(jprol ) = 'FONCTION'
        zk24(jprol+1) = 'LIN LIN '
        zk24(jprol+2) = 'EPSI    '
        zk24(jprol+3) = zk24(jfct+3)
        call wkvect(rdep//'.VALE', 'G V R', 2*nbmax, jvale)
    endif
!
! --- 6 CREATION SI NECESSAIRE D'UNE FONCTION POUR STOCKER BETA
!       (ENTHALPIE VOLUMIQUE) CALCULEE A PARTIR DE RHO_CP
!
    if (nomrc(1:8) .eq. 'THER_NL') then
        do 650 i = 1, nbk
            if (( valk(nbr+nbc+i)(1:4) .eq. 'BETA' )) then
                nomfct = valk(nbr+nbc+nbk+i)
!
! IL N'Y A RIEN A FAIRE, ON TRAVAILLE DIRECTEMENT AVEC BETA
!
                goto 651
            endif
650      continue
        do 660 i = 1, nbk
            if (( valk(nbr+nbc+i)(1:6) .eq. 'RHO_CP' )) then
                nomfct = valk(nbr+nbc+nbk+i)
                goto 661
            endif
660      continue
        goto 651
661      continue
        call gcncon('_', nomint)
        call focain('TRAPEZE', nomfct, 0.d0, nomint, 'G')
!
! SI PROLONGEMENT CONSTANT POUR RHO_CP : ON AFFECTE PROL LINEAIRE A BETA
!
        prol1 = nomfct//'.PROL'
        call jeveuo(prol1, 'L', lpro1)
        prol2 = nomint//'.PROL'
        call assert(lxlgut(nomint).le.24)
        call jeveuo(prol2, 'E', lpro2)
        if (zk24(lpro1+4)(1:1) .eq. 'C') zk24(lpro2+4)(1:1)='L'
        if (zk24(lpro1+4)(2:2) .eq. 'C') zk24(lpro2+4)(2:2)='L'
!
        do 670 i = nbk, 1, -1
            valk(nbr+nbc+nbk+i+1) = valk(nbr+nbc+nbk+i)
670      continue
        nbk = nbk + 1
        valk(nbr+nbc+ nbk) = 'BETA    '
        valk(nbr+nbc+2*nbk) = nomint
651      continue
    endif
!
! --- 7 VERIFICATION DES NOMS DES PARAMETRES DES TABLES
    if (nomrc(1:10) .eq. 'META_ACIER') then
        do 720 i = 1, nbk
            if (valk(nbr+nbc+i)(1:3) .eq. 'TRC') then
                call getvid(nomrc, 'TRC', 1, iarg, 1,&
                            table, n)
                call tbexp2(table, 'VITESSE')
                call tbexp2(table, 'PARA_EQ')
                call tbexp2(table, 'COEF_0')
                call tbexp2(table, 'COEF_1')
                call tbexp2(table, 'COEF_2')
                call tbexp2(table, 'COEF_3')
                call tbexp2(table, 'COEF_4')
                call tbexp2(table, 'COEF_5')
                call tbexp2(table, 'NB_POINT')
                call tbexp2(table, 'Z1')
                call tbexp2(table, 'Z2')
                call tbexp2(table, 'Z3')
                call tbexp2(table, 'TEMP')
                call tbexp2(table, 'SEUIL')
                call tbexp2(table, 'AKM')
                call tbexp2(table, 'BKM')
                call tbexp2(table, 'TPLM')
                call tbexp2(table, 'DREF')
                call tbexp2(table, 'A')
            endif
720      continue
    endif
!
    call jedetr('&&RCSTOC.TYPOBJ')
    call jedetr('&&RCSTOC.NOMOBJ')
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine

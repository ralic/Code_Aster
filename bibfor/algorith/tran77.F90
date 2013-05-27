subroutine tran77(nomres, typres, nomin, basemo)
    implicit none
!
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
!
! IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_SOUS_STRUC
! IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_TRANS'
! IN  : NOMIN  : NOM UTILISATEUR DU CONCEPT TRAN_GENE AMONT
! IN  : BASEMO : NOM UTILISATEUR DU CONCEPT MODE_MECA AMONT
! ----------------------------------------------------------------------
    include 'jeveux.h'
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/cnocre.h'
    include 'asterfort/copmod.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/extrac.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mdgeph.h'
    include 'asterfort/rbph01.h'
    include 'asterfort/rbph02.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsnoch.h'
    include 'asterfort/rstran.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vtcreb.h'
    include 'asterfort/vtcrec.h'
    include 'asterfort/vtdefs.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: valk(2)
! ----------------------------------------------------------------------
    integer :: i, j, itresu(8)
    integer :: foci, focf, fomi, fomf, fomo
    real(kind=8) :: r8b, epsi
    complex(kind=8) :: cbid
    character(len=1) :: k1bid
    character(len=8) :: k8b, blanc, basemo, crit, interp, basem2, mailla, nomres
    character(len=8) :: nomin, mode, nomma, matgen, nomgd
    character(len=14) :: numddl
    character(len=16) :: typres, type(8), typcha, typbas(8), concep
    character(len=19) :: kinst, knume, krefe, trange, typref(8), prof
    character(len=24) :: matric, chamno, crefe(2), nomcha, objve1, k24bid
    character(len=24) :: objve2, objve3, objve4
    logical :: tousno, multap, leffor, prems
    integer :: iarg
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadesc, iadrif, iarchi, iarefe, ibid, ich, idbase
    integer :: idec, idefm, idinsg, idresu, idvecg, ie, inocmp
    integer :: inoecp, inuddl, inumno, iret, iretou, isk
    integer :: j3refe, jc, jinst, jnume, linst, llcha, lrefe
    integer :: lvale, n1, n2, n3, n4, nbcham, nbinsg
    integer :: nbinst, nbmode, nbnoeu, ncmp, neq, nfonct
!-----------------------------------------------------------------------
    data blanc    /'        '/
!      DATA CHAMN2   /'&&TRAN77.CHAMN2'/
!      DATA NOMCMP   /'DX      ','DY      ','DZ      ',
!     &               'DRX     ','DRY     ','DRZ     '/
!     ------------------------------------------------------------------
    call jemarq()
    mode = basemo
    trange = nomin
    call gettco(nomin, concep)
    nomcha=' '
    numddl=' '
!
!     --- RECUPERATION DES ENTITES DU MAILLAGE SUR LESQUELLES ---
!     ---                PORTE LA RESTITUTION                 ---
    tousno = .true.
    prems = .true.
    call getvtx(' ', 'GROUP_NO', 1, iarg, 0,&
                k8b, n1)
    call getvtx(' ', 'NOEUD', 1, iarg, 0,&
                k8b, n2)
    call getvtx(' ', 'GROUP_MA', 1, iarg, 0,&
                k8b, n3)
    call getvtx(' ', 'MAILLE', 1, iarg, 0,&
                k8b, n4)
    if (n1+n2+n3+n4 .ne. 0) tousno = .false.
!
!     --- RECUPERATION DE LA BASE MODALE ---
!
    call jeveuo(trange//'.DESC', 'L', iadesc)
    nbmode = zi(iadesc+1)
!
!
    if (mode .eq. blanc) then
        call jeveuo(trange//'.REFD', 'L', iarefe)
        matgen = zk24(iarefe)(1:8)
        basemo = zk24(iarefe+4)(1:8)
        call jeveuo(basemo//'           .REFD', 'L', iadrif)
        if (matgen(1:8) .ne. blanc) then
            matric = zk24(iadrif)
            if (matric .ne. blanc) then
                call dismoi('F', 'NOM_NUME_DDL', matric, 'MATR_ASSE', ibid,&
                            numddl, iret)
                call dismoi('F', 'NOM_MAILLA', matric, 'MATR_ASSE', ibid,&
                            mailla, iret)
                if (tousno) call dismoi('F', 'NB_EQUA', matric, 'MATR_ASSE', neq,&
                                        k8b, iret)
            else
                numddl = zk24(iadrif+3)(1:14)
                call dismoi('F', 'NOM_GD', numddl, 'NUME_DDL', ibid,&
                            nomgd, ie)
                call dismoi('F', 'NOM_MAILLA', numddl, 'NUME_DDL', ibid,&
                            mailla, iret)
                if (tousno) call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neq,&
                                        k8b, iret)
            endif
        else
!  POUR LES CALCULS SANS MATRICE GENERALISEE (PROJ_MESU_MODAL)
            matric = zk24(iadrif+3)
            if (matric(1:8) .eq. blanc) then
                matric=zk24(iadrif)
                call dismoi('F', 'NOM_NUME_DDL', matric, 'MATR_ASSE', ibid,&
                            numddl, iret)
            else
                numddl = matric(1:8)
            endif
            call jeveuo(numddl//'.NUME.REFN', 'L', j3refe)
            matric = zk24(j3refe)
            mailla = matric(1:8)
            matric = zk24(iadrif)
            if (tousno) call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neq,&
                                    k8b, iret)
        endif
!
        basem2 = basemo
!
!
    else
!         --- BASE MODALE CALCULEE PAR SOUS-STRUCTURATION
!
        call rsexch('F', basemo, 'DEPL', 1, nomcha,&
                    iret)
        nomcha = nomcha(1:19)//'.REFE'
        call dismoi('F', 'NOM_GD', nomcha, 'CHAM_NO', ibid,&
                    nomgd, ie)
        call jeveuo(nomcha, 'L', llcha)
        k24bid=zk24(llcha)
        mailla = k24bid(1:8)
!
!------ON VERIFIE QUE L'UTILISATEUR A RENSEIGNE LE MEME SUPPORT DE
!------RESTITUTION DANS LE FICHIER DE COMMANDE
        call getvid(' ', 'SQUELETTE', 1, iarg, 1,&
                    nomma, isk)
        if (isk .ne. 0) then
            if (nomma .ne. mailla) then
                valk (1) = nomma
                valk (2) = mailla
                call u2mesk('F', 'SOUSTRUC2_9', 2, valk)
            endif
        endif
!
        crefe(1) = zk24(llcha)
        crefe(2) = zk24(llcha+1)
        if (tousno) call jelira(crefe(2)(1:19)//'.NUEQ', 'LONMAX', neq, k8b)
        basem2 = ' '
        call jeveuo(nomcha, 'L', iadrif)
        matric=zk24(iadrif+1)
        numddl=matric(1:14)
    endif
!
    multap = .false.
!
!     ---   RECUPERATION DES VECTEURS DEPLACEMENT, VITESSE ET   ---
!     --- ACCELERATION GENERALISES SUIVANT LES CHAMPS SOUHAITES ---
    nfonct = 0
    call rbph01(trange, nbcham, type, itresu, nfonct,&
                basem2, typref, typbas, tousno, multap)
!
!     --- RECUPERATION DES NUMEROS DES NOEUDS ET DES DDLS ASSOCIES ---
!     ---         DANS LE CAS D'UNE RESTITUTION PARTIELLE          ---
!
    if (.not. tousno) then
        objve1 = '&&TRAN77.NUME_NOEUD  '
        objve2 = '&&TRAN77.NOM_CMP     '
        objve3 = '&&TRAN77.NB_NEQ      '
        objve4 = '&&TRAN77.NUME_DDL    '
        call rbph02(mailla, numddl, nomcha, nomgd, neq,&
                    nbnoeu, objve1, ncmp, objve2, objve3,&
                    objve4)
        call jeveuo(objve1, 'L', inumno)
        call jeveuo(objve2, 'L', inocmp)
        call jeveuo(objve3, 'L', inoecp)
        call jeveuo(objve4, 'L', inuddl)
    endif
!
!     --- RECUPERATION DES INSTANTS ---
!
    call getvtx(' ', 'CRITERE', 0, iarg, 1,&
                crit, n1)
    call getvr8(' ', 'PRECISION', 0, iarg, 1,&
                epsi, n1)
    call getvtx(' ', 'INTERPOL', 0, iarg, 1,&
                interp, n1)
!
    knume = '&&TRAN77.NUM_RANG'
    kinst = '&&TRAN77.INSTANT'
    call rstran(interp, trange, ' ', 1, kinst,&
                knume, nbinst, iretou)
    if (iretou .ne. 0) then
        call u2mess('F', 'UTILITAI4_24')
    endif
    call jeexin(kinst, iret)
    if (iret .gt. 0) then
        call jeveuo(kinst, 'L', jinst)
        call jeveuo(knume, 'L', jnume)
    endif
!
!     --- CREATION DE LA SD RESULTAT ---
    call rscrsd('G', nomres, typres, nbinst)
!
!     --- RESTITUTION SUR LA BASE REELLE ---
!
! VERIFICATION QU'IL Y UN DE CES MOTS CLEFS :
!  'LIST_INST', 'LIST_FREQ', 'INST' ou 'FREQ'
! A MOINS QUE L'ON NE SOIT DANS UN CAS DE DOUBLE RESTITUTION
! APRES UNE DOUBLE PROJECTION (PRESENCE DU MOT CLEF 'MODE_MECA')
    foci = 0
    focf = 0
    fomi = 0
    fomf = 0
    fomo = 0
    call getvid(' ', 'LIST_INST', 0, iarg, 1,&
                k8b, foci)
    call getvid(' ', 'LIST_FREQ', 0, iarg, 1,&
                k8b, focf)
    call getvr8(' ', 'INST', 0, iarg, 1,&
                r8b, fomi)
    call getvr8(' ', 'FREQ', 0, iarg, 1,&
                r8b, fomf)
    call getvid(' ', 'MODE_MECA', 0, iarg, 1,&
                k8b, fomo)
    if ((interp(1:3).ne.'NON') .and.&
        (&
        foci .eq. 0 .and. focf .eq. 0 .and. fomi .eq. 0 .and. fomf .eq. 0 .and. fomo .eq. 0&
        )) then
        call u2mess('F', 'ALGORITH10_95')
    endif
!
    call jeveuo(trange//'.DISC', 'L', idinsg)
    call jelira(trange//'.DISC', 'LONMAX', nbinsg, k8b)
    call wkvect('&&TRAN77.VECTGENE', 'V V R', nbmode, idvecg)
    do 210 ich = 1, nbcham
        leffor=.true.
        if (type(ich) .eq. 'DEPL' .or. type(ich) .eq. 'VITE' .or. type(ich) .eq. 'ACCE' .or.&
            type(ich) .eq. 'ACCE_ABSOLU') leffor=.false.
!
!            --- RECUPERATION DES DEFORMEES MODALES ---
!
        typcha = typbas(ich)
        call rsexch('F', basemo, typcha, 1, nomcha,&
                    iret)
        nomcha = nomcha(1:19)//'.VALE'
        call jeexin(nomcha, ibid)
        if (ibid .gt. 0) then
            nomcha(20:24)='.VALE'
        else
            nomcha(20:24)='.CELV'
        endif
        if (leffor) call jelira(nomcha, 'LONMAX', neq, k1bid)
        call wkvect('&&TRAN77.BASE', 'V V R', nbmode*neq, idbase)
        if (tousno) then
            call copmod(basemo, typcha, neq, numddl, nbmode,&
                        'R', zr(idbase), cbid)
        else
            do 110 j = 1, nbmode
                call rsexch('F', basemo, typcha, j, nomcha,&
                            iret)
                call jeexin(nomcha(1:19)//'.VALE', ibid)
                if (ibid .gt. 0) then
                    nomcha(20:24)='.VALE'
                else
                    nomcha(20:24)='.CELV'
                endif
                call jeveuo(nomcha, 'L', idefm)
                idec = 0
                do 120 i = 1, nbnoeu
                    do 122 jc = 1, ncmp
                        if (zi(inoecp-1+(i-1)*ncmp+jc) .eq. 1) then
                            idec = idec + 1
                            zr(idbase+(j-1)*neq+idec-1) = zr( idefm+zi( inuddl+idec-1)-1 )
                        endif
122                  continue
120              continue
110          continue
        endif
        iarchi = 0
        if (interp(1:3) .eq. 'NON') then
            call jeexin(trange//'.ORDR', iret)
            if (iret .ne. 0 .and. zi(jnume) .eq. 1) iarchi = -1
        endif
        idresu = itresu(ich)
        do 200 i = 0, nbinst-1
            iarchi = iarchi + 1
            call rsexch(' ', nomres, type(ich), iarchi, chamno,&
                        iret)
            if (iret .eq. 0) then
                call u2mesk('A', 'ALGORITH2_64', 1, chamno)
            else if (iret .eq. 100) then
                if (tousno) then
                    if (mode .eq. blanc) then
                        if (leffor) then
                            call vtdefs(chamno, typref(ich), 'G', 'R')
                        else
                            call vtcreb(chamno, numddl, 'G', 'R', neq)
                        endif
                    else
                        call vtcrec(chamno, nomcha, 'G', 'R', neq)
                    endif
                else
                    if (prems) then
                        prems=.false.
                        call cnocre(mailla, nomgd, nbnoeu, zi(inumno), ncmp,&
                                    zk8(inocmp), zi(inoecp), 'G', ' ', chamno)
                        call dismoi('F', 'PROF_CHNO', chamno, 'CHAM_NO', ibid,&
                                    prof, iret)
                    else
                        call cnocre(mailla, nomgd, nbnoeu, zi( inumno), ncmp,&
                                    zk8(inocmp), zi(inoecp), 'G', prof, chamno)
                    endif
                endif
            else
                call assert(.false.)
            endif
            chamno(20:24) = '.VALE'
            call jeexin(chamno, ibid)
            if (ibid .gt. 0) then
                chamno(20:24) = '.VALE'
            else
                chamno(20:24) = '.CELV'
            endif
            call jeveuo(chamno, 'E', lvale)
!
            if (leffor .or. .not.tousno) call jelira(chamno, 'LONMAX', neq, k8b)
            if (interp(1:3) .ne. 'NON') then
                call extrac(interp, epsi, crit, nbinsg, zr(idinsg),&
                            zr(jinst+i), zr(idresu), nbmode, zr(idvecg), ibid)
                call mdgeph(neq, nbmode, zr(idbase), zr(idvecg), zr(lvale))
            else
                call mdgeph(neq, nbmode, zr(idbase), zr(idresu+(zi( jnume+i)-1)*nbmode),&
                            zr(lvale))
            endif
!
            call rsnoch(nomres, type(ich), iarchi)
            call rsadpa(nomres, 'E', 1, 'INST', iarchi,&
                        0, linst, k8b)
            zr(linst) = zr(jinst+i)
200      continue
        call jedetr('&&TRAN77.BASE')
210  continue
!
!
    krefe = nomres
    call wkvect(krefe//'.REFD', 'G V K24', 7, lrefe)
    if (mode .eq. blanc) then
!
        zk24(lrefe ) = zk24(iadrif)
        zk24(lrefe+1) = zk24(iadrif+1)
        zk24(lrefe+2) = zk24(iadrif+2)
        zk24(lrefe+3) = zk24(iadrif+3)
        zk24(lrefe+4) = '        '
        zk24(lrefe+5) = zk24(iadrif+4)
        zk24(lrefe+6) = '        '
!
    endif
    call jelibe(krefe//'.REFD')
!
    call jedetr('&&TRAN77.NUME_NOEUD  ')
    call jedetr('&&TRAN77.NOM_CMP     ')
    call jedetr('&&TRAN77.NB_NEQ      ')
    call jedetr('&&TRAN77.NUME_DDL    ')
    call jedetr('&&TRAN77.NUM_RANG')
    call jedetr('&&TRAN77.INSTANT')
    call jedetr('&&TRAN77.VECTGENE')
!
    call titre()
!
    call jedema()
end subroutine

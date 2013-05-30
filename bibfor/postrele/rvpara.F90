subroutine rvpara(nomtab, mcf, nbpost)
!     ------------------------------------------------------------------
! TOLE CRP_20
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
! IN  NOMTAB  : NOM DE LA TABLE PRINCIPALE PRODUITE PAR LA COMMANDE
! IN  MCF     : MOT-CLE FACTEUR
! IN  NBPOST  : NOMBRE DE POST-TRAITEMENT A CONSIDERER
! ----------------------------------------------------------------------
!     INITIALISE LA TABLE DE POST_RELEVE_T ASSOCIEE A LA TABLE DE
!     REFERENCE NOMTAB
!     ------------------------------------------------------------------
!
    implicit   none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/juveca.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    include 'asterfort/titrea.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utncmp.h'
    include 'asterfort/wkvect.h'
    character(len=6) :: mcf
    character(len=8) :: nomtab
    integer :: nbpost
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'RVPARA' )
!
    integer :: ifm, niv
    integer :: iocc, ibid
    integer :: jchem, jcham, jresu, jncmp, ncmp, i
    integer :: jinva, jprin, jmome, jmail, jmoye, j, jtrad
    integer :: jtran, n1, n2, n3, jcmp1, jcmp2, jcmp3, nbc, nume
    integer :: iret, nbp, jinst, jordr, jmode, jabsc, jfreq
    integer :: jnoeu, n11, n12, n13, n14, n15, n16, n17, n18
    integer :: jncas, jangl, jnocp, numcmp, jnucp, nbordr, jnume
    real(kind=8) :: r8b
    logical :: lmima, lmoye, lextr, lmoygr
    complex(kind=8) :: c16b
    character(len=8) :: k8b, resu, typara(100), nomcmp
    character(len=16) :: k16b, nomsy, tysd
    character(len=24) :: nomobj, chextr, nopara(100), knume
    character(len=24) :: valk(3)
    character(len=24) :: k24bid
!
    character(len=24) :: nocmp
    integer :: jnocmp, ncmpmx
    integer :: iarg
!     ------------------------------------------------------------------
!
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    if (niv .ge. 2) call u2mesk('I', 'POSTRELE_8', 1, nomtab)
!
    nocmp = '&&'//nompro//'_NOM_CMP_TABLE  '
    ncmpmx = 100
    call wkvect(nocmp, 'V V K8', ncmpmx, jnocmp)
!
    jabsc = 0
    jchem = 0
    jcham = 0
    jresu = 0
    jordr = 0
    jmode = 0
    jinst = 0
    jfreq = 0
    jncas = 0
    jangl = 0
    jnocp = 0
    jncmp = 0
    jinva = 0
    jprin = 0
    jmome = 0
    jnoeu = 0
    jmail = 0
    jmoye = 0
    jtran = 0
    jtrad = 0
    ncmp = 0
!
!====
! 2. ON PARCOURT TOUTES LES ACTIONS DEMANDEES
!====
!
    do 2, iocc = 1, nbpost
!
! 2.1. ==> ON CHERCHE SI C'EST LA BONNE TABLE
!
    call getvid(mcf, 'RESULTAT', iocc, iarg, 0,&
                k8b, n3)
!
!
!
    call getvid(mcf, 'CHEMIN', iocc, iarg, 0,&
                k8b, n1)
    if (n1 .ne. 0) jchem = jchem + 1
!
    call getvid(mcf, 'CHAM_GD', iocc, iarg, 0,&
                k8b, n2)
    if (n2 .ne. 0) jcham = jcham + 1
!
    if (n3 .ne. 0) then
        jresu = jresu + 1
        call getvid(mcf, 'RESULTAT', iocc, iarg, 1,&
                    k8b, n3)
        call gettco(k8b, tysd)
        if (tysd .eq. 'EVOL_ELAS' .or. tysd .eq. 'EVOL_THER' .or. tysd .eq. 'EVOL_NOLI'&
            .or. tysd .eq. 'EVOL_CHAR' .or. tysd .eq. 'DYNA_TRANS') then
            jinst = jinst + 1
            elseif ( tysd .eq. 'DYNA_HARMO' .or. tysd .eq.&
            'HARM_GENE' .or. tysd .eq. 'ACOU_HARMO' ) then
            jfreq = jfreq + 1
            elseif ( tysd .eq. 'MODE_MECA' .or. tysd .eq. 'MODE_GENE'&
            .or. tysd .eq. 'MODE_ACOU' ) then
            jfreq = jfreq + 1
            jmode = jmode + 1
            jnocp = jnocp + 1
        else if (tysd .eq. 'MULT_ELAS') then
            jncas = jncas + 1
        else if (tysd(1:8) .eq. 'FOURIER_') then
            jmode = jmode + 1
        else if (tysd .eq. 'COMB_FOURIER') then
            jangl = jangl + 1
        endif
    endif
!
    call getvid(mcf, 'LIST_ORDRE', iocc, iarg, 0,&
                k8b, n11)
    if (n11 .ne. 0) jordr = jordr + 1
!
    call getvis(mcf, 'NUME_ORDRE', iocc, iarg, 0,&
                ibid, n12)
    if (n12 .ne. 0) jordr = jordr + 1
!
    call getvid(mcf, 'LIST_MODE', iocc, iarg, 0,&
                k8b, n13)
    if (n13 .ne. 0) jmode = jmode + 1
!
    call getvis(mcf, 'NUME_MODE', iocc, iarg, 0,&
                ibid, n14)
    if (n14 .ne. 0) jmode = jmode + 1
!
    call getvid(mcf, 'LIST_INST', iocc, iarg, 0,&
                k8b, n15)
    if (n15 .ne. 0) jinst = jinst + 1
!
    call getvr8(mcf, 'INST', iocc, iarg, 0,&
                r8b, n16)
    if (n16 .ne. 0) jinst = jinst + 1
!
    call getvid(mcf, 'LIST_FREQ', iocc, iarg, 0,&
                k8b, n17)
    if (n17 .ne. 0) jfreq = jfreq + 1
!
    call getvr8(mcf, 'FREQ', iocc, iarg, 0,&
                r8b, n18)
    if (n18 .ne. 0) jfreq = jfreq + 1
!
    if ((n2+n11+n12+n13+n14+n15+n16+n17+n18) .eq. 0) jordr = jordr+ 1
!
    call getvtx(mcf, 'TOUT_CMP', iocc, iarg, 0,&
                k8b, n1)
    if (n1 .ne. 0) then
        jncmp = jncmp + 1
        nomobj = '&&'//nompro//'.NCMP'
        if (n2 .ne. 0) then
            call getvid(mcf, 'CHAM_GD', iocc, iarg, 1,&
                        nomsy, n2)
            call utncmp(nomsy, nbc, nomobj)
        else
            call getvid(mcf, 'RESULTAT', iocc, iarg, 1,&
                        resu, n3)
            call getvtx(mcf, 'NOM_CHAM', iocc, iarg, 1,&
                        nomsy, n1)
!
            call rsorac(resu, 'LONUTI', ibid, r8b, k8b,&
                        c16b, r8b, k8b, nbordr, 1,&
                        ibid)
            knume = '&&'//nompro//'.NUME_ORDRE'
            call wkvect(knume, 'V V I', nbordr, jnume)
            call rsorac(resu, 'TOUT_ORDRE', ibid, r8b, k8b,&
                        c16b, r8b, k8b, zi(jnume), nbordr,&
                        ibid)
            do 14 i = 1, nbordr
                nume = zi(jnume+i-1)
                call rsexch(' ', resu, nomsy, nume, chextr,&
                            iret)
                if (iret .eq. 0) goto 16
14          continue
            call u2mesk('F', 'POSTRELE_9', 1, nomsy)
16          continue
            call jedetr(knume)
            call utncmp(chextr, nbc, nomobj)
        endif
        if (nbc .eq. 0) call u2mess('F', 'POSTRELE_59')
        call jeveuo(nomobj, 'L', jcmp1)
        do 10 i = 1, nbc
            do 12 j = 1, ncmp
                if (zk8(jnocmp-1+j) .eq. zk8(jcmp1+i-1)) goto 10
12          continue
            ncmp = ncmp + 1
            if (ncmp .gt. ncmpmx) then
                ncmpmx = 2*ncmpmx
                call juveca(nocmp, ncmpmx)
                call jeveuo(nocmp, 'E', jnocmp)
            endif
            zk8(jnocmp-1+ncmp) = zk8(jcmp1+i-1)
10      continue
        call jedetr(nomobj)
    endif
!
    call getvtx(mcf, 'NOM_CMP', iocc, iarg, 0,&
                k8b, n1)
    if (n1 .ne. 0) then
!
        call getvtx(mcf, 'TRAC_NOR', iocc, iarg, 0,&
                    k8b, n12)
        if (n12 .ne. 0) jtran = jtran + 1
!
        call getvtx(mcf, 'TRAC_DIR', iocc, iarg, 0,&
                    k8b, n14)
        if (n14 .ne. 0) jtrad = jtrad + 1
!
        if ((n12+n14) .ne. 0) goto 24
        jncmp = jncmp + 1
        nbc = -n1
        call wkvect('&&'//nompro//'.NCMP', 'V V K8', nbc, jcmp2)
        call getvtx(mcf, 'NOM_CMP', iocc, iarg, nbc,&
                    zk8(jcmp2), n1)
!           CALL GETVIS ( MCF, 'NUME_CMP', IOCC,1,0, IBID,N11)
        n11=0
        if (n11 .ne. 0) then
            numcmp = -n11
            call wkvect('&&'//nompro//'.NU_CMP', 'V V I', numcmp, jnucp)
!           CALL GETVIS(MCF,'NUME_CMP',IOCC,IARG,NUMCMP,ZI(JNUCP),N11)
            n11=0
            if (zk8(jcmp2)(1:4) .eq. 'VARI') then
                call assert(nbc.eq.1)
                do 120 i = 1, numcmp
                    call codent(zi(jnucp+i-1), 'G', k8b)
                    nomcmp = 'VARI_'//k8b(1:3)
                    do 122 j = 1, ncmp
                        if (zk8(jnocmp-1+j) .eq. nomcmp) goto 120
122                  continue
                    ncmp = ncmp + 1
                    if (ncmp .gt. ncmpmx) then
                        ncmpmx = 2*ncmpmx
                        call juveca(nocmp, ncmpmx)
                        call jeveuo(nocmp, 'E', jnocmp)
                    endif
                    zk8(jnocmp-1+ncmp) = nomcmp
120              continue
            else
                do 124 i = 1, nbc
                    do 126 j = 1, ncmp
                        if (zk8(jnocmp-1+j) .eq. zk8(jcmp2+i-1)) goto 124
126                  continue
                    ncmp = ncmp + 1
                    if (ncmp .gt. ncmpmx) then
                        ncmpmx = 2*ncmpmx
                        call juveca(nocmp, ncmpmx)
                        call jeveuo(nocmp, 'E', jnocmp)
                    endif
                    zk8(jnocmp-1+ncmp) = zk8(jcmp2+i-1)
124              continue
            endif
            call jedetr('&&'//nompro//'.NU_CMP')
        else
            do 20 i = 1, nbc
                do 22 j = 1, ncmp
                    if (zk8(jnocmp-1+j) .eq. zk8(jcmp2+i-1)) goto 20
22              continue
                ncmp = ncmp + 1
                if (ncmp .gt. ncmpmx) then
                    ncmpmx = 2*ncmpmx
                    call juveca(nocmp, ncmpmx)
                    call jeveuo(nocmp, 'E', jnocmp)
                endif
                zk8(jnocmp-1+ncmp) = zk8(jcmp2+i-1)
20          continue
        endif
        call jedetr('&&'//nompro//'.NCMP')
    endif
24  continue
!
    call getvtx(mcf, 'ELEM_PRINCIPAUX', iocc, iarg, 0,&
                k8b, n1)
    if (n1 .ne. 0) jprin = jprin + 1
!
    call getvtx(mcf, 'RESULTANTE', iocc, iarg, 0,&
                k8b, n1)
    call getvtx(mcf, 'MOMENT', iocc, iarg, 0,&
                k8b, n2)
    if ((n1 .ne. 0) .and. (n2 .ne. 0)) jmome = jmome + 1
    if ((n1 .ne. 0) .and. (n2 .eq. 0)) then
        jncmp = jncmp + 1
        nbc = -n1
        call wkvect('&&'//nompro//'.NCMP', 'V V K8', nbc, jcmp3)
        call getvtx(mcf, 'RESULTANTE', iocc, iarg, nbc,&
                    zk8(jcmp3), n1)
        do 30 i = 1, nbc
            do 32 j = 1, ncmp
                if (zk8(jnocmp-1+j) .eq. zk8(jcmp3+i-1)) goto 30
32          continue
            ncmp = ncmp + 1
            if (ncmp .gt. ncmpmx) then
                ncmpmx = 2*ncmpmx
                call juveca(nocmp, ncmpmx)
                call jeveuo(nocmp, 'E', jnocmp)
            endif
            zk8(jnocmp-1+ncmp) = zk8(jcmp3+i-1)
30      continue
        call jedetr('&&'//nompro//'.NCMP')
    endif
!
    lmima = .false.
    lmoygr = .false.
    lmoye = .false.
    lextr = .false.
    call getvtx(mcf, 'OPERATION', iocc, iarg, 1,&
                k16b, n3)
    if (k16b .eq. 'EXTREMA') lmima = .true.
    if (k16b .eq. 'MOYENNE_ARITH') lmoygr = .true.
    if (k16b .eq. 'MOYENNE') then
        jmoye = jmoye + 1
        lmoye = .true.
    endif
    if (k16b .eq. 'EXTRACTION') then
        lextr = .true.
!
        call getvtx(mcf, 'INVARIANT', iocc, iarg, 0,&
                    k8b, n2)
        if (n2 .ne. 0) jinva = jinva + 1
!
        if (n1 .eq. 0) jabsc = jabsc + 1
!
        call getvtx(mcf, 'NOEUD', iocc, iarg, 0,&
                    k8b, n2)
        if ((n1 .eq. 0) .and. (n2 .ne. 0)) jnoeu = jnoeu + 1
!
        call getvtx(mcf, 'GROUP_NO', iocc, iarg, 0,&
                    k8b, n2)
        if ((n1 .eq. 0) .and. (n2 .ne. 0)) jnoeu = jnoeu + 1
    endif
!
    call getvtx(mcf, 'MOYE_NOEUD', iocc, iarg, 0,&
                k8b, n1)
    if (n1 .ne. 0) then
        call getvtx(mcf, 'MOYE_NOEUD', iocc, iarg, 1,&
                    k8b, n1)
        if (k8b(1:3) .eq. 'NON') jmail = jmail + 1
    endif
!
    2 end do
!
!====
! 3. CONNAISSANT LES CARACTERISTIQUES DE LA TABLE, ON INITIALISE
!====
!
! 3.1. ==> MISE EN PLACE DES PARAMETRES
!
    nbp = 1
    nopara(nbp) = 'INTITULE'
    typara(nbp) = 'K16'
    if (jchem .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'CHEMIN'
        typara(nbp) = 'K8'
        nbp = nbp + 1
        nopara(nbp) = 'SEGMENT'
        typara(nbp) = 'I'
        nbp = nbp + 1
        nopara(nbp) = 'CMP_CNX'
        typara(nbp) = 'I'
    endif
    if ((lextr .or. lmoye) .and. jnoeu .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'NOEUD'
        typara(nbp) = 'K8'
    endif
    if (jcham .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'CHAM_GD'
        typara(nbp) = 'K8'
    endif
    if (jresu .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'RESU'
        typara(nbp) = 'K8'
        nbp = nbp + 1
        nopara(nbp) = 'NOM_CHAM'
        typara(nbp) = 'K16'
    endif
    if (jordr .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'NUME_ORDRE'
        typara(nbp) = 'I'
    endif
    if ((lextr .or. lmoye) .and. jmode .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'NUME_MODE'
        typara(nbp) = 'I'
    endif
    if ((lextr .or. lmoye) .and. jinst .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'INST'
        typara(nbp) = 'R'
    endif
    if ((lextr .or. lmoye) .and. jfreq .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'FREQ'
        typara(nbp) = 'R'
    endif
    if ((lextr .or. lmoye) .and. jncas .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'NOM_CAS'
        typara(nbp) = 'K16'
    endif
    if ((lextr .or. lmoye) .and. jnocp .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'NOEUD_CMP'
        typara(nbp) = 'K16'
    endif
    if ((lextr .or. lmoye) .and. jangl .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'ANGL'
        typara(nbp) = 'R'
    endif
    if ((lextr .or. lmoye) .and. jmail .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'MAILLE'
        typara(nbp) = 'K8'
    endif
    if ((lextr .or. lmoye) .and. jabsc .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'ABSC_CURV'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'COOR_X'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'COOR_Y'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'COOR_Z'
        typara(nbp) = 'R'
    endif
    if ((lextr .or. lmoye) .and. jncmp .ne. 0) then
        do 40 i = 1, ncmp
            nbp = nbp + 1
            nopara(nbp) = zk8(jnocmp-1+i)
            typara(nbp) = 'R'
40      continue
    endif
    if ((lextr .or. lmoye) .and. jinva .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'VMIS'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'TRESCA'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'TRACE'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'DETER'
        typara(nbp) = 'R'
    endif
    if ((lextr .or. lmoye) .and. jprin .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'PRIN_1'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'PRIN_2'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'PRIN_3'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'VECT_1_X'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'VECT_1_Y'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'VECT_1_Z'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'VECT_2_X'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'VECT_2_Y'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'VECT_2_Z'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'VECT_3_X'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'VECT_3_Y'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'VECT_3_Z'
        typara(nbp) = 'R'
    endif
    if ((lextr .or. lmoye) .and. jmome .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'RESULT_X'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'RESULT_Y'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'RESULT_Z'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'MOMENT_X'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'MOMENT_Y'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'MOMENT_Z'
        typara(nbp) = 'R'
    endif
    if ((lextr .or. lmoye) .and. jtran .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'TRAC_NOR'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'TR_NOR_1'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'TR_NOR_2'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'TR_NOR_3'
        typara(nbp) = 'R'
    endif
    if ((lextr .or. lmoye) .and. jtrad .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'TRAC_DIR'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'TR_DIR_1'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'TR_DIR_2'
        typara(nbp) = 'R'
        nbp = nbp + 1
        nopara(nbp) = 'TR_DIR_3'
        typara(nbp) = 'R'
    endif
    if ((lextr .or. lmoye) .and. jmoye .ne. 0) then
        nbp = nbp + 1
        nopara(nbp) = 'QUANTITE'
        typara(nbp) = 'K16'
    endif
    if (lmima) then
        if (jresu .ne. 0 .and. jordr .eq. 0) then
            nbp = nbp + 1
            nopara(nbp) = 'NUME_ORDRE'
            typara(nbp) = 'I'
        endif
        nbp = nbp + 1
        nopara(nbp) = 'EXTREMA'
        typara(nbp) = 'K8'
        nbp = nbp + 1
        nopara(nbp) = 'MAILLE'
        typara(nbp) = 'K8'
        nbp = nbp + 1
        nopara(nbp) = 'NOEUD'
        typara(nbp) = 'K8'
        nbp = nbp + 1
        nopara(nbp) = 'CMP'
        typara(nbp) = 'K8'
        nbp = nbp + 1
        nopara(nbp) = 'VALE'
        typara(nbp) = 'R'
    endif
!
    if (lmoygr) then
        if (jresu .ne. 0 .and. jordr .eq. 0) then
            nbp = nbp + 1
            nopara(nbp) = 'NUME_ORDRE'
            typara(nbp) = 'I'
        endif
        nbp = nbp + 1
        nopara(nbp) = 'CMP'
        typara(nbp) = 'K8'
        nbp = nbp + 1
        nopara(nbp) = 'MOYENNE'
        typara(nbp) = 'R'
    endif
!
    if (niv .ge. 2) then
        do 1789 , n1 = 1 , nbp
        valk(1) = nopara(n1)
        valk(2) = typara(n1)
        call u2mesk('I', 'POSTRELE_10', 2, valk)
1789      continue
    endif
!
! 3.2. ==> CREATION/INITIALISATION DE LA TABLE
!
    call tbcrsd(nomtab, 'G')
    call tbajpa(nomtab, nbp, nopara, typara)
!
    k24bid = nomtab(1:8)//'           .TITR'
    call titrea('T', nomtab, nomtab, k24bid, 'C',&
                ' ', 0, 'G', '(1PE12.5)')
!
    call jedetr(nocmp)
!
    call jedema()
!
end subroutine

subroutine calirc(chargz)
! aslint: disable=W1501
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvtx.h'
    include 'asterfort/aflrch.h'
    include 'asterfort/afrela.h'
    include 'asterfort/assert.h'
    include 'asterfort/calir3.h'
    include 'asterfort/calir4.h'
    include 'asterfort/calir5.h'
    include 'asterfort/calirg.h'
    include 'asterfort/canort.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/imprel.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nbnlma.h'
    include 'asterfort/pj2dco.h'
    include 'asterfort/pj3dco.h'
    include 'asterfort/pj4dco.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: chargz
! ----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
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
!
!     CREER LES CARTES CHAR.CHXX.CMULT ET CHAR.CHXX.CIMPO
!          ET REMPLIR LIGRCH, POUR LE MOT-CLE LIAISON_MAIL
!     (COMMANDES AFFE_CHAR_MECA ET AFFE_CHAR_THER)
!
! IN  : CHARGE : NOM UTILISATEUR DU RESULTAT DE CHARGE
!-----------------------------------------------------------------------
!
    integer :: k, kk, nuno1, nuno2, ino1, ino2, ndim, ier, nocc, iocc
    integer :: ibid, jnoma, nnomx, idmax, idnomn, idcoef, idnomd, igeom
    integer :: idirec, idimen, iagma1, iagno2, nbma1, nbno2, nbno2t
    integer :: nno1, i, indire, lno
    integer :: jconb, jconu, jcocf, jcom1, idecal
    integer :: jconb1, jconu1, jcocf1, jcom11, ideca1
    integer :: jconb2, jconu2, jcocf2, jcom12, ideca2
    integer :: nbtyp, nddl2, nbma2, idmai2, jlistk, jdim, ndim1
    integer :: jnunoe, jnorm, idim, ij, ima1, jlisv1
    integer :: kno2, kkno2, jnu2bs, jelim, jcoor
    logical :: lrota, dnor
    real(kind=8) :: beta, coef1, mrota(3, 3), zero, normal(3)
    complex(kind=8) :: betac, cbid
    character(len=2) :: typlag
    character(len=4) :: fonree
    character(len=4) :: typcoe, typlia
    character(len=8) :: noma, mo, m8blan, kelim, kbid
    character(len=8) :: kbeta, nono1, nono2, charge, cmp, ddl2, listyp(8)
    character(len=16) :: motfac, tymocl(4), motcle(4), nomcmd
    character(len=16) :: corres, corre1, corre2, typrac
    character(len=19) :: ligrmo, lisrel
    character(len=24) :: geom2
    character(len=24) :: valk(2)
    character(len=1) :: kb
    real(kind=8) :: rbid
    integer :: iarg
! ----------------------------------------------------------------------
!
    call jemarq()
    motfac='LIAISON_MAIL'
    call getfac(motfac, nocc)
    if (nocc .eq. 0) goto 320
!
    call getres(kb, kb, nomcmd)
    if (nomcmd .eq. 'AFFE_CHAR_MECA') then
        typlia='DEPL'
    else if (nomcmd.eq.'AFFE_CHAR_THER') then
        typlia='TEMP'
    else
        call assert(.false.)
    endif
!
!
    fonree='REEL'
    typcoe='REEL'
    charge=chargz
!
    lisrel='&&CALIRC.RLLISTE'
    zero=0.0d0
    beta=0.0d0
    betac=(0.0d0,0.0d0)
    kbeta=' '
    typlag='12'
    m8blan='        '
    ndim1=3
!
    call dismoi('F', 'NOM_MODELE', charge(1:8), 'CHARGE', ibid,&
                mo, ier)
    ligrmo=mo//'.MODELE'
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma=zk8(jnoma)
!
    call dismoi('F', 'DIM_GEOM', mo, 'MODELE', igeom,&
                kbid, ier)
    if (.not.(igeom.eq.2.or.igeom.eq.3)) call u2mess('F', 'MODELISA2_6')
    if (igeom .eq. 2) then
        ndim=2
    else
        ndim=3
    endif
!
    if (ndim .eq. 2) then
        nbtyp=3
        listyp(1)='SEG2'
        listyp(2)='SEG3'
        listyp(3)='SEG4'
    else if (ndim.eq.3) then
        nbtyp=8
        listyp(1)='TRIA3'
        listyp(2)='TRIA6'
        listyp(3)='QUAD4'
        listyp(4)='QUAD8'
        listyp(5)='QUAD9'
        listyp(6)='SEG2'
        listyp(7)='SEG3'
        listyp(8)='SEG4'
    endif
!
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nnomx,&
                kb, ier)
!     -- IDMAX : NOMBRE MAX DE TERMES D'UNE RELATION LINEAIRE
!              = 2*27 + 3 = 57
    idmax=57
    call wkvect('&&CALIRC.NOMNOE', 'V V K8', idmax, idnomn)
    call wkvect('&&CALIRC.NOMDDL', 'V V K8', idmax, idnomd)
    call wkvect('&&CALIRC.COEF', 'V V R', idmax, idcoef)
    call wkvect('&&CALIRC.DIRECT', 'V V R', idmax*3, idirec)
    call wkvect('&&CALIRC.DIMENSION', 'V V I', idmax, idimen)
!
!
!     &&CALIRC.ELIM(INO) : 0 -> INO PAS ELIMINE
!                          1 -> INO ELIMINE
    call wkvect('&&CALIRC.ELIM', 'V V I', nnomx, jelim)
!
    corres='&&CALIRC.CORRES'
    corre1='&&CALIRC.CORRE1'
    corre2='&&CALIRC.CORRE2'
!
    do 310 iocc = 1, nocc
!
!       IL FAUT REMETTRE à ZERO CES 2 OBJETS ENTRE 2 OCCURENCES :
        do 10,kk=1,idmax
        zi(idimen-1+kk)=0
10      continue
        do 20,kk=1,3*idmax
        zr(idirec-1+kk)=0.d0
20      continue
!
        dnor=.false.
        typrac=' '
        if (typlia .eq. 'DEPL') then
            call getvtx(motfac, 'DDL_ESCL', iocc, iarg, 1,&
                        ddl2, nddl2)
            if (nddl2 .gt. 0) dnor=.true.
            call getvtx(motfac, 'TYPE_RACCORD', iocc, iarg, 1,&
                        typrac, ibid)
            if (typrac .eq. 'COQUE') call assert(ndim.eq.3)
            if (typrac .eq. 'COQUE_MASSIF') call assert(ndim.eq.3)
            if (typrac .eq. 'MASSIF_COQUE') call assert(ndim.eq.3)
        endif
!
!        1.1 RECUPERATION DE LA LISTE DES MAILLE_MAIT :
!        ----------------------------------------------
        motcle(1)='MAILLE_MAIT'
        tymocl(1)='MAILLE'
        motcle(2)='GROUP_MA_MAIT'
        tymocl(2)='GROUP_MA'
        call reliem(mo, noma, 'NU_MAILLE', motfac, iocc,&
                    2, motcle, tymocl, '&&CALIRC.LIMANU1', nbma1)
        call jeveuo('&&CALIRC.LIMANU1', 'L', iagma1)
!
!
!        1.2 RECUPERATION DES NOEUD_ESCL
!        -------------------------------
        if (.not.dnor) then
!
!        -- RECUPERATION DE LA LISTE DES NOEUD_ESCL :
!        --------------------------------------------
            motcle(1)='NOEUD_ESCL'
            tymocl(1)='NOEUD'
            motcle(2)='GROUP_NO_ESCL'
            tymocl(2)='GROUP_NO'
            motcle(3)='MAILLE_ESCL'
            tymocl(3)='MAILLE'
            motcle(4)='GROUP_MA_ESCL'
            tymocl(4)='GROUP_MA'
            call reliem(' ', noma, 'NU_NOEUD', motfac, iocc,&
                        4, motcle, tymocl, '&&CALIRC.LINONU2', nbno2)
            call jeveuo('&&CALIRC.LINONU2', 'L', iagno2)
!
        else
!
!        -- RECUPERATION DE LA LISTE DES MAILLE_ESCL :
!        ---------------------------------------------
            motcle(1)='MAILLE_ESCL'
            tymocl(1)='MAILLE'
            motcle(2)='GROUP_MA_ESCL'
            tymocl(2)='GROUP_MA'
            call reliem(mo, noma, 'NU_MAILLE', motfac, iocc,&
                        2, motcle, tymocl, '&&CALIRC.LIMANU2', nbma2)
            if (nbma2 .eq. 0) then
                valk(1)=motcle(1)
                valk(2)=motcle(2)
                call u2mesg('F', 'MODELISA8_49', 2, valk, 0,&
                            0, 0, 0.d0)
            endif
            call jeveuo('&&CALIRC.LIMANU2', 'L', idmai2)
!
! ---        CREATION DU TABLEAU DES NUMEROS DES NOEUDS '&&NBNLMA.LN'
! ---        ET DES NOMBRES D'OCCURENCES DE CES NOEUDS '&&NBNLMA.NBN'
! ---        DES MAILLES DE PEAU MAILLE_ESCL :
!            -------------------------------
            call nbnlma(noma, nbma2, zi(idmai2), nbtyp, listyp,&
                        nbno2)
!
! ---        CALCUL DES NORMALES EN CHAQUE NOEUD :
!            -----------------------------------
            call wkvect('&&CALIRC.LISTK', 'V V K8', 1, jlistk)
            call jeveuo('&&NBNLMA.LN', 'L', jnunoe)
!
! ---        CREATION DU TABLEAU D'INDIRECTION ENTRE LES INDICES
! ---        DU TABLEAU DES NORMALES ET LES NUMEROS DES NOEUDS :
!            -------------------------------------------------
            call wkvect('&&CALIRC.INDIRE', 'V V I', nnomx, indire)
            call jelira('&&NBNLMA.LN', 'LONUTI', lno, kb)
!
            do 30 i = 1, lno
                zi(indire+zi(jnunoe+i-1)-1)=i
30          continue
!
            call canort(noma, nbma2, zi(idmai2), zk8(jlistk), ndim,&
                        nbno2, zi(jnunoe), 1)
            call jeveuo('&&CANORT.NORMALE', 'L', jnorm)
            call jedupo('&&NBNLMA.LN', 'V', '&&CALIRC.LINONU2', .false.)
            call jeveuo('&&CALIRC.LINONU2', 'L', iagno2)
        endif
!
!
!       1.3 ON ELIMINE DE LINONU2 LES NOEUDS DEJA ELIMINES LORS DES
!           OCCURENCES PRECEDENTES DE LIAISON_MAILLE
!       ---------------------------------------------------------------
        call getvtx(motfac, 'ELIM_MULT', iocc, iarg, 1,&
                    kelim, ibid)
        if (kelim .eq. 'NON') then
            kkno2=0
            call wkvect('&&CALIRC.LINONU2BIS', 'V V I', nbno2, jnu2bs)
            do 40,kno2=1,nbno2
            nuno2=zi(iagno2-1+kno2)
!            -- SI NUNO2 N'EST PAS ENCORE ELIMINE :
            if (zi(jelim-1+nuno2) .eq. 0) then
                zi(jelim-1+nuno2)=1
                kkno2=kkno2+1
                zi(jnu2bs+kkno2)=nuno2
            endif
40          continue
            nbno2=kkno2
            call jedetr('&&CALIRC.LINONU2')
            call wkvect('&&CALIRC.LINONU2', 'V V I', nbno2, iagno2)
            do 50,kno2=1,nbno2
            zi(iagno2-1+kno2)=zi(jnu2bs+kno2)
50          continue
            call jedetr('&&CALIRC.LINONU2BIS')
        endif
!
!
!       1.4 TRANSFORMATION DE LA GEOMETRIE DE GRNO2 :
!       ------------------------------------------
        geom2='&&CALIRC.GEOM_TRANSF'
        call calirg('LIAISON_MAIL', iocc, ndim, noma, '&&CALIRC.LINONU2',&
                    geom2, mrota, lrota)
!       -- LROTA = .TRUE. : ON A UTILISE LE MOT CLE ANGL_NAUT
        if (typrac .eq. 'COQUE_MASSIF') call assert(.not.lrota)
        if (typrac .eq. 'MASSIF_COQUE') call assert(.not.lrota)
        if (typrac .eq. 'COQUE') call assert(.not.lrota)
!
!
!
!       2. CALCUL DE CORRES (+ EVENTUELLEMENT CORRE1, CORRE2) :
!       --------------------------------------------------------
        if (ndim .eq. 2) then
            call assert((typrac.eq.' ') .or. (typrac.eq.'MASSIF'))
            call pj2dco('PARTIE', mo, mo, nbma1, zi(iagma1),&
                        nbno2, zi( iagno2), ' ', geom2, corres,&
                        .false., rbid)
        else if (ndim.eq.3) then
            if ((typrac.eq.' ') .or. (typrac.eq.'MASSIF')) then
                call pj3dco('PARTIE', mo, mo, nbma1, zi(iagma1),&
                            nbno2, zi(iagno2), ' ', geom2, corres,&
                            .false., rbid)
                elseif (typrac.eq.'COQUE' .or. typrac.eq.'MASSIF_COQUE')&
            then
                call pj4dco('PARTIE', mo, mo, nbma1, zi(iagma1),&
                            nbno2, zi(iagno2), ' ', geom2, corres,&
                            .false., rbid, 'NON')
            else if (typrac.eq.'COQUE_MASSIF') then
                call pj3dco('PARTIE', mo, mo, nbma1, zi(iagma1),&
                            nbno2, zi(iagno2), ' ', geom2, corres,&
                            .false., rbid)
                call wkvect('&&CALIRC.LISV1', 'V V R', 3*nnomx, jlisv1)
                call calir3(mo, nbma1, zi(iagma1), nbno2, zi(iagno2),&
                            geom2, corre1, corre2, jlisv1, iocc)
            else
                call assert(.false.)
            endif
!
        endif
!
        call jeveuo(corres//'.PJEF_NB', 'L', jconb)
        call jeveuo(corres//'.PJEF_M1', 'L', jcom1)
        call jeveuo(corres//'.PJEF_NU', 'L', jconu)
        call jeveuo(corres//'.PJEF_CF', 'L', jcocf)
!
        if (typrac .eq. 'COQUE_MASSIF') then
            call jeveuo(corre1//'.PJEF_NB', 'L', jconb1)
            call jeveuo(corre1//'.PJEF_M1', 'L', jcom11)
            call jeveuo(corre1//'.PJEF_NU', 'L', jconu1)
            call jeveuo(corre1//'.PJEF_CF', 'L', jcocf1)
            call jeveuo(corre2//'.PJEF_NB', 'L', jconb2)
            call jeveuo(corre2//'.PJEF_M1', 'L', jcom12)
            call jeveuo(corre2//'.PJEF_NU', 'L', jconu2)
            call jeveuo(corre2//'.PJEF_CF', 'L', jcocf2)
        endif
        call jelira(corres//'.PJEF_NB', 'LONMAX', nbno2t, kb)
        call assert(nbno2t.eq.nnomx)
!
!
!
!       3. ECRITURE DES RELATIONS LINEAIRES :
!       =====================================
!
!
!       3.1 CAS "DEPL" :
!       =================
        if (typlia .eq. 'DEPL') then
!
!       -- 3.1.1 S'IL N'Y A PAS DE ROTATION :
!       -------------------------------------
            if (.not.lrota) then
                idecal=0
                ideca1=0
                ideca2=0
!
!           -- ON BOUCLE SUR TOUS LES NOEUDS DU MAILLAGE :
                do 140 ino2 = 1, nbno2t
!             IMA1: MAILLE A CONNECTER A INO2
                    ima1=zi(jcom1-1+ino2)
!             -- SI IMA1=0, C'EST QUE INO2 NE FAIT PAS PARTIE
!                DES NOEUDS ESCLAVES
                    if (ima1 .eq. 0) goto 140
!
!             NNO1: NB NOEUDS DE IMA1
                    nno1=zi(jconb-1+ino2)
!
                    nuno2=ino2
                    call jenuno(jexnum(noma//'.NOMNOE', nuno2), nono2)
!
                    zk8(idnomn-1+1)=nono2
                    zr(idcoef-1+1)=-1.d0
!
                    do 60,ino1=1,nno1
                    nuno1=zi(jconu+idecal-1+ino1)
                    coef1=zr(jcocf+idecal-1+ino1)
                    call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
                    zk8(idnomn+ino1)=nono1
                    zr(idcoef+ino1)=coef1
!               SI LA RELATION EST UNE TAUTOLOGIE, ON NE L'ECRIT PAS :
                    if (nuno1 .eq. nuno2) then
                        if (abs(zr(idcoef+ino1)-1.d0) .lt. 1.d-2) then
                            call u2mesk('A', 'CALCULEL5_49', 1, nono1)
                            goto 130
!
                        endif
                    endif
60                  continue
!
!           -- AFFECTATION DES RELATIONS CONCERNANT LE NOEUD INO2 :
!           -----------------------------------------------------
                    if (dnor) then
                        do 80 ino1 = 1, nno1+1
                            zi(idimen+ino1-1)=ndim
                            zk8(idnomd-1+ino1)='DEPL'
                            do 70 idim = 1, ndim
                                zr(idirec+(ino1-1)*ndim1+idim-1)=zr(&
                                jnorm+ (zi(indire+ino2-1)-1)*ndim+&
                                idim-1)
70                          continue
80                      continue
                        call afrela(zr(idcoef), cbid, zk8(idnomd), zk8( idnomn), zi(idimen),&
                                    zr(idirec), nno1+1, beta, betac, kbeta,&
                                    typcoe, fonree, typlag, 1.d-6, lisrel)
                    else
!
!               -- RELATIONS CONCERNANT LES TRANSLATIONS (DX/DY/DZ) :
!               -----------------------------------------------------
                        if (typrac .ne. 'MASSIF_COQUE') then
                            do 100,k=1,ndim
                            if (k .eq. 1) cmp='DX'
                            if (k .eq. 2) cmp='DY'
                            if (k .eq. 3) cmp='DZ'
                            do 90,ino1=1,nno1+1
                            zk8(idnomd-1+ino1)=cmp
90                          continue
                            call afrela(zr(idcoef), cbid, zk8( idnomd), zk8(idnomn), zi(idimen),&
                                        zr( idirec), nno1+1, beta, betac, kbeta,&
                                        typcoe, fonree, typlag, 1.d-6, lisrel)
                            call imprel(motfac, nno1+1, zr(idcoef), zk8(idnomd), zk8(idnomn),&
                                        beta)
100                          continue
!
                        else if (typrac.eq.'MASSIF_COQUE') then
                            call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
                            call calir5(noma, lisrel, nono2, nuno2, jcoor,&
                                        idecal, jconb, jcocf, jconu)
                        endif
!
!
!               -- RELATIONS CONCERNANT LES ROTATIONS (DRX/DRY/DRZ) :
!               -----------------------------------------------------
                        if (typrac .eq. 'COQUE') then
                            do 120,k=1,ndim
                            if (k .eq. 1) cmp='DRX'
                            if (k .eq. 2) cmp='DRY'
                            if (k .eq. 3) cmp='DRZ'
                            do 110,ino1=1,nno1+1
                            zk8(idnomd-1+ino1)=cmp
110                          continue
                            call afrela(zr(idcoef), cbid, zk8( idnomd), zk8(idnomn), zi(idimen),&
                                        zr( idirec), nno1+1, beta, betac, kbeta,&
                                        typcoe, fonree, typlag, 1.d-6, lisrel)
                            call imprel(motfac, nno1+1, zr(idcoef), zk8(idnomd), zk8(idnomn),&
                                        beta)
120                          continue
!
                        else if (typrac.eq.'COQUE_MASSIF') then
                            call calir4(noma, lisrel, nono2, ino2, zr(jlisv1+3*(ino2-1)),&
                                        jconb1, jcocf1, jconu1, ideca1, jconb2,&
                                        jcocf2, jconu2, ideca2)
                        endif
                    endif
130                  continue
                    idecal=idecal+nno1
                    if (typrac .eq. 'COQUE_MASSIF') then
                        ideca1=ideca1+zi(jconb1-1+ino2)
                        ideca2=ideca2+zi(jconb2-1+ino2)
                    endif
140              continue
!
!
!       -- 3.1.2  S'IL Y A UNE ROTATION :
!       ---------------------------------
            else
                idecal=0
!
                do 260 ino2 = 1, nbno2t
!
! ---       NNO1: NB DE NOEUD_MAIT LIES A INO2 :
!           ------------------------------------
                    nno1=zi(jconb-1+ino2)
                    if (nno1 .eq. 0) goto 260
                    do 160 k = 1, idmax
                        zk8(idnomn+k-1)=m8blan
                        zk8(idnomd+k-1)=m8blan
                        zr(idcoef+k-1)=zero
                        zi(idimen+k-1)=0
                        do 150 kk = 1, 3
                            zr(idirec+3*(k-1)+kk-1)=zero
150                      continue
160                  continue
!
                    normal(1)=zero
                    normal(2)=zero
                    normal(3)=zero
!
                    nuno2=ino2
                    call jenuno(jexnum(noma//'.NOMNOE', nuno2), nono2)
!
                    if (dnor) then
                        ij=1
                    else
                        ij=ndim
                    endif
!
                    do 170,ino1=1,nno1
                    nuno1=zi(jconu+idecal-1+ino1)
                    coef1=zr(jcocf+idecal-1+ino1)
                    call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
                    zk8(idnomn+ij+ino1-1)=nono1
                    zr(idcoef+ij+ino1-1)=coef1
170                  continue
!
!
!           -- AFFECTATION DES RELATIONS CONCERNANT LE NOEUD INO2 :
!           -----------------------------------------------------
                    if (dnor) then
                        do 190 idim = 1, ndim
                            do 180 jdim = 1, ndim
                                normal(idim)=normal(idim)+ mrota(jdim,&
                                idim)*zr(jnorm+(zi(indire+ ino2-1)-1)*&
                                ndim+jdim-1)
180                          continue
190                      continue
                        zr(idcoef+1-1)=1.0d0
                        zk8(idnomn+1-1)=nono2
                        zk8(idnomd+1-1)='DEPL'
                        zi(idimen+1-1)=ndim
                        do 200 idim = 1, ndim
                            zr(idirec+idim-1)=zr(jnorm+(zi(indire+&
                            ino2-1)-1)*ndim+ idim-1)
200                      continue
                        do 220 ino1 = 2, nno1+1
                            zi(idimen+ino1-1)=ndim
                            zk8(idnomd-1+ino1)='DEPL'
                            do 210 idim = 1, ndim
                                zr(idirec+(ino1-1)*ndim1+idim-1)=-&
                                normal(idim)
210                          continue
220                      continue
                        call afrela(zr(idcoef), cbid, zk8(idnomd), zk8( idnomn), zi(idimen),&
                                    zr(idirec), nno1+1, beta, betac, kbeta,&
                                    typcoe, fonree, typlag, 1.d-6, lisrel)
                    else
                        do 250,k=1,ndim
                        if (k .eq. 1) cmp='DX'
                        if (k .eq. 2) cmp='DY'
                        if (k .eq. 3) cmp='DZ'
                        do 230,ino1=1,nno1
                        zk8(idnomd+ndim+ino1-1)=cmp
230                      continue
                        do 240 kk = 1, ndim
                            if (kk .eq. 1) cmp='DX'
                            if (kk .eq. 2) cmp='DY'
                            if (kk .eq. 3) cmp='DZ'
                            zk8(idnomn+kk-1)=nono2
                            zk8(idnomd+kk-1)=cmp
                            zr(idcoef+kk-1)=-mrota(kk,k)
240                      continue
                        call afrela(zr(idcoef), cbid, zk8(idnomd), zk8(idnomn), zi(idimen),&
                                    zr(idirec), nno1+ ndim, beta, betac, kbeta,&
                                    typcoe, fonree, typlag, 1.d-6, lisrel)
                        call imprel(motfac, nno1+ndim, zr(idcoef), zk8(idnomd), zk8(idnomn),&
                                    beta)
250                      continue
                    endif
                    idecal=idecal+nno1
260              continue
            endif
!
!
!       3.2 CAS "TEMP" :
!       =================
        else if (typlia.eq.'TEMP') then
            idecal=0
            do 300 ino2 = 1, nbno2t
!           NNO1: NB DE NOEUD_MAIT LIES A INO2
                nno1=zi(jconb-1+ino2)
                if (nno1 .eq. 0) goto 300
!
                nuno2=ino2
                call jenuno(jexnum(noma//'.NOMNOE', nuno2), nono2)
!
                zk8(idnomn-1+1)=nono2
                zr(idcoef-1+1)=-1.d0
!
                do 270,ino1=1,nno1
                nuno1=zi(jconu+idecal-1+ino1)
                coef1=zr(jcocf+idecal-1+ino1)
                call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
                zk8(idnomn+ino1)=nono1
                zr(idcoef+ino1)=coef1
!             SI LA RELATION EST UNE TAUTOLOGIE, ON NE L'ECRIT PAS :
                if (nuno1 .eq. nuno2) then
                    if (abs(zr(idcoef+ino1)-1.d0) .lt. 1.d-2) then
                        call u2mesk('A', 'CALCULEL5_49', 1, nono1)
                        goto 290
!
                    endif
                endif
270              continue
!
!           -- AFFECTATION DE LA RELATION CONCERNANT LE NOEUD INO2 :
!           -----------------------------------------------------
                cmp='TEMP'
                do 280,ino1=1,nno1+1
                zk8(idnomd-1+ino1)=cmp
280              continue
                call afrela(zr(idcoef), cbid, zk8(idnomd), zk8(idnomn), zi(idimen),&
                            zr(idirec), nno1+1, beta, betac, kbeta,&
                            typcoe, fonree, typlag, 1.d-6, lisrel)
                call imprel(motfac, nno1+1, zr(idcoef), zk8(idnomd), zk8(idnomn),&
                            beta)
!
290              continue
                idecal=idecal+nno1
300          continue
        else
            call assert(.false.)
        endif
!
        call detrsd('CORRESP_2_MAILLA', corres)
        call detrsd('CORRESP_2_MAILLA', corre1)
        call detrsd('CORRESP_2_MAILLA', corre2)
        call jedetr(geom2)
        call jedetr('&&CALIRC.LIMANU1')
        call jedetr('&&CALIRC.LIMANU2')
        call jedetr('&&CALIRC.LINONU2')
        call jedetr('&&CALIRC.LISTK')
        call jedetr('&&CALIRC.LISV1')
        call jedetr('&&CALIRC.INDIRE')
        call jedetr('&&NBNLMA.LN')
        call jedetr('&&NBNLMA.NBN')
        call jedetr('&&CANORT.NORMALE')
!
310  end do
!
    call jedetr('&&CALIRC.NOMNOE')
    call jedetr('&&CALIRC.NOMDDL')
    call jedetr('&&CALIRC.COEF')
    call jedetr('&&CALIRC.DIRECT')
    call jedetr('&&CALIRC.DIMENSION')
    call jedetr('&&CALIRC.ELIM')
!
! --- AFFECTATION DE LA LISTE DE RELATIONS A LA CHARGE :
!     ------------------------------------------------
    call aflrch(lisrel, charge)
!
320  continue
    call jedema()
end subroutine

subroutine titreb(donnee, iligd, icold, nbtitr, sortie,&
                  iligs, icols, formr)
    implicit none
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/gettco.h'
    include 'asterc/gtoptk.h'
    include 'asterc/r8vide.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/enlird.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jjmmaa.h'
    include 'asterfort/lxcaps.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/lxscan.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsnopa.h'
    include 'asterfort/rsutor.h'
    include 'asterfort/sndbg.h'
    include 'asterfort/titrec.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/utremt.h'
    character(len=*) :: donnee(*), sortie(*), formr
    integer :: iligd, icold, nbtitr, iligs, icols
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_20
!     TRAITEMENT DE DEMONS
!     ------------------------------------------------------------------
! IN DONNEE : K : TABLEAU DES DONNEES
! IN ILIGD  : I :
! IN ICOLD  : I : INDICE DE COLONNE OU SE SITUE LE&
! IN NBTITR : I : NOMBRE MAXIMUM DE LIGNES DE TITRE EN ENTREE
!     ------------------------------------------------------------------
!
!
    integer :: ival, igen, iposa, iposb, iposc, mxdemo, iacc, iad, lenf
    integer :: ibid, iclass, ideb, ierd, ilg, iplace, iret, itit, iuti, jad
    integer :: jpara, ltit, nbacce, nbpa, nbpara, nl, itmp
    integer :: deb, fin, leng
!
    real(kind=8) :: rval, rbid
    character(len=4) :: ctype
    character(len=4) :: ct(3)
    character(len=16) :: formrb
    character(len=80) :: cval
    character(len=255) :: cgen
!
    logical :: lfreq
!
!     REMARQUE :  MXPARA DONNE LE NOMBRE DE PARAMETRES DU DEMON
    parameter          (mxdemo=20)
    character(len=16) :: demons(mxdemo), cbid, k16bid, tysd
    character(len=24) :: para(2)
    integer :: mxpara(mxdemo)
!     ------------------------------------------------------------------
!     --- LISTE DES DEMONS RECONNUS ---
    data demons/&
     &  'DATE'     , 'DATE_HEURE'    , 'HEURE'    ,&
     &  'RESULTAT' , 'TYPE'          , 'COMMANDE' ,&
     &  'CODE'     , 'TITRE_MAILLAGE', 'VERSION'  , 'RL'      ,&
     &  'NB_ELEM'  , 'NB_NOEUD'      , 'PHENOMENE', 'DIM_GEOM',&
     &  'NB_EQUA'  , 'LOC'           , 'NOM_SYMB' , 'NUME_ORDRE',&
     &  'ACCES'    , 'VALEUR'        /
    data mxpara/&
     &    0        ,       0         ,     0      ,&
     &    0        ,       1         ,     0      ,&
     &    0        ,       1         ,     0      ,    0      ,&
     &    1        ,       1         ,     1      ,    1      ,&
     &    1        ,       1         ,     2      ,    2      ,&
     &    2        ,       1         /
!     ------------------------------------------------------------------
!
!     --- LIRE LE NOM DU DEMON DE MINUIT ---
    call jemarq()
!
!     --- ANALYSE DU FORMAT
    if (formr .eq. ' ') then
        leng = 9
        lenf = 12
        formrb = '(1PE12.5)'
    else
        formrb=formr
        leng = lxlgut(formrb)
        deb = 0
        fin = 0
        do 300 itmp = 1, leng
            if (formrb(itmp:itmp) .eq. 'E') deb = itmp+1
            if (formrb(itmp:itmp) .eq. '.') fin = itmp-1
300      continue
        call assert(deb.ne.0.and.fin.ne.0)
        read(formrb(deb:fin),'(I2)') lenf
    endif
!
    nbpara = 0
    lfreq = .false.
    icold = icold + 1
 1  continue
    call lxscan(donnee(iligd), icold, iclass, ival, rval,&
                cval)
    if (iclass .eq. -1) then
        icold = 1
        iligd = iligd + 1
        if (iligd .le. nbtitr) goto 1
    else if (iclass .ne. 3) then
!CC      DEMON INCORRECT
        call sndbg(iunifi('MESSAGE'), iclass, ival, rval, cval)
        cgen = ' '
        igen = 0
    else
!
        cgen = ' '
        igen = 0
        call lxcaps(cval(1:ival))
        call utremt(cval(1:ival), demons, mxdemo, iplace)
        goto( 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130,&
        140, 150, 160, 170, 180, 190, 200 ) iplace
!
!CC      DEMON INCORRECT
        goto 9000
!
!        --- DATE ---
10      continue
        call jjmmaa(ct, cbid(1:12))
        cgen(1:2) = ct(1)(1:2)
        cgen(4:5) = ct(2)(1:2)
        cgen(7:10) = ct(3)
        cgen(6:6) = '/'
        cgen(3:3) = '/'
        igen = 10
        goto 9000
!
!        --- 'DATE_HEURE' ---
20      continue
        call enlird(cgen)
        igen = 24
        goto 9000
!
!        --- 'HEURE' ---
30      continue
        call enlird(cgen)
        cgen(1:8) = cgen(17:24)
        igen = 8
        goto 9000
!
!        --- 'RESULTAT' ---
40      continue
        call getres(cgen, cbid, cbid)
        igen = lxlgut(cgen(1:8))
        goto 9000
!
!        --- 'TYPE' ---
50      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        call gettco(para(1), cgen)
        if (cgen .eq. '  ') goto 9001
        igen = lxlgut(cgen(1:16))
        goto 9000
!
!        --- COMMANDE ---
60      continue
        call getres(cbid, cbid, cgen)
        igen = lxlgut(cgen(1:16))
        goto 9000
!
!        --- CODE --- (SUPPRIME)
70      continue
        cgen = ' '
        igen = 0
        goto 9000
!
!        --- TITRE_MAILLAGE ---
80      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        call gettco(para(1), cgen)
        if (cgen .eq. '  ') cgen = 'CHAMP'
        if (cgen(1:16) .eq. 'MAILLAGE') then
            cbid = para(1)(1:16)
        else
            call dismoi('C', 'NOM_MAILLA', para(1), cgen, ibid,&
                        cbid, ierd)
            if (ierd .ne. 0) goto 9000
        endif
        call jeveuo(cbid(1:8)//'           .TITR', 'L', ltit)
        call jelira(cbid(1:8)//'           .TITR', 'LONMAX', nl, cbid(9:))
!                 ---> LA RECOPIE SE FAIT ICI
        if (icols+igen-1 .gt. len(sortie(1))) then
            iligs = iligs + 1
            icols = 1
        endif
        do 81 itit = 1, nl
            sortie(iligs)(icols:) = zk80(ltit+itit-1)
            iligs = iligs + 1
            icols = 1
81      continue
        igen = 0
        goto 9000
!
!        --- VERSION  ---
90      continue
        call gtoptk('versionD0', cgen(1:8), iret)
        igen = 8
        goto 9000
!
!        --- RETOUR A LA LIGNE ---
100      continue
        iligs = iligs + 1
        icols = 0
        goto 9000
!
!        --- NB_ELEM  ---
110      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        call gettco(para(1), cgen)
        if (cgen .eq. '  ') cgen = 'CHAMP'
        if (cgen(1:16) .eq. 'MAILLAGE') then
            cbid = para(1)(1:16)
        else
            call dismoi('C', 'NOM_MAILLA', para(1), cgen, ibid,&
                        cbid, ierd)
            if (ierd .ne. 0) goto 9000
        endif
        call dismoi('C', 'NB_MA_MAILLA', cbid, 'MAILLAGE', ibid,&
                    cbid, ierd)
        if (ierd .ne. 0) goto 9000
        cgen = '  '
        call codent(ibid, 'G', cgen(1:16))
        igen = lxlgut(cgen(1:16))
        goto 9000
!
!        --- NB_NOEUD ---
120      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        call gettco(para(1), cgen)
        if (cgen .eq. '  ') cgen = 'CHAMP'
        if (cgen(1:16) .eq. 'MAILLAGE') then
            cbid = para(1)(1:16)
        else
            call dismoi('C', 'NOM_MAILLA', para(1), cgen, ibid,&
                        cbid, ierd)
            if (ierd .ne. 0) goto 9000
        endif
        call dismoi('C', 'NB_NO_MAILLA', cbid, 'MAILLAGE', ibid,&
                    cbid, ierd)
        if (ierd .ne. 0) goto 9000
        cgen = '  '
        call codent(ibid, 'G', cgen(1:16))
        igen = lxlgut(cgen(1:16))
        goto 9000
!
!        --- PHENOMENE ---
130      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        call gettco(para(1), cgen)
        if (cgen .eq. '  ') cgen = 'CHAMP'
        if (cgen(1:16) .eq. 'MODELE') then
            cbid = para(1)(1:16)
        else
            call dismoi('C', 'NOM_MODELE', para(1), cgen, ibid,&
                        cbid, ierd)
            if (ierd .ne. 0) goto 9000
        endif
        cgen = '  '
        call dismoi('C', 'PHENOMENE', cbid, 'MODELE', ibid,&
                    cgen(1:16), ierd)
        if (ierd .ne. 0) goto 9000
        igen = lxlgut(cgen(1:16))
        goto 9000
!
!        --- DIMENSION GEOMETRIE ---
140      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        call gettco(para(1), cgen)
        if (cgen .eq. '  ') cgen = 'CHAMP'
        if (cgen(1:16) .eq. 'MAILLAGE') then
            cbid = para(1)(1:16)
        else
            call dismoi('C', 'NOM_MAILLA', para(1), cgen, ibid,&
                        cbid, ierd)
            if (ierd .ne. 0) goto 9000
        endif
        call dismoi('C', 'DIM_GEOM_B', cbid, 'MAILLAGE', ibid,&
                    cbid, ierd)
        if (ierd .ne. 0) goto 9000
        cgen = '.D'
        call codent(ibid, 'G', cgen(1:1))
        igen = 2
        goto 9000
!
!        --- NOMBRE D'EQUATIONS ---
150      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        call gettco(para(1), cgen)
        if (cgen .eq. '  ') cgen = 'CHAMP'
        call dismoi('C', 'NB_EQUA', para(1), cgen, ibid,&
                    cbid, ierd)
        if (ierd .ne. 0) goto 9000
        cgen = '  '
        call codent(ibid, 'G', cgen(1:16))
        igen = lxlgut(cgen(1:16))
        goto 9000
!
!        --- LOCALISATION POUR UN CHAM_ELEM ---
160      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        call dismoi('C', 'TYPE_CHAMP', para(1), 'CHAMP', ibid,&
                    cbid, ierd)
        if (cbid(1:4) .eq. 'ELNO') then
            cgen = 'AUX NOEUDS'
            igen = 10
        else if (cbid(1:4) .eq. 'ELGA') then
            cgen = 'AUX POINTS DE GAUSS'
            igen = 19
        else if (cbid(1:4) .eq. 'ELEM') then
            cgen = 'CONSTANT SUR L''ELEMENT'
            igen = 22
        else
            cgen = 'EN '//cbid(1:4)
            igen = 7
        endif
        goto 9000
!
!        --- NOM SYMBOLIQUE POUR UN CHAMP D'UN RESULTAT ---
170      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        call rsutor(para(1)(1:8), para(2)(1:19), k16bid, ibid)
        cgen = k16bid
        igen = lxlgut(k16bid)
        goto 9000
!
!        --- NUMERO D'ORDRE POUR UN CHAMP D'UN RESULTAT ---
180      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        call rsutor(para(1)(1:8), para(2)(1:19), k16bid, ibid)
        call codent(ibid, 'G', cgen(1:16))
        igen = lxlgut(cgen(1:16))
        goto 9000
!
!        --- ACCES ---
190      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        call rsnopa(para(1)(1:8), 0, '&&TITREB.NOM_ACCE', nbacce, nbpa)
        call jeexin('&&TITREB.NOM_ACCE', iret)
        if (iret .gt. 0) call jeveuo('&&TITREB.NOM_ACCE', 'E', jpara)
        call rsutor(para(1)(1:8), para(2)(1:19), k16bid, ibid)
        do 191 iacc = 1, nbacce
            call gettco(para(1)(1:8), tysd)
            ilg = lxlgut(zk16(jpara-1+iacc))
            cgen(igen+1:igen+ilg) = zk16(jpara-1+iacc)
            cgen(igen+ilg+1:igen+ilg+1) = ':'
!
            call rsadpa(para(1)(1:8), 'L', 1, zk16(jpara-1+iacc), ibid,&
                        1, iad, ctype)
!              TEST SUR LE TYPE DU CONCEPT
!              MODE_MECA A 3 VAR D'ACCES (FREQ,NUME_MODE,et NOEUD_CMP)
            if (tysd .eq. 'MODE_MECA') then
                if (zk16(jpara-1+iacc) .eq. 'FREQ') then
                    if (zr(iad) .eq. r8vide()) then
                        lfreq= .true.
                    else
                        lfreq = .false.
                    endif
                endif
!
                iposa = igen+1
                iposb = igen+ilg
                iposc = igen+ilg+1
!
                if (.not.lfreq) then
!                   PAS D'AFFICHAGE DU TEXTE NOEUD_CMP
                    if (zk16(jpara-1+iacc) .eq. 'NOEUD_CMP') then
                        cgen(iposa:iposb) = ' '
                        cgen(iposc:iposc) = ' '
                    endif
                else
!                   PAS D'AFFICHAGE DU TEXTE FREQ,ET NUME_MODE
                    if (zk16(jpara-1+iacc) .eq. 'FREQ') then
                        cgen(iposa:iposb) = ' '
                        cgen(iposc:iposc) = ' '
                    else if (zk16(jpara-1+iacc).eq.'NUME_MODE') then
                        cgen(iposa:iposb) = ' '
                        cgen(iposc:iposc) = ' '
                    endif
                endif
!
                igen = igen+ilg+2
                if ((ctype(1:1).eq.'I') .and. (.not.lfreq)) then
!                   ENTIER
                    call codent(zi(iad), 'G', cbid)
                    ilg = lxlgut(cbid)
                    cgen(igen+1:igen+ilg) = cbid
                    igen = igen+ilg+1
                else if ((ctype(1:1).eq.'R').and.(.not.lfreq)) then
!                   REEL
                    ilg = lenf+1
                    write(cgen(igen+1:igen+ilg), '(1X,'//formrb(1:&
                    leng)//')')zr(iad)
                    igen = igen+ilg+1
                else if ((ctype(1:3).eq.'K16').and.(lfreq)) then
!                   K16
                    ilg = 16
                    write(cgen(igen+1:igen+ilg),'(A)') zk16(iad)
                    igen = igen+ilg+1
                else
                    if ((ctype(1:3).eq.'K16') .or. (ctype(1:1).eq.'I') .or.&
                        (ctype(1:1).eq.'R')) then
                        goto 191
                    endif
                    call assert(.false.)
                endif
!
            else
!
                igen = igen+ilg+2
                if (ctype(1:1) .eq. 'I') then
!                   ENTIER
                    call codent(zi(iad), 'G', cbid)
                    ilg = lxlgut(cbid)
                    cgen(igen+1:igen+ilg) = cbid
                    igen = igen+ilg+1
                else if (ctype(1:1).eq.'R') then
!                   REEL
                    ilg = lenf+1
                    write(cgen(igen+1:igen+ilg), '(1X,'//formrb(1:&
                    leng)//')')zr(iad)
                    igen = igen+ilg+1
                else if (ctype(1:2).eq.'K8') then
!                   K8
                    ilg = 8
                    write(cgen(igen+1:igen+ilg),'(A)') zk8(iad)
                    igen = igen+ilg+1
                else if (ctype(1:3).eq.'K16') then
!                   K16
                    ilg = 16
                    write(cgen(igen+1:igen+ilg),'(A)') zk16(iad)
                    igen = igen+ilg+1
                else if (ctype(1:3).eq.'K24') then
!                   K24
                    ilg = 24
                    write(cgen(igen+1:igen+ilg),'(A)') zk24(iad)
                    igen = igen+ilg+1
                else if (ctype(1:3).eq.'K32') then
!                   K32
                    ilg = 32
                    write(cgen(igen+1:igen+ilg),'(A)') zk32(iad)
                    igen = igen+ilg+1
                else if (ctype(1:3).eq.'K80') then
!                   K80
                    ilg = 80
                    write(cgen(igen+1:igen+ilg),'(A)') zk80(iad)
                    igen = igen+ilg+1
                else if (ctype(1:1).eq.'C') then
                    call assert(.false.)
                else
                    call assert(.false.)
                endif
            endif
191      continue
        call jedetr('&&TITREB.NOM_ACCE')
        goto 9000
!
!        --- VALEUR PARAMETRE ---
200      continue
        call titrec(donnee, iligd, icold, nbtitr, mxpara(iplace),&
                    para, nbpara)
        ideb = 1
        do 210 iuti = 1, 2
            call jeexin(para(iuti), iret)
            if (iret .eq. 0) then
!CC   CONCEPT INEXISTANT
                goto 210
            endif
            call jelira(para(iuti), 'TYPE', ival, cval)
            call jeveuo(para(iuti), 'L', jad)
            if (cval(1:1) .eq. 'R') then
                rbid = zr(jad)
                write(cgen(ideb:),formrb(1:leng)) rbid
                igen = lxlgut(cgen)
                ideb = igen+1
            else if (cval(1:1).eq.'I') then
                ibid = zi(jad)
                call codent(ibid, 'G', cgen(ideb:))
                igen = lxlgut(cgen)
                ideb = igen+1
            else if (cval(1:1).eq.'K') then
                call jelira(para(iuti), 'LTYP', ival, cval)
                if (ival .eq. 80) then
                    cgen(ideb:) = zk80(jad-1+1)
                    igen = lxlgut(cgen)
                    ideb = igen+1
                else if (ival.eq.32) then
                    cgen(ideb:) = zk32(jad-1+1)
                    igen = lxlgut(cgen)
                    ideb = igen+1
                else if (ival.eq.24) then
                    cgen(ideb:) = zk24(jad-1+1)
                    igen = lxlgut(cgen)
                    ideb = igen+1
                else if (ival.eq.16) then
                    cgen(ideb:) = zk16(jad-1+1)
                    igen = lxlgut(cgen)
                    ideb = igen+1
                else if (ival.eq.8) then
                    cgen(ideb:) = zk8(jad-1+1)
                    igen = lxlgut(cgen)
                    ideb = igen+1
                endif
            endif
210      continue
        igen = lxlgut(cgen)
        goto 9000
    endif
!     ------------------------------------------------------------------
9000  continue
    if (igen .gt. 0) then
!        --- Y A T IL ASSEZ DE PLACE ---
        icols = icols + 1
        if (icols+igen-1 .gt. len(sortie(1))) then
            iligs = iligs + 1
            icols = 1
        endif
        sortie(iligs)(icols:) = cgen(1:igen)
        icols = icols + igen - 1
    endif
    goto 9999
!     ------------------------------------------------------------------
9001  continue
    ilg = lxlgut(para(1))
    call u2mesk('A', 'UTILITAI_99', 1, para(1)(1:ilg))
9999  continue
    call jedema()
end subroutine

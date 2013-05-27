subroutine vrcomp(compom, compop, varmoi, ligrep)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/carces.h'
    include 'asterfort/celces.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/cesred.h'
    include 'asterfort/cestas.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vrcom2.h'
    character(len=*) :: compop, varmoi, compom, ligrep
! ------------------------------------------------------------------
! BUT: VERIFIER LA COHERENCE DU CHAMP DE VARIABLES INTERNES "-" AVEC
!      LE COMPORTEMENT CHOISI.
!      ATTENTION : ON MODIFIE PARFOIS VARMOI POUR TENIR COMPTE DES
!      POSSIBLES CHANGEMENTS DE MODELE ET/OU DE COMPORTEMENT
!
!
!      VARMOI EST LE CHAMP DE VARIABLES INTERNES A L'INSTANT "-"
!      COMPOM EST LA CARTE DE COMPORTEMENT DE VARMOI (OU ' ')
!      COMPOP EST LA CARTE DE COMPOPTEMENT A L'INSTANT "+"
!      LIGREP EST LE LIGREL DU MODELE DE L'INSTANT "+"
!
!      - SI COMPOM = ' ' :
!           ON SE CONTENTE DE COMPARER LE NOMBRE DE V.I. DE VARMOI
!           AVEC LE NOMBRE ATTENDU DANS COMPOP.
!      - SI COMPOM /= ' ':
!           ON PEUT ALORS COMPARER LE NOM DES COMPORTEMENTS DES
!           INSTANTS "+" ET "-"
!           ON EXIGE QUE CES NOMS SOIENT IDENTIQUES OU BIEN QUE :
!             "-" : 'ELAS' OU 'SANS'  -> "+" : N'IMPORTE QUOI
!             "+" : 'ELAS' OU 'SANS'  -> "-" : N'IMPORTE QUOI
!
!
! ------------------------------------------------------------------
!     ARGUMENTS:
! COMPOM   IN/JXIN  K19 : CARTE DE COMPOPTEMENT "-"
! COMPOP   IN/JXIN  K19 : CARTE DE COMPOPTEMENT "+"
! COMPOP   EST AUSSI LE NOM DU CHAM_ELEM_S DE DCEL_I PERMETTANT DE
!          DE CONNAITRE LE NOMBRE DE SOUS-POINTS ET LE NOMBRE DE VARI
! VARMOI   IN/JXVAR K19 : SD CHAM_ELEM   (VARI_R) "-"
!
! REMARQUES :
!  - VARMOI EST PARFOIS MODIFIE POUR ETRE COHERENT AVEC COMPOP
!           ON LE RECREE ALORS SUR LA BASE VOLATILE
!  - ON VERIFIE EGALEMENT LE NOMBRE DES SOUS-POINTS
!
!-----------------------------------------------------------------------
!
    character(len=24) :: valk(3)
!     ------------------------------------------------------------------
    integer :: jdceld, jdcelv, jdcell, jdcelk
    integer :: jce2d, jce2v, jce2l, jce2k
    integer :: iad1, iad2, nbma, nbspp, nbspm, ncmpp, ncmpm
    integer :: ima, iret, kma, ibid
    integer :: iadp, jcoppl, jcoppd, jcoppv, jcoppk, ip
    integer :: iadm, jcopml, jcopmd, jcopmv, jcopmk, im
    integer :: vali(5), tounul, k, nbpgm, n1, jrepp, jrepm
    character(len=8) :: noma, nomail, kbid, nomma2, nomma1
    character(len=16) :: relcop, relcom
    character(len=19) :: dcel, ces2, copm, copp, coto, lig19p, lig19m
    character(len=48) :: comp1, comp2
    logical :: modif, exip, exim
!     ------------------------------------------------------------------
    call jemarq()
!
!     -- MODIF : .TRUE. => IL FAUT MODIFIER VARMOI CAR CERTAINES
!        MAILLES ONT DISPARU OU SONT NOUVELLES OU ONT CHANGE DE
!        COMPORTEMENT
    modif=.false.
!
!     COMPORTEMENTS MISCIBLES ENTRE EUX :
!                     1         2         3         4
!            123456789012345678901234567890123456789012345678
    comp1='LEMAITRE        VMIS_ISOT_LINE  VMIS_ISOT_TRAC  '
    comp2='ELAS            SANS '
!     REMARQUES :
!       'SANS' EST UN COMPORTEMENT INFINIMENT "MOU"
!              QUI N'A PAS DE VARIABLES INTERNES.
!              IL EST UTILISE PAR CABL_PRECONT
!       'ELAS' N'A PAS DE VARIABLES INTERNES.
!        CES 2 COMPORTEMENTS SONT DONC MISCIBLES AVEC TOUS LES AUTRES.
!
!
    coto='&&VRCOMP.COTO'
    copm='&&VRCOMP.COPM'
    copp='&&VRCOMP.COPP'
    ces2='&&VRCOMP.VARI_R'
!
! --- MAILLAGES ATTACHES
!
    call dismoi('F', 'NOM_MAILLA', compop, 'CHAMP', ibid,&
                nomma1, iret)
    call dismoi('F', 'NOM_MAILLA', varmoi, 'CHAMP', ibid,&
                nomma2, iret)
    if (nomma1 .ne. nomma2) then
        call u2mess('F', 'MECANONLINE5_49')
    endif
!
    call carces(compop, 'ELEM', ' ', 'V', coto,&
                'A', iret)
    call cesred(coto, 0, 0, 1, 'RELCOM',&
                'V', copp)
    call detrsd('CHAM_ELEM_S', coto)
!
    call jeveuo(copp//'.CESD', 'L', jcoppd)
    call jeveuo(copp//'.CESV', 'L', jcoppv)
    call jeveuo(copp//'.CESL', 'L', jcoppl)
    call jeveuo(copp//'.CESK', 'L', jcoppk)
!
!
!     DANS COMPOP, ON RECUPERE LE CHAM_ELEM_S DE DCEL_I :
!     ----------------------------------------------------------------
    dcel=compop
    call jeveuo(dcel//'.CESD', 'L', jdceld)
    call jeveuo(dcel//'.CESV', 'L', jdcelv)
    call jeveuo(dcel//'.CESL', 'L', jdcell)
    call jeveuo(dcel//'.CESK', 'L', jdcelk)
    noma=zk8(jdcelk-1+1)
    nbma=zi(jdceld-1+1)
!
!
!     DANS COMPOP ET COMPOM, 'ELAS' EST AFFECTE PA DEFAUT A TOUTES LES
!     MAILLES NON AFFECTEES. DU COUP, ON NE PEUT PAS DISTINGUER UNE
!     MAILLE QUI DISPARAIT (OU QUI APPARAIT) D'UNE MAILLE ELASTIQUE.
!     IL FAUT REGARDER SI ELLE EST DANS LE MODELE.
!     -----------------------------------------------------------------
    lig19p=ligrep
    call jeveuo(lig19p//'.REPE', 'L', jrepp)
    call jelira(lig19p//'.REPE', 'LONMAX', n1, kbid)
    call assert(n1.eq.2*nbma)
!
    call dismoi('F', 'NOM_LIGREL', varmoi, 'CHAM_ELEM', ibid,&
                lig19m, iret)
    call jeveuo(lig19m//'.REPE', 'L', jrepm)
    call jelira(lig19m//'.REPE', 'LONMAX', n1, kbid)
!
!
!
!
!     -- ON TRANSFORME VARMOI EN CHAM_ELEM_S :
!     ----------------------------------------------------------------
    call celces(varmoi, 'V', ces2)
    call cestas(ces2)
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESV', 'L', jce2v)
    call jeveuo(ces2//'.CESL', 'L', jce2l)
    call jeveuo(ces2//'.CESK', 'L', jce2k)
!
!
    if (compom .ne. ' ') then
        call carces(compom, 'ELEM', ' ', 'V', coto,&
                    'A', iret)
        call cesred(coto, 0, 0, 1, 'RELCOM',&
                    'V', copm)
        call detrsd('CHAM_ELEM_S', coto)
!
        call jeveuo(copm//'.CESD', 'L', jcopmd)
        call jeveuo(copm//'.CESV', 'L', jcopmv)
        call jeveuo(copm//'.CESL', 'L', jcopml)
        call jeveuo(copm//'.CESK', 'L', jcopmk)
!
        call assert(zk8(jcopmk-1+1).eq.zk8(jcoppk-1+1))
    endif
!
!
!     1.  SI COMPOM EST DONNE, ON VERIFIE LES NOMS DES COMPORTEMENTS :
!     ---------------------------------------------------------------
    if (compom .ne. ' ') then
        call assert(noma.eq.zk8(jcopmk-1+1))
!
        kma=0
        do 10,ima=1,nbma
        exim=zi(jrepm-1+2*(ima-1)+1).gt.0
        exip=zi(jrepp-1+2*(ima-1)+1).gt.0
        kma=kma+1
        call cesexi('C', jcopmd, jcopml, ima, 1,&
                    1, 1, iadm)
        call cesexi('C', jcoppd, jcoppl, ima, 1,&
                    1, 1, iadp)
        if (iadp .gt. 0) then
            relcop=zk16(jcoppv-1+iadp)
            if (iadm .le. 0) goto 60
            relcom=zk16(jcopmv-1+iadm)
!
!           -- SI RELCOP=RELCOM C'EST OK :
            if (relcom .eq. relcop) goto 10
!
!           -- SI RELCOP ET RELCOM SONT MISCIBLES C'EST OK :
            im=index(comp1,relcom)
            ip=index(comp1,relcop)
            if ((im.gt.0) .and. (ip.gt.0)) goto 10
!
!           -- SI RELCOP OU RELCOM EST 'ELAS' / 'SANS'  C'EST OK :
            im=index(comp2,relcom)
            ip=index(comp2,relcop)
            if ((im.gt.0) .or. (ip.gt.0)) then
                modif=.true.
                if (exip .and. exim) then
                    call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
                    valk(1)=nomail
                    valk(2)=relcom
                    valk(3)=relcop
                    call u2mesk('A', 'CALCULEL3_47', 3, valk)
                endif
                goto 10
            endif
            goto 80
        endif
10      continue
    endif
!
!
!     2.  ON VERIFIE LES NOMBRES DE SOUS-POINTS ET DE CMPS :
!     ------------------------------------------------------------
    do 40,ima=1,nbma
    exim=zi(jrepm-1+2*(ima-1)+1).gt.0
    exip=zi(jrepp-1+2*(ima-1)+1).gt.0
    call cesexi('C', jdceld, jdcell, ima, 1,&
                1, 1, iad1)
    call cesexi('C', jdceld, jdcell, ima, 1,&
                1, 2, iad2)
    call cesexi('C', jcoppd, jcoppl, ima, 1,&
                1, 1, iadp)
!
!       SI LE COMPORTEMENT N'EST PAS PRESENT SUR IMA, ON SAUTE
!       (C'EST LE CAS POUR UN CALCUL SUR UN GROUPE DE MAILLES :)
!       LE COMPOR EST ALORS RESTREINT A CE GROUP_MA, ET LES
!       POUR LES AUTRES MAILLES ON N'A PAS BESOIN DE VERIFIER
!
!
    if (iad1 .le. 0) then
        nbspp=0
        ncmpp=0
    else
        call assert(iad2.gt.0)
        nbspp=zi(jdcelv-1+iad1)
        ncmpp=zi(jdcelv-1+iad2)
    endif
    nbpgm=zi(jce2d-1+5+4*(ima-1)+1)
    nbspm=zi(jce2d-1+5+4*(ima-1)+2)
    ncmpm=zi(jce2d-1+5+4*(ima-1)+3)
!
!       -- PARFOIS LE COMPORTEMENT EST AFFECTE SUR LES MAILLES
!          DE BORD ALORS QUE CES ELEMENTS N'ONT PAS DE VARIABLES
!          INTERNES (I.E. ILS IGNORENT RAPH_MECA).
!          ON NE VEUT PAS FAIRE D'ERREUR <F> :
!
!       -- VERIFICATION DU NOMBRE DE SOUS-POINTS :
    if (nbspp .ne. 0 .and. nbspm .ne. 0) then
        if (nbspp .ne. nbspm .and. nbspm .ne. 0) goto 50
    endif
!
!       -- VERIFICATION DU NOMBRE DE COMPOSANTES :
    if (ncmpp .ne. ncmpm) then
        if ((ncmpm.eq.0) .or. (ncmpp.eq.0)) then
!           -- CE N'EST PAS GRAVE SI LA MAILLE EST NOUVELLE
!              OU SI ELLE A DISPARU DU CALCUL
!           (IL N'Y A PAS LIEU D'EMETTRE UNE ALARME)
            modif=.true.
            goto 40
        endif
!
!         -- CE N'EST PAS GRAVE SI COMPOR "+" = 'ELAS' :
        call assert(iadp.gt.0)
        relcop=zk16(jcoppv-1+iadp)
        if (relcop .eq. 'ELAS' .or. relcop .eq. 'SANS') goto 30
!
!         -- CE N'EST PAS GRAVE SI COMPOR "-" = 'ELAS' :
        if (compom .ne. ' ') then
            call cesexi('C', jcopmd, jcopml, ima, 1,&
                        1, 1, iadm)
            call assert(iadm.gt.0)
            relcom=zk16(jcopmv-1+iadm)
            if (relcom .eq. 'ELAS' .or. relcom .eq. 'SANS') goto 30
        else
!           CE N'EST PAS FACILE A VERIFIER SANS COMPOM !
!           ON VERIFIE :  NCMPM=1 ET VARIM(*)=0.D0
            if (ncmpm .eq. 1) then
                call cesexi('C', jce2d, jce2l, ima, 1,&
                            1, 1, iad2)
                call assert(iad2.gt.0)
                tounul=0
                do 20,k=1,nbpgm*nbspp
                if (zr(jce2v-1+iad2+k-1) .ne. 0.d0) tounul=1
20              continue
                if (tounul .eq. 0) goto 30
            endif
        endif
        goto 70
    endif
!
    goto 40
30  continue
    modif=.true.
!       -- SI COMPOM EST FOURNI, UN MESSAGE PLUS CLAIR
!          A DEJA ETE EMIS ('CALCULEL3_47')
    if ((compom.eq.' ') .and. exip .and. exim) then
        call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
        vali(1)=ncmpm
        vali(2)=ncmpp
        call u2mesg('A', 'CALCULEL3_48', 1, nomail, 2,&
                    vali, 0, 0.d0)
    endif
    40 end do
    goto 90
!
!
!     3. MESSAGES D'ERREUR :
!     ----------------------
50  continue
    call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
    vali(1)=nbspm
    vali(2)=nbspp
    call u2mesg('F', 'CALCULEL6_52', 1, nomail, 2,&
                vali, 0, 0.d0)
!
60  continue
    call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
    call u2mesk('F', 'CALCULEL5_41', 1, nomail)
!
70  continue
    call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
    vali(1)=ncmpm
    vali(2)=ncmpp
    call u2mesg('F', 'CALCULEL3_49', 1, nomail, 2,&
                vali, 0, 0.d0)
!
80  continue
    call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
    valk(1)=relcom
    valk(2)=relcop
    valk(3)=nomail
    call u2mesk('F', 'CALCULEL5_42', 3, valk)
!
!
90  continue
    if (modif) call vrcom2(compop, varmoi, ligrep)
!
!     4. MENAGE :
!     -----------
    call detrsd('CHAM_ELEM_S', ces2)
    call detrsd('CHAM_ELEM_S', copm)
    call detrsd('CHAM_ELEM_S', copp)
!
    call jedema()
!
end subroutine

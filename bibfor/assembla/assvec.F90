subroutine assvec(base, vec, nbvec, tlivec, licoef,&
                  nu, vecpro, motcle, type)
! aslint: disable=W1501
    implicit none
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
!
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cordd2.h"
#include "asterfort/corddl.h"
#include "asterfort/crelil.h"
#include "asterfort/detrsd.h"
#include "asterfort/digdel.h"
#include "asterfort/dismoi.h"
#include "asterfort/fetmpi.h"
#include "asterfort/fettsd.h"
#include "asterfort/gcncon.h"
#include "asterfort/infniv.h"
#include "asterfort/jaexin.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mpicm0.h"
#include "asterfort/mpicm1.h"
#include "asterfort/mpicm2.h"
#include "asterfort/nbec.h"
#include "asterfort/nbno.h"
#include "asterfort/parti0.h"
#include "asterfort/ssvalv.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utimsd.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
    character(len=*) :: vec, tlivec(*), vecpro, base, nu
    character(len=4) :: motcle
    integer :: nbvec, type
    real(kind=8) :: licoef(*), rcoef, r
! ----------------------------------------------------------------------
! OUT K19 VEC   : NOM DU CHAM_NO RESULTAT
!                CHAM_NO ::= CHAM_NO_GD + OBJETS PROVISOIRES POUR L'ASS.
! IN  K* BASE   : NOM DE LA BASE SUR LAQUELLE ON VEUT CREER LE CHAM_NO
! IN  I  NBVEC  : NOMBRE DE VECT_ELEM A ASSEMBLER DANS VEC
! IN  K* TLIVEC : LISTE DES VECT_ELEM A ASSEMBLER
! IN  R  LICOEF : LISTE DES COEF. MULTIPLICATEURS DES VECT_ELEM
! IN  K14 NU    : NOM D'UN NUME_DDL (LE STOCKAGE N'EST PAS NECESSAIRE)
!
! IN  K* VECPRO: NOM D'UN CHAM_NO MODELE(NU OU VECPRO EST OBLIGATOIRE)
! IN  K4 MOTCLE : 'ZERO' (ARGUMENT INUTILE)
! IN  I  TYPE   : TYPE DU VECTEUR ASSEMBLE : 1 --> REEL
!                                            2 --> COMPLEXE
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    character(len=8) :: nomacr, exiele
    character(len=14) :: num2
    character(len=24) :: valk(5)
! ----------------------------------------------------------------------
!     COMMUNS   LOCAUX DE L'OPERATEUR ASSE_VECTEUR
! ----------------------------------------------------------------------
    integer :: gd, nec, nlili
! ---------------------------------------------------------------------
!     VARIABLES LOCALES
! ---------------------------------------------------------------------
    integer :: nbecmx
    parameter(nbecmx=10)
!
    character(len=1) :: k1bid, bas, ktyp
    character(len=8) :: nomsd, kbid, ma, mo, mo2, nogdsi, nogdco, nomcas, partit
    character(len=11) :: k11b
    character(len=14) :: k14b, nudev
    character(len=19) :: k19b, vecas, vprof, vecel, a19, b19, c19, resu
    character(len=24) :: method, sdfeti, k24b, sdfets, kmaila, k24prn, knueq
    character(len=24) :: knulil, kvelil, kveref, kvedsc, nomli, kvale, nomlog
    character(len=24) :: nomlid, infofe, sdfeta
    logical :: lfeti, llimo, llich, llichd, iddok, lfel2, llichp, lfetic
    logical :: lsaute, lbid, lgoto, ldist, ldgrel
    integer :: i, i1, iaconx, iad, iad1, iadlie, iadnem, ialcha
    integer :: iamail, iancmp, ianmcr, ianueq, ianulo, iaprol, iapsdl, iasssa
    integer :: ichar, icmp, iconx1, iconx2, iddesc, idlres
    integer :: idprn1, idprn2, jresl, idveds, idverf, idvref, iec, iel
    integer :: ier, ierd, igr, il, ilim, ilimnu
    integer :: ilinu, ilive, ilivec, ima, imat, inold
    integer :: iresu, iret, j, jec, jnumsd, jvale, k1
    integer :: lgncmp, mode, n1, nbchar, nbelm, nbnoss
    integer :: nbresu, nbsma, nbssa, ncmp, ncmpel, nddl1, nel, nequa
    integer :: nm, nmxcmp, nnoe, nugd, numa, iexi, jrelr, k, jvale1, jvale2
    integer :: icodla(nbecmx), icodge(nbecmx), nbsd
    integer :: idime, idd, iligrp, ifetn, ifetc, irefn, nbrefn
    integer :: admodl, lcmodl, iret1, ifel1, ifel2, ifel3
    integer :: iinf, ifcpu, ibid, ifm, niv, ilimpi, ifel4, ifel5, ilimpb
    integer :: iret2, iret3, iaux1, jfel4, iaux2, iaux3, compt
    integer :: nivmpi, rang, nblog, nbproc, jprti, jprtk
    integer :: lshift
!
    real(kind=8) :: temps(6), rbid
    complex(kind=8) :: cbid
    integer :: vali(4)
!
! --- DEBUT ------------------------------------------------------------
    call jemarq()
!
    call mpicm1('BARRIER', ' ', ibid, ibid, ibid,&
                rbid, cbid)
    call uttcpu('CPU.CALC.1', 'DEBUT', ' ')
    call uttcpu('CPU.ASSE.1', 'DEBUT', ' ')
    call uttcpu('CPU.ASSE.3', 'DEBUT', ' ')
!
!-----RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, niv)
    infofe='FFFFFFFFFFFFFFFFFFFFFFFF'
!
!     IFM = IUNIFI('MESSAGE')
!----------------------------------------------------------------------
!
    vecas=vec
    bas=base
!
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', lcmodl)
    call jeveuo(jexnum('&CATA.TE.MODELOC', 1), 'L', admodl)
    call assert(motcle.eq.'ZERO')
!
!
! --- SI LE CONCEPT VECAS EXISTE DEJA, ON LE DETRUIT:
    call detrsd('CHAMP_GD', vecas)
    call wkvect(vecas//'.LIVE', bas//' V K24 ', nbvec, ilivec)
    do 10 i = 1, nbvec
        zk24(ilivec-1+i)=tlivec(i)
10  end do
!
!
    kmaila='&MAILLA'
    kvelil=vecas//'.LILI'
!
    nudev=nu
    if (nudev(1:1) .eq. ' ') then
        vprof=vecpro
        call jeveuo(vprof//'.REFE', 'L', idvref)
        nudev=zk24(idvref-1+2)(1:14)
    endif
!
!      --- TEST POUR SAVOIR SI LE SOLVEUR EST DE TYPE FETI
    call jelira(nudev(1:14)//'.NUME.REFN', 'LONMAX', nbrefn, kbid)
    if (nbrefn .ne. 4) then
        write (ifm,*)'<FETI/ASSVEC> NUME_DDL/CHAM_NO NON ETENDU '//&
        'POUR FETI',nudev(1:14)//'.NUME.REFN'
        method=' '
        sdfeti=' '
    else
        call jeveuo(nudev(1:14)//'.NUME.REFN', 'L', irefn)
        method=zk24(irefn+2)
        sdfeti=zk24(irefn+3)
        sdfets=sdfeti
        sdfeta=sdfets(1:19)//'.FETA'
    endif
    lfeti=(method(1:4).eq.'FETI')
!
!
! --- CALCUL D UN LILI POUR VECAS
! --- CREATION D'UN VECAS(1:19).ADNE ET VECAS(1:19).ADLI SUR 'V'
    call crelil('C', nbvec, ilivec, kvelil, 'V',&
                kmaila, vecas, gd, ma, nec,&
                ncmp, ilim, nlili, nbelm)
!
    if (nlili .eq. 1) then
        if (.not.lfeti) then
!         -- IL N'Y A AUCUN RESUELEM A ASSEMBLER MAIS IL PEUT
!            Y AVOIR DES CHAM_NO (VECT_ASSE):
            knueq=nudev//'.NUME.NUEQ'
            call jelira(knueq, 'LONMAX', nequa, kbid)
            call vtcreb(vecas, nu, bas, 'R', nequa)
            goto 270
!
        endif
    endif
!
!
    call jeveuo(vecas(1:19)//'.ADLI', 'E', iadlie)
    call jeveuo(vecas(1:19)//'.ADNE', 'E', iadnem)
    call jeexin(ma(1:8)//'.CONNEX', iret)
    if (iret .gt. 0) then
        call jeveuo(ma(1:8)//'.CONNEX', 'L', iconx1)
        call jeveuo(jexatr(ma(1:8)//'.CONNEX', 'LONCUM'), 'L', iconx2)
    endif
!
! --- ON SUPPOSE QUE LE LE LIGREL DE &MAILLA EST LE PREMIER DE LILINU
    ilimnu=1
!
!
    lfetic=.false.
    nbsd=0
    if (lfeti) then
        call jeveuo(sdfeti(1:19)//'.FDIM', 'L', idime)
! NOMBRE DE SOUS-DOMAINES
        nbsd=zi(idime)
! CONSTITUTION DE L'OBJET JEVEUX VECAS.FETC COMPLEMENTAIRE
        call wkvect(vecas//'.FETC', bas//' V K24', nbsd, ifetc)
        call jeveuo('&FETI.FINF', 'L', iinf)
        infofe=zk24(iinf)
        if (infofe(11:11) .eq. 'T') lfetic=.true.
! PREPARATION DE DONNEES AUXILIAIRES POUR TEST
        call fettsd(infofe, ibid, ibid, ibid, sdfeti(1:19),&
                    k24b, ibid, ibid, ibid, ifm,&
                    lbid, ibid, ibid, ibid, k19b,&
                    2, lbid)
    endif
!
! ------------------------------------------------------------------
!     -- SI LES CALCULS ONT ETE "DISTRIBUES" :
!        CALCUL DE :
!           * LDIST : .TRUE. : LES CALCULS ONT ETE DISTRIBUES
!           * LDGREL: .TRUE. : LA PARTITION EST DE TYPE 'GROUP_ELEM'
!           * JNUMSD : ADRESSE DE PARTIT//'.NUPROC.MAILLE'
!
!     -- IL EXISTE TROIS FORMES DE CALCUL DISTRIBUE BASES SUR UNE PARTI
!        TION:
!        * FETI: LE FLAG A ACTIVER EST LE LOGICAL LFETI,
!        * DISTRIBUE (AVEC OU SANS MUMPS) EN STD: FLAG LDIST
!        * DISTRIBUE AVEC MUMPS + OPTION MATR_DISTIBUEE: LDIST (PAS
!             CONCERNE ICI, ON NE RETAILLE QUE LES MATRICES)
!
!        AU SENS ASSVEC, LES DEUX DERNIERS CAS DE FIGURES SONT IDENTI
!        QUES. POUR PLUS D'INFO CF. COMMENTAIRES DS ASSMAM.
!
!         EN BREF ON A 4 CAS DE FIGURES DE CALCUL ASTER ET ILS SE DECLI
!         NENT COMME SUIT VIS-A-VIS DES VARIABLES DE ASSVEC:
!        1/ CALCUL STD SEQ PAS FETI:
!            LFETI='F',LDIST='F'
!        2/ CALCUL FETI SEQ OU PARALLELE (MATRICES MERE ET FILLES)
!            LFETI='T',LDIST='F'
!        3/ CALCUL PARALLELE (AVEC OU SANS MUMPS) DISTRIBUE STD:
!            LFETI='F',LDIST='T'
!        4/ CAS PARTICULIER DU PRECEDENT: SOLVEUR=MUMPS + OPTION MATR
!          DISTRIBUEE ACTIVEE     (PAS CONCERNE ICI)
!            LFETI='F',LDIST='T'
!
! ------------------------------------------------------------------
    ldist=.false.
    ldgrel=.false.
    rang=0
    nbproc=1
    call parti0(nbvec, tlivec, partit)
!     -- FETI N'EST PAS UN CALCUL DISTRIBUE. SES DONNEES SONT COMPLETES
!        POUR CHAQUE PROC
    if ((partit.ne.' ') .and. (.not.lfeti)) then
        ldist=.true.
        call mpicm0(rang, nbproc)
        call jeveuo(partit//'.PRTI', 'L', jprti)
        if (zi(jprti) .ne. nbproc) then
            vali(1)=zi(jprti)
            vali(2)=nbproc
            call u2mesi('F', 'CALCULEL_13', 2, vali)
        endif
!
        call jeveuo(partit//'.PRTK', 'L', jprtk)
        ldgrel=zk24(jprtk-1+1) .eq. 'GROUP_ELEM'
        if (.not.ldgrel) then
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', jnumsd)
        endif
    endif
!
    call dismoi('F', 'NOM_MODELE', nudev, 'NUME_DDL', ibid,&
                mo, ierd)
    call dismoi('F', 'NOM_MAILLA', nudev, 'NUME_DDL', ibid,&
                ma, ierd)
    call dismoi('F', 'NB_NO_SS_MAX', ma, 'MAILLAGE', nbnoss,&
                kbid, ierd)
!
!     100 EST SUPPOSE ETRE LA + GDE DIMENSION D'UNE MAILLE STANDARD:
    nbnoss=max(nbnoss,100)
!     -- NUMLOC(K,INO) (K=1,3)(INO=1,NBNO(MAILLE))
    call wkvect('&&ASSVEC.NUMLOC', 'V V I', 3*nbnoss, ianulo)
!
    call dismoi('F', 'NOM_GD', nudev, 'NUME_DDL', ibid,&
                nogdco, ierd)
    call dismoi('F', 'NOM_GD_SI', nogdco, 'GRANDEUR', ibid,&
                nogdsi, ierd)
    call dismoi('F', 'NB_CMP_MAX', nogdsi, 'GRANDEUR', nmxcmp,&
                kbid, ierd)
    call dismoi('F', 'NUM_GD_SI', nogdsi, 'GRANDEUR', nugd,&
                kbid, ierd)
    nec=nbec(nugd)
    ncmp=nmxcmp
!
    do 20 i = 1, nbecmx
        icodla(i)=0
        icodge(i)=0
20  end do
!
!     -- POSDDL(ICMP) (ICMP=1,NMXCMP(GD_SI))
    call wkvect('&&ASSVEC.POSDDL', 'V V I', nmxcmp, iapsdl)
!
!     -- ON PREPARE L'ASSEMBLAGE DES SOUS-STRUCTURES:
!     -----------------------------------------------
    call dismoi('F', 'NB_NO_MAILLA', mo, 'MODELE', nm,&
                kbid, ier)
!
    call jeexin(ma//'.NOMACR', iret)
    if (iret .gt. 0) then
        if (lfeti) call u2mesk('F', 'ASSEMBLA_12', 1, ma(1:8))
        call jeveuo(ma//'.NOMACR', 'L', ianmcr)
        call jeveuo(jexnom('&CATA.GD.NOMCMP', nogdsi), 'L', iancmp)
        call jelira(jexnom('&CATA.GD.NOMCMP', nogdsi), 'LONMAX', lgncmp, kbid)
        icmp=indik8(zk8(iancmp),'LAGR',1,lgncmp)
        if (icmp .eq. 0) call u2mess('F', 'ASSEMBLA_9')
        if (icmp .gt. 30) call u2mess('F', 'ASSEMBLA_10')
!       -- ICODLA EST L'ENTIER CODE CORRESPONDANT A LA CMP "LAGR"
        jec=(icmp-1)/30+1
        icodla(jec)=lshift(1,icmp)
    endif
!
    if (lfeti) then
!       -- ADRESSE JEVEUX DE LA LISTE DES NUME_DDL ASSOCIES
!          AUX SOUS-DOMAINES
        call jeveuo(nudev//'.FETN', 'L', ifetn)
!       -- STOCKE &&//NOMPRO(1:6)//'.2.' POUR COHERENCE AVEC L'EXISTANT
        k11b=vecas(1:10)//'.'
!       -- ADRESSE JEVEUX DE L'OBJET '&FETI.MAILLE.NUMSD'
        nomlog='&FETI.MAILLE.NUMSD'
        call jeveuo(nomlog, 'L', iligrp)
        iligrp=iligrp-1
!       -- ADRESSE JEVEUX OBJET AFFICHAGE CPU
        call jeveuo('&FETI.INFO.CPU.ASSE', 'E', ifcpu)
!       -- ADRESSE JEVEUX OBJET FETI & MPI
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
        call jeveuo('&FETI.LISTE.SD.MPIB', 'L', ilimpb)
        if (infofe(10:10) .eq. 'T') then
            nivmpi=2
        else
            nivmpi=1
        endif
        call fetmpi(2, ibid, ifm, nivmpi, rang,&
                    ibid, k24b, k24b, k24b, rbid)
    endif
!
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
! IDD=0 --> DOMAINE GLOBAL/ IDD=I --> IEME SOUS-DOMAINE
    do 260 idd = 0, nbsd
!
! TRAVAIL PREALABLE POUR DETERMINER SI ON EFFECTUE LA BOUCLE SUIVANT
! LE SOLVEUR (FETI OU NON), LE TYPE DE RESOLUTION (PARALLELE OU
! SEQUENTIELLE) ET L'ADEQUATION "RANG DU PROCESSEUR-NUMERO DU SD"
        if (.not.lfeti) then
            iddok=.true.
        else
            if (zi(ilimpi+idd) .eq. 1) then
                iddok=.true.
            else
                iddok=.false.
            endif
        endif
        if (iddok) then
!
            if (lfeti) call jemarq()
            if ((niv.ge.2) .or. lfetic) then
                call uttcpu('CPU.ASSVEC', 'INIT ', ' ')
                call uttcpu('CPU.ASSVEC', 'DEBUT', ' ')
            endif
            if (idd .eq. 0) then
                k24prn=nudev//'.NUME.PRNO'
                knulil=nudev//'.NUME.LILI'
                knueq=nudev//'.NUME.NUEQ'
            else
                k14b=zk24(ifetn+idd-1)(1:14)
                k24prn(1:14)=k14b
                knulil(1:14)=k14b
                knueq(1:14)=k14b
            endif
            call jeveuo(k24prn, 'L', idprn1)
            call jeveuo(jexatr(k24prn, 'LONCUM'), 'L', idprn2)
            call jeveuo(knueq, 'L', ianueq)
            call jelira(knueq, 'LONMAX', nequa, kbid)
!
!       --  REMPLISSAGE DES .REFE ET .DESC
            if (idd .eq. 0) then
!         -- SI NON FETI OU FETI DOMAINE GLOBAL
                kveref=vecas//'.REFE'
                kvale=vecas//'.VALE'
                kvedsc=vecas//'.DESC'
            else
!         -- SI SOUS-DOMAINE FETI
                call jenuno(jexnum(sdfeta, idd), nomsd)
!         K19B=K11B//NOMSD
!         NOUVELLE CONVENTION POUR LES CHAM_NOS FILS, GESTTION DE NOMS
!         ALEATOIRES
                call gcncon('.', kbid)
                kbid(1:1)='F'
                k19b=k11b(1:11)//kbid
                zk24(ifetc+idd-1)=k19b
                kveref(1:19)=k19b
                kvedsc(1:19)=k19b
                kvale(1:19)=k19b
                call jeveuo(k14b//'.NUME.REFN', 'L', irefn)
                method=zk24(irefn+2)
                sdfeti=zk24(irefn+3)
            endif
            call jecreo(kveref, bas//' V K24')
            call jeecra(kveref, 'LONMAX', 4, ' ')
            call jeveuo(kveref, 'E', idverf)
            call jecreo(kvedsc, bas//' V I')
            call jeecra(kvedsc, 'LONMAX', 2, ' ')
            call jeecra(kvedsc, 'DOCU', ibid, 'CHNO')
            call jeveuo(kvedsc, 'E', idveds)
            zk24(idverf)=ma
            zk24(idverf+1)=k24prn(1:14)//'.NUME'
            if (method .eq. 'FETI') then
                zk24(idverf+2)='FETI'
                zk24(idverf+3)=sdfeti
            endif
            zi(idveds)=gd
            zi(idveds+1)=1
!
!      -- ALLOCATION .VALE EN R OU C SUIVANT TYPE
            if (type .eq. 1) then
                call jecreo(kvale, bas//' V R8')
            else if (type.eq.2) then
                call jecreo(kvale, bas//' V C16')
            else
                call u2mess('F', 'ASSEMBLA_11')
            endif
            call jeecra(kvale, 'LONMAX', nequa, ' ')
            call jeveuo(kvale, 'E', jvale)
!
!       -- PREPARATION DE DONNEES AUXILIAIRES POUR TEST
            lgoto=.false.
            k24b(1:14)=nudev
            call fettsd(infofe, idd, nequa, ibid, sdfeti(1:19),&
                        k24b, ifetn, jvale, ibid, ifm,&
                        lbid, ibid, ibid, ibid, k19b,&
                        4, lgoto)
            if (lgoto) goto 250
!
!       ==========================
!        BOUCLE SUR LES VECT_ELEM
!       ==========================
            do 240 imat = 1, nbvec
                rcoef=licoef(imat)
                vecel=zk24(ilivec+imat-1)(1:19)
                call dismoi('F', 'NOM_MODELE', vecel, 'VECT_ELEM', ibid,&
                            mo2, ierd)
                if (mo2 .ne. mo) call u2mess('F', 'ASSEMBLA_5')
!
!         -- TRAITEMENT DES SOUS-STRUCTURES :
!         -----------------------------------
                call dismoi('F', 'EXI_ELEM', mo, 'MODELE', ibid,&
                            exiele, ierd)
                call dismoi('F', 'NB_SS_ACTI', vecel, 'VECT_ELEM', nbssa,&
                            kbid, ierd)
                if (nbssa .gt. 0) then
                    nomcas=' '
                    call dismoi('F', 'NB_SM_MAILLA', mo, 'MODELE', nbsma,&
                                kbid, ierd)
                    call dismoi('F', 'NOM_MAILLA', mo, 'MODELE', ibid,&
                                ma, ierd)
                    call jeveuo(mo//'.MODELE    .SSSA', 'L', iasssa)
                    call ssvalv('DEBUT', nomcas, mo, ma, 0,&
                                jresl, ncmpel)
                    call jelira(vecel//'.RELC', 'NUTIOC', nbchar, kbid)
!
                    do 90 ichar = 1, nbchar
                        call jenuno(jexnum(vecel//'.RELC', ichar), nomcas)
                        call jeveuo(jexnum(vecel//'.RELC', ichar), 'L', ialcha)
!
                        do 80 ima = 1, nbsma
!             -- ON N'ASSEMBLE QUE LES SSS VRAIMENT ACTIVES :
                            if (zi(iasssa-1+ima) .eq. 0) goto 80
                            if (zi(ialcha-1+ima) .eq. 0) goto 80
                            call jeveuo(jexnum(ma//'.SUPMAIL', ima), 'L', iamail)
                            call jelira(jexnum(ma//'.SUPMAIL', ima), 'LONMAX', nnoe, kbid)
                            call ssvalv(' ', nomcas, mo, ma, ima,&
                                        jresl, ncmpel)
                            nomacr=zk8(ianmcr-1+ima)
                            call dismoi('F', 'NOM_NUME_DDL', nomacr, 'MACR_ELEM_STAT', ibid,&
                                        num2, ierd)
                            call jeveuo(nomacr//'.CONX', 'L', iaconx)
                            call jeveuo(jexnum(num2//'.NUME.PRNO', 1), 'L', iaprol)
                            il=0
                            do 70 k1 = 1, nnoe
                                n1=zi(iamail-1+k1)
                                if (n1 .gt. nm) then
                                    do 30 iec = 1, nbecmx
                                        icodge(iec)=icodla(iec)
30                                  continue
                                else
                                    inold=zi(iaconx-1+3*(k1-1)+2)
                                    do 40 iec = 1, nec
                                        icodge(iec)=zi(iaprol-1+(nec+&
                                        2)*(inold-1)+2+iec)
40                                  continue
                                endif
!
                                iad1=zi(idprn1-1+zi(idprn2+ilimnu-1)+(&
                                n1-1)*(nec+2))
                                call cordd2(idprn1, idprn2, ilimnu, icodge, nec,&
                                            ncmp, n1, nddl1, zi(iapsdl))
!
                                if (type .eq. 1) then
                                    do 50 i1 = 1, nddl1
                                        il=il+1
                                        zr(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+i1)- 1))=zr(&
                                        jvale-1+zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zr(jresl+il-&
                                        1)*rcoef
50                                  continue
                                else if (type.eq.2) then
                                    do 60 i1 = 1, nddl1
                                        il=il+1
                                        zc(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+i1)- 1))=zc(&
                                        jvale-1+zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zc(jresl+il-&
                                        1)*rcoef
60                                  continue
                                endif
70                          continue
80                      continue
90                  continue
                    call ssvalv('FIN', nomcas, mo, ma, 0,&
                                jresl, ncmpel)
                endif
!
!
!         -- TRAITEMENT DES ELEMENTS FINIS CLASSIQUES :
!         ---------------------------------------------
                call jeexin(vecel//'.RELR', iret)
                if (iret .gt. 0) then
!
!
!           ==========================
!            BOUCLE SUR LES RESU_ELEM
!           ==========================
                    call jelira(vecel//'.RELR', 'LONUTI ', nbresu, k1bid)
                    if (nbresu .gt. 0) call jeveuo(vecel//'.RELR', 'L', idlres)
                    do 230 iresu = 1, nbresu
                        resu=zk24(idlres+iresu-1)(1:19)
                        call jeexin(resu//'.NOLI', iexi)
                        if (iexi .eq. 0) goto 230
                        call jeveuo(resu//'.NOLI', 'L', iad)
!             NOM DU LIGREL GLOBAL
                        nomli=zk24(iad)
!
!             -- POUR FETI & LIGREL TARDIF: DEBUT
!                RECHERCHE D'OBJET TEMPORAIRE SI FETI
!                PAR DEFAUT LIGREL DE MODELE
                        llimo=.true.
                        llich=.false.
                        llichd=.false.
                        llichp=.false.
                        if (lfeti) then
                            nomlog=nomli(1:19)//'.FEL1'
                            call jeexin(nomlog, iret1)
                            if (iret1 .ne. 0) then
!                 -- LIGREL DE CHARGE A MAILLES TARDIVES
                                call jeveuo(nomlog, 'L', ifel1)
                                llich=.true.
                                llimo=.false.
                                if (idd .eq. 0) then
                                    lsaute=.true.
                                    call jelira(nomlog, 'LONMAX', nblog, kbid)
                                    do 100 i = 1, nblog
                                        if (zk24(ifel1-1+i) .ne. ' ') lsaute=.false.
100                                  continue
                                else
                                    lsaute=.false.
                                    if (zk24(ifel1-1+idd) .eq. ' ') lsaute=.true.
                                endif
!                 -- LIGREL NE CONCERNANT PAS LE SOUS-DOMAINE IDD
!                    OU LE DOMAINE GLOBAL
                                if (lsaute) goto 230
!
                                call jeexin(nomli(1:19)//'.FEL2', iret2)
                                if (iret2 .ne. 0) then
! LIGREL DE CHARGE A MAILLES TARDIVES DUPLIQUEES DE FILS NOMLID
! DDL_IMPO, FORCE_NODALE...
                                    llichd=.true.
! VRAI NOM DU LIGREL DUPLIQUE CONTENU DANS PROF_CHNO.LILI LOCAL
                                    if (idd .ne. 0) nomlid=zk24(ifel1-1+ idd)
                                    call jeveuo(nomli(1:19)//'.FEL2', 'L', ifel2)
                                    call jeexin(nomli(1:19)//'.FEL3', iret3)
                                    if (iret3 .ne. 0) then
                                        call jeveuo(nomli(1:19)// '.FEL3', 'L', ifel3)
! LIGREL DE CHARGE A NOEUDS TARDIFS DUPLIQUES (DDL_IMPO...)
                                        llichp=.true.
                                    else
! PAS DE NOEUD TARDIF DUPLIQUE (FORCE_NODALE)
                                        llichp=.false.
                                    endif
                                    call jeexin(nomli(1:19)//'.FEL4', iret3)
                                    if (iret3 .ne. 0) call jeveuo(nomli(1:19 )//'.FEL4', 'L',&
                                                                  ifel4)
                                    call jeexin(nomli(1:19)//'.FEL5', iret3)
                                    if (iret3 .ne. 0) call jeveuo(nomli(1:19 )//'.FEL5', 'L',&
                                                                  ifel5)
                                else
! LIGREL DE CHARGE NON DUPLIQUE
                                    llichd=.false.
                                endif
                            else
! LIGREL DE MODELE
                                llimo=.true.
                            endif
                        endif
!             -- POUR FETI & LIGREL TARDIF: FIN
!
! ILIVE: INDICE DANS LIST_RESU (GLOBAL) DES VECT_ELEM.LILI DU NOMLI
! ILINU: INDICE DANS PROF_CHNO.LILI (GLOBAL OU LOCAL) DU NOMLI
                        call jenonu(jexnom(kvelil, nomli), ilive)
                        if (llichd .and. (idd.ne.0)) then
                            call jenonu(jexnom(knulil, nomlid), ilinu)
                        else
                            call jenonu(jexnom(knulil, nomli), ilinu)
                        endif
!
!             ==========================
!              BOUCLE SUR LES GRELS DU LIGREL GLOBAL NOMLI/ILIMA
!             ==========================
                        do 220 igr = 1, zi(iadlie+3*(ilive-1))
                            if (ldgrel .and. mod(igr,nbproc) .ne. rang) goto 220
!
!               -- IL SE PEUT QUE LE GREL IGR SOIT VIDE :
                            call jaexin(jexnum(resu//'.RESL', igr), iexi)
                            if (iexi .eq. 0) goto 220
!
                            call jeveuo(resu//'.DESC', 'L', iddesc)
                            mode=zi(iddesc+igr+1)
!
                            if (mode .gt. 0) then
                                nnoe=nbno(mode)
!                 -- NOMBRE D'ELEMENTS DU GREL IGR
                                nel=zi(zi(iadlie+3*(ilive-1)+2)+igr)-&
                                zi(zi(iadlie+3*(ilive-1)+2)+igr-1)-1
                                call jeveuo(jexnum(resu//'.RESL', igr), 'L', jresl)
                                ncmpel=digdel(mode)
!
!                  =========================
!                  BOUCLE SUR LES ELEMENTS DU GREL IGR
!                  =========================
                                do 210 iel = 1, nel
!                   NUMA : NUMERO DE LA MAILLE
                                    numa=zi(zi(iadlie+3*(ilive-1)+1)-&
                                    1+ zi(zi(iadlie+3*(ilive-1)+2)+&
                                    igr-1)+iel-1)
!
!                   -- MONITORING
                                    if ((infofe(5:5).eq.'T') .and. lfeti) then
                                        write (ifm,*)'<FETI/ASSVEC>',&
                                        'IDD',idd,'LIGREL', nomli,&
                                        'ILIVE',ilive,'RANG',rang
                                        write (ifm,*)'IGR',igr,'IEL',&
                                        iel,'NUMA',numa
                                        if (llimo) write (ifm, *) '.LOGI', zi(iligrp+abs(numa))
                                        if (llich) then
                                            if (llichd) then
                                                write (ifm,*)'LIGREL DE CHARGE PROJETE '//&
     &                        'DE FILS ',nomlid
                                            else
                                                write (ifm,*)'LIGREL DE CHARGE INITIAL'
                                            endif
                                            write (ifm,*)'MAILLE ET/OU NOEUD TARDIF'
                                        endif
                                    endif
!
                                    r=rcoef
!
                                    if (lfeti) then
! SI ON EST DANS UN CALCUL FETI SUR UN SOUS-DOMAINE, ON SE POSE LA
! QUESTION DE L'APPARTENANCE DE LA MAILLE NUMA AU SOUS-DOMAINE IDD
                                        if (numa .gt. 0) then
                                            if (llich) call u2mess('F', 'ASSEMBLA_6')
! ELLE APPARTIENT AU GREL IGR DU LIGREL PHYSIQUE ILIMA
                                            if (idd .ne. 0) then
! CHAQUE PROC ASSEMBLE LA PARTIE PHYSIQUE DES SECONDS MEMBRES FETI LE
! CONCERNANT
                                                if (zi(iligrp+numa) .ne. idd) goto 210
                                            else
! IDEM POUR LA PARTIE PHYSIQUE DU CHAM_NO GLOBAL
                                                ibid=zi(iligrp+numa)
                                                if (ibid .gt. 0) then
                                                    if (zi(ilimpb+ibid-1) .ne. rang) goto 210
                                                endif
                                            endif
                                        else
! ELLE APPARTIENT AU GREL IGR DU LIGREL TARDIF ILIMA
                                            if (llimo) call u2mess('F', 'ASSEMBLA_7')
                                        endif
                                    endif
!
!
                                    if (ldist .and. .not.ldgrel) then
                                        if (numa .gt. 0) then
                                            if (zi(jnumsd-1+numa) .ne. rang) goto 210
                                        else
                                            if (rang .ne. 0) goto 210
                                        endif
                                    endif
!
                                    if (numa .gt. 0) then
                                        il=0
                                        do 130 k1 = 1, nnoe
                                            n1=zi(iconx1-1+zi(iconx2+numa-&
                                        1)+k1-1)
                                            iad1=zi(idprn1-1+zi(idprn2+&
                                        ilimnu-1)+ (n1-1)*(nec+2)+1-1)
                                            call corddl(admodl, lcmodl, idprn1, idprn2, ilimnu,&
                                                        mode, nec, ncmp, n1, k1,&
                                                        nddl1, zi( iapsdl))
                                            if (nddl1 .eq. 0) goto 130
                                            if (iad1 .eq. 0) then
                                                vali(1)=n1
                                                valk(1)=resu
                                                valk(2)=vecel
                                                valk(3)=nudev
                                                call u2mesg('F', 'ASSEMBLA_41', 3, valk, 1,&
                                                            vali, 0, 0.d0)
                                            endif
!
                                            if (iad1 .gt. nequa) then
                                                vali(1)=n1
                                                vali(2)=iad1
                                                vali(3)=nequa
                                                valk(1)=resu
                                                valk(2)=vecel
                                                call u2mesg('F', 'ASSEMBLA_42', 2, valk, 3,&
                                                            vali, 0, 0.d0)
                                            endif
!
                                            if (nddl1 .gt. 100) then
                                                vali(1)=nddl1
                                                vali(2)=100
                                                call u2mesg('F', 'ASSEMBLA_43', 0, ' ', 2,&
                                                            vali, 0, 0.d0)
                                            endif
!
                                            if (type .eq. 1) then
                                                do 110 i1 = 1, nddl1
                                                    il=il+1
                                                    zr(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+ i1)-1))=zr(&
                                        jvale-1+ zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zr(jresl+(&
                                        iel-1)*ncmpel+ il-1)*r
110                                              continue
!
                                            else
                                                do 120 i1 = 1, nddl1
                                                    il=il+1
                                                    zc(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+ i1)-1))=zc(&
                                        jvale-1+ zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zc(jresl+(&
                                        iel-1)*ncmpel+ il-1)*r
120                                              continue
                                            endif
!
130                                      continue
!
                                    else
!                     -- MAILLE TARDIVE:
!                     -------------------
                                        numa=-numa
!
!--------- POUR FETI & LIGREL TARDIF: DEBUT
! SI POUR FETI, MAILLE TARDIVE DUPLIQUEE, ON SE POSE LA QUESTION DE
! L'APPARTENANCE DE CETTE MAILLE TARDIVE AU SOUS-DOMAINE IDD VIA
! L'OBJET .FEL2 (C'EST LE PENDANT DE &FETI.MAILLE.NUMSD POUR LES
! MAILLES DU MODELE)
                                        if (llichd) then
! LFEL2=.TRUE. ON ASSEMBLE LES CONTRIBUTIONS DE CETTE MAILLE TARDIVE
! LFEL2=.FALSE. ON LA SAUTE
                                            lfel2=.false.
                                            iaux1=zi(ifel2+2*(numa-1)+1)
! C'EST UNE MAILLE TARDIVE NON SITUEE SUR UNE INTERFACE
                                            if (iaux1 .gt. 0) then
                                                if (idd .ne. 0) then
! ELLE CONCERNE LE SD, ON L'ASSEMBLE
                                                    if (iaux1 .eq. idd) lfel2=.true.
                                                else
                                                    if (zi(ilimpb+iaux1-1) .eq. rang) lfel2=.true&
                                                                                      &.
                                                endif
! C'EST UNE MAILLE TRADIVE SITUEE SUR UNE INTERFACE, DONC PARTAGEE
! ENTRE PLUSIEURS SOUS-DOMAINES
                                            else if (iaux1.lt.0) then
                                                compt=0
                                                iaux2=(zi(ifel4)/3)-1
                                                do 140 jfel4 = 0, iaux2
                                                    iaux3=ifel4+3*jfel4+3
                                                    if (zi(iaux3) .eq. numa) then
                                                        compt=compt+1
                                                        if (zi(iaux3-1) .eq. idd) then
! ELLE CONCERNE LE SD, ON L'ASSEMBLE
                                                            lfel2=.true.
                                                            goto 150
!
                                                        endif
! ON A LU TOUTES LES VALEURS POSSIBLES, ON SORT DE LA BOUCLE
                                                        if (compt .eq. -iaux1) goto 150
                                                    endif
140                                              continue
150                                              continue
                                            endif
! ON SAUTE LA CONTRIBUTION
                                            if (.not.lfel2) goto 210
!--------- POUR FETI & LIGREL TARDIF: FIN
                                        endif
!
!
!                     -- N1 : NBRE DE NOEUDS DE LA MAILLE NUMA
                                        n1=zi(zi(iadnem+3*(ilive-1)+2)&
                                        +numa)- zi(zi(iadnem+3*(&
                                        ilive-1)+2)+numa-1)-1
                                        if (nnoe .ne. n1) then
                                            valk(1)=vecel
                                            valk(2)=resu
                                            valk(3)=nomli
                                            vali(1)=igr
                                            vali(2)=numa
                                            vali(3)=n1
                                            vali(4)=nnoe
                                            call u2mesg('F', 'ASSEMBLA_44', 3, valk, 4,&
                                                        vali, 0, 0.d0)
                                        endif
                                        il=0
                                        do 200 k1 = 1, nnoe
! N1 : INDICE DU NOEUDS DS LE .NEMA DU LIGREL DE CHARGE GLOBAL OU LOCAL
                                            n1=zi(zi(iadnem+3*(ilive-1)+1)&
                                        -1+ zi(zi(iadnem+3*(ilive-1)+&
                                        2)+numa-1)+k1-1)
! NOEUD TARDIF
                                            if (n1 .lt. 0) then
                                                n1=-n1
!
!--------- POUR FETI & LIGREL TARDIF: DEBUT
! SI POUR FETI, NOEUD TARDIF DUPLIQUE, VERITABLE N1 DANS LE LIGREL DUPL
                                                if (llichp .and. (idd.ne.0)) then
                                                    iaux1=zi(ifel3+2*(n1-1)+1)
                                                    if (iaux1 .gt. 0) then
! C'EST UN NOEUD TARDIF LIE A UN DDL PHYSIQUE NON SUR L'INTERFACE
                                                        n1=-zi(ifel3+2*(n1-1))
                                                    else if (iaux1.lt.0) then
! C'EST UN NOEUD TARDIF LIE A UN DDL PHYSIQUE DE L'INTERFACE
                                                        iaux2=(zi(ifel5)/3)-1
                                                        do 160 jfel4 = 0, iaux2
                                                            iaux3=ifel5+3*jfel4+3
                                                            if (zi(iaux3) .eq. n1) then
                                                                if (zi( iaux3-1 ) .eq. idd) then
! VOICI SON NUMERO LOCAL CONCERNANT LE SD
                                                                    n1=-zi(iaux3-2)
                                                                    goto 170
!
                                                                endif
                                                            endif
160                                                      continue
170                                                      continue
                                                    endif
!                         -- POUR FETI & LIGREL TARDIF: FIN
                                                endif
!
                                                if (ilinu .eq. 0) then
                                                    valk(1)=nomli
                                                    valk(2)=resu
                                                    valk(3)=vecel
                                                    valk(4)=nudev
                                                    valk(5)=nomli(1:8)
                                                    vali(1)=n1
                                                    vali(2)=numa
                                                    call u2mesg('F', 'ASSEMBLA_45', 5, valk, 2,&
                                                                vali, 0, 0.d0)
                                                endif
!
!                         -- NUMERO D'EQUATION DU PREMIER DDL DE N1
                                                iad1=zi(idprn1-1+zi(idprn2+&
                                        ilinu-1)+ (n1-1)*(nec+2)+1-1)
                                                call corddl(admodl, lcmodl, idprn1, idprn2,&
                                                            ilinu, mode, nec, ncmp, n1,&
                                                            k1, nddl1, zi(iapsdl))
                                                if (nddl1 .gt. 100) then
                                                    vali(1)=nddl1
                                                    vali(2)=100
                                                    call u2mesg('F', 'ASSEMBLA_46', 0, ' ', 2,&
                                                                vali, 0, 0.d0)
                                                endif
                                            else
! NOEUD PHYSIQUE
!
                                                iad1=zi(idprn1-1+zi(idprn2+&
                                        ilimnu-1)+ (n1-1)*(nec+2)+1-1)
                                                call corddl(admodl, lcmodl, idprn1, idprn2,&
                                                            ilimnu, mode, nec, ncmp, n1,&
                                                            k1, nddl1, zi( iapsdl))
                                                if (nddl1 .gt. 100) then
                                                    vali(1)=nddl1
                                                    vali(2)=100
                                                    call u2mesg('F', 'ASSEMBLA_47', 0, ' ', 2,&
                                                                vali, 0, 0.d0)
                                                endif
                                            endif
                                            if (iad1 .eq. 0) then
                                                vali(1)=n1
                                                valk(1)=resu
                                                valk(2)=vecel
                                                valk(3)=nudev
                                                call u2mesg('F', 'ASSEMBLA_48', 3, valk, 1,&
                                                            vali, 0, 0.d0)
                                            endif
                                            if (iad1 .gt. nequa) then
                                                vali(1)=n1
                                                vali(2)=iad1
                                                vali(3)=nequa
                                                valk(1)=resu
                                                valk(2)=vecel
                                                call u2mesg('F', 'ASSEMBLA_49', 2, valk, 3,&
                                                            vali, 0, 0.d0)
                                            endif
                                            if (type .eq. 1) then
                                                do 180 i1 = 1, nddl1
                                                    il=il+1
                                                    zr(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+ i1)-1))=zr(&
                                        jvale-1+ zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zr(jresl+(&
                                        iel-1)*ncmpel+ il-1)*r
180                                              continue
                                            else
                                                do 190 i1 = 1, nddl1
                                                    il=il+1
                                                    zc(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+ i1)-1))=zc(&
                                        jvale-1+ zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zc(jresl+(&
                                        iel-1)*ncmpel+ il-1)*r
190                                              continue
                                            endif
200                                      continue
                                    endif
210                              continue
                                call jelibe(jexnum(resu//'.RESL', igr))
                            endif
220                      continue
230                  continue
                endif
240          continue
250          continue
!
!
!       -- MONITORING
            if (lfeti .and. (infofe(1:1).eq.'T')) then
                write (ifm,*)
                write (ifm,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
                if (idd .eq. 0) then
                    write (ifm,*)'<FETI/ASSVEC> DOMAINE GLOBAL'
                else
                    write (ifm,*)'<FETI/ASSVEC>NUMERO DE SOUS-DOMAINE: ',idd
                endif
                write (ifm,*)'<FETI/ASSVEC> REMPLISSAGE OBJETS JEVEUX ',&
     &        kvale(1:19)
                write (ifm,*)
            endif
            if ((infofe(3:3).eq.'T') .and. (idd.ne.0)) call utimsd(ifm, 2, .false., .true.,&
                                                                   kvale(1:19), 1, ' ')
            if ((infofe(3:3).eq.'T') .and. (idd.eq.nbsd)) call utimsd(ifm, 2, .false., .true.,&
                                                                      vecas(1:19), 1, ' ')
!
            if ((niv.ge.2) .or. lfetic) then
                call uttcpu('CPU.ASSVEC', 'FIN', ' ')
                call uttcpr('CPU.ASSVEC', 6, temps)
                if (niv .ge. 2) write (ifm, '(A44,D11.4,D11.4)'&
                                ) 'TEMPS CPU/SYS ASSEMBLAGE V                : ',&
                                temps(5), temps(6)
                if (lfetic) zr(ifcpu+idd)=zr(ifcpu+idd)+temps(5)+temps( 6)
            endif
            if (lfeti) call jedema()
!
        endif
260  end do
!
!
!
!
!     -- REDUCTION + DIFFUSION DE VECAS A TOUS LES PROC
    if (ldist) call mpicm2('MPI_SUM', kvale)
!
!
!
270  continue
!     -- LES VECT_ELEM PEUVENT CONTENIR DES CHAM_NO (VECT_ASSE)
!        IL FAUT LES CUMULER DANS KVALE :
!     ----------------------------------------------------------
    kvale=vecas//'.VALE'
    do 300,i=1,nbvec
    a19=tlivec(i)
    call jeexin(a19//'.RELR', iexi)
    if (iexi .eq. 0) goto 300
    call jeveuo(a19//'.RELR', 'L', jrelr)
    call jelira(a19//'.RELR', 'LONUTI', n1, kbid)
    do 290,k=1,n1
    b19=zk24(jrelr-1+k)(1:19)
    call jeexin(b19//'.VALE', iexi)
    if (iexi .gt. 0) then
        call assert(.not.lfeti)
        call jeveuo(kvale, 'E', jvale1)
        call jelira(kvale, 'TYPE', ibid, ktyp)
        call assert(ktyp.eq.'R')
        call assert(type.eq.1)
        c19='&&ASSVEC.CHAMNO'
        call vtcreb(c19, nu, 'V', ktyp, nequa)
!
        call vtcopy(b19, c19, 'F', iret)
        call jeveuo(c19//'.VALE', 'L', jvale2)
        do 280,j=1,nequa
        zr(jvale1-1+j)=zr(jvale1-1+j)+zr(jvale2-1+j)
280      continue
        call detrsd('CHAMP_GD', c19)
    endif
290  continue
    300 end do
!
!
    call jedetr(vecas//'.LILI')
    call jedetr(vecas//'.LIVE')
    call jedetr(vecas//'.ADNE')
    call jedetr(vecas//'.ADLI')
    call jedetr('&&ASSVEC.POSDDL')
    call jedetr('&&ASSVEC.NUMLOC')
!
!
    call mpicm1('BARRIER', ' ', ibid, ibid, ibid,&
                rbid, cbid)
    call uttcpu('CPU.CALC.1', 'FIN', ' ')
    call uttcpu('CPU.ASSE.1', 'FIN', ' ')
    call uttcpu('CPU.ASSE.3', 'FIN', ' ')
    call jedema()
end subroutine

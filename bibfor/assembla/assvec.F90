subroutine assvec(base, vec, nbvec, tlivec, licoef,&
                  nu, vecpro, motcle, type)
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
! aslint: disable=W1501
#include "aster_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/asmpi_barrier.h"
#include "asterfort/asmpi_comm_jev.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/cordd2.h"
#include "asterfort/corddl.h"
#include "asterfort/crelil.h"
#include "asterfort/dbgobj.h"
#include "asterfort/detrsd.h"
#include "asterfort/digdel.h"
#include "asterfort/dismoi.h"
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
#include "asterfort/nbec.h"
#include "asterfort/nbno.h"
#include "asterfort/parti0.h"
#include "asterfort/ssvalv.h"
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
!
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
!     variables locales
! ---------------------------------------------------------------------
    integer :: nbecmx
    parameter(nbecmx=10)
!
    character(len=1) :: bas, ktyp
    character(len=8) :: kbid, ma, mo, mo2, nogdsi, nogdco, nomcas, partit
    character(len=14) :: nudev
    character(len=19) :: vecas, vprof, vecel, a19, b19, c19, resu
    character(len=24) :: kmaila, k24prn, knueq
    character(len=24) :: knulil, kvelil, kveref, kvedsc, nomli, kvale
    logical :: ldist, ldgrel, dbg
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
    integer :: icodla(nbecmx), icodge(nbecmx), lshift
    integer :: admodl, lcmodl, ibid, ifm, niv, rang, nbproc, jprti, jprtk
!
    real(kind=8) :: temps(6)
    integer :: vali(4)
    mpi_int :: mrank, msize
!
! --- DEBUT ------------------------------------------------------------
    call jemarq()
!
    call asmpi_barrier()
    call uttcpu('CPU.CALC.1', 'DEBUT', ' ')
    call uttcpu('CPU.ASSE.1', 'DEBUT', ' ')
    call uttcpu('CPU.ASSE.3', 'DEBUT', ' ')
!
!-----RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, niv)
!
!
!     IFM = IUNIFI('MESSAGE')
!----------------------------------------------------------------------
!
    vecas=vec
    bas=base
!
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', lcmodl)
    call jeveuo(jexnum('&CATA.TE.MODELOC', 1), 'L', admodl)
    ASSERT(motcle.eq.'ZERO')
!
!
! --- SI LE CONCEPT VECAS EXISTE DEJA, ON LE DETRUIT:
    call detrsd('CHAMP_GD', vecas)
    call wkvect(vecas//'.LIVE', bas//' V K24 ', nbvec, ilivec)
    do i = 1, nbvec
        zk24(ilivec-1+i)=tlivec(i)
    end do
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
!
! --- CALCUL D UN LILI POUR VECAS
! --- CREATION D'UN VECAS(1:19).ADNE ET VECAS(1:19).ADLI SUR 'V'
    call crelil('C', nbvec, ilivec, kvelil, 'V',&
                kmaila, vecas, gd, ma, nec,&
                ncmp, ilim, nlili, nbelm)
!
    if (nlili .eq. 1) then
!         -- IL N'Y A AUCUN RESUELEM A ASSEMBLER MAIS IL PEUT
!            Y AVOIR DES CHAM_NO (VECT_ASSE):
        knueq=nudev//'.NUME.NUEQ'
        call jelira(knueq, 'LONMAX', nequa)
        call vtcreb(vecas, nu, bas, 'R', nequa)
        goto 270
!
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
!
!
! ------------------------------------------------------------------
!     -- SI LES CALCULS ONT ETE "DISTRIBUES" :
!        CALCUL DE :
!           * LDIST : .TRUE. : LES CALCULS ONT ETE DISTRIBUES
!           * LDGREL: .TRUE. : LA PARTITION EST DE TYPE 'GROUP_ELEM'
!           * JNUMSD : ADRESSE DE PARTIT//'.NUPROC.MAILLE'
!
!     -- IL EXISTE DEUX FORMES DE CALCUL DISTRIBUE BASES SUR UNE PARTI
!        TION:
!        * DISTRIBUE (AVEC OU SANS MUMPS) EN STD: FLAG LDIST
!        * DISTRIBUE AVEC MUMPS + OPTION MATR_DISTIBUEE: LDIST (PAS
!             CONCERNE ICI, ON NE RETAILLE QUE LES MATRICES)
!
!        AU SENS ASSVEC, LES DEUX DERNIERS CAS DE FIGURES SONT IDENTI
!        QUES. POUR PLUS D'INFO CF. COMMENTAIRES DS ASSMAM.
!
!         EN BREF ON A 3 CAS DE FIGURES DE CALCUL ASTER ET ILS SE DECLI
!         NENT COMME SUIT VIS-A-VIS DES VARIABLES DE ASSVEC:
!        1/ CALCUL STD SEQ :
!            LDIST='F'
!        2/ CALCUL PARALLELE (AVEC OU SANS MUMPS) DISTRIBUE STD:
!            LDIST='T'
!        3/ CAS PARTICULIER DU PRECEDENT: SOLVEUR=MUMPS + OPTION MATR
!          DISTRIBUEE ACTIVEE     (PAS CONCERNE ICI)
!            LDIST='T'
!
! ------------------------------------------------------------------
    ldist=.false.
    ldgrel=.false.
    rang=0
    nbproc=1
    call parti0(nbvec, tlivec, partit)
!
    if (partit .ne. ' ') then
        ldist=.true.
        call asmpi_info(rank=mrank, size=msize)
        rang = to_aster_int(mrank)
        nbproc = to_aster_int(msize)
        call jeveuo(partit//'.PRTI', 'L', jprti)
        if (zi(jprti) .ne. nbproc) then
            vali(1)=zi(jprti)
            vali(2)=nbproc
            call utmess('F', 'CALCULEL_13', ni=2, vali=vali)
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
    do i = 1, nbecmx
        icodla(i)=0
        icodge(i)=0
    end do
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
        call jeveuo(ma//'.NOMACR', 'L', ianmcr)
        call jeveuo(jexnom('&CATA.GD.NOMCMP', nogdsi), 'L', iancmp)
        call jelira(jexnom('&CATA.GD.NOMCMP', nogdsi), 'LONMAX', lgncmp)
        icmp=indik8(zk8(iancmp),'LAGR',1,lgncmp)
        if (icmp .eq. 0) then
            call utmess('F', 'ASSEMBLA_9')
        endif
        if (icmp .gt. 30) then
            call utmess('F', 'ASSEMBLA_10')
        endif
!       -- ICODLA EST L'ENTIER CODE CORRESPONDANT A LA CMP "LAGR"
        jec=(icmp-1)/30+1
        icodla(jec)=lshift(1,icmp)
    endif
!
!
!
    if (niv .ge. 2) then
        call uttcpu('CPU.ASSVEC', 'INIT ', ' ')
        call uttcpu('CPU.ASSVEC', 'DEBUT', ' ')
    endif
!
    k24prn=nudev//'.NUME.PRNO'
    knulil=nudev//'.NUME.LILI'
    knueq=nudev//'.NUME.NUEQ'
!
    call jeveuo(k24prn, 'L', idprn1)
    call jeveuo(jexatr(k24prn, 'LONCUM'), 'L', idprn2)
    call jeveuo(knueq, 'L', ianueq)
    call jelira(knueq, 'LONMAX', nequa)
!
    kveref=vecas//'.REFE'
    kvale=vecas//'.VALE'
    kvedsc=vecas//'.DESC'
!
    call jecreo(kveref, bas//' V K24')
    call jeecra(kveref, 'LONMAX', 4)
    call jeveuo(kveref, 'E', idverf)
    call jecreo(kvedsc, bas//' V I')
    call jeecra(kvedsc, 'LONMAX', 2)
    call jeecra(kvedsc, 'DOCU', cval='CHNO')
    call jeveuo(kvedsc, 'E', idveds)
    zk24(idverf)=ma
    zk24(idverf+1)=k24prn(1:14)//'.NUME'
    zi(idveds)=gd
    zi(idveds+1)=1
!
!      -- ALLOCATION .VALE EN R OU C SUIVANT TYPE
    if (type .eq. 1) then
        call jecreo(kvale, bas//' V R8')
    else if (type.eq.2) then
        call jecreo(kvale, bas//' V C16')
    else
        call utmess('F', 'ASSEMBLA_11')
    endif
    call jeecra(kvale, 'LONMAX', nequa)
    call jeveuo(kvale, 'E', jvale)
!
!
!   ==========================
!    BOUCLE SUR LES VECT_ELEM
!   ==========================
    do imat = 1, nbvec
        rcoef=licoef(imat)
        vecel=zk24(ilivec+imat-1)(1:19)
        call dismoi('F', 'NOM_MODELE', vecel, 'VECT_ELEM', ibid,&
                    mo2, ierd)
        if (mo2 .ne. mo) then
            call utmess('F', 'ASSEMBLA_5')
        endif
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
            call jelira(vecel//'.RELC', 'NUTIOC', nbchar)
!
            do ichar = 1, nbchar
                call jenuno(jexnum(vecel//'.RELC', ichar), nomcas)
                call jeveuo(jexnum(vecel//'.RELC', ichar), 'L', ialcha)
!
                do ima = 1, nbsma
!             -- ON N'ASSEMBLE QUE LES SSS VRAIMENT ACTIVES :
                    if (zi(iasssa-1+ima) .eq. 0) goto 80
                    if (zi(ialcha-1+ima) .eq. 0) goto 80
                    call jeveuo(jexnum(ma//'.SUPMAIL', ima), 'L', iamail)
                    call jelira(jexnum(ma//'.SUPMAIL', ima), 'LONMAX', nnoe)
                    call ssvalv(' ', nomcas, mo, ma, ima,&
                                jresl, ncmpel)
                    nomacr=zk8(ianmcr-1+ima)
                    call dismoi('F', 'NOM_NUME_DDL', nomacr, 'MACR_ELEM_STAT', ibid,&
                                num2, ierd)
                    call jeveuo(nomacr//'.CONX', 'L', iaconx)
                    call jeveuo(jexnum(num2//'.NUME.PRNO', 1), 'L', iaprol)
                    il=0
                    do k1 = 1, nnoe
                        n1=zi(iamail-1+k1)
                        if (n1 .gt. nm) then
                            do iec = 1, nbecmx
                                icodge(iec)=icodla(iec)
                            end do
                        else
                            inold=zi(iaconx-1+3*(k1-1)+2)
                            do iec = 1, nec
                                icodge(iec)=zi(iaprol-1+(nec+&
                                        2)*(inold-1)+2+iec)
                            end do
                        endif
!
                        iad1=zi(idprn1-1+zi(idprn2+ilimnu-1)+(&
                                n1-1)*(nec+2))
                        call cordd2(idprn1, idprn2, ilimnu, icodge, nec,&
                                    ncmp, n1, nddl1, zi(iapsdl))
!
                        if (type .eq. 1) then
                            do i1 = 1, nddl1
                                il=il+1
                                zr(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+i1)- 1))=zr(&
                                        jvale-1+zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zr(jresl+il-&
                                        1)*rcoef
                            end do
                        else if (type.eq.2) then
                            do i1 = 1, nddl1
                                il=il+1
                                zc(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+i1)- 1))=zc(&
                                        jvale-1+zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zc(jresl+il-&
                                        1)*rcoef
                            end do
                        endif
                    end do
 80                 continue
                end do
            end do
            call ssvalv('FIN', nomcas, mo, ma, 0,&
                        jresl, ncmpel)
        endif
!
!
!       -- TRAITEMENT DES ELEMENTS FINIS CLASSIQUES :
!       ---------------------------------------------
        call jeexin(vecel//'.RELR', iret)
        if (iret .gt. 0) then
!
!
!           ==========================
!            BOUCLE SUR LES RESU_ELEM
!           ==========================
            call jelira(vecel//'.RELR', 'LONUTI', nbresu)
            if (nbresu .gt. 0) call jeveuo(vecel//'.RELR', 'L', idlres)
            do iresu = 1, nbresu
                resu=zk24(idlres+iresu-1)(1:19)
                call jeexin(resu//'.NOLI', iexi)
                if (iexi .eq. 0) goto 230
                call jeveuo(resu//'.NOLI', 'L', iad)
                nomli=zk24(iad)
!
                call jenonu(jexnom(kvelil, nomli), ilive)
                call jenonu(jexnom(knulil, nomli), ilinu)
!
!               ==========================
!               BOUCLE SUR LES GRELS DU LIGREL
!               ==========================
                do igr = 1, zi(iadlie+3*(ilive-1))
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
!                       -- nel : nombre d'elements du grel igr
                        nel=zi(zi(iadlie+3*(ilive-1)+2)+igr)-&
                                zi(zi(iadlie+3*(ilive-1)+2)+igr-1)-1
                        call jeveuo(jexnum(resu//'.RESL', igr), 'L', jresl)
                        ncmpel=digdel(mode)
!
!                       =========================
!                       BOUCLE SUR LES ELEMENTS DU GREL IGR
!                       =========================
                        do iel = 1, nel
!                           NUMA : NUMERO DE LA MAILLE
                            numa=zi(zi(iadlie+3*(ilive-1)+1)-&
                                    1+ zi(zi(iadlie+3*(ilive-1)+2)+&
                                    igr-1)+iel-1)
                            r=rcoef
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
                                do k1 = 1, nnoe
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
                                        call utmess('F', 'ASSEMBLA_41', nk=3, valk=valk,&
                                                    si=vali(1))
                                    endif
!
                                    if (iad1 .gt. nequa) then
                                        vali(1)=n1
                                        vali(2)=iad1
                                        vali(3)=nequa
                                        valk(1)=resu
                                        valk(2)=vecel
                                        call utmess('F', 'ASSEMBLA_42', nk=2, valk=valk, ni=3,&
                                                    vali=vali)
                                    endif
!
                                    if (nddl1 .gt. 100) then
                                        vali(1)=nddl1
                                        vali(2)=100
                                        call utmess('F', 'ASSEMBLA_43', ni=2, vali=vali)
                                    endif
!
                                    if (type .eq. 1) then
                                        do i1 = 1, nddl1
                                            il=il+1
                                            zr(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+ i1)-1))=zr(&
                                        jvale-1+ zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zr(jresl+(&
                                        iel-1)*ncmpel+ il-1)*r
                                        end do
!
                                    else
                                        do i1 = 1, nddl1
                                            il=il+1
                                            zc(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+ i1)-1))=zc(&
                                        jvale-1+ zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zc(jresl+(&
                                        iel-1)*ncmpel+ il-1)*r
                                        end do
                                    endif
130                                 continue
                                end do
!
                            else
!
!                               -- MAILLE TARDIVE:
!                               -------------------
                                numa=-numa
!
!                               -- N1 : NBRE DE NOEUDS DE LA MAILLE NUMA
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
                                    call utmess('F', 'ASSEMBLA_44', nk=3, valk=valk, ni=4,&
                                                vali=vali)
                                endif
                                il=0
                                do k1 = 1, nnoe
! n1 : indice du noeuds ds le .nema du ligrel de charge
                                    n1=zi(zi(iadnem+3*(ilive-1)+1)&
                                        -1+ zi(zi(iadnem+3*(ilive-1)+&
                                        2)+numa-1)+k1-1)
                                    if (n1 .lt. 0) then
! NOEUD TARDIF
                                        n1=-n1
!
!
                                        if (ilinu .eq. 0) then
                                            valk(1)=nomli
                                            valk(2)=resu
                                            valk(3)=vecel
                                            valk(4)=nudev
                                            valk(5)=nomli(1:8)
                                            vali(1)=n1
                                            vali(2)=numa
                                            call utmess('F', 'ASSEMBLA_45', nk=5, valk=valk,&
                                                        ni=2, vali=vali)
                                        endif
!
!                                       -- iad1 : numero d'equation du premier ddl de n1
                                        iad1=zi(idprn1-1+zi(idprn2+&
                                        ilinu-1)+ (n1-1)*(nec+2)+1-1)
                                        call corddl(admodl, lcmodl, idprn1, idprn2, ilinu,&
                                                    mode, nec, ncmp, n1, k1,&
                                                    nddl1, zi(iapsdl))
                                        if (nddl1 .gt. 100) then
                                            vali(1)=nddl1
                                            vali(2)=100
                                            call utmess('F', 'ASSEMBLA_46', ni=2, vali=vali)
                                        endif
                                    else
! NOEUD PHYSIQUE
                                        iad1=zi(idprn1-1+zi(idprn2+&
                                        ilimnu-1)+ (n1-1)*(nec+2)+1-1)
                                        call corddl(admodl, lcmodl, idprn1, idprn2, ilimnu,&
                                                    mode, nec, ncmp, n1, k1,&
                                                    nddl1, zi( iapsdl))
                                        if (nddl1 .gt. 100) then
                                            vali(1)=nddl1
                                            vali(2)=100
                                            call utmess('F', 'ASSEMBLA_47', ni=2, vali=vali)
                                        endif
                                    endif
                                    if (iad1 .eq. 0) then
                                        vali(1)=n1
                                        valk(1)=resu
                                        valk(2)=vecel
                                        valk(3)=nudev
                                        call utmess('F', 'ASSEMBLA_48', nk=3, valk=valk,&
                                                    si=vali(1))
                                    endif
                                    if (iad1 .gt. nequa) then
                                        vali(1)=n1
                                        vali(2)=iad1
                                        vali(3)=nequa
                                        valk(1)=resu
                                        valk(2)=vecel
                                        call utmess('F', 'ASSEMBLA_49', nk=2, valk=valk, ni=3,&
                                                    vali=vali)
                                    endif
                                    if (type .eq. 1) then
                                        do i1 = 1, nddl1
                                            il=il+1
                                            zr(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+ i1)-1))=zr(&
                                        jvale-1+ zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zr(jresl+(&
                                        iel-1)*ncmpel+ il-1)*r
                                        end do
                                    else
                                        do i1 = 1, nddl1
                                            il=il+1
                                            zc(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+ i1)-1))=zc(&
                                        jvale-1+ zi(ianueq-1+iad1+zi(&
                                        iapsdl-1+ i1)-1))+zc(jresl+(&
                                        iel-1)*ncmpel+ il-1)*r
                                        end do
                                    endif
                                end do
                            endif
210                         continue
                        end do
                        call jelibe(jexnum(resu//'.RESL', igr))
                    endif
220                 continue
                end do
230             continue
            end do
        endif
    end do
!
!
!
    if (niv .ge. 2) then
        call uttcpu('CPU.ASSVEC', 'FIN', ' ')
        call uttcpr('CPU.ASSVEC', 6, temps)
        if (niv .ge. 2) write (ifm, '(A44,D11.4,D11.4)'&
                        ) 'TEMPS CPU/SYS ASSEMBLAGE V                : ',&
                        temps(5), temps(6)
    endif
!
!
!
!
!
!
!   -- reduction + diffusion de vecas a tous les proc
    if (ldist) call asmpi_comm_jev('MPI_SUM', kvale)
!
!
270 continue
!
!     -- les vect_elem peuvent contenir des cham_no (vect_asse)
!        il faut les cumuler dans kvale :
!     ----------------------------------------------------------
    kvale=vecas//'.VALE'
    do i = 1, nbvec
        a19=tlivec(i)
        call jeexin(a19//'.RELR', iexi)
        if (iexi .eq. 0) goto 300
        call jeveuo(a19//'.RELR', 'L', jrelr)
        call jelira(a19//'.RELR', 'LONUTI', n1)
        do k = 1, n1
            b19=zk24(jrelr-1+k)(1:19)
            call jeexin(b19//'.VALE', iexi)
            if (iexi .gt. 0) then
                call jeveuo(kvale, 'E', jvale1)
                call jelira(kvale, 'TYPE', cval=ktyp)
                ASSERT(ktyp.eq.'R')
                ASSERT(type.eq.1)
                c19='&&ASSVEC.CHAMNO'
                call vtcreb(c19, nu, 'V', ktyp, nequa)
!
                call vtcopy(b19, c19, 'F', iret)
                call jeveuo(c19//'.VALE', 'L', jvale2)
                do j = 1, nequa
                    zr(jvale1-1+j)=zr(jvale1-1+j)+zr(jvale2-1+j)
                end do
                call detrsd('CHAMP_GD', c19)
            endif
        end do
300     continue
    end do
!
    dbg=.false.
    if (dbg) then
        call dbgobj(kvale, 'OUI', 6, '&&ASSVEC')
    endif
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
    call asmpi_barrier()
    call uttcpu('CPU.CALC.1', 'FIN', ' ')
    call uttcpu('CPU.ASSE.1', 'FIN', ' ')
    call uttcpu('CPU.ASSE.3', 'FIN', ' ')
    call jedema()
end subroutine

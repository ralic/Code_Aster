subroutine assmiv(base, vec, nbvec, tlivec, licoef,&
                  nu, vecpro, motcle, type)
    implicit none
!
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
!
#include "jeveux.h"
!
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/corddl.h"
#include "asterfort/crelil.h"
#include "asterfort/detrsd.h"
#include "asterfort/digdel.h"
#include "asterfort/dismoi.h"
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
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mpicm0.h"
#include "asterfort/mpicm1.h"
#include "asterfort/nbec.h"
#include "asterfort/nbno.h"
#include "asterfort/parti0.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: vec, tlivec(*), vecpro, base
    character(len=*) :: nu
    character(len=4) :: motcle
    integer :: nbvec, type
    real(kind=8) :: licoef(*), rcoef, r
! ----------------------------------------------------------------------
!    ASSEMBLAGE "PARTICULIER" POUR CONVERGENCE EN CONTRAINTES
!    GENERALISEES
!    REALISE LE MIN DES VECT_
!    GROSSIEREMENT POMPE SUR ASSVEC
! OUT K19 VEC   : NOM DU CHAM_NO RESULTAT
!                CHAM_NO ::= CHAM_NO_GD + OBJETS PROVISOIRES POUR L'ASS.
! IN  K* BASE   : NOM DE LA BASE SUR LAQUELLE ON VEUT CREER LE CHAM_NO
! IN  I  NBVEC  : NOMBRE DE VECT_ELEM A ASSEMBLER DANS VEC
! IN  K* TLIVEC : LISTE DES VECT_ELEM A ASSEMBLER
! IN  R  LICOEF : LISTE DES COEF. MULTIPLICATEURS DES VECT_ELEM
! IN  K* NU     : NOM D'UN NUMERO_DDL
! IN  K* VECPRO: NOM D'UN CHAM_NO MODELE(NU OU VECPRO EST OBLIGATOIRE)
! IN  K4 MOTCLE : 'ZERO' OU 'CUMU'
! IN  I  TYPE   : TYPE DU VECTEUR ASSEMBLE : 1 --> REEL
!                                            2 --> COMPLEXE
!
! ----------------------------------------------------------------------
!     FONCTIONS JEVEUX
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    character(len=24) :: valk(5)
! ----------------------------------------------------------------------
!     COMMUNS   LOCAUX DE L'OPERATEUR ASSE_VECTEUR
! ----------------------------------------------------------------------
    integer :: gd, nec, nlili
! ---------------------------------------------------------------------
!     VARIABLES LOCALES
! ---------------------------------------------------------------------
    integer :: rang, nbproc, iret, ifm, niv, ibid
    character(len=1) :: bas
    character(len=8) :: ma, mo, mo2, nogdsi, nogdco
    character(len=8) :: kbid, partit
    character(len=14) :: nudev
    character(len=19) :: vecas, vprof, vecel, resu
    character(len=24) :: kmaila, k24prn, knulil, kvelil, kveref, kvedsc, nomli
    character(len=24) :: knequa, kvale
    character(len=1) :: k1bid
    integer :: admodl, lcmodl, iexi
    logical :: ldist, ldgrel
! ----------------------------------------------------------------------
!     FONCTIONS LOCALES D'ACCES AUX DIFFERENTS CHAMPS DES
!     S.D. MANIPULEES DANS LE SOUS PROGRAMME
! ----------------------------------------------------------------------
    integer :: vali(4)
    complex(kind=8) :: cbid
!
! --- DEBUT ------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, i1, iad, iad1, iadlie, iadnem, ianueq
    integer :: ianulo, iapsdl, iconx1, iconx2, iddesc, idlres, idnequ
    integer :: idprn1, idprn2, idveds, idverf, idvref, iel, ier
    integer :: ierd, igr, il, ilim, ilimnu, ilinu, ilive
    integer :: ilivec, imat, iresu, jnumsd, jprtk, jresl, jvale
    integer :: k1, mode, n1, nbelm, nbnoss, nbresu, ncmp
    integer :: ncmpel, nddl1, nel, nequa, nm, nmxcmp, nnoe
    integer :: nugd, numa
!-----------------------------------------------------------------------
    call jemarq()
!
!-----RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, niv)
!----------------------------------------------------------------------
!
! --- VERIF DE MOTCLE:
    if (motcle(1:4) .eq. 'ZERO') then
!
    else if (motcle(1:4).eq.'CUMU') then
!
    else
        call u2mesk('F', 'ASSEMBLA_8', 1, motcle)
    endif
!
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', lcmodl)
    call jeveuo(jexnum('&CATA.TE.MODELOC', 1), 'L', admodl)
!
    vecas=vec
    bas=base
!
! ------------------------------------------------------------------
!     -- SI LES CALCULS ONT ETE "DISTRIBUES" :
!        CALCUL DE :
!           * LDIST  : .TRUE. : LES CALCULS ONT ETE DISTRIBUES
!           * LDGREL : .TRUE. : LES CALCULS ONT ETE DISTRIBUES PAR GREL
!           * JNUMSD : ADRESSE DE PARTIT//'.NUPROC.MAILLE'
!
!     -- IL EXISTE TROIS FORMES DE CALCUL DISTRIBUE BASES SUR UNE PARTI
!        TION:
!        * FETI: LE FLAG A ACTIVER EST LE LOGICAL LFETI
!              (PAS CONCERNE ICI, HORS PERIMETRE FETI)
!        * DISTRIBUE (AVEC OU SANS MUMPS) EN STD: FLAG LDIST
!        * DISTRIBUE AVEC MUMPS + OPTION MATR_DISTIBUEE: LDIST (PAS CON
!            CERNE ICI, ON NE RETAILLE QUE LES MATRICES)
!
!        AU SENS ASSMIV, LES DEUX DERNIERS CAS DE FIGURES SONT IDENTI
!        QUES. POUR PLUS D'INFO CF. COMMENTAIRES DS ASSMAM.
!
!         EN BREF ON A 4 CAS DE FIGURES DE CALCUL ASTER ET ILS SE DECLI
!         NENT COMME SUIT VIS-A-VIS DES VARIABLES DE ASSVEC:
!        1/ CALCUL STD SEQ PAS FETI:
!            LFETI='F',LDIST='F'
!        2/ CALCUL FETI SEQ OU PARALLELE (MATRICES MERE ET FILLES)
!            LFETI='T',LDIST='F'  (PAS CONCERNE ICI)
!        3/ CALCUL PARALLELE (AVEC OU SANS MUMPS) DISTRIBUE STD:
!            LFETI='F',LDIST='T'
!        4/ CAS PARTICULIER DU PRECEDENT: SOLVEUR=MUMPS + OPTION MATR
!          DISTRIBUEE ACTIVEE      (PAS CONCERNE ICI)
!            LFETI='F',LDIST='T'
!
! ------------------------------------------------------------------
    ldist=.false.
    ldgrel=.false.
    rang=0
    nbproc=1
    call parti0(nbvec, tlivec, partit)
    if (partit .ne. ' ') then
        ldist=.true.
        call jeveuo(partit//'.PRTK', 'L', jprtk)
        ldgrel=zk24(jprtk-1+1).eq.'GROUP_ELEM'
        call mpicm0(rang, nbproc)
        if (.not.ldgrel) then
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', jnumsd)
        endif
    endif
!
!
! --- SI LE CONCEPT VECAS EXISTE DEJA,ON LE DETRUIT:
    call detrsd('CHAMP_GD', vecas)
    call wkvect(vecas//'.LIVE', bas//' V K24 ', nbvec, ilivec)
    do 10 i = 1, nbvec
        zk24(ilivec-1+i)=tlivec(i)
10  end do
!
! --- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A VECAS
    kmaila='&MAILLA                 '
    kvelil=vecas//'.LILI'
    kveref=vecas//'.REFE'
    kvale=vecas//'.VALE'
    kvedsc=vecas//'.DESC'
!
! --- CREATION DE REFE ET DESC
    call jecreo(kveref, bas//' V K24')
    call jeecra(kveref, 'LONMAX', 4, ' ')
    call jeveuo(kveref, 'E', idverf)
    call jecreo(kvedsc, bas//' V I')
    call jeecra(kvedsc, 'LONMAX', 2, ' ')
    call jeecra(kvedsc, 'DOCU', ibid, 'CHNO')
    call jeveuo(kvedsc, 'E', idveds)
!
! --- CALCUL D UN LILI POUR VECAS
! --- CREATION D'UN VECAS(1:19).ADNE ET VECAS(1:19).ADLI SUR 'V'
    call crelil('F', nbvec, ilivec, kvelil, 'V',&
                kmaila, vecas, gd, ma, nec,&
                ncmp, ilim, nlili, nbelm)
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
! --- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A NU
! --- IL FAUT ESPERER QUE LE CHAM_NO EST EN INDIRECTION AVEC UN
!     PROF_CHNO APPARTENANT A UNE NUMEROTATION SINON CA VA PLANTER
!     DANS LE JEVEUO SUR KNEQUA
    nudev=nu
    if (nudev(1:1) .eq. ' ') then
        vprof=vecpro
        call jeveuo(vprof//'.REFE', 'L', idvref)
        nudev=zk24(idvref-1+2)(1:14)
    endif
!
    knequa=nudev//'.NUME.NEQU'
    k24prn=nudev//'.NUME.PRNO'
    knulil=nudev//'.NUME.LILI'
    call jeveuo(nudev//'.NUME.NUEQ', 'L', ianueq)
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
    call wkvect('&&ASSMIV.NUMLOC', 'V V I', 3*nbnoss, ianulo)
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
!
!     -- POSDDL(ICMP) (ICMP=1,NMXCMP(GD_SI))
    call wkvect('&&ASSMIV.POSDDL', 'V V I', nmxcmp, iapsdl)
!
    call dismoi('F', 'NB_NO_MAILLA', mo, 'MODELE', nm,&
                kbid, ier)
!
!
! ---  RECUPERATION DE PRNO
    call jeveuo(k24prn, 'L', idprn1)
    call jeveuo(jexatr(k24prn, 'LONCUM'), 'L', idprn2)
!
! ---  RECUPERATION DE NEQUA
    call jeveuo(knequa, 'L', idnequ)
    nequa=zi(idnequ)
!
! ---  REMPLISSAGE DE REFE ET DESC
    zk24(idverf)=ma
    zk24(idverf+1)=k24prn(1:14)//'.NUME'
    zi(idveds)=gd
    zi(idveds+1)=1
!
!
! --- ALLOCATION VALE
    call assert(type.eq.1)
    call wkvect(kvale, bas//' V R8', nequa, jvale)
!
    do 20 i = 1, nequa
        zr(jvale+i-1)=r8maem()
20  end do
!
!
!     -- REMPLISSAGE DE .VALE
!     ------------------------
    do 90 imat = 1, nbvec
        rcoef=licoef(imat)
        vecel=zk24(ilivec+imat-1)(1:19)
!
        call dismoi('F', 'NOM_MODELE', vecel, 'VECT_ELEM', ibid,&
                    mo2, ierd)
        if (mo2 .ne. mo) call u2mess('F', 'ASSEMBLA_5')
!
        call jeexin(vecel//'.RELR', iret)
        if (iret .eq. 0) goto 90
!
        call jeveuo(vecel//'.RELR', 'L', idlres)
        call jelira(vecel//'.RELR', 'LONUTI ', nbresu, k1bid)
        do 80 iresu = 1, nbresu
            resu=zk24(idlres+iresu-1)(1:19)
            call jeveuo(resu//'.NOLI', 'L', iad)
            nomli=zk24(iad)
!
            call jenonu(jexnom(kvelil, nomli), ilive)
            call jenonu(jexnom(knulil, nomli), ilinu)
!
            do 70 igr = 1, zi(iadlie+3*(ilive-1))
                if (ldgrel .and. mod(igr,nbproc) .ne. rang) goto 70
!
!             -- IL SE PEUT QUE LE GREL IGR SOIT VIDE :
                call jaexin(jexnum(resu//'.RESL', igr), iexi)
                if (iexi .eq. 0) goto 70
!
                call jeveuo(resu//'.DESC', 'L', iddesc)
                mode=zi(iddesc+igr+1)
                if (mode .gt. 0) then
                    nnoe=nbno(mode)
                    nel=zi(zi(iadlie+3*(ilive-1)+2)+igr)- zi(zi(&
                    iadlie+3*(ilive-1)+2)+igr-1)-1
                    call jeveuo(jexnum(resu//'.RESL', igr), 'L', jresl)
                    ncmpel=digdel(mode)
!
                    do 60 iel = 1, nel
                        numa=zi(zi(iadlie+3*(ilive-1)+1)-1+ zi(zi(&
                        iadlie+3*(ilive-1)+2)+igr-1)+iel-1)
                        r=rcoef
!
                        if (ldist .and. .not.ldgrel) then
                            if (numa .gt. 0) then
                                if (zi(jnumsd-1+numa) .ne. rang) goto 60
                            endif
                        endif
!
                        if (numa .gt. 0) then
                            il=0
                            do 50 k1 = 1, nnoe
                                n1=zi(iconx1-1+zi(iconx2+numa-1)+k1-1)
                                iad1=zi(idprn1-1+zi(idprn2+ilimnu-1)+&
                                (n1-1)*(nec+2)+1-1)
                                call corddl(admodl, lcmodl, idprn1, idprn2, ilimnu,&
                                            mode, nec, ncmp, n1, k1,&
                                            nddl1, zi(iapsdl))
                                if (nddl1 .eq. 0) goto 50
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
                                    do 30 i1 = 1, nddl1
                                        il=il+1
                                        zr(jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+i1)- 1))=min(zr(&
                                        jvale-1+zi(ianueq-1+iad1+&
                                        zi(iapsdl-1+i1)-1)), zr(jresl+&
                                        (iel-1)*ncmpel+il-1)*r)
30                                  continue
                                endif
50                          continue
                        endif
60                  continue
                    call jelibe(jexnum(resu//'.RESL', igr))
                endif
70          continue
80      continue
!
90  end do
    call jedetr(vecas//'.LILI')
    call jedetr(vecas//'.LIVE')
    call jedetr(vecas//'.ADNE')
    call jedetr(vecas//'.ADLI')
    call jedetr('&&ASSMIV.POSDDL')
    call jedetr('&&ASSMIV.NUMLOC')
!
!        -- REDUCTION + DIFFUSION DE VECAS A TOUS LES PROC
    if (ldist) call mpicm1('MPI_MIN', 'R', nequa, ibid, ibid,&
                           zr(jvale), cbid)
!
!
    call jedema()
end subroutine

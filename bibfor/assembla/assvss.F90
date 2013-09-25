subroutine assvss(base, vec, vecel, nu, vecpro,&
                  motcle, type, fomult, instap)
    implicit none
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
!
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/cordd2.h"
#include "asterfort/crelil.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/gcncon.h"
#include "asterfort/infniv.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/ssvalv.h"
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: vec, vecpro, base, nu
    character(len=19) :: vecel
    character(len=4) :: motcle
    character(len=24) :: fomult
    integer :: type
    real(kind=8) :: instap
! ----------------------------------------------------------------------
! OUT K19 VEC   : NOM DU CHAM_NO RESULTAT
!                CHAM_NO ::= CHAM_NO_GD + OBJETS PROVISOIRES POUR L'ASS.
! IN  K* BASE   : NOM DE LA BASE SUR LAQUELLE ON VEUT CREER LE CHAM_NO
! IN  K* VECEL  : VECT_ELEM A ASSEMBLER
! IN  K* NU     : NOM D'UN NUMERO_DDL
! IN  K* VECPRO : NOM D'UN CHAM_NO MODELE(NU OU VECPRO EST OBLIGATOIRE)
! IN  K4 MOTCLE : 'ZERO' OU 'CUMU'
! IN  K24 FOMULT: TABLEAU DE FONCTIONS MULTIPLICATRICES DE CHARGES
! IN  R8 INSTAP : INSTANT D'INTERPOLATION
! IN  I  TYPE   : TYPE DU VECTEUR ASSEMBLE : 1 --> REEL
!                                            2 --> COMPLEXE
!----------------------------------------------------------------------
    character(len=8) :: nomacr, exiele
    character(len=14) :: num2
    integer :: gd, nec, nlili
!-----------------------------------------------------------------------
    integer :: i, i1, iaconx, iad1, iadlie, iadnem, iadval
    integer :: ialcha, iamail, iancmp, ianmcr, ianueq, ianulo, iaprol
    integer :: iapsdl, iasssa, ichar, icmp, iconx1, iconx2, idnequ
    integer :: idprn1, idprn2, idresl, idveds, idverf, idvref, iec
    integer :: ier, ierd, il, ilim, ilimnu, ilivec, ima
    integer :: inold, iret, jec, k1, lgncmp, n1, nbchar
    integer :: nbecmx, nbelm, nbnoss, nbsma, nbssa, ncmp, ncmpel
    integer :: nddl1, nequa, nm, nmxcmp, nnoe, nugd
!-----------------------------------------------------------------------
    parameter(nbecmx=10)
!
    character(len=1) :: bas
    character(len=8) :: nomsd, k8bid, ma, mo, mo2, nogdsi, nogdco, nomcas, kbid
    character(len=11) :: k11b
    character(len=14) :: k14b, nudev
    character(len=19) :: k19b, vecas, vprof
    character(len=24) :: k24b, knueq, kmaila, k24prn
    character(len=24) :: kvelil, kveref, kvedsc, knequa, kvale, nomlog
    integer :: icodla(nbecmx), icodge(nbecmx), iligrp
    integer :: irefn, admodl, lcmodl, ibid, ifm, niv
    integer :: jfonct
    real(kind=8) :: temps(6), rbid, rcoef
!
! --- DEBUT ------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
    if (motcle(1:4) .eq. 'ZERO') then
    else if (motcle(1:4).eq.'CUMU') then
    else
        call utmess('F', 'ASSEMBLA_8', sk=motcle)
    endif
!
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', lcmodl)
    call jeveuo(jexnum('&CATA.TE.MODELOC', 1), 'L', admodl)
!
    vecas=vec
    bas=base
!
! --- SI LE CONCEPT VECAS EXISTE DEJA,ON LE DETRUIT:
    call detrsd('CHAMP_GD', vecas)
    call wkvect(vecas//'.LIVE', bas//' V K24 ', 1, ilivec)
    zk24(ilivec)=vecel
!
! --- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A VECAS
    kmaila='&MAILLA                 '
    kvelil=vecas//'.LILI'
!
!
! --- CALCUL D UN LILI POUR VECAS
! --- CREATION D'UN VECAS(1:19).ADNE ET VECAS(1:19).ADLI SUR 'V'
    call crelil('F', 1, ilivec, kvelil, 'V',&
                kmaila, vecas, gd, ma, nec,&
                ncmp, ilim, nlili, nbelm)
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
    do 10 i = 1, nbecmx
        icodla(i)=0
        icodge(i)=0
 10 end do

!   -- POSDDL(ICMP) (ICMP=1,NMXCMP(GD_SI))
    call wkvect('&&ASSVEC.POSDDL', 'V V I', nmxcmp, iapsdl)
!
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
!       -- icodla est l'entier code correspondant a la cmp "lagr"
        jec=(icmp-1)/30+1
        icodla(jec)=2**icmp
    endif
!
    k24prn=nudev//'.NUME.PRNO'
    knueq=nudev//'.NUME.NUEQ'
    knequa=nudev//'.NUME.NEQU'
!
    call jeveuo(k24prn, 'L', idprn1)
    call jeveuo(jexatr(k24prn, 'LONCUM'), 'L', idprn2)
    call jeveuo(knueq, 'L', ianueq)
    call jeveuo(knequa, 'L', idnequ)
    nequa=zi(idnequ)
!
!
    kveref=vecas//'.REFE'
    kvale=vecas//'.VALE'
    kvedsc=vecas//'.DESC'

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


    if (type .eq. 1) then
        call jecreo(kvale, bas//' V R8')
    else if (type.eq.2) then
        call jecreo(kvale, bas//' V C16')
    else
        call utmess('F', 'ASSEMBLA_11')
    endif
    call jeecra(kvale, 'LONMAX', nequa)
    call jeveuo(kvale, 'E', iadval)


    call dismoi('F', 'NOM_MODELE', vecel, 'VECT_ELEM', ibid,&
                mo2, ierd)
    if (mo2 .ne. mo) then
        call utmess('F', 'ASSEMBLA_5')
    endif

    call dismoi('F', 'EXI_ELEM', mo, 'MODELE', ibid,&
                exiele, ierd)
    call dismoi('F', 'NB_SS_ACTI', vecel, 'VECT_ELEM', nbssa,&
                kbid, ierd)


!   -- TRAITEMENT DES SOUS-STRUCTURES
!   ----------------------------------------------------------
    if (nbssa .gt. 0) then
        nomcas=' '
        call dismoi('F', 'NB_SM_MAILLA', mo, 'MODELE', nbsma,&
                    kbid, ierd)
        call dismoi('F', 'NOM_MAILLA', mo, 'MODELE', ibid,&
                    ma, ierd)
        call jeveuo(mo//'.MODELE    .SSSA', 'L', iasssa)
        call ssvalv('DEBUT', nomcas, mo, ma, 0,&
                    idresl, ncmpel)
        call jelira(vecel//'.RELC', 'NUTIOC', nbchar)
        call jeveuo(fomult, 'L', jfonct)
!
        do 80 ichar = 1, nbchar
            call jenuno(jexnum(vecel//'.RELC', ichar), nomcas)
            call jeveuo(jexnum(vecel//'.RELC', ichar), 'L', ialcha)
            if (zk24(jfonct+ichar-1)(1:8) .eq. '&&CONSTA') then
                rcoef=1.0d0
            else
                call fointe('F ', zk24(jfonct+ichar-1)(1:8), 1, ['INST'], [instap],&
                            rcoef, ierd)
            endif
            do 70 ima = 1, nbsma
!               -- ON N'ASSEMBLE QUE LES SSS VRAIMENT ACTIVES :
                if (zi(iasssa-1+ima) .eq. 0) goto 70
                if (zi(ialcha-1+ima) .eq. 0) goto 70
                call jeveuo(jexnum(ma//'.SUPMAIL', ima), 'L', iamail)
                call jelira(jexnum(ma//'.SUPMAIL', ima), 'LONMAX', nnoe)
                call ssvalv(' ', nomcas, mo, ma, ima,&
                            idresl, ncmpel)
                nomacr=zk8(ianmcr-1+ima)
                call dismoi('F', 'NOM_NUME_DDL', nomacr, 'MACR_ELEM_STAT', ibid,&
                            num2, ierd)
                call jeveuo(nomacr//'.CONX', 'L', iaconx)
                call jeveuo(jexnum(num2//'.NUME.PRNO', 1), 'L', iaprol)
                il=0
                do 60 k1 = 1, nnoe
                    n1=zi(iamail-1+k1)
                    if (n1 .gt. nm) then
                        do 20 iec = 1, nbecmx
                            icodge(iec)=icodla(iec)
 20                     continue
                    else
                        inold=zi(iaconx-1+3*(k1-1)+2)
                        do 30 iec = 1, nec
                            icodge(iec)=zi(iaprol-1+(nec+2)*(&
                                    inold-1)+2+iec)
 30                     continue
                    endif
!
                    iad1=zi(idprn1-1+zi(idprn2+ilimnu-1)+(n1-&
                            1)*(nec+2))
                    call cordd2(idprn1, idprn2, ilimnu, icodge, nec,&
                                ncmp, n1, nddl1, zi(iapsdl))
!
                    if (type .eq. 1) then
                        do 40 i1 = 1, nddl1
                            il=il+1
                            zr(iadval-1+zi(ianueq-1+iad1+zi(&
                                    iapsdl-1+i1)- 1))=zr(iadval-1+zi(&
                                    ianueq-1+iad1+zi(iapsdl-1+&
                                    i1)-1))+zr(idresl+il-1)*rcoef
 40                     continue
                    else if (type.eq.2) then
                        do 50 i1 = 1, nddl1
                            il=il+1
                            zc(iadval-1+zi(ianueq-1+iad1+zi(&
                                    iapsdl-1+i1)- 1))=zc(iadval-1+zi(&
                                    ianueq-1+iad1+zi(iapsdl-1+&
                                    i1)-1))+zc(idresl+il-1)*rcoef
 50                     continue
                    endif
 60             continue
 70         continue
 80     continue
        call ssvalv('FIN', nomcas, mo, ma, 0,&
                    idresl, ncmpel)
    endif
!
!
    call jedetr(vecas//'.LILI')
    call jedetr(vecas//'.LIVE')
    call jedetr(vecas//'.ADNE')
    call jedetr(vecas//'.ADLI')
    call jedetr('&&ASSVEC.POSDDL')
    call jedetr('&&ASSVEC.NUMLOC')
    call jedema()
end subroutine

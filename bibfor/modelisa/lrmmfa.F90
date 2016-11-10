subroutine lrmmfa(fid, nomamd, nbnoeu, nbmail, grpnoe,&
                  gpptnn, grpmai, gpptnm, nbgrno, nbgrma,&
                  typgeo, nomtyp, nmatyp, prefix, infmed,&
                  vecgrm, nbcgrm)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     LECTURE DU MAILLAGE - FORMAT MED - LES FAMILLES
!     -    -     -                 -         --
!-----------------------------------------------------------------------
!
! ENTREES :
!  FID    : IDENTIFIANT DU FICHIER MED
!  NOMAMD : NOM DU MAILLAGE MED
!  NBNOEU : NOMBRE DE NOEUDS DU MAILLAGE
!  NBMAIL : NOMBRE DE MAILLES DU MAILLAGE
!  TYPGEO : TYPE MED POUR CHAQUE MAILLE
!  NOMTYP : NOM DES TYPES POUR CHAQUE MAILLE
!  NMATYP : NOMBRE DE MAILLES PAR TYPE
!  PREFIX : PREFIXE POUR LES TABLEAUX DES RENUMEROTATIONS
! SORTIES :
!  GRPNOE : OBJETS DES GROUPES DE NOEUDS
!  GRPMAI : OBJETS DES GROUPES DE MAILLES
!  NBGRNO : NOMBRE DE GROUPES DE NOEUDS
!  NBGRMA : NOMBRE DE GROUPES DE MAILLES
! DIVERS
! INFMED : NIVEAU DES INFORMATIONS A IMPRIMER
!-----------------------------------------------------------------------
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/as_mfafai.h"
#include "asterfort/as_mfanfa.h"
#include "asterfort/as_mfanfg.h"
#include "asterfort/as_mfaofi.h"
#include "asterfort/as_mfaona.h"
#include "asterfort/as_mfinvr.h"
#include "asterfort/as_mmhfnr.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/juagrn.h"
#include "asterfort/jucroc.h"
#include "asterfort/lxlgut.h"
#include "asterfort/lxnoac.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: ntymax
    parameter (ntymax = 69)
!
! 0.1. ==> ARGUMENTS
!
    integer :: fid
    integer :: nbnoeu, nbmail, nbgrno, nbgrma, nbcgrm
    integer :: typgeo(ntymax), nmatyp(ntymax)
    integer :: infmed
!
    character(len=6) :: prefix
    character(len=8) :: nomtyp(ntymax)
    character(len=*) :: nomamd
!
    character(len=24) :: grpnoe, grpmai, vecgrm
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRMMFA' )
!
    integer :: ednoeu
    parameter (ednoeu=3)
    integer :: edmail
    parameter (edmail=0)
    integer :: typnoe
    parameter (typnoe=0)
!
    integer :: codret, major, minor, rel, cret, nblim1, nblim2
    integer :: nbgr, jv2, jv3, num, ifam, igrp, jnogrp, numfam, nbver
    integer :: ityp, ilmed, jgrp, ecart, jv4
    integer :: nbrfam, jnbnog, jadcor, ino, jcolno, jnbno, jvec, nbno
    integer :: adfano, val_max, val_min, ngro, ima, jreno, ingrp
    integer :: ifm, nivinf, jcolma, jnbma, nbma, nummai, nbattr
    integer :: jnumty(ntymax), jfamma(ntymax), idatfa(200), vaatfa(200)
    aster_logical :: bgrpno
!
    character(len=8) :: saux08
    character(len=19) :: nocorf
    character(len=24) :: famnoe, gpptnn, gpptnm, nomgro, nomtmp, nonbgr
    character(len=24) :: nonufa, nonogn, nonogm, nofnog, noadco, noreno
    character(len=64) :: nomfam
    character(len=80) :: nomgrp, valk(4), newgrm
    character(len=200) :: descat(200)
!
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
    call infniv(ifm, nivinf)
    call as_mfinvr(fid, major, minor, rel, cret)
!
    if (nivinf .ge. 2) then
!
        write (ifm,1001) nompro
        1001 format( 60('='),/,'DEBUT DU PROGRAMME ',a)
!
    endif

    noreno = '&&LRMMFA.RENOMME'
    if( nbcgrm.ne.0 ) then
        call jecreo(noreno, 'V N K24')
        call jeecra(noreno, 'NOMMAX', ival=nbcgrm)
        call jeveuo(vecgrm, 'L', jreno)
        do ingrp = 1, nbcgrm
            nomgro = zk80(jreno+(ingrp-1)*2)
            call jenonu(jexnom(noreno, nomgro), num)
            if( num.eq.0 ) then
                call jecroc(jexnom(noreno, nomgro))
            else
                ASSERT(.false.)
            endif
        enddo
    endif
!
!====
! 2. LECTURES DE DIMENSIONNEMENT
!====
!
    nbgrno = 0
    nbgrma = 0
!
! 2.1. ==> RECHERCHE DU NOMBRE DE FAMILLES ENREGISTREES
!
    call as_mfanfa(fid, nomamd, nbrfam, codret)
    if (codret .ne. 0) then
        saux08='mfanfa'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
    if (infmed .ge. 3) then
!
        write (ifm,2101) nbrfam
        2101 format('NOMBRE DE FAMILLES DANS LE FICHIER A LIRE :',i5)
!
    endif
!
! 2.2. ==> SI PAS DE FAMILLE, PAS DE GROUPE ! DONC, ON S'EN VA.
!          C'EST QUAND MEME BIZARRE, ALORS ON EMET UNE ALARME
!
    if (nbrfam .eq. 0) then
        call utmess('A', 'MED_17')
    else
!
!====
! 3. ON LIT LES TABLES DES NUMEROS DE FAMILLES POUR NOEUDS ET MAILLES
!====
!
! 3.1. ==> LA FAMILLE D'APPARTENANCE DE CHAQUE NOEUD
!
        famnoe = '&&'//nompro//'.FAMILLE_NO     '
        call wkvect(famnoe, 'V V I', nbnoeu, adfano)
!
        call as_mmhfnr(fid, nomamd, zi(adfano), nbnoeu, ednoeu,&
                       typnoe, codret)
!      DANS MED3.0, LE CODE RETOUR PEUT ETRE NEGATIF SANS POUR
!      AUTANT QU'IL Y AIT UN PROBLEME...
!
! 3.2. ==> LA FAMILLE D'APPARTENANCE DE CHAQUE MAILLE
!          ON DOIT LIRE TYPE PAR TYPE
!
        do ityp = 1 , ntymax
!
            if (nmatyp(ityp) .ne. 0) then
!
                call wkvect('&&'//nompro//'.FAMMA.'//nomtyp(ityp), 'V V I',&
                            nmatyp(ityp), jfamma(ityp))
                call as_mmhfnr(fid, nomamd, zi(jfamma(ityp)), nmatyp( ityp), edmail,&
                               typgeo(ityp), codret)
!         DANS MED3.0, LE CODE RETOUR PEUT ETRE NEGATIF SANS POUR
!         AUTANT QU'IL Y AIT UN PROBLEME...
                call jeveuo('&&'//prefix//'.NUM.'//nomtyp(ityp), 'L', jnumty(ityp))
!
            endif
!
        end do
!
!====
! 4. LECTURE DES CARACTERISTIQUES DES FAMILLES
!====
!
        nonufa = '&&LRMMFA.NUM_FAM'
        nonogn = '&&LRMMFA.NOM_GRPNO'
        nonogm = '&&LRMMFA.NOM_GRPMA'
        nofnog = '&&LRMMFA.FAM_NOM_GRP'
        nocorf = '&&LRMMFA.COR_FAM_GR'
        noadco = '&&LRMMFA.ADR_CORR'
        nonbgr = '&&LRMMFA.NB_GR'
        call wkvect(nonufa, 'V V I', nbrfam, jv2)
        nblim1 = nbrfam
        call jecreo(nonogn, 'V N K24')
        call jeecra(nonogn, 'NOMMAX', ival=nbrfam)
        nblim2 = nbrfam
        call jecreo(nonogm, 'V N K24')
        call jeecra(nonogm, 'NOMMAX', ival=nbrfam)
!
        call jecrec(nocorf, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbrfam)
        call wkvect(noadco, 'V V I', nbrfam, jadcor)
        call wkvect(nonbgr, 'V V I', nbrfam, jv4)
!
        val_min = 9999999
        val_max = -9999999
        nbver = nbrfam
        bgrpno = .true.
        call wkvect(nofnog, 'V V K80', nbver, jnogrp)
        do ifam = 1, nbrfam
!
            call as_mfanfg(fid, nomamd, ifam, nbgr, codret)
            if( codret.ne.0 ) then
                saux08='mfanfg'
                call utmess('F', 'DVP_97', sk=saux08, si=codret)
            endif
            if( nbgr.gt.nbver ) then
                call jedetr(nofnog)
                nbver = 2*nbver
                call wkvect(nofnog, 'V V K80', nbver, jnogrp)
            endif
            if( nbgr.ne.0 ) then
                if (major .eq. 3) then
                    call as_mfafai(fid, nomamd, ifam, nomfam, numfam,&
                                   zk80(jnogrp), codret)
                    nbattr = 0
                else
                    call as_mfaona(fid, nomamd, ifam, nbattr, codret)
                    call as_mfaofi(fid, nomamd, ifam, nomfam, numfam,&
                                   idatfa, vaatfa, descat, nbattr, zk80(jnogrp),&
                                   codret)
                endif
                if( codret.ne.0 ) then
                    saux08='mfafai'
                    call utmess('F', 'DVP_97', sk=saux08, si=codret)
                endif

                call jecroc(jexnum(nocorf, ifam))
                call jeecra(jexnum(nocorf, ifam), 'LONMAX', nbgr)
                call jeveuo(jexnum(nocorf, ifam), 'E', jgrp)
                zi(jadcor+ifam-1) = jgrp
                zi(jv4+ifam-1) = nbgr
                zi(jv2+ifam-1) = numfam
                val_max = max(numfam, val_max)
                val_min = min(numfam, val_min)
!
                bgrpno = .false.
                nomtmp = nonogm
                if( numfam.gt.0 ) then
                    bgrpno = .true.
                    nomtmp = nonogn
                endif
!
                do igrp = 1, nbgr
                    nomgrp = zk80(jnogrp+igrp-1)
                    ilmed = lxlgut(nomgrp)
                    if( nbcgrm.eq.0.and.ilmed.gt.24 ) then
                        valk(1) = nomgrp
                        call utmess('F', 'MED_7', nk=1, valk=valk)
                    endif
                    nomgro = nomgrp
                    if( nbcgrm.ne.0 ) then
                        call jenonu(jexnom(noreno, nomgrp), num)
                        if( num.ne.0 ) nomgro = zk80(jreno+(num*2-1))
                    endif
                    call lxnoac(nomgro, newgrm)
                    if( nomgro.ne.newgrm ) then
                        valk(1) = nomgro
                        valk(2) = newgrm
                        call utmess('A', 'MED_10', nk=2, valk=valk)
                        nomgro = newgrm(1:24)
                    endif
                    call jenonu(jexnom(nomtmp, nomgro), num)
                    if( num.eq.0 ) then
                        if( bgrpno ) then
                            nbgrno = nbgrno+1
                            if( nbgrno.gt.nblim1 ) then
                                call juagrn(nomtmp, 2*nblim1)
                                nblim1 = 2*nblim1
                            endif
                        else
                            nbgrma = nbgrma+1
                            if( nbgrma.gt.nblim2 ) then
                                call juagrn(nomtmp, 2*nblim2)
                                nblim2 = 2*nblim2
                            endif
                        endif
                        call jecroc(jexnom(nomtmp, nomgro))
                        call jenonu(jexnom(nomtmp, nomgro), num)
                    endif
                    ASSERT(num.ne.0)
                    zi(jgrp+igrp-1) = num
                enddo
            endif
        enddo
        call jedetr(nofnog)
!
        if( nbgrno.eq.0.and.nbgrma.eq.0 ) call utmess('A', 'MED2_8')
!
        if( .not.(val_max.eq.-9999999.and.val_min.eq.9999999) ) then
            ecart = val_max - val_min + 1
            call wkvect('&&LRMMFA.COR_NUM_FAM', 'V V I', ecart, jv3)
            do ifam = 1, nbrfam
                numfam = zi(jv2+ifam-1)
                if( numfam.ne.0 ) then
                    zi(jv3+numfam-val_min) = ifam
                endif
            enddo
        endif
        call jedetr(nonufa)
!
        if( nbgrno.gt.0 ) then
            call jecreo(gpptnn, 'G N K24')
            call jeecra(gpptnn, 'NOMMAX', nbgrno)
            call jecrec(grpnoe, 'G V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE',&
                        nbgrno)
            call wkvect('&&LRMMFA.NB_NO_GRP', 'V V I', nbgrno, jnbnog)
!
            do ino = 1, nbnoeu
                numfam = zi(adfano+ino-1)
                if( numfam.ne.0 ) then
                    ifam = zi(jv3+numfam-val_min)
                    ASSERT(ifam.le.nbrfam.and.ifam.ne.0)
                    jgrp = zi(jadcor+ifam-1)
                    nbgr = zi(jv4+ifam-1)
                    do igrp = 1, nbgr
                        ngro = zi(jgrp+igrp-1)
                        ASSERT(ngro.le.nbgrno)
                        zi(jnbnog+ngro-1) = zi(jnbnog+ngro-1) + 1
                    enddo
                endif
            enddo

            call wkvect('&&LRMMFA.COL_NO', 'V V I', nbgrno, jcolno)
            do igrp = 1, nbgrno
!
                nbno = zi(jnbnog-1+igrp)
                if( nbno.gt.0 ) then
                    call jenuno(jexnum(nonogn, igrp), nomgrp)
                    call jucroc(grpnoe, nomgrp, 0, nbno, jvec)
                    zi(jcolno+igrp-1) = jvec
                endif
!
            enddo
!
            call wkvect('&&LRMMFA.NB_NO_GR', 'V V I', nbgrno, jnbno)
            do ino = 1, nbnoeu
                numfam = zi(adfano+ino-1)
                if( numfam.ne.0 ) then
                    ifam = zi(jv3+numfam-val_min)
                    jgrp = zi(jadcor+ifam-1)
                    nbgr = zi(jv4+ifam-1)
                    do igrp = 1, nbgr
                        ngro = zi(jgrp+igrp-1)
                        nbno = zi(jnbno+ngro-1)
                        ASSERT(nbno.le.zi(jnbnog+ngro-1))
                        jvec = zi(jcolno+ngro-1)
                        zi(jvec+nbno) = ino
                        zi(jnbno+ngro-1) = nbno + 1
                    enddo
                endif
            enddo
            call jedetr('&&LRMMFA.COL_NO')
            call jedetr('&&LRMMFA.NB_NO_GRP')
            call jedetr('&&LRMMFA.NB_NO_GR')
        endif
        call jedetr(nonogn)
!
        if( nbgrma.gt.0 ) then
            call jecreo(gpptnm, 'G N K24')
            call jeecra(gpptnm, 'NOMMAX', nbgrma)
            call jecrec(grpmai, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                        nbgrma)
            call wkvect('&&LRMMFA.NB_MA_GRP', 'V V I', nbgrma, jnbnog)
!
            do ityp = 1 , ntymax
!
                if( nmatyp(ityp).ne.0 ) then
                    do ima = 1, nmatyp(ityp)
                        numfam = zi(jfamma(ityp)+ima-1)
                        if( numfam.ne.0 ) then
                            ifam = zi(jv3+numfam-val_min)
                            ASSERT(ifam.le.nbrfam.and.ifam.ne.0)
                            jgrp = zi(jadcor+ifam-1)
                            nbgr = zi(jv4+ifam-1)
                            do igrp = 1, nbgr
                                ngro = zi(jgrp+igrp-1)
                                ASSERT(ngro.le.nbgrma)
                                zi(jnbnog+ngro-1) = zi(jnbnog+ngro-1) + 1
                            enddo
                        endif
                    enddo
                endif
!
            enddo

            call wkvect('&&LRMMFA.COL_MA', 'V V I', nbgrma, jcolma)
            do igrp = 1, nbgrma
!
                nbma = zi(jnbnog-1+igrp)
                if( nbma.gt.0 ) then
                    call jenuno(jexnum(nonogm, igrp), nomgrp)
                    call jucroc(grpmai, nomgrp, 0, nbma, jvec)
                    zi(jcolma+igrp-1) = jvec
                endif
!
            enddo
!
            call wkvect('&&LRMMFA.NB_MA_GR', 'V V I', nbgrma, jnbma)
            do ityp = 1 , ntymax
!
                if( nmatyp(ityp).ne.0 ) then
                    do ima = 1, nmatyp(ityp)
                        numfam = zi(jfamma(ityp)+ima-1)
                        if( numfam.ne.0 ) then
                            ifam = zi(jv3+numfam-val_min)
                            jgrp = zi(jadcor+ifam-1)
                            nbgr = zi(jv4+ifam-1)
                            nummai = zi(jnumty(ityp)+ima-1)
                            do igrp = 1, nbgr
                                ngro = zi(jgrp+igrp-1)
                                nbma = zi(jnbma+ngro-1)
                                ASSERT(nbma.le.zi(jnbnog+ngro-1))
                                jvec = zi(jcolma+ngro-1)
                                zi(jvec+nbma) = nummai
                                zi(jnbma+ngro-1) = nbma + 1
                            enddo
                        endif
                    enddo
                endif
!
            enddo
            call jedetr('&&LRMMFA.COL_MA')
            call jedetr('&&LRMMFA.NB_MA_GRP')
            call jedetr('&&LRMMFA.NB_MA_GR')
!
            do ityp = 1, ntymax
                if (nmatyp(ityp) .ne. 0) then
                    call jedetr('&&'//nompro//'.FAMMA.'//nomtyp(ityp))
                endif
            end do
        endif
        call jedetr(nonbgr)
        call jedetr(nonogn)
        call jedetr(nonogm)
        call jedetr(noadco)
        call jedetr('&&LRMMFA.COR_NUM_FAM')
        call jedetr(nocorf)
!
    endif
!
    call jedema()
!
    if (nivinf .ge. 2) then
        write (ifm,6001) nompro
        6001 format(/,'FIN DU PROGRAMME ',a,/,60('='))
    endif
!
end subroutine

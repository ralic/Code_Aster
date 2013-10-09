subroutine vecgen(nomres, numeg)
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
    implicit none
!
!***********************************************************************
!    T. KERBER      DATE 12/05/93
!-----------------------------------------------------------------------
!  BUT: ASSEMBLER UN VECTEUR ISSU D'UN MODELE GENERALISE
!
!     CONCEPT CREE: VECT_ASSE_GENE
!
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/chpver.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgutdm.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
!
    integer :: ier
!
!
!
!
!
    character(len=6) :: pgc
    character(len=8) :: nomres, numeg, modgen, nomsst, nom2mb, nomddl, sstold
    character(len=8) :: basmod, typve
    character(len=16) :: motfac
    character(len=19) :: profg, chasou
    character(len=24) :: resdsc, resref, resval
    character(len=24) :: valk(3)
    character(len=24) :: chadsc, chalis, chaval
    character(len=24) :: nuchar, nubamo
    character(len=24) :: nomcha, deeq, typeba, seliai, sizlia, sst
    character(len=8) :: kbid
    complex(kind=8) :: cbid
    integer :: gd, gd0, nblia, ibid, elim, neqet, neqred, lmapro, lsilia, lsst
    integer :: nbsst, i1, j1
    integer :: vali(3), tmod(1)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iadmod, iavale, iddeeq, idvale, idvect, ioc
    integer :: ipos, iret, j, ladrve, lddesc, ldesc, ldnddl
    integer :: ldnsst, ldnvec, ldprs, ldstr, llneq, llref
    integer :: lrdesc, lrefe, lrref, lrval, nbchar, nbmod, nddl0
    integer :: neq, neqgen, num, nusst
    real(kind=8) :: rbid
!-----------------------------------------------------------------------
    data pgc/'VECGEN'/
!-----------------------------------------------------------------------
!
    call jemarq()
!
!-----------------------------------------------------------------------
!     A/ RECUPERATION DU MODELE GENERALISE
!-----------------------------------------------------------------------
!
    profg = numeg//'      .NUME'
    call jeveuo(profg//'.REFN', 'E', llref)
    modgen = zk24(llref) (1:8)
!
!     0/ TEST SI ON ELIMINE LES CONTRAINTES
!     =====================================
!
    elim=0
    seliai=numeg(1:8)//'      .ELIM.BASE'
    sizlia=numeg(1:8)//'      .ELIM.TAIL'
    sst=   numeg(1:8)//'      .ELIM.NOMS'
    call jeexin(seliai, elim)
!
!     RECUPERATION DE LA BASE SI ELIMINATION
!
    if (elim .ne. 0) then
        call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst)
        neqet=0
        call jeveuo(numeg//'      .NUME.NEQU', 'L', ibid)
        neqred=zi(ibid)
        call jeveuo(seliai, 'L', lmapro)
        call jeveuo(sizlia, 'L', lsilia)
        call jeveuo(sst, 'L', lsst)
        do 9 i = 1, nbsst
            neqet=neqet+zi(lsilia+i-1)
  9     continue
    endif
!
!     1/ LECTURE ET STOCKAGE DES INFORMATIONS
!     =======================================
!
!-----------------------------------------------------------------------
!     1.1/ RECUPERATION CONCEPTS AMONT
!-----------------------------------------------------------------------
!
!     NOMBRE DE SOUS-STRUCTURES SOUMISES A CHARGEMENTS
    call getfac('CHAR_SOUS_STRUC', nbchar)
!
    if (elim .eq. 0) then
!       VERIFIER QUE LA NUMEROTATION EST COHERENTE
        call jenonu(jexnom(profg//'.LILI', 'LIAISONS'), ibid)
        call jelira(jexnum(profg//'.PRNO', ibid), 'LONMAX', nblia)
        if (nblia .eq. 1) then
            call utmess('F', 'ALGORITH_32')
        endif
!
!       VERIFIER QUE LE NOMBRE NBCHAR DE SOUS-STRUCTURES CHARGEES EST
!       INFERIEUR AU NOMBRE TOTAL NBSST DE SOUS-STRUCTURES
        call jenonu(jexnom(profg//'.LILI', '&SOUSSTR'), ibid)
        call jelira(jexnum(profg//'.ORIG', ibid), 'LONMAX', nbsst)
        if (nbchar .gt. nbsst) then
            vali (1) = nbchar
            vali (2) = nbsst
            call utmess('F', 'ALGORITH15_69', ni=2, vali=vali)
        endif
    endif
!
!-----------------------------------------------------------------------
!     1.2/ CREATION DE LA STRUCTURE DE DONNEES NOMRES
!-----------------------------------------------------------------------
!
!     CETTE STRUCTURE DE DONNEES, DE TYPE VECT_ASSE_GENE, COMPREND
!     LES ARTICLES USUELS D'UN VECT_ASSE, PLUS UN .ELEM CONTENANT DES
!     INFORMATIONS PROPRES AUX DIVERSES SOUS-STRUCTURES.
!
!     CELUI-CI COMPREND LES ARTICLES QUI SUIVENT.
!        - UN .DESC, AVEC LE NOMBRE DE SOUS-STRUCTURES SOUMISES A
!          CHARGEMENT
!        - UN .LICH, REGROUPANT LES INFORMATIONS SUR LES SOUS-STRUCTURES
!          CHARGEES : NOM DE LA SOUS-STRUCTURE, DU SECOND MEMBRE
!          ASSEMBLE POUR CELLE-CI, ET DU NUME_DDL ASSOCIE.
!        - UN .VALE, QUI CONTIENDRA LES PROJECTIONS DES SECOND MEMBRES
!          ASSEMBLES, SUR LA BASE MODALE CORRESPONDANTE. C'EST UNE
!          COLLECTION NOMMEE PAR LES NOMS DES SOUS-STRUCTURES.
!
!     NOTONS QUE DANS LE .REFE, LE MAILLAGE EST REMPLACE PAR LE MODELE
!     GENERALISE.
!
    resdsc = nomres//'           .DESC'
    resref = nomres//'           .REFE'
    resval = nomres//'           .VALE'
!
    chasou = nomres//'      .ELEM'
    chadsc = chasou//'.DESC'
    chalis = chasou//'.LICH'
    chaval = chasou//'.VALE'
!
    call wkvect(resdsc, 'G V I', 3, lrdesc)
    call jeecra(resdsc, 'DOCU', cval='CHNO')
!
    call wkvect(resref, 'G V K24', 2, lrref)
!
!     RECUPERATION DU NOMBRE TOTAL DE D.D.L. GENERALISES, POUR
!     L'ALLOCATION DU .VALE.
    call jeveuo(profg//'.NEQU', 'L', llneq)
    neqgen = zi(llneq)
!
    call wkvect(resval, 'G V R', neqgen, lrval)
    call wkvect(chadsc, 'V V I', 1, lddesc)
!
    call jecrec(chalis, 'V V K8', 'NO', 'CONTIG', 'CONSTANT',&
                3)
    call jecroc(jexnom(chalis, 'SOUSSTR'))
    call jecroc(jexnom(chalis, 'VECTASS'))
    call jecroc(jexnom(chalis, 'NUMEDDL'))
    call jeecra(jexnom(chalis, 'SOUSSTR'), 'LONMAX', nbchar)
!
    call jeveuo(jexnom(chalis, 'SOUSSTR'), 'E', ldnsst)
    call jeveuo(jexnom(chalis, 'VECTASS'), 'E', ldnvec)
    call jeveuo(jexnom(chalis, 'NUMEDDL'), 'E', ldnddl)
!
    call jecrec(chaval, 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                nbchar)
!
!-----------------------------------------------------------------------
!     1.3/ REMPLISSAGE DES INFORMATIONS DE NOMRES
!-----------------------------------------------------------------------
!
!     ECRITURE DU .DESC DANS LE .ELEM
    zi(lddesc) = nbchar
!
!     ECRITURE DES INFORMATIONS DANS CHARLIS
    motfac = 'CHAR_SOUS_STRUC'
!
!     BOUCLE SUR LES SOUS-STRUCTURES CHARGEES
    do 10 i = 1, nbchar
!
!-----------------------------------------------------------------------
!     B/ RECUPERATION DU NOM DE LA SOUS-STRUCTURE ET ECRITURE DANS
!        LE .LICH.
!-----------------------------------------------------------------------
!
        call getvtx(motfac, 'SOUS_STRUC', iocc=i, nbval=0, nbret=ioc)
        ioc = -ioc
        if (ioc .ne. 1) then
            vali (1) = i
            vali (2) = 1
            vali (3) = ioc
            call utmess('F', 'ALGORITH15_70', ni=3, vali=vali)
        else
            call getvtx(motfac, 'SOUS_STRUC', iocc=i, scal=nomsst, nbret=ioc)
        endif
        zk8(ldnsst+i-1) = nomsst
!
!-----------------------------------------------------------------------
!     C/ RECUPERATION DU NOM DU SECOND MEMBRE ASSEMBLE, ET ECRITURE
!        DANS LE .LICH
!-----------------------------------------------------------------------
!
        call getvid(motfac, 'VECT_ASSE', iocc=i, nbval=0, nbret=ioc)
        ioc = -ioc
        if (ioc .ne. 1) then
            vali (1) = i
            vali (2) = 1
            vali (3) = ioc
            call utmess('F', 'ALGORITH15_71', ni=3, vali=vali)
        else
            call getvid(motfac, 'VECT_ASSE', iocc=i, scal=nom2mb, nbret=ioc)
            call chpver('F', nom2mb, 'NOEU', 'DEPL_R', ier)
        endif
!
!     RECUPERATION DU NUME_DDL ASSOCIE AU SECOND MEMBRE.
!
!     ON VERIFIE D'ABORD QU'ON A BIEN LE NOM DU PROF_CHNO DANS LE
!     .REFE DU CHAMNO SECOND MEMBRE. POUR CE FAIRE, ON CONTROLE QUE
!     LA VALEUR DE NUME DANS LE .DESC EST BIEN POSITIVE.
!
        call jeveuo(nom2mb//'           .DESC', 'L', ldesc)
        num = zi(ldesc+1)
        if (num .lt. 0) then
            valk (1) = nom2mb
            call utmess('F', 'ALGORITH15_72', sk=valk(1))
        endif
        call jeveuo(nom2mb//'           .REFE', 'L', lrefe)
        nuchar = zk24(lrefe+1)
!
!     VERIFICATION DE LA COHERENCE DES GRANDEURS ENTRE CHARGEMENTS.
!
        gd = zi(ldesc)
        if (i .eq. 1) then
            gd0 = gd
        endif
        sstold='        '
        if (gd .ne. gd0) then
            vali (1) = gd0
            vali (2) = gd
            valk (1) = sstold
            valk (2) = nomsst
            call utmess('F+', 'ALGORITH15_73', nk=2, valk=valk, ni=2,&
                        vali=vali)
        endif
        gd0 = gd
        sstold = nomsst
        zk8(ldnvec+i-1) = nom2mb
!
!-----------------------------------------------------------------------
!     D/ RECUPERATION DU NUME_DDL ET ENREGISTREMENT DANS LE .LICH
!-----------------------------------------------------------------------
!
!     ON UTILISE LA BASE MODALE, ET ON VERIFIE AU PASSAGE QUE
!     SON TYPE EST BIEN CELUI D'UNE BASE CLASSIQUE OU DE RITZ.
!
        call mgutdm(modgen, nomsst, 0, 'NOM_BASE_MODALE', ibid,&
                    basmod)
!
        call dismoi('NUME_DDL', basmod, 'RESU_DYNA', repk=nubamo)
        call dismoi('TYPE_BASE', basmod, 'RESU_DYNA', repk=typeba)
!
        if (typeba(1:4) .ne. 'RITZ' .and. typeba(1:9) .ne. 'CLASSIQUE') then
            valk (1) = nomsst
            call utmess('F', 'ALGORITH15_74', sk=valk(1))
        endif
!
!     PAR SECURITE, ON S'ASSURE QUE LE NUME_DDL ASSOCIE AU CHARGEMENT
!     COINCIDE AVEC CELUI ASSOCIE A LA SOUS-STRUCTURE.
!
        if (nuchar(1:14) .ne. nubamo(1:14)) then
            valk (1) = nomsst
            valk (2) = nubamo
            valk (3) = nuchar
            call utmess('F', 'ALGORITH15_75', nk=3, valk=valk)
        endif
!
!     COPIE DU NUME_DDL DANS LE .LICH
        zk8(ldnddl+i-1) = nubamo(1:8)
 10 continue
!
!     ECRITURE DU .REFE
    zk24(llref) = modgen
    zk24(lrref+1) = profg
!
!     ECRITURE DU .DESC
    zi(lrdesc) = gd
    zi(lrdesc+1) = 1
!
!     2/ PROJECTION DES CHARGEMENTS SUR LES BASES MODALES
!     ===================================================
!
!-----------------------------------------------------------------------
!     BOUCLES SUR LES SOUS-STRUCTURES CHARGEES
!-----------------------------------------------------------------------
    do 50 i = 1, nbchar
!
!-----------------------------------------------------------------------
!     2.1/ RECUPERATION D'INFORMATIONS DE BASE
!-----------------------------------------------------------------------
!
!     NOMS STOCKES DANS LE .LICH
        nomsst = zk8(ldnsst+i-1)
        nom2mb = zk8(ldnvec+i-1)
        nomddl = zk8(ldnddl+i-1)
!
!     RECUPERATION DE LA BASE MODALE ASSOCIEE A LA SOUS-STRUCTURE
        call mgutdm(modgen, nomsst, 0, 'NOM_BASE_MODALE', ibid,&
                    basmod)
!
!     NOMBRE DE MODES NBMOD DE LA BASE MODALE
        call rsorac(basmod, 'LONUTI', 0, rbid, kbid,&
                    cbid, rbid, kbid, tmod, 1,&
                    ibid)
        nbmod=tmod(1)
!
!     RECUPERATION DU .VALE ASSOCIE AU SECOND MEMBRE
        call jeveuo(nom2mb//'           .VALE', 'L', ladrve)
        call jelira(nom2mb//'           .VALE', 'TYPE', cval=typve)
!
!     NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE, POUR LA SOUS-STRUCTURE
        call dismoi('NB_EQUA', nomddl, 'NUME_DDL', repi=neq)
!
!     POSITIONNEMENT DANS LE .DEEQ, AFIN DE DISPOSER DES CORRESPONDANCES
!     ENTRE NUMEROS D'EQUATIONS ET NOEUDS ET D.D.L.
        deeq = nomddl//'      .NUME.DEEQ'
        call jeveuo(deeq, 'L', iddeeq)
!
!-----------------------------------------------------------------------
!     2.2/ CREATION DE L'OBJET CHARGEMENT PROJETE DU .VALE
!-----------------------------------------------------------------------
!
        call jecroc(jexnom(chaval, nomsst))
        call jeecra(jexnom(chaval, nomsst), 'LONMAX', nbmod)
!
!-----------------------------------------------------------------------
!     2.3/ PROJECTION EFFECTIVE
!-----------------------------------------------------------------------
!
!     ALLOCATION DE LA PLACE POUR UN VECTEUR TEMPORAIRE
        call wkvect('&&'//pgc//'.VECTA', 'V V R', neq, idvect)
!
!     ACCES AU CHAMP DE CHARVAL ASSOCIE A NOMSST
        call jeveuo(jexnom(chaval, nomsst), 'E', iavale)
!
!     BOUCLE SUR LES MODES
        do 20 j = 1, nbmod
!
!     EXTRACTION DU CHAMP DE DEPLACEMENTS ASSOCIE AU MODE J
            call rsexch('F', basmod, 'DEPL', j, nomcha,&
                        iret)
            nomcha = nomcha(1:19)//'.VALE'
            call jeveuo(nomcha, 'L', iadmod)
!
!     RECOPIE DU CHAMP DANS LE VECTEUR TEMPORAIRE
            call dcopy(neq, zr(iadmod), 1, zr(idvect), 1)
!
!     MISE A ZERO DES D.D.L. DE LAGRANGE
            call zerlag(neq, zi(iddeeq), vectr=zr(idvect))
!
!     PRODUIT SCALAIRE SECOND MEMBRE ET MODE
            zr(iavale+j-1) = ddot(neq,zr(idvect),1,zr(ladrve),1)
!
 20     continue
        call jedetr('&&'//pgc//'.VECTA')
 50 continue
!
!     3/ ASSEMBLAGE
!     =============
!
!     ACCES AU PRNO DU PROF_CHNO GENERALISE, POUR LES SOUS-STRUCTURES.
    call jenonu(jexnom(profg//'.LILI', '&SOUSSTR'), ibid)
    call jeveuo(jexnum(profg//'.PRNO', ibid), 'L', ldprs)
!
!     ACCES AUX NOMS DES SOUS-STRUCTURES CHARGEES
    call jeveuo(jexnom(chalis, 'SOUSSTR'), 'L', ldstr)
!
!     BOUCLE SUR LES SOUS-STRUCTURES CHARGEES
    do 200 i = 1, nbchar
!
!     RECUPERATION DU NOM DE LA SOUS-STRUCTURE ET DU CHARGEMENT
!     PROJETE ASSOCIE.
        nomsst = zk8(ldstr+i-1)
!
!     RECUPERATION DU NUMERO GLOBAL DE LA SOUS-STRUCTURE
        if (elim .eq. 0) then
            call jenonu(jexnom(modgen//'      .MODG.SSNO', nomsst), nusst)
        else
!     CAS OU ON RECOURS A L'ELIMINATION
            do 60 i1 = 1, nbsst
                if (zk8(lsst+i1-1) .eq. nomsst) then
                    nusst=i1
                endif
 60         continue
        endif
!
!     RECUPERATION DU D.D.L. GENERALISE DE DEPART ET DU NOMBRE TOTAL
!     DE D.D.L., ASSOCIES A LA SOUS-STRUCTURE.
        if (elim .eq. 0) then
            nddl0 = zi(ldprs+2*nusst-2)
            nbmod = zi(ldprs+2*nusst-1)
        else
!     CAS OU ON RECOURS A L'ELIMINATION
            nddl0=0
            nbmod=0
            do 70 i1 = 1, nusst-1
                nddl0 = nddl0 + zi(lsilia+i1-1)
 70         continue
            nbmod = zi(lsilia+nusst-1)
        endif
!
!     ASSEMBLAGE DES VALEURS DE CHARGEMENT
!
!     LE VECTEUR GLOBAL EST POSITIONNE EN ZR(LRVAL) (DANS RESVAL)
!     CELUI LOCAL A LA SOUS-STRUCTURE EN ZR(IDVALE)
        call jeveuo(jexnom(chaval, nomsst), 'L', idvale)
!
!     BOUCLE SUR LES MODES DE LA SOUS-STRUCTURE
        if (elim .eq. 0) then
            do 80 j = 1, nbmod
                ipos=(nddl0 - 1) + (j - 1)
                zr(lrval+ipos) = zr(lrval+ipos) + zr(idvale+j-1)
 80         continue
        else
!     CAS OU ON RECOURS A L'ELIMINATION
!     ON RE PROJETTE SUR LA BASE T : F_proj = T^t * (Phi^t * F)
            do 100 j1 = 1, neqred
                zr(lrval+j1-1)=0.0d0
                do 90 i1 = 1, nbmod
                    zr(lrval+j1-1) = zr(lrval+j1-1) + zr(idvale+i1-1)* zr(lmapro+(j1-1)*neqet+ndd&
                                     &l0+i1-1)
 90             continue
100         continue
        endif
200 continue
!
    call jedema()
end subroutine

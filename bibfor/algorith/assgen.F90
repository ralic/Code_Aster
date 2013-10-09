subroutine assgen(nomres, option, nugene)
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 13/10/92
!-----------------------------------------------------------------------
!  BUT:      < ASSEMBLAGE GENERALISEE >
!
!  ASSEMBLER UNE MATRICE A PARTIR D'UNE NUMEROTATION GENERALISEE
!  ET D'UNE OPTION (RIGI_GENE,MASS_GENE,AMOR_GENE)
!
! REMARQUE : L'ASSEMBLAGE DONNE UNE MATRICE ASSEMBLEE LIGNE DE CIEL
!            IL CONSIDERE LES MATRICE ELEMENTAIRE GENERALISEES
!  A ASSEMBLER COMME DES BLOCS
!  CHAQUE MATRICE ELEMENTAIRE POUVANT ETRE CONSTITUE DE PLUSIEURS BLOCS
!  CE QUI SEMBLE COMPLIQUER NETTEMENT LA TACHE POUR LE MOMENT MAIS
!  LE TRAVAIL POUR CONSIDERE UNE MATRICE ASSEMBLEE LIGNE DE CIEL
!     COMME UNE MATRICE ELEMENTAIRE DEVRAIT ETRE MINIME
!
!-----------------------------------------------------------------------
!
! NOM----- / /:
!
! NOMRES   /I/: NOM K8 DE LA MATRICE GENERALISEE RESULTAT
! OPTION   /I/: OPTION DE CALCUL (RIGI_GENE,MASS_GENE)
! NUGENE   /I/: NOM K14 DE LA NUMEROTATION GENERALISEE
! STOLCI   /I/: NOM K19 DU STOCKAGE DE LA MATRICE (LIGN_CIEL)
!
!
!
#include "jeveux.h"
#include "asterfort/asgnbc.h"
#include "asterfort/asgnbn.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
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
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/prasml.h"
#include "asterfort/prasmp.h"
#include "asterfort/ualfva.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
!
    character(len=8) :: nomres, modgen, nomprn
    character(len=14) :: nugene
    character(len=19) :: prgene, stolci
    character(len=9) :: rigopt, ksst, lsst
    character(len=24) :: tmadbl, tmnobl, tminbl, tmnomb, tmnumb, tmrep, tmconl
    character(len=11) :: ricopt, option
    character(len=24) :: nomblo
    real(kind=8) :: zero, un
    real(kind=8) :: valr
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, iblel, iblo, icomp, j, jrefa
    integer :: jscde, ldblo, ldconl, ldlim, llorl, llprof, llref
    integer :: ltadbl, ltconl, ltinbl, ltnobl, ltnomb, ltnumb, nbblel
    integer :: nblia, nbloc, nbprno, nbsst, nbterm, neq, ntbloc
    integer :: ntprno, numblo
    real(kind=8) :: epsi, ssconl, ssmax, xcon, xmaxbl
!-----------------------------------------------------------------------
    data rigopt,ricopt/'RIGI_GENE','RIGI_GENE_C'/
    data ksst /'&SOUSSTR'/
    data lsst /'LIAISONS'/
    data zero , un / 0.0d+00 , 1.0d+00 /
    data epsi / 1.d-20 /
!-----------------------------------------------------------------------
!
!--------------------------CREATION DU .REFA----------------------------
!
!
    call jemarq()
    prgene=nugene//'.NUME'
    stolci=nugene//'.SLCS'
!
    call wkvect(nomres//'           .REFA', 'G V K24', 20, jrefa)
    zk24(jrefa-1+11)='MPI_COMPLET'
    zk24(jrefa-1+1)=' '
    zk24(jrefa-1+2)=nugene
    zk24(jrefa-1+8) = 'ASSE'
    zk24(jrefa-1+9) = 'MS'
    zk24(jrefa-1+10) = 'GENE'
!
!--------------------RECUPERATION DU MODE_GENE AMONT--------------------
!
    call jeveuo(prgene//'.REFN', 'L', llref)
    modgen=zk24(llref)(1:8)
!
!--
!-- VERIFICATION QU'IL S'AGIT BIEN D'UN MODELE GENERALIE, PAS D'UN MODE
!--   ( NUME_DDL_GENE CONSTRUIT AVEC "MODELE_GENE", PAS "BASE" )
!--
!
    call jeexin(modgen//'      .MODG.SSNO', ibid)
    if (ibid .eq. 0) then
        call utmess('F', 'MATRICE0_13')
    endif
!
!
!
!--------------------------CREATION DU .LIME----------------------------
!   POUR L'INSTANT ON DONNE LE NOM DU MODELE GENERALISE
!
    call wkvect(nomres//'           .LIME', 'G V K24', 1, ldlim)
    zk24(ldlim)=modgen
!
!
!------------------RECUPERATION DU NOMBRE DE SOUS-STRUCTURE-------------
!
    call jenonu(jexnom(prgene//'.LILI', ksst), ibid)
    call jelira(jexnum(prgene//'.PRNO', ibid), 'LONMAX', nbsst)
    nbsst=nbsst/2
!------------------RECUPERATION DU NOMBRE DE LIAISON-------------
!
    call jenonu(jexnom(prgene//'.LILI', lsst), ibid)
    call jelira(jexnum(prgene//'.PRNO', ibid), 'LONMAX', nblia)
    if (nblia .eq. 1) then
        call utmess('F', 'ALGORITH_32')
    endif
!
!
!--------------------RECUPERATION DES CARACTERISTIQUES BLOCS------------
!
    call jeveuo(stolci//'.SCDE', 'L', jscde)
    neq=zi(jscde-1+1)
    ntbloc=zi(jscde-1+2)
    nbloc=zi(jscde-1+3)
    call jelibe(stolci//'.SCDE')
    if (option .eq. ricopt) then
        call jecrec(nomres//'           .UALF', 'G V C', 'NU', 'DISPERSE', 'CONSTANT',&
                    nbloc)
    else
        call jecrec(nomres//'           .UALF', 'G V R', 'NU', 'DISPERSE', 'CONSTANT',&
                    nbloc)
    endif
    call jeecra(nomres//'           .UALF', 'LONMAX', ntbloc)
!
!------------------CREATION DU NOM A CONCATENER-------------------------
!   POUR RECUPERER LE NOM DES MATRICES PROJETEES
!
!
!------------------------RECUPERATION DU NOMBRE DE LIGRELS--------------
!
    call jelira(prgene//'.PRNO', 'NMAXOC', nbprno)
    if ((option.ne.rigopt) .and. (option.ne.ricopt)) nbprno=1
!
!--------------INITIALISATION DES NOMS OBJETS COURANTS------------------
!
! REPERTOIRE DES NOMS DES LIGRELS
    tmrep ='&&ASSGEN.REP.NOM.PROF'
!
! FAMILLE NOMMES, DONNANT POUR CHAQUE ELEMENTS A ASSEMBLER LE NUMERO
!  DE SON PREMIER BLOC DANS LA LISTE TOTALE
    tminbl='&&ASSGEN.BLOCEL.PROF'
!
! FAMILLE NUMEROTEE (NMAXOC=NOMBRE DE BLOCS ELEMENTAIRES) ET
! CHAQUE OBJET DIMMENSIONNE A LA TAILLE DU BLOC DONT IL PORTE LE
! NUMERO DONNE POUR CHAQUE TERME DU BLOC L'ADRESSE RELATIVE DANS
!  SON BLOC DE DESTINATION
    tmadbl='&&ASSGEN.BLOCEL.ADBLO'
!
! FAMILLE NUMEROTEE (NMAXOC=NOMBRE DE BLOCS ELEMENTAIRES) ET
! CHAQUE OBJET DIMMENSIONNE A LA TAILLE DU BLOC DONT IL PORTE LE
! NUMERO DONNE POUR CHAQUE TERME DU BLOC ELEMENTAIRE LE NUMERO DE SON
!  BLOC D'ARRIVEE
    tmnobl='&&ASSGEN.BLOCEL.NOBLO'
!
! VECTEUR DIMENSIONNE AU NOMBRE DE BLOCS ELEMENTAIRES ET DONNANT LE
!   NOM K24 DU BLOC OU DE LA FAMILLE CONTENNANT LE BLOC
    tmnomb='&&ASSGEN.NOM.BLOCEL'
!
! VECTEUR DIMENSIONNE AU NOMBRE DE BLOCS ELEMENTAIRES ET DONNANT LE
!   NUMERO  DU BLOC ELEMENTAIRE DANS SA FAMILLE OU 0
    tmnumb='&&ASSGEN.NUM.BLOCEL'
!
! VECTEUR DIMENSIONNE AU NOMBRE DE BLOCS ELEMENTAIRES ET DONNANT LE
!   COEF DE CONDITIONNEMENT DU BLOC
    tmconl='&&ASSGEN.CONL.BLOCEL'
!
!
    call jecreo(tmrep, 'V N K8')
    call jeecra(tmrep, 'NOMMAX', nbprno)
    call jecrec(tminbl, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbprno)
!
!--------------------COMPTAGE DU NOMBRE DE BLOCS ELEMENTAIRES----------
!
    icomp=0
!
!   BOUCLE SUR LE LIGRELS
!
    do i = 1, nbprno
        call jenuno(jexnum(prgene//'.LILI', i), nomprn)
        call jelira(jexnum(prgene//'.PRNO', i), 'LONMAX', ntprno)
        ntprno=ntprno/2
!
!  TEST SI ON EST SUR LE LIGREL DES SOUS-STRUCTURES
!
        if (nomprn .eq. ksst) then
            call jecroc(jexnom(tmrep, nomprn))
            call jenonu(jexnom(tmrep, nomprn), ibid)
            call jeecra(jexnum(tminbl, ibid), 'LONMAX', ntprno*2)
            call jeveuo(jexnum(tminbl, ibid), 'E', ltinbl)
!
!     BOUCLE SUR LES ELEMENTS DU LIGREL COURANTS
!           MATRICE PROJETEE=1BLOC
!
            do j = 1, ntprno
                icomp=icomp+1
                zi(ltinbl+(j-1)*2)=icomp
                zi(ltinbl+(j-1)*2+1)=1
            end do
!
!
! TEST SI ON EST SUR DES LAGRANGES ET SI L'OPTION EST RIGI_GENE
!
            elseif(nomprn.ne.ksst.and. (&
        option.eq.rigopt.or.option.eq.ricopt)) then
            call jecroc(jexnom(tmrep, nomprn))
            call jenonu(jexnom(tmrep, nomprn), ibid)
            call jeecra(jexnum(tminbl, ibid), 'LONMAX', ntprno*3)
            call jeveuo(jexnum(tminbl, ibid), 'E', ltinbl)
            call jeveuo(modgen//'      .MODG.LIPR', 'L', llprof)
            call jenonu(jexnom(prgene//'.LILI', nomprn), ibid)
            call jeveuo(jexnum(prgene//'.ORIG', ibid), 'L', llorl)
!
!    BOUCLE SUR LES ELEMENTS DU LIGREL COURANTS
!
            do j = 1, ntprno
! NUMERO DE BLOCS MATRICE LIAISON 1
                zi(ltinbl+(j-1)*3)=icomp+1
                icomp=icomp+1
! NUMERO DE BLOCS MATRICE LIAISON 2
                zi(ltinbl+(j-1)*3+1)=icomp+1
                icomp=icomp+1
! NUMERO LAGRANGE-LAGRANGE
                zi(ltinbl+(j-1)*3+2)=icomp+1
                icomp=icomp+1
            end do
!
            call jelibe(modgen//'      .MODG.LIPR')
!
        endif
    end do
!
!   NOMBRE DE BLOC ELEMENTAIRES A ASSEMBLER
    nbblel=icomp
!
    call jecrec(tmadbl, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbblel)
    call jecrec(tmnobl, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbblel)
    call wkvect(tmnomb, 'V V K24', nbblel, ltnomb)
    call wkvect(tmnumb, 'V V I', nbblel, ltnumb)
!
!
!---------------------REMPLISSAGE DES OBJETS DE TRAVAIL-----------------
!
    ssmax=zero
!
    call wkvect(nomres//'           .CONL', 'G V R', neq, ldconl)
    call wkvect(tmconl, 'V V R', nbblel, ltconl)
!
!      BOUCLE SUR LES LIGRELS
!
    do i = 1, nbprno
!
        call jenuno(jexnum(tmrep, i), nomprn)
!
!    CAS DU LIGRELS DES MATRICES PROJETEES
!
        call prasmp(option, nugene, tminbl, nomprn, modgen,&
                    tmnobl, tmadbl, zk24(ltnomb), zi(ltnumb), ssmax)
!
!
!    CAS D'UN LIGREL DE LAGRANGES ET OPTION=RIGI_GENE
!
        call prasml(option, nugene, tminbl, nomprn, modgen,&
                    tmnobl, tmadbl, zk24(ltnomb), zi(ltnumb), zr(ldconl),&
                    zr(ltconl))
!
    end do
!
!
    call jedetr(tminbl)
!
!------------------------------TRAITEMENT DU CONDITIONNEMENT-----------
!  ON TIENT COMPTE DES TERMES EXTRA DIAGONAUX DANS LES BLOCS PHYSIQUES
!
!  CONDITIONNEMENT = MAX(BLOC PHYSIQUE)/MAX(BLOC LAGRANGE)
!
    xmaxbl=0.d0
    do i = 1, nbblel
        xmaxbl=max(xmaxbl,abs(zr(ltconl+i-1)))
    end do
!
    if (xmaxbl .gt. epsi) then
        ssconl=ssmax/xmaxbl
    else
        ssconl=un
    endif
!
    valr = ssconl
    call utmess('I', 'ALGORITH14_79', sr=valr)
!
    do i = 1, nbblel
        if (zr(ltconl+i-1) .ne. zero) then
            zr(ltconl+i-1)=ssconl
        else
            zr(ltconl+i-1)=un
        endif
    end do
    do i = 1, neq
        if (zr(ldconl+i-1) .ne. zero) then
            zr(ldconl+i-1)=ssconl
        else
            zr(ldconl+i-1)=un
        endif
    end do
!
!-----------------------------------ASSEMBLAGE--------------------------
!
!    BOUCLE SUR LES BLOCS RESULTATS
!
    do iblo = 1, nbloc
!
        call jecroc(jexnum(nomres//'           .UALF', iblo))
        call jeveuo(jexnum(nomres//'           .UALF', iblo), 'E', ldblo)
!
!    BOUCLE SUR LES BLOCS ELEMENTAIRES
!
        do iblel = 1, nbblel
!
! PRISE EN COMPTE DU CONDITIONNEMENT
!
            xcon=zr(ltconl+iblel-1)
            call jeveuo(jexnum(tmadbl, iblel), 'L', ltadbl)
            call jeveuo(jexnum(tmnobl, iblel), 'L', ltnobl)
            nomblo=zk24(ltnomb+iblel-1)
            numblo=zi(ltnumb+iblel-1)
            call jelira(jexnum(tmnobl, iblel), 'LONMAX', nbterm)
            if (option .eq. 'RIGI_GENE_C') then
                call asgnbc(iblo, zc(ldblo), nbterm, zi(ltnobl), zi( ltadbl),&
                            nomblo, numblo, xcon)
!
            else
                call asgnbn(iblo, zr(ldblo), nbterm, zi(ltnobl), zi( ltadbl),&
                            nomblo, numblo, xcon)
!
            endif
            call jelibe(jexnum(tmadbl, iblel))
            call jelibe(jexnum(tmnobl, iblel))
!
        end do
!
        call jelibe(jexnum(nomres//'           .UALF', iblo))
!
    end do
!
    call ualfva(nomres, 'G')
!
    call jedetr(tmadbl)
    call jedetr(tmnobl)
    call jedetr(tmnomb)
    call jedetr(tmnumb)
    call jedetr(tmrep)
    call jedetr(tmconl)
!
    call jedema()
end subroutine

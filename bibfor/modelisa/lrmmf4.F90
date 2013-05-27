subroutine lrmmf4(nbrfam, carafa, nbnoeu, nbmail, nomgro,&
                  numgro, nument, grpnoe, gpptnn, grpmai,&
                  gpptnm, nbgrno, nbgrma, infmed, nivinf,&
                  ifm)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     LECTURE DU MAILLAGE - FORMAT MED - LES FAMILLES - 4
!     -    -     -                 -         -          -
!-----------------------------------------------------------------------
!
! ENTREES :
!   NBRFAM : NOMBRE DE FAMILLES POUR CE MAILLAGE
!   CARAFA : CARACTERISTIQUES DE CHAQUE FAMILLE
!     CARAFA(1,I) = NOMBRE DE GROUPES
!     CARAFA(2,I) = NOMBRE D'ATTRIBUTS
!     CARAFA(3,I) = NOMBRE D'ENTITES
!   NBNOEU : NOMBRE DE NOEUDS DU MAILLAGE
!   NBMAIL : NOMBRE DE MAILLES DU MAILLAGE
!   NOMGRO : COLLECTION DES NOMS DES GROUPES A CREER
!   NUMGRO : COLLECTION DES NUMEROS DES GROUPES A CREER
!   NUMENT : COLLECTION DES NUMEROS DES ENTITES DANS LES GROUPES
! SORTIES :
!  GRPNOE : OBJETS DES GROUPES DE NOEUDS
!  GRPMAI : OBJETS DES GROUPES DE MAILLES
!  NBGRNO : NOMBRE DE GROUPES DE NOEUDS
!  NBGRMA : NOMBRE DE GROUPES DE MAILLES
! DIVERS
!   INFMED : NIVEAU DES INFORMATIONS SPECIFIQUES A MED A IMPRIMER
!   NIVINF : NIVEAU DES INFORMATIONS GENERALES
!   IFM    : UNITE LOGIQUE DU FICHIER DE MESSAGE
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
!
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/lrmgrp.h'
    include 'asterfort/lrmngr.h'
    include 'asterfort/wkvect.h'
    integer :: nbrfam, carafa(3, nbrfam)
    integer :: nbnoeu, nbmail, nbgrno, nbgrma
    integer :: infmed
    integer :: ifm, nivinf
!
    character(len=24) :: grpnoe, grpmai, gpptnn, gpptnm
    character(len=*) :: nomgro, numgro, nument
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRMMF4' )
!
    integer :: codret
    integer :: iaux, jaux
    integer :: nbnufa, numgrp, nbgrou
    integer :: ngnmax, ngmmax
    integer :: jnogno, jnogma, jlggno, jlggma
    integer :: jfnomg, jfnumg
!
    character(len=8) :: saux08
    character(len=24) :: nomgrp
    character(len=24) :: nomgno, nomgma, longno, longma
    character(len=32) :: nomj
!
!     ------------------------------------------------------------------
!
    if (nivinf .ge. 2) then
!
        write (ifm,1001) nompro
        1001 format( 60('-'),/,'DEBUT DU PROGRAMME ',a)
!
    endif
!
!====
! 1. ALLOCATIONS
!====
!               12   345678   9012345678901234
    nomgno = '&&'//nompro//'.NOM_GR_NOE     '
    nomgma = '&&'//nompro//'.NOM_GR_MAI     '
    longno = '&&'//nompro//'.LON_GR_NOE     '
    longma = '&&'//nompro//'.LON_GR_MAI     '
!
    ngnmax = nbnoeu
    call wkvect(nomgno, 'V V K24', ngnmax, jnogno)
    call wkvect(longno, 'V V I', ngnmax, jlggno)
    ngmmax = nbmail
    call wkvect(nomgma, 'V V K24', ngmmax, jnogma)
    call wkvect(longma, 'V V I', ngmmax, jlggma)
!
    nbgrno = 0
    nbgrma = 0
!
!====
! 2. ON PASSE TOUTES LES FAMILLES EN REVUE
!====
!
    do 20 , iaux = 1 , nbrfam
!
!     SI OBJET DE COLL INEXISTANT -> PAS DE GROUPE DANS CETTE FAMILLE
!
    nomj = jexnum(nomgro,iaux)
    call jeexin(nomj, codret)
!
    if (codret .gt. 0) then
!
        nbnufa = carafa(3,iaux)
        call jeveuo(nomj, 'L', jfnomg)
        call jelira(nomj, 'LONMAX', nbgrou, saux08)
        call jeveuo(jexnum(numgro, iaux), 'L', jfnumg)
!
        if (infmed .ge. 3) then
            write (ifm,2001) iaux, nbgrou, nbnufa
        endif
        2001 format(&
     &  /,'. FAMILLE DE RANG ',i4,&
     &  /,'... NOMBRE DE GROUPES  : ',i9,&
     &  /,'... NOMBRE D''ENTITES   : ',i9)
!
        do 21 , jaux = 1 , nbgrou
!
        numgrp = zi (jfnumg+jaux-1)
        nomgrp = zk24(jfnomg+jaux-1)
!
!             CEST UN GROUPE DE NOEUDS  (>0)
!
        if (numgrp .gt. 0) then
            call lrmngr(ngnmax, nbgrno, numgrp, nomgrp, jnogno,&
                        jlggno, nbnufa, nomgno, longno)
!
!             CEST UN GROUPE DE MAILLES (<0)
!
        else if (numgrp.lt.0) then
            call lrmngr(ngmmax, nbgrma, -numgrp, nomgrp, jnogma,&
                        jlggma, nbnufa, nomgma, longma)
!
        endif
!
21      continue
!
    endif
!
    20 end do
!
!====
! 3. CREATION COLLECTIONS FINALES
!                    GROUPNO X -> NO I,NO J...
!                    GROUPMA Y -> MA I,MA J...
!====
!
    if (nbgrno .ne. 0 .or. nbgrma .ne. 0) then
!
        call lrmgrp(grpnoe, gpptnn, nbgrno, jnogno, jlggno,&
                    grpmai, gpptnm, nbgrma, jnogma, jlggma,&
                    nomgro, numgro, nument, nbrfam)
!
    endif
!
!====
! 6. LA FIN
!====
!
    call jedetr(nomgno)
    call jedetr(nomgma)
    call jedetr(longno)
    call jedetr(longma)
!
    if (nivinf .ge. 2) then
!
        write (ifm,6001) nompro
        6001 format(/,'FIN DU PROGRAMME ',a,/,60('-'))
!
    endif
!
end subroutine

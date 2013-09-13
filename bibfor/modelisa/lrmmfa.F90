subroutine lrmmfa(fid, nomamd, nbnoeu, nbmail, grpnoe,&
                  gpptnn, grpmai, gpptnm, nbgrno, nbgrma,&
                  typgeo, nomtyp, nmatyp, prefix, infmed,&
                  vecgrm, nbcgrm)
!
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
#include "asterfort/as_mfanfa.h"
#include "asterfort/as_mmhfnr.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lrmmf1.h"
#include "asterfort/lrmmf4.h"
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
    integer :: codret
    integer :: iaux
    integer :: ityp
    integer :: nbrfam
    integer :: adcarf, adfano, adtaba
    integer :: ifm, nivinf
    integer :: jnumty(ntymax), jfamma(ntymax)
!
    character(len=8) :: saux08
    character(len=24) :: famnoe, carafa, tabaux, gpptnn, gpptnm
    character(len=24) :: nomgro, numgro, nument
!
!GN      REAL*8 TPS1(4), TPS2(4)
!
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
    call infniv(ifm, nivinf)
!
    if (nivinf .ge. 2) then
!
        write (ifm,1001) nompro
        1001 format( 60('='),/,'DEBUT DU PROGRAMME ',a)
!
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
!               12   345678   9012345678901234
        famnoe = '&&'//nompro//'.FAMILLE_NO     '
        call wkvect(famnoe, 'V V I', nbnoeu, adfano)
!
        call as_mmhfnr(fid, nomamd, zi(adfano), nbnoeu, ednoeu,&
                       typnoe, codret)
!      DANS MED3.0, LE CODE RETOUR PEUT ETRE NEGATIF SANS POUR
!      AUTANT QU'IL Y AIT UN PROBLEME...
!      IF ( CODRET.NE.0 ) THEN
!        SAUX08='mmhfnr'
!        CALL U2MESG('F','DVP_97',1,SAUX08,1,CODRET,0,0.D0)
!      ENDIF
!
!
! 3.2. ==> LA FAMILLE D'APPARTENANCE DE CHAQUE MAILLE
!          ON DOIT LIRE TYPE PAR TYPE
!
        do 32 , ityp = 1 , ntymax
!
        if (nmatyp(ityp) .ne. 0) then
!
            call wkvect('&&'//nompro//'.FAMMA.'//nomtyp(ityp), 'V V I', nmatyp(ityp),&
                        jfamma(ityp))
            call as_mmhfnr(fid, nomamd, zi(jfamma(ityp)), nmatyp( ityp), edmail,&
                           typgeo(ityp), codret)
!         DANS MED3.0, LE CODE RETOUR PEUT ETRE NEGATIF SANS POUR
!         AUTANT QU'IL Y AIT UN PROBLEME...
!          IF ( CODRET.NE.0 ) THEN
!            SAUX08='mmhfnr'
!            CALL U2MESG('F','DVP_97',1,SAUX08,1,CODRET,0,0.D0)
!          ENDIF
!
        endif
!
        32     end do
!
!====
! 4. LECTURE DES CARACTERISTIQUES DES FAMILLES
!====
!
!
! 4.1. ==> ALLOCATIONS
!               12   345678   9012345678901234
        carafa = '&&'//nompro//'.CARAC_FAM      '
        tabaux = '&&'//nompro//'.TABAUX         '
        nomgro = '&&'//nompro//'.FANOMG         '
        numgro = '&&'//nompro//'.FANUMG         '
        nument = '&&'//nompro//'.FANUM          '
!
!     CARACTERISTIQUES DES FAMILLES
        iaux = 3*nbrfam
        call wkvect(carafa, 'V V I', iaux, adcarf)
!
!     COLLECTION             FAM I -> NOMGNO X , NOMGMA Y ...
        call jecrec(nomgro, 'V V K24', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbrfam)
!
!     COLLECTION             FAM I -> NUMGNO X , NUMGMA Y ...
        call jecrec(numgro, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbrfam)
!
!     COLLECTION INVERSE     FAM I -> NUMNO(MA) X, NUMNO(MA) Y..
        call jecrec(nument, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbrfam)
!
!     VECTEUR  TEMPO         FAM I -> NUMNO(MA)
        iaux = max ( nbnoeu, nbmail )
        call wkvect(tabaux, 'V V I', iaux, adtaba)
!
! 4.2. ==> RECUPERATION DU TABLEAU DES RENUMEROTATIONS
!
        do 42 , ityp = 1 , ntymax
        if (nmatyp(ityp) .ne. 0) then
            call jeveuo('&&'//prefix//'.NUM.'//nomtyp(ityp), 'L', jnumty(ityp))
        endif
        42     end do
!
! 4.3. ==> APPEL DU PROGRAMME DE LECTURE
!
        call lrmmf1(fid, nomamd, nbrfam, zi(adcarf), nbnoeu,&
                    zi( adfano), nmatyp, jfamma, jnumty, zi(adtaba),&
                    nomgro, numgro, nument, infmed, nivinf,&
                    ifm, vecgrm, nbcgrm)
!
! 4.4. ==> MENAGE PARTIEL
!
        call jedetr(tabaux)
!
!GN      WRITE (IFM,*)
!GN     >'TEMPS CPU POUR LIRE LES CARACTERISTIQUES DES FAMILLES  :',
!GN     >TPS2(4)
!
!====
! 5. CREATION DES VECTEURS DE NOMS DE GROUPNO + GROUPMA
!====
!
!
        call lrmmf4(nbrfam, zi(adcarf), nbnoeu, nbmail, nomgro,&
                    numgro, nument, grpnoe, gpptnn, grpmai,&
                    gpptnm, nbgrno, nbgrma, infmed, nivinf,&
                    ifm)
!
!GN      WRITE (IFM,*)
!GN     >'TEMPS CPU POUR CREER LES GROUPES  :',TPS2(4)
!
!====
! 6. LA FIN
!====
!
        call jedetr(famnoe)
        call jedetr(carafa)
        call jedetr(nomgro)
        call jedetr(numgro)
        call jedetr(nument)
        do 8 , ityp = 1 , ntymax
        if (nmatyp(ityp) .ne. 0) then
            call jedetr('&&'//nompro//'.FAMMA.'//nomtyp(ityp))
        endif
        8     end do
!
    endif
!
    call jedema()
!
    if (nivinf .ge. 2) then
!
        write (ifm,6001) nompro
        6001 format(/,'FIN DU PROGRAMME ',a,/,60('='))
!
    endif
!
!GN      WRITE (IFM,*) '==> DUREE TOTALE DE ',NOMPRO,' :',TPS1(4)
!
end subroutine

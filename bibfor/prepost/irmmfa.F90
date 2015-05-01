subroutine irmmfa(fid, nomamd, nbnoeu, nbmail, nomast,&
                  nbgrno, nomgno, nbgrma, nomgma, prefix,&
                  typgeo, nomtyp, nmatyp, infmed)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ECRITURE DU MAILLAGE - FORMAT MED - LES FAMILLES
!        -  -     -                 -         --
!-----------------------------------------------------------------------
!     L'ENSEMBLE DES FAMILLES EST L'INTERSECTION DE L'ENSEMBLE
!     DES GROUPES : UN NOEUD/MAILLE APPARAIT AU PLUS DANS 1 FAMILLE
!     TABLE  NUMEROS DES FAMILLES POUR LES NOEUDS  <-> TABLE  DES COO
!     TABLES NUMEROS DES FAMILLES POUR MAILLE/TYPE <-> TABLES DES CNX
!     PAR CONVENTION, LES FAMILLES DE NOEUDS SONT NUMEROTEES >0 ET LES
!     FAMILLES DE MAILLES SONT NUMEROTEES <0. LA FAMILLE NULLE EST
!     DESTINEE AUX NOEUDS / ELEMENTS SANS FAMILLE.
!     ENTREE:
!       FID    : IDENTIFIANT DU FICHIER MED
!       NOMAMD : NOM DU MAILLAGE MED
!       NBNOEU : NOMBRE DE NOEUDS DU MAILLAGE
!       NBMAIL : NOMBRE DE MAILLES DU MAILLAGE
!       NOMAST : NOM DU MAILLAGE ASTER
!       NBGRNO : NOMBRE DE GROUPES DE NOEUDS
!       NBGRMA : NOMBRE DE GROUPES DE MAILLES
!       NOMGNO : VECTEUR NOMS DES GROUPES DE NOEUDS
!       NOMGMA : VECTEUR NOMS DES GROUPES DE MAILLES
!       PREFIX : PREFIXE POUR LES TABLEAUX DES RENUMEROTATIONS
!       TYPGEO : TYPE MED POUR CHAQUE MAILLE
!       NOMTYP : NOM DES TYPES POUR CHAQUE MAILLE
!       NMATYP : NOMBRE D'ENTITES PAR TYPE
!       INFMED : NIVEAU DES INFORMATIONS A IMPRIMER
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/as_mfacre.h"
#include "asterfort/desgfa.h"
#include "asterfort/infniv.h"
#include "asterfort/irmmf1.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: fid
    integer :: typgeo(*), nmatyp(*)
    integer :: nbnoeu, nbmail, nbgrno, nbgrma
    integer :: infmed
!
    character(len=6) :: prefix
    character(len=8) :: nomast
    character(len=24) :: nomgno(*), nomgma(*)
    character(len=8) :: nomtyp(*)
    character(len=*) :: nomamd
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRMMFA' )
!
    integer :: tygeno
    parameter (tygeno=0)
!
    integer :: codret
    integer :: iaux, jaux, kaux
    integer :: numfam
    integer :: ino, natt
    integer :: ima
    integer :: jnofam
    integer :: jmafam
    integer :: ifm, nivinf
    integer :: tabaux(1)
!
    character(len=8) :: saux08
    character(len=24) :: nufano, nufama
    character(len=64) :: nomfam
    character(len=80) :: saux80
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
!     VECTEUR NUMEROS DES FAMILLES DES ENTITES = NB ENTITES
!     PAR DEFAUT, JEVEUX MET TOUT A 0. CELA SIGNIFIE QUE, PAR DEFAUT,
!     LES ENTITES APPARTIENNENT A LA FAMILLE NULLE.
!               12   345678   9012345678901234
    nufano = '&&'//nompro//'.NU_FAM_NOE     '
    nufama = '&&'//nompro//'.NU_FAM_MAI     '
!
    call wkvect(nufano, 'V V I', nbnoeu, jnofam)
    call wkvect(nufama, 'V V I', nbmail, jmafam)
!
!====
! 2. LES FAMILLES DE NOEUDS
!====
!
!
    iaux = tygeno
    call irmmf1(fid, nomamd, iaux, nbnoeu, nbgrno,&
                nomgno, zi(jnofam), nomast, prefix, typgeo,&
                nomtyp, nmatyp, infmed, nivinf, ifm)
!
!GN      WRITE (IFM,*)
!GN     >'TEMPS CPU POUR CREER/ECRIRE LES FAMILLES DE NOEUDS  :',TPS2(4)
!
!====
! 3. LES FAMILLES DE MAILLES
!====
!
!
    iaux = tygeno + 1
    call irmmf1(fid, nomamd, iaux, nbmail, nbgrma,&
                nomgma, zi(jmafam), nomast, prefix, typgeo,&
                nomtyp, nmatyp, infmed, nivinf, ifm)
!
!GN      WRITE (IFM,*)
!GN     >'TEMPS CPU POUR CREER/ECRIRE LES FAMILLES DE MAILLES :',TPS2(4)
!
!====
! 4. ON CREE TOUJOURS UNE FAMILLE DE NUMERO 0 NE REFERENCANT RIEN,
!    POUR LES NOEUDS ET ELEMENTS N'APPARTENANT A AUCUN GROUPE
!    REMARQUE : IL FAUT LE FAIRE A LA FIN POUR AVOIR LES BONNES
!    IMRPESSIONS
!====
!
! 4.1. ==> CARACTERISTIQUE
!
    numfam = 0
    natt = 0
!               12345678901234567890123456789012
    nomfam = 'FAMILLE_NULLE___________________'//'________________________________'
!
!
! 4.2. ==> INFORMATION EVENTUELLE
!
    if (infmed .ge. 2) then
!
        jaux = 0
        kaux = 0
        do 421 , ino = 1,nbnoeu
        if (zi(jnofam-1+ino) .eq. numfam) then
            jaux = jaux + 1
        endif
421      continue
        do 422 , ima = 1,nbmail
        if (zi(jmafam-1+ima) .eq. numfam) then
            kaux = kaux + 1
        endif
422      continue
        iaux = 0
        call desgfa(0, numfam, nomfam, iaux, saux80,&
                    natt, tabaux, jaux, kaux, ifm,&
                    codret)
!
    endif
!
! 4.3. ==> ECRITURE
!
    call as_mfacre(fid, nomamd, nomfam, numfam, 0,&
                   saux80, codret)
    if (codret .ne. 0) then
        saux08='mfacre'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
!====
! 5. LA FIN
!====
!
    call jedetr(nufano)
    call jedetr(nufama)
!
!GN      WRITE (IFM,*) '==> DUREE TOTALE DE ',NOMPRO,' :',TPS1(4)
!
    call jedema()
!
end subroutine

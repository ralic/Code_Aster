subroutine refdaj(arret, result, nbordr, numer, typre, conre, codret)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/refdag.h"
#include "asterfort/wkvect.h"
!     ------------------------------------------------------------------
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
! person_in_charge: hassan.berro at edf.fr
!     ------------------------------------------------------------------
!                              FONCTION
!     _______________________________________________________________
!    | AJOUTER UNE NOUVELLE ENTREE DE REFERENCE A UN RESULTAT        |
!    | DYNAMIQUE : CALCUL VIBRATOIRE OU CALCUL MODAL                 |
!    |_______________________________________________________________|
!
! ---------
! EXEMPLES: call refdaj ('F',modmec,nbordr,prchno,'DYNAMIQUE'  ,matric,iret)
! --------- call refdaj ('F',tragen,nbordr,numddl,'DYNAMIQUE'  ,matric,iret)
!           call refdaj ('F',ritzba,nbordr,numddl,'INTERF_DYNA',intdyn,iret)
!           call refdaj ('F',ritzba,nbordr,numddl,'INTERF_STAT',modsta,iret)
!
!                     DESCRIPTIVE DES VARIABLES
!   ___________________________________________________________________
!  | IN > ARRET  : 'F' OU ' ' : ON ARRETE AVEC ERREUR FATALE EN    [K1]|
!  |               CAS DE PROBLEME DE COMPATIBILITE ?                  |
!  |                                                                   |
!  | IN > RESULT : RESULTAT DYNAMIQUE A ENRICHIR                   [K8]|
!  |OUT <                                                              |
!  |                                                                   |
!  | IN > NBORDR : NOMBRE DE NUMEROS D'ORDRE (CHAMPS) QUI SERONT   [I] |
!  |               ARCHIVES DANS LA SD DYNAMIQUE AVEC CES REFERENCES   |
!  |                                                                   |
!  | IN > NUMER  : NUMEROTATION DES CONCEPTS DE REFERENCE          [K*]|
!  |               SOIT : NUME_DDL                                     |
!  |               SOIT : PROF_CHNO                                    |
!  |                                                                   |
!  | IN > TYPRE  : TYPE DE REFERENCE A AJOUTER                     [K4]|
!  |               POSSIBILITES : 'DYNAMIQUE'                          |
!  |                              'INTERF_DYNA'                        |
!  |                              'INTERF_STAT'                        |
!  |                              'MESURE'                             |
!  |                                                                   |
!  | IN > CONREF : LISTE DES NOMS DE SD A REFERENCER          ARR [K24]|
!   ___________________________________________________________________
!  |OUT < CODRET : CODE RETOUR                                      [I]|
!  |               SOIT : > 0 SI L'OPERATION S'EST DEROULEE AVEC SUCCES|
!  |                      = 1 SI UNE MODIFICATION A ETE APPORTEE       |
!  |                      = 2 SI AUCUNE MODIFICATION A ETE NECESSAIRE  |
!  |                      = 0 SI L'OPERATION A ECHOUEE                 |
!   ___________________________________________________________________
!
!   ___________________________________________________________________
!
!  - 0 - INITIALISATIONS DIVERSES
!   ___________________________________________________________________
!
!     0.1 - DECLARATION DES VARIABLES D'ENTREE/SORTIE
!
    character(len=1) :: arret
    character(len=8) :: result
    character(len=*) :: numer
    character(len=*) :: typre
    character(len=*) :: conre(3)
    integer :: nbordr, codret
!
!     0.2 - DECLARATION DES VARIABLES LOCALES
!
    logical :: oktres, newref, oktref
    integer :: lonref(4), indref, jrefe, nbrefs, nbrefsmax, nbinit, nbord1
    integer :: ibid, jbid, jindi, nbord0, ir, nbcham
    character(len=1) :: jvb
    character(len=8) :: k8bid, resu2
    character(len=24) :: typres, accres(10), accref(5), obindi, corefd, typref, kbid
    character(len=24) :: numer1, bl24, conref(3)
    character(len=32) :: jexnum
!
    data  accres /'ACOU_HARMO','DYNA_HARMO','DYNA_TRANS' ,'HARM_GENE','MODE_ACOU',&
                  'MODE_FLAMB','MODE_MECA' ,'MODE_MECA_C','TRAN_GENE','EVOL_NOLI'/

    data  accref /'DYNAMIQUE','INTERF_DYNA','INTERF_STAT','MESURE','INIT'/
    data  lonref / 3,1,1,1 /
!
    typref = typre
    conref = conre
    numer1 = numer
    codret = 0
    bl24   = '                        '
    jvb    = 'G'
    typres = ' '
    nbinit = 4
    nbord1 = nbordr
!
!     0.3 - ACTUALISATION DE LA VALEUR DE LA MARQUE COURANTE
!
    call jemarq()
!  ____________________________________________________________________
!
!  - 1 - VERIFICATION DES PARAMETRES D'ENTREE
!  ____________________________________________________________________
!
!     1.1 - CONDITION D'ARRET
    if ((arret.ne.'F') .and. (arret.ne.' ')) ASSERT(.false.)
!
    call getres(resu2, typres, kbid)
    if (result(1:2) .eq. '&&') then
        jvb = 'V'
    else if (result .eq. ' ') then
        result = resu2
    else if (result .ne. resu2) then
        call gettco(result, typres)
    endif
!
!     1.2 - PRESENCE DU .INDI (OBJET INT) ET .REFD (COLLECTION D'OBJ K8)
    newref = .false.
    obindi = result//'           .INDI'
    corefd = result//'           .REFD'
    call jeexin(obindi, ibid)
    call jeexin(corefd, jbid)
    if ((ibid*jbid) .eq. 0 .and. (ibid+jbid) .ne. 0) then
        if (arret .eq. 'F') ASSERT(.false.)
        codret = 0
        goto 27
    endif
    if ((ibid+jbid) .eq. 0) newref = .true.
!
!     1.3 - TYPE DE RESULTAT A TRAITER, CAS D'UN CONCEPT RE-ENTRANT
    if (.not.(newref)) then
        oktres = .false.
        do 12 ibid = 1, 10
            if (typres .eq. accres(ibid)) oktres = .true.
12      continue
        if (.not.(oktres)) then
            if (arret .eq. 'F') ASSERT(.false.)
            codret = 0
            goto 27
        endif
    endif
!
!     1.4 - TYPE DE REFERENCE A AJOUTER
    oktref = .false.
    do 15 ibid = 1, 5
        if (typref .eq. accref(ibid)) then
            indref = ibid
            oktref = .true.
            goto 16
        endif
15  continue
16  continue
    if (.not.(oktref)) then
        if (arret .eq. 'F') ASSERT(.false.)
        codret = 0
        goto 27
    endif
!  ____________________________________________________________________
!
!  - 2 - AJOUT DES ENTREES DE REFERENCE DANS LES .INDI ET .REFD
!  ____________________________________________________________________
!     2.1 - CREATION DU .INDI ET .REFD SI BESOIN
    if (newref) then
        call wkvect(obindi, jvb//' V I', nbinit, jbid)
        call jecrec(corefd, jvb//' V K24', 'NU', 'CONTIG', 'CONSTANT',nbinit)
        call jeecra(corefd, 'LONT', nbinit*5, k8bid)
!       INITIALISATION DES ELEMENTS DE .INDI A (-100)
        do 17 ibid = 1, nbinit
            zi(jbid+ibid-1) = -100
17      continue
        if (typref .eq. 'INIT') then
            codret = 1
            goto 27
        endif
    endif
!
!     2.2 - RAJOUT SI BESOIN DE L'ENTREE DE REF. A LA COLLECTION REFD
    call jelira(corefd, 'NUTIOC', nbrefs, k8bid)
    call jelira(corefd, 'LONT'  , nbrefsmax, k8bid)
    nbrefsmax = nbrefsmax/5
!
!     2.2.1 - VERIFIER QUE LE NOMBRE MAX D'OBJETS DE LA COLLECTION REFD
!             N'A PAS DEJA ETE ETEINT, DOUBLER SA TAILLE LE CAS ECHEANT
    if (nbrefs .ge. nbrefsmax) then
        call refdag(result)
    endif
!
!     2.2.2 - VERIFIER SI L'ENTREE DE REFERENCE EXISTE DEJA
!             CRITERES DE LA VERIFICATION :
!             1. TYPE D'ENTREE IDENTIQUE
!             2. NUMEROTATION IDENTIQUE
!             3. PREMIERE ENTREE IDENTIQUE
    if (nbrefs .ge. 1) then
        do 20 ibid = 1, nbrefs
            call jeveuo(jexnum(corefd, ibid), 'E', jrefe)
            if ( (typre   .eq. zk24(jrefe))  .and. &
                 (numer   .eq. zk24(jrefe+1)).and. &
                 (conre(1).eq. zk24(jrefe+2)) ) then
!               --- ALORS METTRE A JOUR L'ENTREE DU .REFD
                do 21 jbid = 1, lonref(indref)
                    zk24(jrefe+jbid+1) = conref(jbid)
21              continue
!               --- VERIFIER EGALEMENT ET METTRE A JOUR LE .INDI SI LE NOMBRE DE 
!                 - NUMEROS D'ORDRES INITIAL N'A PAS ETE CORRECTEMENT RENSEIGNE
!                 - (N2 - N1 = -1 / APPEL A REFDAJ AVEC NBCHAM = -1 )
                if (ibid .eq. nbrefs) then
                    nbord0 = 0
                    call jeveuo(obindi, 'E', jindi)
                    if (ibid .gt. 1) nbord0 = zi(jindi+nbrefs-1)
                    if ((zi(jindi+nbrefs) - nbord0) .eq. -1) then
                        zi(jindi+nbrefs) = nbord0 + nbord1
                    endif
                endif
                codret = 1
                goto 27
            endif
20      continue
    endif
!
!   2.2.2 - RAJOUTER UN OBJET A LA COLLECTION .REFD AVEC LES INFOS
!           DE CONREF ET M.A.J L'OBJET .INDI
!
!   --- CUMULER L'INDICE CORRESPONDANT A LA NOUVELLE REFERENCE
    nbord0 = 0
    call jeveuo(obindi, 'E', jindi)
    if (nbrefs .ge. 1) then
        nbord0 = zi(jindi+nbrefs-1)
    else 
!       --- TRAITEMENT SPECIAL SI IL S'AGIT DE LA PREMIERE ENTREE INDI :
!           SI nbord1 EST EGALE A -1 ET UN NOMBRE NON NULL DES CHAMPS EST DEJA STOCKE
!           DANS LA SD : ON Y MET CE NOMBRE
        call dismoi('C', 'NB_CHAMPS', result, 'RESU_DYNA', nbcham, k8bid, ir)
        if ((ir .eq. 0) .and. (nbcham .ne. 0)) then
            nbord1 = nbcham
        endif
    endif
!
    zi(jindi+nbrefs) = nbord0 + nbord1
!
!   --- RAJOUTER UNE ENTREE A LA COLLECTION .REFD
    call jecroc(jexnum(corefd, nbrefs+1))
    call jeveuo(jexnum(corefd, nbrefs+1), 'E', jrefe)
    zk24(jrefe) = typre
    zk24(jrefe+1) = numer1
    do 25 ibid = 1, lonref(indref)
        zk24(jrefe+ibid+1) = conref(ibid)
25  continue
    if (lonref(indref) .lt. 3) then
        do 26 ibid = lonref(indref)+1, 3
            zk24(jrefe+ibid+1) = bl24
26      continue
    endif
    codret = 1
!
27  continue
!
!   For debugging purposes only
!   call utimsd(6, 1, .false., .true., corefd,1, 'G')
!   call utimsd(6, 1, .false., .true., obindi,1, 'G')
!
    call jedema()
!
end subroutine
subroutine conori(ma)
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
!  ROUTINE CONORI
!    ROUTINE RACINE D'ORIENTATION DES MAILLES DE FISSURE
!  DECLARATIONS
!    NBGCO  : NOMBRE DE GROUPE DE FISSURE
!    IC     : NB D'OCCURENCE DE GROUP_MA
!    ICOC   : INDICE COURANT DES CONNEX     POUR UNE MAILLE FISSURE
!    ICOR   : INDICE COURANT DES CONNEX     POUR UNE MAILLE REFERENCE
!    IDUM   : ENTIER DE TRAVAIL
!    IFM    : IUNIT DU FICHIER MESSAGE
!    IGCO   : INDICE COURANT SUR LES GROUPE DE FISSURE
!    IGMA   : INDICE COURANT SUR LES GROUP_MA
!    IMAC   : INDICE COURANT DES MAILLES    POUR UNE MAILLE FISSURE
!    IMAG   : INDICE COURANT SUR LES MAILLES D UN GROUPE
!    IMAR   : INDICE COURANT DES MAILLES    POUR UNE MAILLE REFERENCE
!    IMICOC : INDICE DE  CONNEX    DANS ZK8 POUR UNE MAILLE FISSURE
!    IMICOR : INDICE DE  CONNEX    DANS ZK8 POUR UNE MAILLE REFERENCE
!    IMIGMA : INDICE DE  GROUPEMA  DANS ZI
!    IMITYC : INDICE DE  TYPMAIL   DANS ZI  POUR UNE MAILLE FISSURE
!    IMITYR : INDICE DE  TYPMAIL   DANS ZI  POUR UNE MAILLE REFERENCE
!    INOC   : NUMERO D UN NOEUD             POUR UNE MAILLE FISSURE
!    INOR   : NUMERO D UN NOEUD             POUR UNE MAILLE REFERENCE
!    IO8GCO : INDICE DE OP0154 NOGCO DANS ZK8
!    ITYC   : INDICE COURANT DU TYPE        POUR UNE MAILLE FISSURE
!    ITYR   : INDICE COURANT DU TYPE        POUR UNE MAILLE REFERENCE
!    JEXNOM : FUNCTION D ASTER
!    JEXNUM : FUNCTION D ASTER
!    KBID   : CHARACTER DE TRAVAIL
!    KMAC   : NOM D UNE MAILLE              POUR UNE MAILLE FISSURE
!    KMAR   : NOM D UNE MAILLE              POUR UNE MAILLE REFERENCE
!    KNOC   : NOM D UN NOEUD                POUR UNE MAILLE FISSURE
!    KNOR   : NOM D UN NOEUD                POUR UNE MAILLE REFERENCE
!    KTYC   : NOM DU TYPE                   POUR UNE MAILLE FISSURE
!    KTYR   : NOM DU TYPE                   POUR UNE MAILLE REFERENCE
!    LOCONT : LOGICAL PRECISANT SI LA MAILLE EST UNE MAILLE FISSURE
!    LOMODI : LOGICAL PRECISANT SI LA MAILLE EST UNE MAILLE MODIFIE
!    LOREOR : LOGICAL PRECISANT SI LA MAILLE EST UNE MAILLE REORIENTEE
!    MA     : L OBJET DU MAILLAGE
!    MACOC  : TABLEAU DES NOMS DES NOEUDS   POUR UNE MAILLE FISSURE
!    MACOR  : TABLEAU DES NOMS DES NOEUDS   POUR UNE MAILLE REFERENCE
!    NBCOC  : NOMBRE DE CONNEX              POUR UNE MAILLE FISSURE
!    NBCOR  : NOMBRE DE CONNEX              POUR UNE MAILLE REFERENCE
!    NBGMA  : NOMBRE DE GROUP_MA
!    NBMAG  : NOMBRE DE MAILLE DANS UN GROUP_MA
!    NBMAR  : NOMBRE DE MAILLE              POUR UNE MAILLE REFERENCE
!    NBNOMX : NOMBRE DE NOEUD MAXIMUM POUR UNE MAILLE ( 100 )
!    NIV    : NIVEAU D'IMPRESSION (OPTION INFO)
!
!
!
    implicit none
!
!
#include "jeveux.h"
#include "asterc/chkmsg.h"
#include "asterfort/conini.h"
#include "asterfort/contac.h"
#include "asterfort/getvem.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    integer :: idum, ic, ifm, niv, ichk
    integer :: io8gco, nbgco, igco
    integer :: imigma, nbgma, igma
    integer :: nbmag, imag
    integer :: imac
    integer :: imityc, ityc
    integer :: imicoc, nbcoc, icoc
    integer :: inoc
    integer :: nbmar, imar
    integer :: imicor, nbcor, icor
    integer :: iatyma
!
    character(len=8) :: kmac, ktyc, knoc, kmar, ktyr, knor
    character(len=8) :: ma, kbid
!
    logical :: lomodi, loreo0, loreor, lomod0, locor0, lface, lface0
!
!-----------------------------------------------------------------------
    integer :: i, ikmar, iktyr, imai, imarc, imaz, inoe
    integer :: inor, jmb, jmic, nbmac, nbmarc, nbnoe, nbnomx
!
!-----------------------------------------------------------------------
    parameter(nbnomx=100)
    character(len=8) :: macor(nbnomx+2), macoc(nbnomx+2), macos(nbnomx+2)
    integer :: iarg
!
    call jemarq()
!
!     ==================================================================
!     ------------------------------------------------------------------
!     FORMAT ET UNIT D ECRITURE ET NIVEAU D'IMPRESSION
!     ------------------------------------------------------------------
    call infniv(ifm, niv)
!     ------------------------------------------------------------------
!     RECHERCHE DU NOMBRE DE GROUP_MA_FISSURE DANS .COMM
!     ------------------------------------------------------------------
    ic=1
    call getvem(ma, 'GROUP_MA', 'ORIE_FISSURE', 'GROUP_MA', ic,&
                iarg, 0, kbid, nbgco)
    nbgco=-nbgco
!
!     ==================================================================
    if (nbgco .ne. 0) then
!     ------------------------------------------------------------------
!     RECHERCHE DU NOMBRE DE GROUP_MA DANS .MAIL
!     ------------------------------------------------------------------
        call jelira(ma//'.GROUPEMA', 'NUTIOC', nbgma)
        if (niv .eq. 2) then
            write (ifm,*)' '
            write (ifm,*)' LA LISTE DES GROUP_MA '
            write (ifm,*)' '
        endif
!     ------------------------------------------------------------------
!     RECHERCHE DES NOMS DES GROUP_MA DANS .MAIL
!     ------------------------------------------------------------------
        do 10 igma = 1, nbgma
            call jenuno(jexnum(ma//'.GROUPEMA', igma), kbid)
            if (niv .eq. 2) then
                write (ifm,*)'   GROUP_MA     : ',kbid
            endif
10      continue
        write (ifm,*)' '
!     ------------------------------------------------------------------
!     CREATION D UN TABLEAU DE TRAVAIL
!     ------------------------------------------------------------------
        call wkvect('&&OP0154.NOGCO', 'V V K24', nbgco, io8gco)
!     ------------------------------------------------------------------
!     RECHERCHE DES NOMS DES GROUP_MA_FISSURE DANS .COMM
!     ------------------------------------------------------------------
        call getvem(ma, 'GROUP_MA', 'ORIE_FISSURE', 'GROUP_MA', ic,&
                    iarg, nbgco, zk24(io8gco), idum)
        if (niv .eq. 2) then
            write (ifm,*)' '
            write (ifm,*)' LA LISTE DES ORIE_FISSURE'
            write (ifm,*)' '
            do 20 igco = 1, nbgco
                write (ifm,*)'   ORIE_FISSURE: ',zk24(io8gco+igco-1)
20          continue
            write (ifm,*)' '
        endif
!     ------------------------------------------------------------------
        call jelira(ma//'.NOMMAI', 'NOMUTI', nbmar)
        call jelira(ma//'.NOMNOE', 'NOMUTI', nbnoe)
!
        call wkvect('&&OP0154.NOE', 'V V I', nbnoe, inoe)
        call wkvect('&&OP0154.MAI', 'V V I', nbmar, imai)
        call wkvect('&&OP0154.MAR', 'V V I', nbmar, imaz)
        call wkvect('&&OP0154.KMR', 'V V K8', nbmar, ikmar)
        call wkvect('&&OP0154.KTR', 'V V K8', nbmar, iktyr)
        call wkvect('&&OP0154.IMI', 'V V I', nbmar, jmic)
        call wkvect('&&OP0154.MBL', 'V V I', nbmar, jmb)
        call conini(ma, zi(inoe), zi(imai), zi(imaz), nbmar,&
                    nbnoe, nbmarc, zk8(ikmar), zi(jmic), zi(jmb),&
                    zk8(iktyr), nbgco, io8gco)
        write (ifm,*)'NOMBRE DE MAILLES DE REFERENCE TESTEES : ',&
        nbmarc
!
!     ==================================================================
!     ------------------------------------------------------------------
!     BOUCLE SUR LES GROUPE_MA_FISSURE
!     ------------------------------------------------------------------
        do 90 igco = 1, nbgco
!     ------------------------------------------------------------------
!     RECHERCHE D EXISTENCE DU GROUP_MA_FISSURE CONSIDERE
!     ------------------------------------------------------------------
            call jenonu(jexnom(ma//'.GROUPEMA', zk24(io8gco+igco-1)), igma)
!
            if (niv .eq. 2) then
                write (ifm,*)' '
                write (ifm,*)' TRAITEMENT DE ',zk24(io8gco+igco-1)
                write (ifm,*)' '
            endif
            if (igma .eq. 0) then
!     ------------------------------------------------------------------
!     TRAITEMENT DU CAS DE NON-EXISTENCE
!     ------------------------------------------------------------------
                call u2mesk('I', 'ALGORITH2_26', 1, zk24(io8gco+igco-1))
!
            else
!     ------------------------------------------------------------------
!     TRAITEMENT DU CAS D EXISTENCE
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!     RECHERCHE DE L'ADRESSE DU GROUP_MA DANS ZI
!     ------------------------------------------------------------------
                call jeveuo(jexnum(ma//'.GROUPEMA', igma), 'L', imigma)
!     ------------------------------------------------------------------
!     RECHERCHE DU NOMBRE DE MAILLE DU GROUP_MA
!     ------------------------------------------------------------------
                call jelira(jexnum(ma//'.GROUPEMA', igma), 'LONMAX', nbmag)
                if (niv .eq. 2) then
                    write (ifm,*)'   LA LISTE DES MAILLES DU GROUPE '
                    write (ifm,*)' '
                endif
!
!     ------------------------------------------------------------------
!     BOUCLE SUR LES MAILLES DU GROUP_MA
!     ------------------------------------------------------------------
                do 80 imag = 1, nbmag
                    imac=zi(imigma+imag-1)
!     ------------------------------------------------------------------
!     RECHERCHE DU NOM DE LA MAILLE
!     ------------------------------------------------------------------
                    call jenuno(jexnum(ma//'.NOMMAI', imac), kmac)
!     ------------------------------------------------------------------
!     RECHERCHE DE L'ADRESSE DU TYPE DE LA MAILLE DANS ZI
!     ------------------------------------------------------------------
                    call jeveuo(ma//'.TYPMAIL', 'L', iatyma)
                    imityc=iatyma-1+imac
                    ityc=zi(imityc)
!     ------------------------------------------------------------------
!     RECHERCHE DU TYPE DE LA MAILLE DANS CATA.TM.NOMTM
!     ------------------------------------------------------------------
                    call jenuno(jexnum('&CATA.TM.NOMTM', ityc), ktyc)
                    if (niv .eq. 2) then
                        write (ifm,*)'     MAILLE NU : ',imag,' NOM : ',kmac,&
     &            ' ORDRE : ',imac,' TYPE : ',ityc,' TYPE : ',ktyc
                    endif
                    macoc(1)=kmac
                    macoc(2)=ktyc
!
!     ------------------------------------------------------------------
!     RECHERCHE DE L ADRESSE DES CONNEXIONS DE LA MAILLE
!     ------------------------------------------------------------------
                    call jeveuo(jexnum(ma//'.CONNEX', imac), 'E', imicoc)
!     ------------------------------------------------------------------
!     RECHERCHE DU NOMBRE DE CONNEXIONS DE LA MAILLE
!     ------------------------------------------------------------------
                    call jelira(jexnum(ma//'.CONNEX', imac), 'LONMAX', nbcoc)
!
!     ------------------------------------------------------------------
!     BOUCLE SUR LES CONNEXIONS DE LA MAILLE
!     ------------------------------------------------------------------
                    do 30 icoc = 1, nbcoc
                        inoc=zi(imicoc+icoc-1)
!     ------------------------------------------------------------------
!     RECHERCHE DU NOM DU NOEUD
!     ------------------------------------------------------------------
                        call jenuno(jexnum(ma//'.NOMNOE', inoc), knoc)
                        macoc(icoc+2)=knoc
30                  continue
!
!     ------------------------------------------------------------------
!     SAUVEGARDE DE LA MAILLE DE FISSURE
!     ------------------------------------------------------------------
                    do 40 idum = 1, nbcoc+2
                        macos(idum)=macoc(idum)
40                  continue
!     ==================================================================
!     ------------------------------------------------------------------
!     BOUCLE SUR LES MAILLES DU MAILLAGE
!     ------------------------------------------------------------------
                    lomodi=.false.
                    loreor=.false.
                    nbmac=0
                    do 60 imarc = 1, nbmarc
                        imar=zi(imaz-1+imarc)
!     ------------------------------------------------------------------
!     RECHERCHE DU NOM DE LA MAILLE
!     ------------------------------------------------------------------
                        kmar=zk8(ikmar-1+imar)
!     ------------------------------------------------------------------
!     RECHERCHE DU TYPE DE LA MAILLE
!     ------------------------------------------------------------------
                        ktyr=zk8(iktyr-1+imar)
!
                        macor(1)=kmar
                        macor(2)=ktyr
!
!     ------------------------------------------------------------------
!     RECHERCHE DE L ADRESSE DES CONNEXIONS DE LA MAILLE
!     ------------------------------------------------------------------
                        imicor=zi(jmic-1+imar)
!     ------------------------------------------------------------------
!     RECHERCHE DU NOMBRE DE CONNEXIONS DE LA MAILLE
!     ------------------------------------------------------------------
                        nbcor=zi(jmb-1+imar)
!     ------------------------------------------------------------------
!     BOUCLE SUR LES CONNEXIONS DE LA MAILLE
!     ------------------------------------------------------------------
                        do 50 icor = 1, nbcor
                            inor=zi(imicor+icor-1)
!     ------------------------------------------------------------------
!     RECHERCHE DU NOM DU NOEUD
!     ------------------------------------------------------------------
                            call jenuno(jexnum(ma//'.NOMNOE', inor), knor)
                            macor(icor+2)=knor
50                      continue
!     ==================================================================
!     ------------------------------------------------------------------
!     APPEL DE CONTAC
!                      ORIENTATION DE LA MAILLE FISSURE SI NECESSAIRE
!                      LOMODI = .TRUE.  SI MODIFICATION
!                      LOMODI = .FALSE. SINON
!     ------------------------------------------------------------------
                        lomod0=.false.
                        locor0=.false.
                        call contac(macor, nbcor, macoc, nbcoc, lface0,&
                                    lomod0, locor0, loreo0, ma)
                        if (loreo0) lface0= .not. lface0
                        if (locor0 .or. lomod0) then
                            nbmac=nbmac+1
                            if (niv .eq. 2) then
                                write (ifm,*)'LA MAILLE DE FISSURE   ',macoc(1),&
     &                ' DE TYPE ',macoc(2)
                                write (ifm,*)(macoc(i+2),i=1,nbcoc)
                                write (ifm,*)'S''APPUIE SUR LA MAILLE ',macor(1),&
     &                ' DE TYPE ',macor(2)
                                write (ifm,*)(macor(i+2),i=1,nbcor)
                                if (lface0) then
                                    write (ifm,*)'PAR SA FACE INFERIEURE'
                                else
                                    write (ifm,*)'PAR SA FACE SUPERIEURE'
                                endif
                                if (lomod0) then
                                    write (ifm,*)&
     &                  'UNE REORIENTATION POUR L''APPUI A EU LIEU'
                                endif
                                if (loreo0) then
                                    write (ifm,*)&
     &                  'UNE REORIENTATION POUR LA NORMALE A EU LIEU'
                                endif
                                write (ifm,*)
                            endif
                            if (nbmac .eq. 3) call u2mess('F', 'ALGORITH2_30')
                            if (nbmac .eq. 2 .and. (lface0.eqv. lface)) then
                                call u2mess('F', 'ALGORITH2_30')
                            endif
                            lface=lface0
                            if (lomod0) lomodi=.true.
                            if (loreo0) loreor=.true.
                            if (nbmac .eq. 2 .and. (lomod0.or.loreo0)) then
                                call u2mess('F', 'ALGORITH2_30')
                            endif
                        endif
!
!     ==================================================================
60                  continue
                    if (nbmac .eq. 0) call u2mess('F', 'ALGORITH2_30')
!
                    if (lomodi .or. loreor) then
!     ------------------------------------------------------------------
!     ECRITURE DES MAILLES MODIFIEES
!     ------------------------------------------------------------------
                        if (niv .eq. 2) then
                            write (ifm,*)' '
                            write (ifm,*)'       MODIFICATION DE LA MAILLE'
                            write (ifm,*)' '
                            write (ifm,*)'       AVANT'
                            write (ifm,9000)(macos(idum),idum=1,nbcoc+&
                            2)
                            write (ifm,*)'       APRES'
                            write (ifm,9000)(macoc(idum),idum=1,nbcoc+&
                            2)
                            write (ifm,*)' '
                        endif
!
                        do 70 icoc = 1, nbcoc
                            knoc=macoc(icoc+2)
!     ------------------------------------------------------------------
!     RECHERCHE DE L'ORDRE DU NOEUD
!     ------------------------------------------------------------------
                            call jenonu(jexnom(ma//'.NOMNOE', knoc), inoc)
!     ------------------------------------------------------------------
!     MODIFICATION DE L ORIENTATION DE LA MAILLE
!     ------------------------------------------------------------------
                            zi(imicoc+icoc-1)=inoc
70                      continue
                    endif
!     ==================================================================
!
80              continue
!     ------------------------------------------------------------------
            endif
!     ------------------------------------------------------------------
90      continue
!
    endif
!     ------------------------------------------------------------------
!     ==================================================================
!     EMISSION D'UNE ERREUR <F> SI UNE ERREUR <E> S'EST PRODUITE
    call chkmsg(0, ichk)
    call jedema()
!
    9000 format (6x,6(2x,a8),(/,26x,4(2x,a8)))
end subroutine

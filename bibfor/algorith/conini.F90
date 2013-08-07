subroutine conini(ma, noecon, maicon, marcon, nbmar,&
                  nbnoe, nbmarc, nommar, jmicor, mbcor,&
                  nomtyr, nbgco, io8gco)
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
!  ROUTINE CONINI
!    ROUTINE DE PREPARATION DE TABLEAUX PERMETTANT D'OPTIMISER
!    LA ROUTINE CONORI
!  DECLARATIONS
!    NBGCO  : NOMBRE DE GROUPE DE FISSURE
!    IC     : NB D'OCCURENCE DE GROUP_MA
!    ICOC   : INDICE COURANT DES CONNEX     POUR UNE MAILLE FISSURE
!    ICOR   : INDICE COURANT DES CONNEX     POUR UNE MAILLE REFERENCE
!    IDUM   : ENTIER DE TRAVAIL
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
!    NBMAG  : NOMBRE DE MAILLE DANS UN GROUP_MA
!    NBMAR  : NOMBRE DE MAILLE              POUR UNE MAILLE REFERENCE
!
!  MOT_CLEF : ORIE_FISSURE
!
!
    implicit none
!
!
#include "jeveux.h"
!
#include "asterfort/infniv.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: io8gco, nbgco, igco
    integer :: imigma, igma
    integer :: nbmag, imag
    integer :: imac
    integer :: imityc, ityc
    integer :: imicoc, nbcoc, icoc
    integer :: inoc
    integer :: nbmar, imar
    integer :: imityr, ityr
    integer :: imicor, nbcor, icor
    integer :: iatyma
!
    character(len=8) :: kmac, ktyc, kmar, ktyr
    character(len=24) :: valk(2)
    character(len=8) :: ma
!
    logical :: inval
    logical :: cas2d, cas3d, valid
!
    integer :: noecon(nbnoe), maicon(nbmar), marcon(nbmar)
    integer :: mbcor(nbmar), jmicor(nbmar)
    character(len=8) :: nommar(nbmar), nomtyr(nbmar)
    integer :: ierr, ifm, imai, inoe, inor, itest, nbcom
    integer :: nbmarc, nbnoe, niv
!-----------------------------------------------------------------------
!     TYPES VALIDES POUR LES MAILLES DE REFERENCE
    valid()=(cas2d .and. (ktyr(:4).eq.'TRIA'.or.ktyc(:&
     &        4).eq.'QUAD')) .or. (cas3d .and.&
     &        (ktyr(:5).eq.'PENTA'.or.ktyr(:4).eq.'HEXA'.or.ktyr(:&
     &        5).eq.'PYRAM'.or.ktyr(:5).eq.'TETRA'))
!
!
    inval=.false.
    cas2d=.false.
    cas3d=.false.
!CC     ON COMMENTE JEMARQ CAR ADRESSES PASSEES EN ARGUMENT
!CC      CALL JEMARQ()
    call infniv(ifm, niv)
!
!     ==================================================================
!
    do 10 inoe = 1, nbnoe
        noecon(inoe)=0
10  end do
!
    do 20 imai = 1, nbmar
        maicon(imai)=0
20  end do
!
    nbmarc=0
    ierr=0
30  continue
!     ------------------------------------------------------------------
!     BOUCLE SUR LES GROUPE_MA_FISSURE
!     ------------------------------------------------------------------
    do 60 igco = 1, nbgco
!     ------------------------------------------------------------------
!     RECHERCHE D EXISTENCE DU GROUP_MA_FISSURE CONSIDERE
!     ------------------------------------------------------------------
        call jenonu(jexnom(ma//'.GROUPEMA', zk24(io8gco+igco-1)), igma)
!
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
!     ------------------------------------------------------------------
!     BOUCLE SUR LES MAILLES DU GROUP_MA
!     ------------------------------------------------------------------
            do 50 imag = 1, nbmag
                imac=zi(imigma+imag-1)
                maicon(imac)=maicon(imac)+1
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
!
                if (ktyc(:5) .eq. 'QUAD4' .or. ktyc(:5) .eq. 'QUAD8') then
                    cas2d=.true.
                    if (ierr .ne. 0) write (ifm, *)'MAILLE 2D : ', kmac, ' DE TYPE ', ktyc
                    elseif (ktyc(:5).eq.'PENTA' .or. ktyc(:4).eq.'HEXA')&
                then
                    cas3d=.true.
                    if (ierr .ne. 0) write (ifm, *)'MAILLE 3D : ', kmac, ' DE TYPE ', ktyc
                else
                    inval=.true.
                    valk(1)=kmac
                    valk(2)=ktyc
                    call u2mesk('E', 'ALGORITH2_27', 2, valk)
                endif
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
                do 40 icoc = 1, nbcoc
                    inoc=zi(imicoc+icoc-1)
                    noecon(inoc)=noecon(inoc)+1
40              continue
50          continue
!     ------------------------------------------------------------------
        endif
!     ------------------------------------------------------------------
60  end do
    if (inval) call u2mess('F', 'ALGORITH2_28')
!
    if (cas2d .and. cas3d) then
        if (ierr .eq. 0) then
!       ON RETOURNE DANS LA BOUCLE AVEC DEMANDE DE MESSAGES
            ierr=1
            goto 30
!
        else
            call u2mess('F', 'ALGORITH2_29')
        endif
    endif
    if (cas2d) itest=2
    if (cas3d) itest=3
!
!     ------------------------------------------------------------------
!     BOUCLE SUR LES MAILLES DU MAILLAGE
!     ------------------------------------------------------------------
    do 80 imar = 1, nbmar
        if (maicon(imar) .ne. 0) goto 80
!     ------------------------------------------------------------------
!     RECHERCHE DU NOM DE LA MAILLE
!     ------------------------------------------------------------------
        call jenuno(jexnum(ma//'.NOMMAI', imar), kmar)
        nommar(imar)=kmar
!
!     ------------------------------------------------------------------
!     RECHERCHE DE L ADRESSE DES CONNEXIONS DE LA MAILLE
!     ------------------------------------------------------------------
        call jeveuo(jexnum(ma//'.CONNEX', imar), 'L', imicor)
        jmicor(imar)=imicor
!     ------------------------------------------------------------------
!     RECHERCHE DU NOMBRE DE CONNEXIONS DE LA MAILLE
!     ------------------------------------------------------------------
        call jelira(jexnum(ma//'.CONNEX', imar), 'LONMAX', nbcor)
        mbcor(imar)=nbcor
!     ------------------------------------------------------------------
!     BOUCLE SUR LES CONNEXIONS DE LA MAILLE
!     ------------------------------------------------------------------
        nbcom=0
        do 70 icor = 1, nbcor
            inor=zi(imicor+icor-1)
            if (noecon(inor) .ne. 0) nbcom=nbcom+1
!
70      continue
        if (nbcom .ge. itest) then
!     ------------------------------------------------------------------
!     RECHERCHE DE L'ADRESSE DU TYPE DE LA MAILLE DANS ZI
!     ------------------------------------------------------------------
            call jeveuo(ma//'.TYPMAIL', 'L', iatyma)
            imityr=iatyma-1+imar
            ityr=zi(imityr)
!     ------------------------------------------------------------------
!     RECHERCHE DU TYPE DE LA MAILLE DANS CATA.TM.NOMTM
!     ------------------------------------------------------------------
            call jenuno(jexnum('&CATA.TM.NOMTM', ityr), ktyr)
            nomtyr(imar)=ktyr
!
            if (valid()) then
                nbmarc=nbmarc+1
                marcon(nbmarc)=imar
            endif
        endif
80  end do
!     ==================================================================
!CC      ON COMMENTE JEMARQ CAR ADRESSES PASSEES EN ARGUMENT
!CC      CALL JEDEMA()
end subroutine

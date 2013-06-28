subroutine irmhdf(ifi, ndim, nbnoeu, coordo, nbmail,&
                  connex, point, nomast, typma, titre,&
                  nbtitr, nbgrno, nomgno, nbgrma, nomgma,&
                  nommai, nomnoe, infmed)
!     ------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
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
!     ECRITURE DU MAILLAGE - FORMAT MED
!        -  -     -                 ---
!-----------------------------------------------------------------------
!     ENTREE:
!       IFI    : UNITE LOGIQUE D'IMPRESSION DU MAILLAGE
!       NDIM   : DIMENSION DU PROBLEME (2  OU 3)
!       NBNOEU : NOMBRE DE NOEUDS DU MAILLAGE
!       COORDO : VECTEUR DES COORDONNEES DES NOEUDS
!       NBMAIL : NOMBRE DE MAILLES DU MAILLAGE
!       CONNEX : CONNECTIVITES
!       POINT  : VECTEUR POINTEUR DES CONNECTIVITES (LONGUEURS CUMULEES)
!       NOMAST : NOM DU MAILLAGE
!       TYPMA  : VECTEUR TYPES DES MAILLES
!       TITRE  : TITRE ASSOCIE AU MAILLAGE
!       NBGRNO : NOMBRE DE GROUPES DE NOEUDS
!       NBGRMA : NOMBRE DE GROUPES DE MAILLES
!       NOMGNO : VECTEUR NOMS DES GROUPES DE NOEUDS
!       NOMGMA : VECTEUR NOMS DES GROUPES DE MAILLES
!       NOMMAI : VECTEUR NOMS DES MAILLES
!       NOMNOE : VECTEUR NOMS DES NOEUDS
!       INFMED : NIVEAU DES INFORMATIONS A IMPRIMER
!     ------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#   include "asterfort/codent.h"
#   include "asterfort/infniv.h"
#   include "asterfort/irmdes.h"
#   include "asterfort/irmmfa.h"
#   include "asterfort/irmmma.h"
#   include "asterfort/irmmno.h"
#   include "asterfort/jedema.h"
#   include "asterfort/jedetc.h"
#   include "asterfort/jemarq.h"
#   include "asterfort/lrmtyp.h"
#   include "asterfort/mdexma.h"
#   include "asterfort/mdnoma.h"
#   include "asterfort/as_mficlo.h"
#   include "asterfort/as_mmhcre.h"
#   include "asterfort/as_mfiope.h"
#   include "asterfort/u2mesg.h"
#   include "asterfort/u2mesk.h"
#   include "asterfort/ulisog.h"
    integer :: connex(*), typma(*), point(*)
    integer :: ifi, ndim, nbnoeu, nbmail, nbgrno, nbgrma
    integer :: infmed, nbtitr
!
    character(len=80) :: titre(*)
    character(len=8) :: nommai(*), nomnoe(*), nomast
    character(len=24) :: nomgno(*), nomgma(*)
!
    real(kind=8) :: coordo(*)
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRMHDF' )
!
    integer :: ntymax
    parameter (ntymax = 69)
    integer :: nnomax
    parameter (nnomax=27)
    integer :: edlect
    parameter (edlect=0)
    integer :: edleaj
    parameter (edleaj=1)
    integer :: edcrea
    parameter (edcrea=3)
    integer :: ednstr
    parameter (ednstr=0)
    integer :: edcart
    parameter (edcart=0)
!
    integer :: edmode, codret
    integer :: nbtyp, fid
    integer :: nmatyp(ntymax), nnotyp(ntymax), typgeo(ntymax)
    integer :: renumd(ntymax), modnum(ntymax), numnoa(ntymax, nnomax)
    integer :: iaux, jaux, nuanom(ntymax, nnomax)
    integer :: lnomam, ifimed
    integer :: ifm, nivinf
!
    character(len=1) :: saux01
    character(len=6) :: saux06
    character(len=8) :: nomtyp(ntymax)
    character(len=8) :: saux08
    character(len=16) :: saux16(0:3)
    character(len=16) :: nomcoo(3), unicoo(3)
    character(len=64) :: nomamd
    character(len=80) :: descdt
    character(len=200) :: nofimd, desc
    character(len=255) :: kfic
    character(len=64) :: valk(2)
!
    logical :: existm, ficexi
!
!     ------------------------------------------------------------------
!
    call jemarq()
!
!====
! 1. PREALABLES
!====
!
    nomcoo(1) = 'X               '
    nomcoo(2) = 'Y               '
    nomcoo(3) = 'Z               '
!
    unicoo(1) = 'INCONNUE        '
    unicoo(2) = 'INCONNUE        '
    unicoo(3) = 'INCONNUE        '
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, nivinf)
!
! 1.2. ==> NOM DU FICHIER MED
!
    call ulisog(ifi, kfic, saux01)
    if (kfic(1:1) .eq. ' ') then
        call codent(ifi, 'G', saux08)
        nofimd = 'fort.'//saux08
    else
        nofimd = kfic(1:200)
    endif
!
    if (nivinf .gt. 1) then
        write (ifm,*) '<',nompro,'> NOM DU FICHIER MED : ',nofimd
    endif
!
! 1.3. ==> NOM DU MAILLAGE
!
    call mdnoma(nomamd, lnomam, nomast, codret)
    if (codret .ne. 0) then
        saux08='MDNOMA  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
!
! 1.4. ==> LE MAILLAGE EST-IL DEJA PRESENT DANS LE FICHIER ?
!          SI OUI, ON NE FAIT RIEN DE PLUS QU'EMETTRE UNE INFORMATION
!
    iaux = 0
    ifimed = 0
    call mdexma(nofimd, ifimed, nomamd, iaux, existm,&
                jaux, codret)
!
    if (existm) then
!
        valk (1) = nofimd(1:32)
        valk (2) = nomamd
        call u2mesg('A', 'MED_67', 2, valk, 0,&
                    0, 0, 0.d0)
!
!     ------------------------------------------------------------------
!
    else
!
!====
! 2. DEMARRAGE
!====
!
! 2.1. ==> OUVERTURE FICHIER MED EN MODE
!      SOIT 'CREATION' SI LE FICHIER N'EXISTE PAS ENCORE,
!      SOIT 'LECTURE_AJOUT' (CELA SIGNIFIE QUE LE FICHIER EST ENRICHI).
!
!     TEST L'EXISTENCE DU FICHIER
        inquire(file=nofimd,exist=ficexi)
        if (ficexi) then
            edmode = edlect
            call as_mfiope(fid, nofimd, edmode, codret)
            if (codret .ne. 0) then
                edmode = edcrea
            else
                edmode = edleaj
                call as_mficlo(fid, codret)
                if (codret .ne. 0) then
                    saux08='mficlo'
                    call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                                codret, 0, 0.d0)
                endif
            endif
        else
            edmode = edcrea
        endif
        call as_mfiope(fid, nofimd, edmode, codret)
        if (codret .ne. 0) then
            saux08='mfiope'
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
!
        if (infmed .ge. 2) then
!                         1234567890123456
            saux16(edlect) = 'LECTURE SEULE.  '
            saux16(edleaj) = 'LECTURE/ECRITURE'
            saux16(edcrea) = 'CREATION.       '
            call codent(edmode, 'G', saux08)
            valk(1) = saux08
            valk(2) = saux16(edmode)
            call u2mesk('I', 'MED_40', 2, valk)
        endif
!
! 2.2. ==> CREATION DU MAILLAGE AU SENS MED (TYPE MED_NON_STRUCTURE)
!
!GN      PRINT *,'APPEL DE as_mmhcre AVEC :'
!GN      PRINT *,NOMAMD
!GN      PRINT *,NDIM
!GN      PRINT *,EDNSTR
        desc = 'CREE PAR CODE_ASTER'
        descdt = 'SANS UNITES'
        call as_mmhcre(fid, nomamd, ndim, ednstr, desc,&
                    descdt, edcart, nomcoo, unicoo, codret)
        if (codret .ne. 0) then
            saux08='mmhcre'
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
!
! 2.3. ==> . RECUPERATION DES NB/NOMS/NBNO/NBITEM DES TYPES DE MAILLES
!            DANS CATALOGUE
!          . RECUPERATION DES TYPES GEOMETRIE CORRESPONDANT POUR MED
!          . VERIF COHERENCE AVEC LE CATALOGUE
!
        call lrmtyp(nbtyp, nomtyp, nnotyp, typgeo, renumd,&
                    modnum, nuanom, numnoa)
!
!====
! 3. LA DESCRIPTION
!====
!
        if (edmode .eq. edcrea) then
!
            call irmdes(fid, titre, nbtitr, infmed)
!
        endif
!
!====
! 4. LES NOEUDS
!====
!
        call irmmno(fid, nomamd, ndim, nbnoeu, coordo,&
                    nomnoe)
!
!====
! 5. LES MAILLES
!====
!
        saux06 = nompro
!
        call irmmma(fid, nomamd, nbmail, connex, point,&
                    typma, nommai, saux06, nbtyp, typgeo,&
                    nomtyp, nnotyp, renumd, nmatyp, infmed,&
                    modnum, nuanom)
!
!====
! 6. LES FAMILLES
!====
!
        saux06 = nompro
!
        call irmmfa(fid, nomamd, nbnoeu, nbmail, nomast,&
                    nbgrno, nomgno, nbgrma, nomgma, saux06,&
                    typgeo, nomtyp, nmatyp, infmed)
!
!====
! 7. LES EQUIVALENCES
!====
!
!     CALL IRMMEQ ()  ! NE FAIT RIEN ...
!
!====
! 8. FERMETURE DU FICHIER MED
!====
!
        call as_mficlo(fid, codret)
        if (codret .ne. 0) then
            saux08='mficlo'
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
!
!====
! 9. LA FIN
!====
!
        call jedetc('V', '&&'//nompro, 1)
!
    endif
!
!     ------------------------------------------------------------------
!
    call jedema()
!
end subroutine

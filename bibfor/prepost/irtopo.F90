subroutine irtopo(ioccur, formaf, ifichi, leresu, lresul,&
                  nbmato, nonuma, nbnoto, nonuno, codret)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/irmama.h"
#include "asterfort/irmano.h"
#include "asterfort/irnono.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: ioccur, nbnoto, nbmato, ifichi, codret
    character(len=8) :: formaf, leresu
    character(len=24) :: nonuma, nonuno
    aster_logical :: lresul
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
! ----------------------------------------------------------------------
!  IMPR_RESU - TRAITEMENT DU MOT CLE FACTEUR IOCCUR
!  -    -                    -       ---
! ----------------------------------------------------------------------
!
!  CETTE ROUTINE SCRUTE LES MOTS-CLES NOEUD, GROUP_NO, ...
!   ET REALISE DES IMPRESSIONS POUR LE FORMAT RESULTAT
!
! IN  :
!   IOCCUR  I    NUMERO D'OCCURENCE DU MOT CLE FACTEUR
!   FORMAF  K8   FORMAT DU FICHIER A IMPRIMER
!   IFICHI  I    UNITE LOGIQUE DU FICHIER A IMPRIMER
!   LERESU  K9   NOM DU CHAMP OU DU RESULTAT
!   LRESUL  L    INDIQUE SI LERESU EST UN RESU OU UN CHAMP
!
! IN/OUT :
!   NONUMA  K24  NOM DE L'OBJET A CREER POUR LES MAILLES
!   NONUNO  K24  NOM DE L'OBJET A CREER POUR LES NOEUDS
!
! OUT :
!   NBMATO  I    NOMBRE TOTAL DE MAILLES TROUVEES (INDIQUE SI UN
!                 JEVEUO SUR NONUMA EST FAISABLE)
!   NBNOTO  I    NOMBRE TOTAL DE NOEUDS TROUVES (INDIQUE SI UN
!                 JEVEUO SUR NONUNO EST FAISABLE)
!   CODRET  I    CODE RETOUR (0 SI OK, 1 SINON)
!
!
    integer :: nbno, nbgrn, nbma, nbgrm, nbnofa, nbgnfa, nbmafa
    integer :: nbgmfa, jtopo, jlgrn, ibid, jlno, jlgrm, jngrm
    integer :: jlma, jmma, nbnoe, ino, jnunou, nbnou, nbele, jnuma
    integer :: nbnos, ii, igrm, igrn, ima, nbnomx, nbgnmx
    integer :: nbmamx, nbgmmx, imxno, imxgn, imxma, imxgm, idebu, jnunot
    integer :: iutil
!
    character(len=8) :: nomma
    character(len=24) :: texte
    integer, pointer :: filtre_no(:) => null()
    integer, pointer :: ind_noeu(:) => null()
    character(len=80), pointer :: nom_grno(:) => null()
    character(len=80), pointer :: nom_noe(:) => null()
    integer, pointer :: numnos(:) => null()
!
    call jemarq()
!
!     --- TRAITEMENT DES NOEUDS,MAILLES,GPES DE NOEUDS ET MAILLES
!         (OPERANDE DE SELECTION SUR DES ENTITES TOPOLOGIQUES)
!
    jlno = 1
    nbno = 0
    nbgrn = 0
    nbma = 0
    nbgrm = 0
    nbmato = 0
    nbnos = 0
    nbnoto = 0
    nbnou = 0
!
    codret=0
    call getvtx('RESU', 'NOEUD', iocc=ioccur, nbval=0, nbret=nbnofa)
    call getvtx('RESU', 'GROUP_NO', iocc=ioccur, nbval=0, nbret=nbgnfa)
    call getvtx('RESU', 'MAILLE', iocc=ioccur, nbval=0, nbret=nbmafa)
    call getvtx('RESU', 'GROUP_MA', iocc=ioccur, nbval=0, nbret=nbgmfa)
    if ((nbnofa.ne.0.or.nbgnfa.ne.0.or.nbmafa.ne.0.or.nbgmfa.ne.0) .and.&
        (formaf(1:6).eq.'CASTEM')) then
        call utmess('A', 'PREPOST3_73')
    endif
!
!     *** ON S'ALLOUE UN TABLEAU DE 8 ENTIERS POUR LA TOPOLOGIE
    call wkvect('&&IRTOPO.LIST_TOPO', 'V V I', 8, jtopo)
!
    if (formaf(1:6) .ne. 'CASTEM') then
!       *** CAS D'UNE LISTE DE GROUPES DE NOEUDS
        if (nbgnfa .lt. 0) then
            nbgrn = -nbgnfa
!         - ON S'ALLOUE :
!           UN TABLEAU DE K8, LISTE DES NOMS DE GPES DE NOEUDS
!           UN TABLEAU DE K80 (POUR FORMAT 'RESULTAT')
            call wkvect('&&IRTOPO.LIST_GRNO', 'V V K24', nbgrn, jlgrn)
            AS_ALLOCATE(vk80=nom_grno, size=nbgrn)
            call getvtx('RESU', 'GROUP_NO', iocc=ioccur, nbval=nbgrn, vect=zk24( jlgrn),&
                        nbret=ibid)
            zi(jtopo-1+3) = nbgrn
        else
            jlgrn=1
        endif
!
!       *** CAS D'UNE LISTE DE NOEUDS
        if (nbnofa .lt. 0) then
            nbno = -nbnofa
!         - ON S'ALLOUE :
!           UN TABLEAU DE K8, LISTE DES NOMS DE NOEUDS
!           UN TABLEAU DE K80 (POUR FORMAT 'RESULTAT')
            call wkvect('&&IRTOPO.LIST_NOE', 'V V K8', nbno, jlno)
            AS_ALLOCATE(vk80=nom_noe, size=nbno)
            call getvtx('RESU', 'NOEUD', iocc=ioccur, nbval=nbno, vect=zk8(jlno),&
                        nbret=ibid)
            zi(jtopo-1+1) = nbno
        endif
!
!       *** CAS D'UNE LISTE DE GROUPES DE MAILLES
        if (nbgmfa .lt. 0) then
            nbgrm = -nbgmfa
!         - ON S'ALLOUE :
!           UN TABLEAU DE K8, LISTE DES NOMS DE GPES DE MAILLES
!           UN TABLEAU DE K80 (POUR FORMAT 'RESULTAT')
            call wkvect('&&IRTOPO.LIST_GRMA', 'V V K24', nbgrm, jlgrm)
            call wkvect('&&IRTOPO.NOM_GRMA', 'V V K80', nbgrm, jngrm)
            call getvtx('RESU', 'GROUP_MA', iocc=ioccur, nbval=nbgrm, vect=zk24( jlgrm),&
                        nbret=ibid)
            zi(jtopo-1+7) = nbgrm
        else
            jlgrm=1
            jngrm=1
        endif
!
!       ***  CAS D'UNE LISTE DE MAILLES
        if (nbmafa .lt. 0) then
            nbma = -nbmafa
!         - ON S'ALLOUE :
!           UN TABLEAU DE K8, LISTE DES NOMS DE MAILLES
!           UN TABLEAU DE K80 (POUR FORMAT 'RESULTAT')
            call wkvect('&&IRTOPO.LIST_MAI', 'V V K8', nbma, jlma)
            call wkvect('&&IRTOPO.NOM_MAI', 'V V K80', nbma, jmma)
            call getvtx('RESU', 'MAILLE', iocc=ioccur, nbval=nbma, vect=zk8(jlma),&
                        nbret=ibid)
            zi(jtopo-1+5) = nbma
        else
            jlma=1
            jmma=1
        endif
!
!       ***  IL Y A SELECTION EN OPERANDE SUR NOEUDS OU MAILLES
!            OU DES GROUPES DE NOEUDS OU DES GROUPES DE MAILLES
        if (nbno .ne. 0 .or. nbgrn .ne. 0 .or. nbma .ne. 0 .or. nbgrm .ne. 0) then
            if (lresul) then
!           - C'EST UN RESULTAT COMPOSE: NOM DU MAILLAGE DANS NOMMA
                call dismoi('NOM_MAILLA', leresu, 'RESULTAT', repk=nomma)
            else
!           - C'EST UN CHAM_GD: NOM DU MAILLAGE DANS NOMMA
                call dismoi('NOM_MAILLA', leresu, 'CHAMP', repk=nomma)
            endif
!         - NOMBRE TOTAL DE NOEUDS DU MAILLAGE NOMMA = NBNOE
            call dismoi('NB_NO_MAILLA', nomma, 'MAILLAGE', repi=nbnoe)
            AS_ALLOCATE(vi=ind_noeu, size=nbnoe)
            do ino = 1, nbnoe
                ind_noeu(ino)=0
            end do
        endif
!
!       ***  SELECTION SUR DES NOEUDS OU GROUPES DE NOEUDS
        if (nbno .ne. 0 .or. nbgrn .ne. 0) then
!         - ON S'ALLOUE UN TABLEAU D'ENTIERS A PARTIR DE ZI(JNUNOU)
!           POUR LA LISTE DES NUMEROS DES NOEUDS A IMPRIMER
            call wkvect('&&IRTOPO.NUMNOE', 'V V I', nbnoe, jnunou)
!         - ON RECUPERE A PARTIR DE ZI(JNUNOU) LES NUMEROS DES
!           NOEUDS DE LA LISTE DE NOEUDS OU DE GROUPES DE NOEUDS
!           (NBNOU EST LE NBRE TOTAL DE NOEUDS TROUVES A IMPRIMER)
            call irnono(nomma, nbnoe, nbno, zk8(jlno), nbgrn,&
                        zk24(jlgrn), '&&IRTOPO.NUMNOE', nbnou, ind_noeu, '&&IRTOPO.LIST_TOPO')
!         - ON RECUPERE DE NOUVEAU L'ADRESSE DE .NUMNOE CAR IRNONO
!           A PU AGRANDIR CET OBJET :
            call jeveuo('&&IRTOPO.NUMNOE', 'L', jnunou)
            nbnoto = nbnoto + nbnou
        endif
!
!       ***  SELECTION SUR DES MAILLES OU GROUPES DE MAILLES
        if (nbma .ne. 0 .or. nbgrm .ne. 0) then
!         - ON S'ALLOUE UN TABLEAU POUR LES NUMEROS DES MAILLES ET
!           UN TABLEAU POUR LES NUMEROS DES NOEUDS DE CES MAILLES
            call jelira(nomma//'.NOMMAI', 'NOMMAX', nbele)
            call wkvect(nonuma, 'V V I', nbele, jnuma)
            AS_ALLOCATE(vi=numnos, size=nbnoe)
!         - ON RECUPERE A PARTIR DE ZI(JNUMA) LES NUMEROS DES
!           MAILLES DE LA LISTE DE MAILLES OU DE GROUPES DE MAILLES
!           (NBMATO = NBRE TOTAL DE MAILLES TROUVEES A IMPRIMER)
            call irmama(nomma, nbma, zk8(jlma), nbgrm, zk24(jlgrm),&
                        nonuma, nbmato, '&&IRTOPO.LIST_TOPO')
!         - ON RECUPERE DE NOUVEAU L'ADRESSE DE .NUMMAI CAR IRMAMA
!           A PU AGRANDIR CET OBJET :
            call jeveuo(nonuma, 'L', jnuma)
!
!         - ON RECUPERE A PARTIR DE ZI(JNUNOS) LA LISTE DES NUMEROS
!           DES NOEUDS SOMMETS DE CES MAILLES
!          (NBNOS = NOMBRE DE NOEUDS SOMMETS DE CETTE LISTE)
            call irmano(nomma, nbmato, zi(jnuma), nbnos, numnos)
            if (nbnos .eq. 0) then
                call utmess('F', 'PREPOST5_4')
            endif
            AS_ALLOCATE(vi=filtre_no, size=nbnos)
            ii=0
            do ino = 1, nbnos
                if (ind_noeu(1+numnos(ino)-1) .eq. 0) then
                    ii=ii+1
                    filtre_no(ii)=numnos(ino)
                endif
            end do
            nbnos=ii
            nbnoto = nbnoto + nbnos
        endif
!
        if (nbno .ne. 0 .or. nbgrn .ne. 0 .or. nbma .ne. 0 .or. nbgrm .ne. 0) then
            if (nbnou .eq. 0 .and. nbmato .eq. 0) then
                codret=1
                goto 999
            endif
        endif
!
!       ***  ON CREE UNE LISTE DE NUMEROS DE NOEUDS ISSUS
!            DE GROUP_NO ET GROUP_MA
        if (nbnoto .gt. 0) then
!         - ON S'ALLOUE UN TABLEAU POUR LES NUMEROS DE CES NOEUDS
            call wkvect(nonuno, 'V V I', nbnoto, jnunot)
        endif
        if (nbnou .gt. 0) then
!         - LISTE DES NUMEROS DE NOEUDS
            do ino = 1, nbnou
                zi(jnunot-1+ino)=zi(jnunou-1+ino)
            end do
        endif
        if (nbnos .gt. 0) then
!         - SUIVIE DE LA LISTE DES NUMEROS DE NOEUDS SOMMETS
            do ino = 1, nbnos
                zi(jnunot-1+nbnou+ino)= filtre_no(ino)
            end do
        endif
    endif
!
!     ***************************************************************
!     - CHAM_GD OU RESULTAT COMPOSE AU FORMAT 'RESULTAT':
!       IMPRESSION LISTES DES NOMS DES NOEUDS ET MAILLES SELECTIONNES
!     ***************************************************************
    if (formaf .eq. 'RESULTAT') then
        nbnomx = zi(jtopo-1+2)
        nbgnmx = zi(jtopo-1+4)
        nbmamx = zi(jtopo-1+6)
        nbgmmx = zi(jtopo-1+8)
        imxno = 0
        imxgn = 0
        imxma = 0
        imxgm = 0
        if (nbnomx .ne. 0) then
            idebu = 12
            imxno = imxno+1
            do ino = 1, nbno
                texte = zk8(jlno-1+ino)
                iutil = lxlgut(texte)
                if (iutil .ne. 0) then
                    if ((idebu+iutil) .gt. 80) then
                        imxno = imxno + 1
                        idebu = 1
                    endif
                    nom_noe(imxno)(idebu:idebu+iutil)=texte(1:&
                    iutil)
                    idebu=idebu+iutil+1
                endif
            end do
        endif
        if (nbgnmx .ne. 0) then
            idebu = 12
            imxgn = imxgn + 1
            do igrn = 1, nbgrn
                texte = zk24(jlgrn-1+igrn)
                iutil = lxlgut(texte)
                if (iutil .ne. 0) then
                    if ((idebu+iutil) .gt. 80) then
                        imxgn = imxgn + 1
                        idebu = 1
                    endif
                    nom_grno(imxgn)(idebu:idebu+iutil)=texte(1:&
                    iutil)
                    idebu=idebu+iutil+1
                endif
            end do
        endif
        if (nbgmmx .ne. 0) then
            idebu = 12
            imxgm = imxgm + 1
            do igrm = 1, nbgrm
                texte = zk24(jlgrm-1+igrm)
                iutil = lxlgut(texte)
                if (iutil .ne. 0) then
                    if ((idebu+iutil) .gt. 80) then
                        idebu = 1
                    endif
                    zk80(jngrm-1+imxgm)(idebu:idebu+iutil)=texte(1:&
                    iutil)
                    idebu=idebu+iutil+1
                endif
            end do
        endif
        if (nbmamx .ne. 0) then
            idebu = 12
            imxma = imxma + 1
            do ima = 1, nbma
                texte = zk8(jlma-1+ima)
                iutil = lxlgut(texte)
                if (iutil .ne. 0) then
                    if ((idebu+iutil) .gt. 80) then
                        imxma = imxma + 1
                        idebu = 1
                    endif
                    zk80(jmma-1+imxma)(idebu:idebu+iutil)=texte(1:&
                    iutil)
                    idebu=idebu+iutil+1
                endif
            end do
        endif
        call jeveuo('&&IRTOPO.LIST_TOPO', 'L', jtopo)
        if (imxno .ne. 0 .or. imxgn .ne. 0 .or. imxma .ne. 0 .or. imxgm .ne. 0) then
            write(ifichi, '(/,20X,A)') 'ENTITES ' //'TOPOLOGIQUES SELECTIONNEES '
        endif
        if (imxno .ne. 0) then
            nom_noe(1)(1:11) = 'NOEUD    : '
            write(ifichi,'(1X,A80)') (nom_noe(ino),ino=1,imxno)
        endif
        if (imxgn .ne. 0) then
            nom_grno(1)(1:11) = 'GROUP_NO : '
            write(ifichi,'(1X,A80)') (nom_grno(igrn),igrn=1,imxgn)
        endif
        if (imxma .ne. 0) then
            zk80(jmma-1+1)(1:11) = 'MAILLE   : '
            write(ifichi,'(1X,A80)') (zk80(jmma-1+ima),ima=1,imxma)
        endif
        if (imxgm .ne. 0) then
            zk80(jngrm-1+1)(1:11) = 'GROUP_MA : '
            write(ifichi,'(1X,A80)') (zk80(jngrm-1+igrm),igrm=1,imxgm)
        endif
        write(ifichi,'(A)')
    endif
999 continue
    call jedetr('&&IRTOPO.LIST_TOPO')
    call jedetr('&&IRTOPO.LIST_GRNO')
    call jedetr('&&IRTOPO.LIST_NOE')
    call jedetr('&&IRTOPO.LIST_GRMA')
    call jedetr('&&IRTOPO.NOM_GRMA')
    call jedetr('&&IRTOPO.LIST_MAI')
    call jedetr('&&IRTOPO.NOM_MAI')
    call jedetr('&&IRTOPO.NUMNOE')
    AS_DEALLOCATE(vk80=nom_noe)
    AS_DEALLOCATE(vk80=nom_grno)
    AS_DEALLOCATE(vi=ind_noeu)
    AS_DEALLOCATE(vi=numnos)
    AS_DEALLOCATE(vi=filtre_no)
!
    call jedema()
!
end subroutine

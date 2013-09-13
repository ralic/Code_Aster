subroutine ircmpf(nofimd, nvalty, profil, noprof)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!_______________________________________________________________________
!  ECRITURE D'UN CHAMP - FORMAT MED - PROFIL
!     -  -       -              -     -  -
!_______________________________________________________________________
!     ENTREES :
!       NOFIMD : NOM DU FICHIER MED
!       NVALTY : NOMBRE DE VALEURS DU TYPE
!       PROFIL : PROFIL ENTIER
!     SORTIES :
!       NOPROF : NOM DU PROFIL AU SENS MED
!_______________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/as_mficlo.h"
#include "asterfort/as_mfiope.h"
#include "asterfort/as_mpfnpf.h"
#include "asterfort/as_mpfpfi.h"
#include "asterfort/as_mpfprr.h"
#include "asterfort/as_mpfprw.h"
#include "asterfort/codent.h"
#include "asterfort/infniv.h"
#include "asterfort/jedetr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nvalty, profil(nvalty)
!
    character(len=*) :: nofimd
    character(len=*) :: noprof
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRCMPF' )
    integer :: edlect
    parameter (edlect=0)
    integer :: edleaj
    parameter (edleaj=1)
!
    integer :: ifm, nivinf
!
    integer :: idfimd
    integer :: nbprof, lgprof, adprof, adnopf, nrprty
    integer :: iaux, jaux
    integer :: codret
!
    character(len=8) :: saux08
    character(len=24) :: ntprof, ntnopf
    character(len=64) :: nopr64
!
!====
! 1. PREALABLES
!====
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, nivinf)
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'DEBUT DE '//nompro
    endif
    1001 format(/,4x,10('='),a,10('='),/)
!
! 1.2. ==> NOMS DES TABLEAUX
!               12   345678   9012345678901234
    ntprof = '&&'//nompro//'.PROFIL_MED_LU  '
    ntnopf = '&&'//nompro//'.NOM_PROFIL_MED '
!
! 1.3. ==> A PRIORI, PAS DE PROFIL DANS LE FICHIER
!
    nrprty = 0
!
!====
! 2. ON OUVRE LE FICHIER EN LECTURE
!====
!
    call as_mfiope(idfimd, nofimd, edlect, codret)
    if (codret .ne. 0) then
        saux08='mfiope'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
!====
! 3. REPERAGE DU NOMBRE DE PROFILS DEJA ENREGISTRES
!    S'IL Y EN A, ON ALLOUE UN TABLEAU POUR STOCKER LEURS NOMS
!====
!
    call as_mpfnpf(idfimd, nbprof, codret)
    if (codret .ne. 0) then
        saux08='mpfnpf'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
!
    if (nivinf .gt. 1) then
        write (ifm,*) '   NOMBRE DE PROFILS DANS LE FICHIER : ',nbprof
    endif
!
    if (nbprof .ne. 0) then
        call wkvect(ntnopf, 'V V K80', nbprof, adnopf)
    endif
!
!====
! 4. LECTURE DE CHACUN DES PROFILS ET COMPARAISON AVEC CELUI RETENU
!====
!
    do 41 , iaux = 1 , nbprof
!
! 4.1. ==> NOM ET NOMBRE DE VALEURS DU IAUX-EME PROFIL
!
    call as_mpfpfi(idfimd, iaux, nopr64, lgprof, codret)
    noprof = nopr64
    if (codret .ne. 0) then
        saux08='mpfpfi'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
    if (nivinf .gt. 1) then
        write (ifm,4101) iaux, noprof, lgprof
    endif
    4101  format(5x,'LECTURE DU PROFIL NUMERO',i8,&
     &       /,5x,'...NOM       : ',a,&
     &       /,5x,'... LONGUEUR : ',i8)
!
    zk80(adnopf+iaux-1) = noprof
!
! 4.2. ==> SI LA LONGUEUR EST LA MEME ET QU'ON N'A TOUJOURS PAS TROUVE
!          UN PPOFIL IDENTIQUE, ON LIT LE PROFIL COURANT ET ON COMPARE
!
    if (nvalty .eq. lgprof .and. nrprty .eq. 0) then
!
! 4.2.1. ==> LECTURE DES VALEURS DU PROFIL
!
        call wkvect(ntprof, 'V V I', lgprof, adprof)
!
        call as_mpfprr(idfimd, zi(adprof), lgprof, noprof, codret)
        if (codret .ne. 0) then
            saux08='mpfprr'
            call utmess('F', 'DVP_97', sk=saux08, si=codret)
        endif
!
        if (nivinf .gt. 1) then
            write (ifm,4201) zi(adprof),zi(adprof+lgprof-1)
        endif
        4201    format(5x,'... 1ERE ET DERNIERE VALEURS : ',2i8)
!
! 4.2.2. ==> ON COMPARE TERME A TERME.
!            DES QU'UNE VALEUR DIFFERE, ON PASSE AU PROFIL SUIVANT.
!            SI TOUS LES TERMES SONT EGAUX, C'EST LE BON PROFIL !
!            ON PEUT SORTIR DE LA RECHERCHE DES PROFILS.
!
        do 422 , jaux = 1 , lgprof
        if (profil(jaux) .ne. zi(adprof+jaux-1)) then
            goto 423
        endif
422      continue
!
        nrprty = iaux
!
        if (nivinf .gt. 1) then
            write (ifm,4202)
        endif
        4202    format('...... CE PROFIL EST IDENTIQUE A CELUI VOULU')
!
        goto 51
!
! 4.2.3. ==> MENAGE AVANT D'EXAMINER UN NOUVEAU PROFIL
!
423      continue
!
        call jedetr(ntprof)
!
    endif
!
    41 end do
!
!====
! 5. FERMETURE DU FICHIER
!====
!
51  continue
!
    call as_mficlo(idfimd, codret)
    if (codret .ne. 0) then
        saux08='mficlo'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
!====
! 6. SI AUCUN PROFIL N'A ETE TROUVE, ON ECRIT LE NOTRE DANS LE FICHIER
!====
!
    if (nrprty .eq. 0) then
!
! 6.1. ==> OUVERTURE FICHIER MED EN MODE 'LECTURE_AJOUT'
!    CELA SIGNIFIE QUE LE FICHIER EST ENRICHI MAIS ON NE PEUT PAS
!    ECRASER UNE DONNEE EXISTANTE
!
        call as_mfiope(idfimd, nofimd, edleaj, codret)
        if (codret .ne. 0) then
            saux08='mfiope'
            call utmess('F', 'DVP_97', sk=saux08, si=codret)
        endif
!
! 6.2. ==> ELABORATION D'UN NOM DE PROFIL
!          IL SERA DU TYPE 'PROFIL_00000000N'
!          POUR ETRE SUR DE FINIR PAR TROUVER UN NOM QUI N'A PAS SERVI,
!          IL SUFFIT D'ESSAYER PLUS DE NUMEROS QUE DE PROFILS DEJA
!          ENREGISTRES. QUELLE RUSE DIABOLIQUE !
!
        noprof(17:64)='                                                '
!                      123456789012345678901234567890123456789012345678
!
        do 62 , iaux = 1 , nbprof + 1
!
        call codent(iaux, 'D0', saux08)
!                         12345678
        noprof(1:16) = 'PROFIL__'//saux08
!
!GN        WRITE (IFM,*) 'TEST DU NOM DE PROFIL : ',NOPROF
        do 621 , jaux = 0 , nbprof-1
!GN        WRITE (IFM,*) '... COMPARAISON AVEC  : ',ZK80(ADNOPF+JAUX)
        if (noprof .eq. zk80(adnopf+jaux)(1:64)) then
            goto 62
        endif
621      continue
        goto 622
62      continue
!
622      continue
!
! 6.3. ==> ECRITURE DU PROFIL
!
!GN      PRINT 1789,(PROFIL(IAUX),IAUX=1,NVALTY)
!GN 1789  FORMAT(10I5)
        if (nivinf .gt. 1) then
            write (ifm,6301) noprof, nvalty, profil(1), profil(nvalty)
            6301    format(4x,'PROFIL A CREER :',&
     &         /,4x,'. NOM                      = ',a,&
     &         /,4x,'. LONGUEUR                 = ',i8,&
     &         /,4x,'. 1ERE ET DERNIERE VALEURS = ',2i8)
        endif
        call as_mpfprw(idfimd, profil, nvalty, noprof, codret)
        if (codret .ne. 0) then
            saux08='mpfprw'
            call utmess('F', 'DVP_97', sk=saux08, si=codret)
        endif
!
! 6.4. ==> FERMETURE FICHIER MED
!
        call as_mficlo(idfimd, codret)
        if (codret .ne. 0) then
            saux08='mficlo'
            call utmess('F', 'DVP_97', sk=saux08, si=codret)
        endif
!
    endif
!
!====
! 7. LA FIN
!====
!
    call jedetr(ntnopf)
    call jedetr(ntprof)
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
!
end subroutine

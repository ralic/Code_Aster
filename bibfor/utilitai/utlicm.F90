subroutine utlicm(nbcmpv, nomcmp, nomgd, ncmprf, nomcmr,&
                  ncmpve, numcmp, ntncmp, ntucmp)
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
!-----------------------------------------------------------------------
!     UTILITAIRE - CREATION D'UNE LISTE DE COMPOSANTES
!     --                          --       - -
!-----------------------------------------------------------------------
!     ENTREES :
!       NBCMPV : NOMBRE DE COMPOSANTES VOULUES.
!                . S'IL EST NUL, ON PREND TOUTES LES COMPOSANTES
!                . SI NON NUL, ON PREND CELLES DONNEES PAR LE TABLEAU
!                  NOMCP. ON VERIFIE QUE LES NOMS DES COMPOSANTES
!                  SONT VALIDES
!       NOMCMP : NOMS DES COMPOSANTES VOULUES, SI NBCMPV > 0
!       NOMGD  : NOM DE LA GRANDEUR ASSOCIEE AU CHAMP
!       NCMPRF : NOMBRE DE COMPOSANTES DU CHAMP DE REFERENCE
!       NOMCMR : NOMS DES COMPOSANTES DE REFERENCE
!     SORTIES :
!       NCMPVE : NOMBRE DE COMPOSANTES VALIDES.
!       NUMCMP : SD DES NUMEROS DES COMPOSANTES VALIDES
!       NTNCMP : SD DES NOMS DES COMPOSANTES VALIDES (K8)
!       NTUCMP : SD DES UNITES DES COMPOSANTES VALIDES (K16)
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/infniv.h"
#include "asterfort/irccmp.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: ncmprf, nbcmpv, ncmpve
!
    character(len=8) :: nomgd
    character(len=*) :: nomcmp(*), nomcmr(*)
    character(len=*) :: numcmp, ntncmp, ntucmp
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'UTLICM' )
!
    integer :: adnucm, adncmp, aducmp
    integer :: ifm, nivinf
!
    integer :: iaux, jaux
!
!     RECUPERATION DU NIVEAU D'IMPRESSION
!     -----------------------------------
!
    call infniv(ifm, nivinf)
!
!====
! 1. LISTE DES NUMEROS DES COMPOSANTES VALIDES
!====
!
! 1.1. ==> ALLOCATION DE LA STRUCTURE QUI CONTIENDRA LES
!          NUMEROS DES COMPOSANTES VALIDES
!
    if (nbcmpv .eq. 0) then
        ncmpve = ncmprf
    else if (nbcmpv.gt.0) then
        ncmpve = nbcmpv
    else
        call utmess('F', 'UTILITAI5_45', sk=nomgd)
    endif
!
    if (nivinf .gt. 1) then
        write (ifm,*) nompro, ' : NOMBRE DE COMPOSANTES DEMANDEES : ',&
        ncmpve
    endif
!
    call wkvect(numcmp, 'V V I', ncmpve, adnucm)
!
! 1.2. ==> RIEN N'EST PRECISE : ON PREND TOUTES LES COMPOSANTES DANS
!          L'ORDRE DE REFERENCE
!
    if (nbcmpv .eq. 0) then
!
        do 12 , iaux = 1 , ncmpve
        zi(adnucm+iaux-1) = iaux
12      continue
!
! 1.3. ==> UN EXTRAIT EST DEMANDE : ON CONTROLE LEUR EXISTENCE
!          ON RECUPERE LE NOMBRE DE COMPOSANTES ACCEPTEES ET LEURS
!          NUMEROS DANS LA LISTE OFFICIELLE
!
    else
!
        call irccmp('A', nomgd, ncmprf, nomcmr, nbcmpv,&
                    nomcmp, iaux, adnucm)
        if (iaux .ne. nbcmpv) then
            call utmess('F', 'UTILITAI5_46', sk=nomgd)
        endif
!
    endif
!
!====
! 2. NOMS ET UNITES DES COMPOSANTES RETENUES
!====
!
    call wkvect(ntncmp, 'V V K16', ncmpve, adncmp)
    call wkvect(ntucmp, 'V V K16', ncmpve, aducmp)
!
    do 21 , iaux = 1 , ncmpve
    jaux = zi(adnucm+iaux-1)
!       CONVERSION DES NOMS DE COMPOSANTES DE K8 EN K16
    zk16(adncmp-1+iaux) = nomcmr(jaux)
!        ZK16(ADUCMP-1+IAUX) = '        '
!                              1234567890123456
    zk16(aducmp-1+iaux) = '                '
    21 end do
!      IF ( NIVINF.GT.1 ) THEN
!       DO 2100 , IAUX = 1 , NCMPVE
!        WRITE(IFM,*) '.. COMPOSANTE ', IAUX, ' : ', ZK16(ADNCMP-1+IAUX)
! 2100  CONTINUE
!      ENDIF
!
end subroutine

subroutine ccchcf(nomfor, nbcmp, valin, licmp, nbcmpr,&
                  valres, iret)
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
    implicit none
#include "jeveux.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbcmp, nbcmpr, iret
    real(kind=8) :: valin(nbcmp), valres(nbcmpr)
    character(len=8) :: nomfor(nbcmpr), licmp(nbcmp)
! person_in_charge: mathieu.courtois at edf.fr
! ----------------------------------------------------------------------
!  CALC_CHAMP - TRAITEMENT DE CHAM_UTIL - CALCUL FORMULE
!  -    -                     --          -      -
!  EVALUE LES FORMULES (SUR LA GRANDEUR 'NOMGD') SUR LES
!  'NBCMP' VALEURS DES COMPOSANTES. RANGE LE RESULTAT DANS 'VALRES'.
! ----------------------------------------------------------------------
! IN  :
!   NOMFOR K8(*) NOMS DES FORMULES
!   NOMGD  K8    NOM DE LA GRANDEUR DU CHAMP IN
!   NBCMP  I     NBRE DE COMPOSANTES DEFINIES SUR LE POINT COURANT
!   VALIN  R(*)  VALEURS DES COMPOSANTES
!   LICMP  K8(*) NOM DES COMPOSANTES EFFECTIVEMENT REMPLIES
!   NBCMPR I     NOMBRE DE COMPOSANTES EN SORTIE
! IN :
!   VALRES R(*)  VALEURS DU CRITERE
!   IRET   I     CODE RETOUR : = 0 OK,
!                              > 0 ON N'A PAS PU CALCULER LE CRITERE
! ----------------------------------------------------------------------
    integer :: i
    character(len=1) :: codmes
    character(len=8) :: nomf
!     ----- FIN  DECLARATIONS ------------------------------------------
!
    call jemarq()
    iret = 1
!     METTRE 'A' POUR DEBUG, ' ' EN PROD
    codmes = 'A'
    if (nbcmp .eq. 0) goto 9999
!
    do 100 i = 1, nbcmpr
        nomf = nomfor(i)
!       VERIFIER QUE LES PARAMETRES DE LA FORMULE SONT DANS LES
!       COMPOSANTES FOURNIES
        call fointe(codmes, nomf, nbcmp, licmp, valin,&
                    valres(i), iret)
        if (iret .ne. 0) then
            goto 9999
        endif
100  end do
!
9999  continue
    call jedema()
!
end subroutine

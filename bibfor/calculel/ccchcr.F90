subroutine ccchcr(crit, nomgd, nbcmp, valin, licmp,&
                  nbcmpr, valres, iret)
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
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lciv2e.h"
#include "asterfort/lciv2s.h"
#include "asterfort/u2mesk.h"
    integer :: nbcmp, nbcmpr, iret
    real(kind=8) :: valin(nbcmp), valres(nbcmpr)
    character(len=8) :: nomgd, licmp(nbcmp)
    character(len=16) :: crit
! person_in_charge: mathieu.courtois at edf.fr
! ----------------------------------------------------------------------
!  CALC_CHAMP - TRAITEMENT DE CHAM_UTIL - CRITERE
!  -    -                     --          --
!  CALCUL DU CRITERE 'CRIT' (SUR LA GRANDEUR 'NOMGD') A PARTIR DES
!  'NBCMP' VALEURS DES COMPOSANTES. RANGE LE RESULTAT DANS 'VALRES'.
! ----------------------------------------------------------------------
! IN  :
!   CRIT   K16   NOM DU CRITERE A CALCULER
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
!
    integer :: ncsig
    parameter   (ncsig=6)
    integer :: i
    real(kind=8) :: rac2, vale(6)
    character(len=4) :: cmpsig(ncsig), cmpeps(ncsig), nomcmp(ncsig)
    character(len=16) :: valk(2)
    integer :: nt, nd
    common /tdim/ nt,nd
    data cmpsig / 'SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ', 'SIYZ'/
    data cmpeps / 'EPXX', 'EPYY', 'EPZZ', 'EPXY', 'EPXZ', 'EPYZ'/
!     ----- FIN  DECLARATIONS ------------------------------------------
!
    call jemarq()
    iret = 1
!
! --- VERIFICATIONS COMMUNES POUR VMIS, INVA_2, TRACE
    if (crit .eq. 'VMIS' .or. crit .eq. 'INVA_2' .or. crit .eq. 'TRACE') then
        ASSERT(nbcmpr.eq.1)
        if (nomgd .eq. 'SIEF_R') then
            nomcmp = cmpsig
        else if (nomgd.eq.'EPSI_R') then
            nomcmp = cmpeps
        else
            valk(1) = crit
            valk(2) = nomgd
            call u2mesk('F', 'CHAMPS_7', 2, valk)
        endif
!       CELCES ASSURE QUE LES COMPOSANTES SONT DANS L'ORDRE DU CATALOGUE
!       LE MEME QUE CELUI DE CMPSIG/CMPEPS
!       IL SUFFIT DONC DE VERIFIER QUE :
!       - SI NBCMP=6, CE SONT CELLES DE CMPSIG/CMPEPS
!       - SI NBCMP=4, CE SONT LES 4 PREMIERES
        if (nbcmp .ne. 4 .and. nbcmp .ne. 6) then
            goto 9999
        endif
        do 10 i = 1, nbcmp
            if (nomcmp(i) .ne. licmp(i)) then
                goto 9999
            endif
10      continue
    endif
!
! --- VMIS
    if (crit .eq. 'VMIS') then
        if (nomgd .ne. 'SIEF_R') then
            valk(1) = crit
            valk(2) = nomgd
            call u2mesk('F', 'CHAMPS_7', 2, valk)
        endif
!       COMMON POUR LCIV2S
        nt = nbcmp
        nd = 3
        rac2 = sqrt(2.0d0)
        do 21 i = 1, nd
            vale(i) = valin(i)
21      continue
        do 22 i = nd+1, nt
            vale(i) = rac2 * valin(i)
22      continue
        valres(1) = lciv2s(vale)
        iret = 0
!
! --- INVA_2
    else if (crit .eq. 'INVA_2') then
        if (nomgd .ne. 'EPSI_R') then
            valk(1) = crit
            valk(2) = nomgd
            call u2mesk('F', 'CHAMPS_7', 2, valk)
        endif
!       COMMON POUR LCIV2S
        nt = nbcmp
        nd = 3
        rac2 = sqrt(2.0d0)
        do 31 i = 1, nd
            vale(i) = valin(i)
31      continue
        do 32 i = nd+1, nt
            vale(i) = rac2 * valin(i)
32      continue
        valres(1) = lciv2e(vale)
        iret = 0
!
! --- TRACE
    else if (crit .eq. 'TRACE') then
        valres(1) = valin(1) + valin(2) + valin(3)
        iret = 0
!
    else
! ---   INTERDIT DANS LE CATALOGUE
        ASSERT(.false.)
    endif
!
9999  continue
    call jedema()
!
end subroutine

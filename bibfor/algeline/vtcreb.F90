subroutine vtcreb(champz, numedz, basez, typcz, neq)
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
!     CREATION D'UNE STRUCTURE CHAM_NO "CHAMP"
!
!     IN  CHAMPZ : K19 : NOM DU CHAM_NO A CREER
!     IN  NUMEDZ : K24 : PROF_CHNO DU CHAM_NO
!     IN  BASEZ  : CH1 : NOM DE LA BASE SUR LAQUELLE LE CHAM_NO DOIT
!                        ETRE CREE
!     IN  TYPCZ  :     : TYPE DES VALEURS DU CHAM_NO A CREER
!                  'R'    ==> COEFFICIENTS REELS
!                  'C'    ==> COEFFICIENTS COMPLEXES
!     OUT   NEQ   : I   : INTEGER
!     REMARQUE:  AUCUN CONTROLE SUR LE "TYPC" QUE L'ON PASSE TEL QUEL
!                A JEVEUX_MON_NEVEU
!     PRECAUTIONS D'EMPLOI :
!       1) LE CHAM_NO "CHAMP" NE DOIT PAS EXISTER
!       2) LES COEFFICIENTS DU CHAM_NO "CHAMP" NE SONT PAS AFFECTES
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/vtcre1.h"
#include "asterfort/wkvect.h"
    character(len=*) :: champz, numedz, basez, typcz
    integer :: neq
!
!
! DECLARATION VARIABLES LOCALES
    integer :: idime, neql
    character(len=1) :: classe, typc
    character(len=8) :: k8bid
    character(len=11) :: k11b
    character(len=24) :: champ, numedd, k24bid, k24b
!
!------------------------------------------------------------------
    call jemarq()
    champ = champz
    numedd = numedz
    classe = basez(1:1)
    typc = typcz(1:1)
    call vtcre1(champ, numedd, classe, typc, neq)
    call jedema()
end subroutine

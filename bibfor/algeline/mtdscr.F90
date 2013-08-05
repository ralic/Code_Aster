subroutine mtdscr(nommat)
    implicit none
#include "jeveux.h"
!
#include "asterc/ismaem.h"
#include "asterfort/assert.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nommat
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     ALLOCATION DES DESCRIPTEURS D'UNE MATRICE
!     ------------------------------------------------------------------
!
! IN  NOMMAT  : K19 : NOM DE LA MATRICE
!     ------------------------------------------------------------------
!     CETTE ROUTINE CREE 2 OBJETS DE TRAVAIL SUR LA BASE VOLATILE
!
!     DE NOM  NOMMAT//'.&INT'   VECTEUR D'ENTIER
!             NOMMAT//'.&IN2'   VECTEUR DE K24
!
!     ZI(+0) : INUTILISE
!     ZK24(ZI(+1) : NOM DEVELOPPEUR DE LA MATRICE
!     ZI(+2) : NOMBRE GLOBAL D'EQUATIONS
!     ZI(+3) : TYPE DE VALEURS
!                1 : REELLE
!                2 : COMPLEXE
!     ZI(+4) : PROPRIETE DE SYMETRIE DE LA MATRICE
!                0 : QUELCONQUE
!                1 : SYMETRIQUE
!     ZI(+5) : NOMBRE LOCAL D'EQUATIONS
!     ZI(+6) : INUTILISE
!     ZI(+7) : NOMBRE DE DDLS IMPOSES PAR DES CHARGES CINEMATIQUES DANS
!              LA MATRICE ASSEMBLEE = NELIM
!
!     ZI(+10) : INUTILISE
!     ZI(+11) : INUTILISE
!     ZI(+12) : INUTILISE
!     ZI(+13) : INUTILISE
!     ZI(+14) : LONGUEUR DU BLOC POUR LA MATRICE MORSE
!     ZI(+15) : INUTILISE
!     ZI(+16) : INUTILISE
!     ZI(+17) : INUTILISE
!     ZI(+18) : INUTILISE
!     ------------------------------------------------------------------
!
!
!
!     ----- PARAMETRES DE DEFINITION DES MATRICES ----------------------
    integer :: imatd
    character(len=2) :: tyma
    character(len=4) :: kbid
    character(len=14) :: nu
    character(len=19) :: mat19, nomsto
!     ------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    integer :: ibid, ier, iret, jccid, jnequ, jrefa
    integer :: jscde, jsmde, k, lccid, lmat, lnom, nb1
    integer :: nb2
!-----------------------------------------------------------------------
    call jemarq()
    mat19 = nommat
!
!
!        ------ ALLOCATION DES OBJETS SI NECESSAIRE :
    call jeexin(mat19//'.&INT', ier)
    if (ier .eq. 0) then
        call jecreo(mat19//'.&INT', ' V V I')
        call jeecra(mat19//'.&INT', 'LONMAX', 19, '  ')
    endif
!
    call jeveuo(mat19//'.&INT', 'E', lmat)
    do 10,k = 1,19
    zi(lmat-1+k) = ismaem()
    10 end do
!
    call jeexin(mat19//'.&IN2', ier)
    if (ier .eq. 0) then
        call wkvect(mat19//'.&IN2', ' V V K24', 1, lnom)
    endif
!
    call jeveut(mat19//'.&IN2', 'E', lnom)
    zk24(lnom) = mat19
!
!
!     -- LMAT+1 :
!     ------------
    zi(lmat+1) = lnom
!
!
    call jeexin(mat19//'.REFA', ier)
    ASSERT(ier.ne.0)
!
!
    call jeveuo(mat19//'.REFA', 'L', jrefa)
    nu = zk24(jrefa-1+2)
    nomsto = nu//'.SMOS'
!
!     -- LMAT+2 ET +5 :
!     ------------
    call jeveuo(nomsto//'.SMDE', 'L', jsmde)
    call jeveuo(nu//'.NUME.NEQU', 'L', jnequ)
    zi(lmat+2) = zi(jnequ-1+1)
    call jeexin(nu//'.NUML.NULG', imatd)
    if (imatd .ne. 0) then
        call jeveuo(nu//'.NUML.NEQU', 'L', jnequ)
        zi(lmat+5) = zi(jnequ-1+1)
    else
        zi(lmat+5) = zi(lmat+2)
    endif
!
!
!     -- LMAT+3 :
!     ------------
    call jeexin(mat19//'.VALM', iret)
!     -- POUR TRAITER LE CAS OU ON A DETRUIT VOLONTAIREMENT LE .VALM
    if (iret .gt. 0) then
        call jelira(mat19//'.VALM', 'TYPE', ibid, kbid)
    else
        call jelira(mat19//'.UALF', 'TYPE', ibid, kbid)
    endif
!
    ASSERT(kbid(1:1).eq.'R' .or. kbid(1:1).eq.'C')
    if (kbid(1:1) .eq. 'R') zi(lmat+3) = 1
    if (kbid(1:1) .eq. 'C') zi(lmat+3) = 2
!
!
!     -- LMAT+4 :
!     ------------
    call jeexin(mat19//'.VALM', iret)
!     -- POUR TRAITER LE CAS OU ON A DETRUIT VOLONTAIREMENT LE .VALM
    if (iret .gt. 0) then
        tyma = zk24(jrefa-1+9)
        if (tyma .eq. 'MS') then
            zi(lmat+4) = 1
!
        else if (tyma.eq.'MR') then
            zi(lmat+4) = 0
!
        else
            ASSERT(.false.)
        endif
!
    else
        call jelira(mat19//'.UALF', 'NMAXOC', nb1, kbid)
        call jeveuo(nu//'.SLCS.SCDE', 'L', jscde)
        nb2 = zi(jscde-1+3)
        if (nb1 .eq. nb2) then
            zi(lmat+4) = 1
!
        else if (nb1.eq.2*nb2) then
            zi(lmat+4) = 0
!
        else
            ASSERT(.false.)
        endif
!
    endif
!
!
!     -- LMAT+7    (SI CHARGES CINEMATIQUES) :
!     -------------------------------------------------
    call jeexin(mat19//'.CCID', ier)
    if (ier .ne. 0) then
        call jeveuo(mat19//'.CCID', 'L', jccid)
        call jelira(mat19//'.CCID', 'LONMAX', lccid, kbid)
        zi(lmat+7) = zi(jccid-1+lccid+1)
    else
        zi(lmat+7) = 0
    endif
!
!
!     -- LMAT+14
!     ----------
    zi(lmat+14) = zi(jsmde-1+2)
!
!
    call jedema()
end subroutine

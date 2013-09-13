subroutine utlisi(motcle, a, na, b, nb,&
                  c, nc, ntrou)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ARGUMENTS:
!     ----------
#include "asterfort/indiis.h"
#include "asterfort/utmess.h"
    character(len=*) :: motcle
    integer :: a(*), b(*), c(*), na, nb, nc, ntrou
! ----------------------------------------------------------------------
!     BUT :
!
!     UTILITAIRE D'OPERATION LOGIQUE SUR DES LISTES D'ENTIERS:
!          C = SINGLETON(A)
!          C = INTERSECTION (A,B)
!          C = UNION        (A,B)
!          C = A - B
!
!     ATTENTION AUX EFFETS DE BORD :
!     -------------------------------
!     LA PROGRAMMATION SUPPOSE QUE LES TABLEAUX A, B, C SONT DISTINCTS.
!
!     ATTENTION AUX DOUBLONS :
!     ------------------------
!     (SI LES LISTES A ET B CONTIENNENT DES ELEMENTS MULTIPLES) :
!     ALGORITHMES :
!       * SINGL :  ON RECOPIE DANS C LES ELEMENTS DE A DANS L'ORDRE
!                  DE A EN RECOPIANT PAS PLUSIEURS FOIS UN MEME
!                  ELEMENT. LES ELEMENTS DE C SONT DONC TOUS DIFF.
!       * UNION :  ON RECOPIE A DANS C, PUIS ON RECOPIE LES ELEMENTS
!                  DE B QUI NE SONT PAS DANS A.
!                  (ON A DONC OTE LES DOUBLONS (A,B) MAIS PAS
!                    CEUX DE (A,A) NI (B,B)!)
!       * INTER :  ON RECOPIE DANS C LES ELEMENTS DE A QUI EXISTENT
!                  DANS B. LES DOUBLONS (A,A) PEUVENT PERSISTER.
!       * A - B :  ON RECOPIE DANS C LES ELEMENTS DE A QUI N'EXISTENT
!                  PAS DANS B. LES DOUBLONS (A,A) PEUVENT PERSISTER.
!
!
!     ENTREES:
!       MOTCLE : ACTION DEMANDEE :  / 'SINGL'(ETON)
!                                   / 'UNION'
!                                   / 'INTER'(SECTION)
!                                   / 'DIFFE'(RENCE)
!       A,B    : LISTES D'ENTIER.
!       NA,NB  : DIMENSIONS DES LISTES A ET B
!       NC     : DIMENSION DE LA LISTE C.
!
!     SORTIES:
!       C  : LISTE D'ENTIERS TROUVEE.
!     NTROU: "+" NB D'ENTIERS DS LA LISTE C (SI NC "IN" SUFFISANT).
!     NTROU: "-" NB D'ENTIERS DS LA LISTE C (SI NC "IN" INSUFFISANT).
!
! ----------------------------------------------------------------------
    character(len=5) :: motcl2
    integer :: ia, ib, ic, ii
! DEB-------------------------------------------------------------------
    motcl2=motcle
!
!
    if (motcl2 .eq. 'SINGL') then
!     ---------------------------
        ic=0
        do 1, ia=1,na
        ii= indiis(a,a(ia),1,ia-1)
        if (ii .eq. 0) then
            ic=ic+1
            if (ic .le. nc) c(ic)=a(ia)
        endif
 1      continue
        ntrou=ic
        if (ic .gt. nc) ntrou= -ntrou
!
!
    else if (motcl2.eq.'UNION') then
!     ---------------------------------
        ic=0
        do 21, ia=1,na
        ic=ic+1
        if (ic .le. nc) c(ic)=a(ia)
21      continue
        do 22, ib=1,nb
        ii= indiis(a,b(ib),1,na)
        if (ii .eq. 0) then
            ic=ic+1
            if (ic .le. nc) c(ic)=b(ib)
        endif
22      continue
        ntrou=ic
        if (ic .gt. nc) ntrou= -ntrou
!
!
    else if (motcl2.eq.'INTER') then
!     ---------------------------------
        ic=0
        do 31, ia=1,na
        ii= indiis(b,a(ia),1,nb)
        if (ii .gt. 0) then
            ic=ic+1
            if (ic .le. nc) c(ic)=a(ia)
        endif
31      continue
        ntrou=ic
        if (ic .gt. nc) ntrou= -ntrou
!
!
    else if (motcl2(1:5).eq.'DIFFE') then
!     ---------------------------------
        ic=0
        do 41, ia=1,na
        ii= indiis(b,a(ia),1,nb)
        if (ii .eq. 0) then
            ic=ic+1
            if (ic .le. nc) c(ic)=a(ia)
        endif
41      continue
        ntrou=ic
        if (ic .gt. nc) ntrou= -ntrou
!
!
    else
!     -----
        call utmess('F', 'UTILITAI5_47', sk=motcl2)
    endif
!
end subroutine

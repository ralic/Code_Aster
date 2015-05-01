subroutine i2vois(conec, type, maille, n, v1,&
                  v2)
    implicit none
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!**********************************************************************
!
!     RECHERCHE DES VOISINS (AU PLUS 2 PAR HYPOTHESE) DES MAILLES
!     D' UN ENSEMBLE DE MAILLES
!
!       CONEC  (IN)  : NOM DE L' OBJET CONNECTIVITE DU MAILLAGE
!
!       TYPE   (IN)  : NOM DE L' OBJET CONTENANT LES TYPES DES MAILLES
!
!       MAILLE (IN)  : TABLEAU DES NUMERO DE MAILLES
!
!       N      (IN)  : NOMBRE DE MAILLES DE L' ENSEMBLE TRAITE
!
!       V1     (OUT) : TABLEAU D' ACCES AUX VOISINS NUMERO 1
!
!       V2     (OUT) : TABLEAU D' ACCES AUX VOISINS NUMERO 2
!
!
!     EXPLICATION DE LA STRUCTURE DE DONNEES
!
!        MAILLE (V1(I)) <---VOISIN NUMERO 1 DE MAILLE(I)
!
!        MAILLE (V2(I)) <---VOISIN NUMERO 2 DE MAILLE(I)
!
!**********************************************************************
!
#include "asterf_types.h"
#include "asterfort/i2extf.h"
    integer :: n, v1(*), v2(*), maille(*)
    character(len=24) :: conec, type
!
    integer :: i, j, mi, mj
    integer :: nig, njg
    integer :: nid, njd
    aster_logical :: nonv1, nonv2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    i = 0
    j = 0
!
    mi = 0
    mj = 0
!
    nig = 0
    nig = 0
    njg = 0
    njg = 0
    nid = 0
    nid = 0
    njd = 0
    njd = 0
!
    nonv1 = .true.
    nonv2 = .true.
!
    do 10 i = 1, n, 1
!
        mi = maille(i)
!
        call i2extf(mi, 1, conec(1:15), type(1:16), nig,&
                    nid)
!
        nonv1 = .true.
        nonv2 = .true.
!
        j = 0
!
 20     continue
        if ((nonv1 .or. nonv2) .and. (j .lt. n)) then
!
            j = j + 1
!
            if (i .ne. j) then
!
                mj = maille(j)
!
                call i2extf(mj, 1, conec(1:15), type(1:16), njg,&
                            njd)
!
                if ((nig .eq. njg) .or. (nid .eq. njg) .or. (nig .eq. njd) .or.&
                    (nid .eq. njd)) then
!
                    if (nonv1) then
!
                        nonv1 = .false.
!
                        v1(i) = j
!
                    else if (nonv2) then
!
                        nonv2 = .false.
!
                        v2(i) = j
!
                    else
!
                    endif
!
                endif
!
            endif
!
            goto 20
!
        endif
!
        if (nonv1) then
!
            v1(i) = 0
!
        endif
!
        if (nonv2) then
!
            v2(i) = 0
!
        endif
!
 10 end do
!
end subroutine

subroutine compno(mailla, nbgr, nomgr, nbto)
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 13/07/90
!-----------------------------------------------------------------------
!  BUT: COMPTAGE DU NOMBRE DE NOEUDS CORRESPONDANTS A UNE LISTE DE
!     GROUPENO
!        NOTA BENE: LES NOEUDS PEUVENT APPARAITRE PLUSIEURS FOIS
!
!-----------------------------------------------------------------------
!
! MAILLA /I/: NOM UTILISATEUR DU MAILLAGE
! NBGR     /I/: NOMBRE DE GROUPES DE NOEUDS
! NOMGR    /I/: NOMS DES GROUPES DE NOEUDS
! NBTO     /O/: NOMBRE DE NOEUDS
!
!
!
!
#include "jeveux.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
!
    integer :: nbgr
    character(len=8) :: mailla
    character(len=24) :: valk(2), nomcou, nomgr(nbgr)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ier, nb, nbto, num
!-----------------------------------------------------------------------
    if (nbgr .eq. 0) then
        nbto=0
        goto 9999
    endif
!
!-------RECUPERATION DES POINTEURS DE GROU_NO---------------------------
!
    call jeexin(mailla//'.GROUPENO', ier)
    if (ier .eq. 0) then
        valk (1) = mailla
        call utmess('F', 'ALGORITH12_57', sk=valk(1))
    endif
!
!-------COMPTAGE DES NOEUD DEFINIS PAR GROUPES--------------------------
!
    nbto=0
!
    do 10 i = 1, nbgr
        nomcou=nomgr(i)
        call jenonu(jexnom(mailla//'.GROUPENO', nomcou), num)
!
        if (num .eq. 0) then
            valk (1) = mailla
            valk (2) = nomcou
            call utmess('F', 'ALGORITH12_58', nk=2, valk=valk)
        endif
!
        call jelira(jexnom(mailla//'.GROUPENO', nomcou), 'LONUTI', nb)
        nbto=nbto+nb
!
10  end do
!
9999  continue
end subroutine

subroutine compma(mailla, nbgr, nomgr, nbto)
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
!    P. RICHARD     DATE 17/01/92
!-----------------------------------------------------------------------
!  BUT: COMPTAGE DU NOMBRE DE MAILLES CORRESPONDANTS A UNE LISTE DE
!     GROUPEMA
!
!        NOTA BENE: LES MAILLES PEUVENT APPARAITRE PLUSIEURS FOIS
!
!-----------------------------------------------------------------------
!
! MAILLA /I/: NOM UTILISATEUR DU MAILLAGE
! NBGR     /I/: NOMBRE DE GROUPES DE MAILLES
! NOMGR    /I/: NOMS DES GROUPES DE MAILLE
! NBTO     /O/: NOMBRE DE MAILLES
!
!
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/u2mesg.h'
    character(len=8) :: mailla
    character(len=24) :: valk(2), nomcou, nomgr(nbgr)
    character(len=1) :: k1bid
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ier, nb, nbgr, nbto, num
!-----------------------------------------------------------------------
    if (nbgr .eq. 0) then
        nbto=0
        goto 9999
    endif
!
!-------RECUPERATION DES POINTEURS DE GROU_MA---------------------------
!
    call jeexin(mailla//'.GROUPEMA', ier)
    if (ier .eq. 0) then
        valk (1) = mailla
        call u2mesg('E', 'ALGORITH12_55', 1, valk, 0,&
                    0, 0, 0.d0)
    endif
!
!-------COMPTAGE DES MAILLES DEFINIS PAR GROUPES------------------------
!
    nbto=0
!
    do 10 i = 1, nbgr
        nomcou=nomgr(i)
        call jenonu(jexnom(mailla//'.GROUPEMA', nomcou), num)
!
        if (num .eq. 0) then
            valk (1) = mailla
            valk (2) = nomcou
            call u2mesg('E', 'ALGORITH12_56', 2, valk, 0,&
                        0, 0, 0.d0)
        endif
!
        call jelira(jexnom(mailla//'.GROUPEMA', nomcou), 'LONUTI', nb, k1bid)
        nbto=nbto+nb
!
10  end do
!
9999  continue
end subroutine

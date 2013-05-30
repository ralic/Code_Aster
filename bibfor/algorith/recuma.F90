subroutine recuma(mailla, nbma, nbgr, nomma, nomgr,&
                  nbto, numnot)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!***********************************************************************
!    P. RICHARD     DATE 13/07/90
!-----------------------------------------------------------------------
!  BUT: RASSEMBLER LES MAILLES DE NOMMA ET DES GROUPNO DE NOMGR
    implicit none
!          ET TRANSCODER DANS NUMNOT
!
!-----------------------------------------------------------------------
!
! MAILLA   /I/: NOM UTILISATEUR DU MAILLAGE
! NBMA     /I/: NOMBRE DE MAILLE EN ARGUMENT DE LA COMMANDE
! NBGR     /I/: NOMBRE DE GROUPES DE MAILLES EN ARGUMENTS
! NOMMA    /I/: NOMS DES MAILLES DONNES EN ARGUMENTS
! NOMGR    /I/: NOMS DES GROUPES DE MAILLES EN ARGUMENTS
! NBTO     /O/: NOMBRE TOTAL DE MAILLES ASSOCIES A L'INTERFACE
! NUMNOT   /O/: VECTEUR DES NUMERO DES MAILLES D'INTERFACE
!
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/u2mesg.h'
    character(len=8) :: mailla, nomma(nbma)
    character(len=24) :: valk(2), nomgr(nbgr), nomcou
    integer :: numnot(nbto)
    character(len=1) :: k1bid
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iadg, icomp, j, nb, nbgr, nbma
    integer :: nbto, numa
!-----------------------------------------------------------------------
    call jemarq()
    icomp=0
!
!-------RECUPERATION ET TRANSCODAGE DES MAILLES DES GROUPES-------------
!
    if (nbgr .gt. 0) then
        do 10 i = 1, nbgr
            nomcou=nomgr(i)
            call jelira(jexnom(mailla//'.GROUPEMA', nomcou), 'LONUTI', nb, k1bid)
            call jeveuo(jexnom(mailla//'.GROUPEMA', nomcou), 'L', iadg)
            do 20 j = 1, nb
                icomp=icomp+1
                numnot(icomp)=zi(iadg+j-1)
20          continue
10      continue
    endif
!
!
!-------RECUPERATION ET TRANSCODAGE DES MAILLES-------------------------
!
!
!
    if (nbma .gt. 0) then
        do 30 i = 1, nbma
            nomcou=nomma(i)
            call jenonu(jexnom(mailla//'.NOMMAI', nomcou), numa)
!
            if (numa .eq. 0) then
                valk (1) = mailla
                valk (2) = nomcou
                call u2mesg('E', 'ALGORITH14_10', 2, valk, 0,&
                            0, 0, 0.d0)
            endif
!
            icomp=icomp+1
            numnot(icomp)=numa
!
30      continue
    endif
    nbto=icomp
!
    call jedema()
end subroutine

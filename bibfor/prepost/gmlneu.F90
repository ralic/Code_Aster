subroutine gmlneu(igmsh, nbnode)
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
!.======================================================================
    implicit none
!
!      GMLNEU --   LECTURE DES NUMEROS DE NOEUDS ET DE LEURS
!                  COORDONNEES SUR LE FICHIER DE SORTIE DE GMSH
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NBNODE         OUT   I         NOMBRE DE NOEUDS DU MAILLAGE
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    include 'jeveux.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    integer :: igmsh, nbnode
! -----  VARIABLES LOCALES
!
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!-----------------------------------------------------------------------
    integer :: imes, inode, jcoor, jdetr, jinfo, ndmax
    integer :: node
    real(kind=8) :: x, y, z
!-----------------------------------------------------------------------
    call jemarq()
!
! --- INITIALISATION :
!     --------------
    nbnode = 0
!
! --- RECUPERATION DES NUMEROS D'UNITE LOGIQUE :
!     ----------------------------------------
    imes = iunifi('MESSAGE')
!
! --- LECTURE DU NOMBRE DE NOEUDS :
!     ---------------------------
    read(igmsh,'(I10)') nbnode
!
! --- CREATION DE VECTEURS DE TRAVAIL :
!     -------------------------------
    call jedetr('&&PREGMS.INFO.NOEUDS')
    call jedetr('&&PREGMS.DETR.NOEUDS')
    call jedetr('&&PREGMS.COOR.NOEUDS')
!
    call wkvect('&&PREGMS.INFO.NOEUDS', 'V V I', nbnode, jinfo)
    call wkvect('&&PREGMS.COOR.NOEUDS', 'V V R', 3*nbnode, jcoor)
!
! --- LECTURE DES NUMEROS DE NOEUDS ET DE LEURS COORDONNEES :
!     -----------------------------------------------------
    ndmax = 0
    do 10 inode = 1, nbnode
!        READ(IGMSH,'(I10,3(E25.16))') NODE,X,Y,Z
        read(igmsh,*) node,x,y,z
        ndmax = max(node,ndmax)
!
        zi(jinfo+inode-1) = node
        zr(jcoor-1+3*(inode-1)+1) = x
        zr(jcoor-1+3*(inode-1)+2) = y
        zr(jcoor-1+3*(inode-1)+3) = z
!
10  end do
!
!
!    LISTE DES NOEUDS A DETRUIRE (0: A DETRUIRE, 1: A CONSERVER)
    call wkvect('&&PREGMS.DETR.NOEUDS', 'V V I', ndmax+1, jdetr)
    do 12 node = 0, ndmax
        zi(jdetr+node) = 0
12  end do
!
!
    write(imes,*) 'NOMBRE DE NOEUDS : ',nbnode
!
    call jedema()
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine

subroutine gmeneu(imod, nbnode)
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
!      GMENEU --   ECRITURE DES NUMEROS DE NOEUDS ET DE LEURS
!                  COORDONNEES VENANT D'UN FICHIER .GMSH DANS
!                  UN FICHIER .MAIL
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NBNODE         IN    I         NOMBRE DE NOEUDS DU MAILLAGE
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/codnop.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jjmmaa.h"
    integer :: imod, nbnode
! -----  VARIABLES LOCALES
    character(len=1) :: prfnoe
    character(len=4) :: ct(3)
    character(len=8) :: chnode
    character(len=12) :: chenti, aut
    character(len=80) :: chfone
!
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!-----------------------------------------------------------------------
    integer :: inode, jcoor, jdetr, jinfo, node
    real(kind=8) :: x, y, z
!-----------------------------------------------------------------------
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    chnode = '        '
    prfnoe = 'N'
    chfone = '%FORMAT=(1*NOM_DE_NOEUD,3*COORD)'
    chenti = 'NBOBJ=      '
    call codent(nbnode, 'G', chenti(7:12))
!
! --- DATE :
!     ----
    call jjmmaa(ct, aut)
!
! --- RECUPERATION DES VECTEURS DE TRAVAIL :
!     ------------------------------------
    call jeveuo('&&PREGMS.INFO.NOEUDS', 'L', jinfo)
    call jeveuo('&&PREGMS.DETR.NOEUDS', 'L', jdetr)
    call jeveuo('&&PREGMS.COOR.NOEUDS', 'L', jcoor)
!
    call codnop(chnode, prfnoe, 1, 1)
!
    write(imod,'(A,4X,A)')'COOR_3D',chenti
    write(imod,'(A)') chfone
!
! --- ECRITURE DES NUMEROS DE NOEUDS ET DE LEURS COORDONNEES :
!     ------------------------------------------------------
    do 20 inode = 1, nbnode
        node = zi(jinfo+inode-1)
!
!      ON N'ECRIT PAS LES NOEUDS ORPHELINS
        if (zi(jdetr+node) .eq. 0) goto 20
!
        x = zr(jcoor-1+3*(inode-1)+1)
        y = zr(jcoor-1+3*(inode-1)+2)
        z = zr(jcoor-1+3*(inode-1)+3)
!
        call codent(node, 'G', chnode(2:8))
        write(imod,'(2X,A,2X,3(1PE21.14),1X)') chnode,x,y,z
!
20  end do
!
    write(imod,'(A)') 'FINSF'
    write(imod,'(A)') '%'
!
    call jedema()
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine

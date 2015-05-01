subroutine parotr(nomma, iageom, ima, nbno, o,&
                  mrot, t, coor)
    implicit none
#include "jeveux.h"
#include "asterfort/pacoor.h"
    character(len=8) :: nomma
    integer :: ima, nbno
    real(kind=8) :: o(3), mrot(3, 3), t(3), coor(*)
!---------------------------------------------------------------------
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
!     BUT: DONNER LA LISTE DES COORDONNEES DES NBNO 1ERS NOEUDS DE LA
!          MAILLE IMA DU MAILLAGE NOMMA APRES UNE ROTATION (O,MROT)
!          ET UNE TRANSLATION T OU DU NOEUD IMA SI NBNO=0
!     VERIFICTION : NBNO < OU = NBRE DE NOUDS DE LA MAILLE
! ARGUMENTS D'ENTREE:
! IN   NOMMA  K8  : NOM DU MAILLAGE
! IN   IAGEOM I   : ADRESSE DE L'OBJET MAILLAGE//'.COORDO   .VALE'
! IN   IMA    I   : NUMERO DE LA MAILLE OU DU NOEUD SI NBNO=0
! IN   NBNO   I   : NOMBRE DE NOEUDS DE LA MAILLE A EXTRAIRE OU 0
! IN   O      R(3): CENTRE DE ROTATION
! IN   MROT   R(9): MATRICE DE ROTATION
! IN   T      R(3): VECTEUR DE TRANSLATION
! OUT  COOR   R(*): COORDONNEES DES NBNO 1ERS NOEUDS DE LA MAILLE
!                   APRES TRANSFORMATION OU DU NOEUD
!                   POUR INO = 1,NBNO OU INO = IMA SI NBNO=0
!                   COOR(3*(INO-1)+1)= X1'(INO)
!                   COOR(3*(INO-1)+2)= X2'(INO)
!                   COOR(3*(INO-1)+3)= X3'(INO) ( EN 2D 0)
    real(kind=8) :: x(3)
! --- DEBUT
!-----------------------------------------------------------------------
    integer :: i, iageom, icoor, ino, j, nbnot
!-----------------------------------------------------------------------
    if (nbno .eq. 0) then
        nbnot = 1
        coor(1)=zr(iageom-1+3*(ima-1)+1)
        coor(2)=zr(iageom-1+3*(ima-1)+2)
        coor(3)=zr(iageom-1+3*(ima-1)+3)
    else
        nbnot = nbno
        call pacoor(nomma, ima, nbno, coor)
    endif
!
    do 1 ino = 1, nbnot
        icoor = 3*(ino-1)
        do 2 i = 1, 3
            x(i) = 0.d0
            do 3 j = 1, 3
                x(i) = x(i) + mrot(j,i)*(coor(icoor+j)-o(j))
 3          continue
 2      continue
        do 4 i = 1, 3
            coor(icoor+i) = x(i)+o(i)+t(i)
 4      continue
 1  end do
end subroutine

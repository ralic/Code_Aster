subroutine cgnop0(nbnoe, coor, x0, vecnor, prec,&
                  nbno, lisnoe)
    implicit   none
!.=====================================================================
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
!.========================= DEBUT DES DECLARATIONS ====================
!
! -----  ARGUMENTS
    integer :: nbnoe, nbno, lisnoe(*)
    real(kind=8) :: coor(*), x0(*), vecnor(*), prec
!
! --------- VARIABLES LOCALES ---------------------------
    integer :: ino
    real(kind=8) :: x(3), xx0(3), d
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- PARCOURS DES NOEUDS DU MAILLAGE :
!     --------------------------------
    nbno = 0
    do 10 ino = 1, nbnoe
!
! ---     COORDONNEES DU NOEUD :
!         --------------------
        x(1) = coor(3*(ino-1)+1)
        x(2) = coor(3*(ino-1)+2)
        x(3) = coor(3*(ino-1)+3)
!
        xx0(1) = x(1) - x0(1)
        xx0(2) = x(2) - x0(2)
        xx0(3) = x(3) - x0(3)
!
! ---     CALCUL DE LA DISTANCE DU NOEUD COURANT AU PLAN OU A
! ---     LA DROITE :
!         ---------
        d = xx0(1)*vecnor(1) + xx0(2)*vecnor(2) + xx0(3)*vecnor(3)
!
! ---     SI LE NOEUD COURANT EST SITUE DANS LE PLAN OU LA DROITE,
! ---     ON L'AFFECTE A LA LISTE DE NOEUDS QUI SERA AFFECTEE
! ---     AU GROUP_NO :
!         ----------
        if (abs(d) .le. prec) then
            nbno = nbno + 1
            lisnoe(nbno) = ino
        endif
!
10  end do
!.============================ FIN DE LA ROUTINE ======================
end subroutine

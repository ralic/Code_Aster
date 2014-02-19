subroutine pmftor(ety, etz, sk)
! ======================================================================
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    real(kind=8) :: ety, etz, sk(78)
!    -------------------------------------------------------------------
!     ENRICHISSEMENT DE LA MATRICE DE RIGIDITE PAR LES TERMES DE
!     COUPLAGE DE LA TORSION-FLEXION
!     ETY = EXCENTRICITE DU CENTRE DE TORSION SELON Y
!     ETZ = EXCENTRICITE DU CENTRE DE TORSION SELON Z
!     SK = MATRICE DE RIGIDITE
!    -------------------------------------------------------------------
    integer :: ip(12)
    real(kind=8) :: etz2,ety2
    data ip/0,1,3,6,10,15,21,28,36,45,55,66/
!
    ety2 = ety*ety
    etz2 = etz*etz
!
!   rectification pour la torsion
    sk(ip(4)+4)  =  sk(ip(4)+4) + etz2*sk(ip(2)+2) + ety2*sk(ip(3)+3)
    sk(ip(10)+4) = -sk(ip(4)+4)
    sk(ip(10)+10)=  sk(ip(4)+4)
!
!   terme induit par l'excentricite
    sk(ip(4)+2)  = -etz*sk(ip(2)+2) + ety*sk(ip(3)+2)
    sk(ip(10)+2) = -sk(ip(4)+2)
    sk(ip(4)+3)  = -etz*sk(ip(3)+2) + ety*sk(ip(3)+3)
    sk(ip(10)+3) = -sk(ip(4)+3)
    sk(ip(5)+4)  = -etz*sk(ip(5)+2) + ety*sk(ip(5)+3)
    sk(ip(6)+4)  = -etz*sk(ip(6)+2) + ety*sk(ip(6)+3)
    sk(ip(8)+4)  =  sk(ip(10)+2)
    sk(ip(9)+4)  =  sk(ip(10)+3)
    sk(ip(11)+4) =  sk(ip(5)+4)
    sk(ip(12)+4) =  sk(ip(6)+4)
    sk(ip(10)+5) = -sk(ip(5)+4)
    sk(ip(10)+6) = -sk(ip(6)+4)
    sk(ip(10)+8) =  sk(ip(4)+2)
    sk(ip(10)+9) =  sk(ip(4)+3)
    sk(ip(11)+10)=  sk(ip(10)+5)
    sk(ip(12)+10)=  sk(ip(10)+6)
!
end subroutine

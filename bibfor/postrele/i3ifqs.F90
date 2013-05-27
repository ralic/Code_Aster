subroutine i3ifqs(epsi, k, f, desc, desctm,&
                  conexk, coordo, sgt, nbpt, lstpt,&
                  fink)
    implicit  none
    include 'asterfort/i3iqgs.h'
    include 'asterfort/i3iqps.h'
    include 'asterfort/i3tstf.h'
    integer :: k, f, desc(*), desctm(*), conexk(*), nbpt, lstpt(*)
    real(kind=8) :: epsi, sgt(*), coordo(*)
    logical :: fink
!
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     ------------------------------------------------------------------
!     INTERSECTION FACE QUADRANGLE F SGT (AB)
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  K      : I : -
! IN  DESC   : I :  !--> OBJ MAILLE POINTEE (ET CE QU' ELLE POINTE)
! IN  DESCTM : I : -
! IN  F      : I : NUMERO LOCALE DE LA FACE TRAITEE
! IN  CONEXK : I : CONNECTIVITE DE LA MAILLE POINTEE
! IN  COORDO : R : TABLE GLOBALE DES COORDONEES
! IN  SGT    : R : COORDONNEES DES POINTS A ET B -
! VAR FINK   : L : INDICATEUR DE FIN DE REPERAGE NIVEAU MAILLE 3D
! OUT NBPT   : I : NOMBRE DE POINT TROUVE
!            :   : CONVENTION NBPT = -2 <=> CARD(INTER) = INFINI
!            :   : DANS CE CAS OUT = EXTREMITES
! OUT LSTPT  : I : OBJ LISTE_POINT
!     ------------------------------------------------------------------
!
    logical :: gauche
!
!======================================================================
!
    call i3tstf(k, f, desc, desctm, conexk,&
                coordo, gauche, epsi)
!
    if (gauche) then
        call i3iqgs(epsi, k, f, desc, desctm,&
                    conexk, coordo, sgt, nbpt, lstpt,&
                    fink)
    else
        call i3iqps(epsi, k, f, desc, desctm,&
                    conexk, coordo, sgt, nbpt, lstpt,&
                    fink)
    endif
!
end subroutine

subroutine nmnet2(zimat, nmnbn, cnbn, cplas, czef,&
                  czeg, cief, cdeps, cdtg, cier,&
                  dc1, dc2, depsp2, normm)
    implicit  none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DE CNBN ET CDEPSP
!     QUAND DEUX CRITERES PLASTIQUES SONT ACTIVES
!
! IN  ZIMAT : ADRESSE DE LA LISTE DE MATERIAU CODE
! IN  NMNBN : FORCE - BACKFORCE
! IN  CDTG : MATRICE TANGENTE
! IN  DC1 : MATRICE ELASTIQUE + CONSTANTES DE PRAGER
! IN  DC2 : MATRICE ELASTIQUE + CONSTANTES DE PRAGER
!
! OUT CNBN : NOUVELLE FORCE - BACKFORCE
! OUT CPLAS : NOUVEAUX MOMENTS LIMITES DE PLASTICITE
! OUT CZEF : NOUVEAU ZERO ADIMENSIONNEL POUR LE CRITERE F
! OUT CZEG : NOUVEAU ZERO ADIMENSIONNEL POUR LE CRITERE G
! OUT CIEF : NOUVEAU CIEF > 0 : NBN HORS DE LA ZONE DE DEFINITION DE MP
! OUT CDEPS : NOUVEL INCREMENT DE DEFORMATION DANS LE REPERE ORTHO
! OUT CIER : NOUVEAU CODE ERREUR
! OUT DEPSP2 : NOUVEL INCREMENT DE DEF PLASTIQUE DANS LE REPERE ORTHO
! OUT NORMM : NORME SUR LA FONCTION MP = F(N)
!
    include 'asterfort/gplass.h'
    include 'asterfort/matmul.h'
    include 'asterfort/mppffn.h'
    integer :: j, cief, cier, zimat
!
    real(kind=8) :: nmnbn(6), cnbn(6), cplas(2, 3), czef, czeg
    real(kind=8) :: cdeps(6), cdtg(6, 6), dc1(6, 6), dc2(6, 6), normm
    real(kind=8) :: depsp2(6, 2), cp(6)
!
    call matmul(cdtg, cdeps, 6, 6, 1,&
                cnbn)
    call matmul(dc1, depsp2, 6, 6, 1,&
                cp)
!
    do 10, j = 1,6
    cnbn(j) = nmnbn(j) + cnbn(j) - cp(j)
    10 end do
!
    call matmul(dc2, depsp2(1, 2), 6, 6, 1,&
                cp)
!
    do 20, j = 1,6
    cnbn(j) = cnbn(j) - cp(j)
    20 end do
!
!     CALCUL DES MOMENTS LIMITES DE PLASTICITE
!     ET DES ZEROS DES CRITERES
    call mppffn(zimat, cnbn, cplas, czef, czeg,&
                cief, normm)
!
    if (cief .gt. 0) then
        cier=2
        goto 30
    endif
!
    if ((gplass(cnbn,cplas,1) .gt. czeg) .or. (gplass(cnbn,cplas,2) .gt. czeg)) then
        cier=1
    else
        cier=0
    endif
!
30  continue
!
end subroutine

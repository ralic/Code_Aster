subroutine brbagl(zimat, nmnbn, nmplas, nmdpla, nmddpl,&
                  nmzef, nmzeg, nmief, nmprox, depsp,&
                  ddissi, dc1, dc2, dtg, bbok,&
                  normm, normn)
!
    implicit none
!-----------------------------------------------------------------------
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
!     INTEGRE LA LOI DE COMPORTEMENT GLRC_DAMAGE POUR UN INCREMENT
!     DE DEFORMATION DEPS DEFINIT DANS LE REPERE D ORTHOTROPIE
!
! IN  ZIMAT : ADRESSE DE LA LISTE DE MATERIAU CODE
! IN  DC1 : MATRICE ELASTIQUE + CONSTANTES DE PRAGER EN MEMBRANCE
! IN  DC2 : MATRICE ELASTIQUE + CONSTANTES DE PRAGER EN FLEXION
! IN  DTG : MATRICE TANGENTE
! IN  NORMM : NORME SUR LA FONCTION MP = F(N)
! IN  NORMN : NORME SUR LA FONCTION MP = F(N)
!
! IN/OUT NMNBN : FORCE - BACKFORCE
! IN/OUT NMPLAS : MOMENTS LIMITES DE PLASTICITE
! IN/OUT NMDPLA : DERIVEES DES MOMENTS LIMITES DE PLASTICITE
! IN/OUT NMDDPL : DERIVEES SECONDES DES MOMENTS LIMITES DE PLASTICITE
! IN/OUT NMZEF : ZERO ADIMENSIONNEL POUR LE CRITERE F
! IN/OUT NMZEG : ZERO ADIMENSIONNEL POUR LE CRITERE G
! IN/OUT NMIEF : NMIEF > 0 : NBN HORS DE LA ZONE DE DEFINITION DE MP
! IN/OUT NMPROX : NMPROX > 0 : NBN DANS ZONE DE CRITIQUE
! IN/OUT DEPSP : INCREMENT DE DEFORMATION DANS LE REPERE ORTHO
! IN/OUT DDISSI : INCREMENT DE DISSIPATION
!
! OUT BBOK : CRITERE DE CV DE L ALGO BRINGBACK
!
#include "asterf_types.h"
#include "asterfort/critnu.h"
#include "asterfort/dndiss.h"
#include "asterfort/fplass.h"
#include "asterfort/r8inir.h"
    aster_logical :: bbok
!
!      INTEGER NEWPRO(2)
    integer :: nmief, newief, nmprox(2), zimat
    integer :: ncrit, ncrnew, nbb, ier, i, j, ipara(4)
    integer :: nbbmax
!
    real(kind=8) :: nmnbn(6), newnbn(6)
    real(kind=8) :: nmplas(2, 3), newpla(2, 3)
    real(kind=8) :: nmdpla(2, 2), newdpl(2, 2)
    real(kind=8) :: nmddpl(2, 2), newddp(2, 2)
    real(kind=8) :: nmzef, newzef
    real(kind=8) :: nmzeg, newzeg, newzfg(2)
    real(kind=8) :: depsp(6), ddissi, normm, normn
    real(kind=8) :: dc1(6, 6), dc2(6, 6), dtg(6, 6)
    real(kind=8) :: depslo(6), depsbb(6), f1, f2, ddisbb
!
    nbb = 0
    nbbmax = 1000
    bbok = .false.
!
    call r8inir(6, 0.0d0, depslo, 1)
!
!     ATTENTION : NCRIT DOIT ETRE SUPERIEUR A ZERO
!     CELA SIGNIFIE QUE (M-BACKM) N EST PAS
!     DANS LE CRITERE DE PLASTICITE
!      NCRIT = CRITNU(ZIMAT,NMNBN,NMDPLA,DEPSLO,DTG,NORMM)
    ncrit = critnu(zimat,nmnbn,depslo,dtg,normm)
!
    do 190 nbb = 1, nbbmax
!
!       NEWZFG(1) = NEWZEF
!       NEWZFG(2) = NEWZEG
        ipara(1) = zimat
        ipara(2) = ncrit
!       IPARA(3) = NEWIEF
!       IPARA(4) = IER
!
!     CALCUL DU NOUVEAU MOMENT
!     DE L INCREMENT DE COURBURE PLASTIQUE ET DE LA DISSIPATION
        call dndiss(ipara, nmnbn, nmplas, nmdpla, nmddpl,&
                    nmprox, depslo, newnbn, newpla, newdpl,&
                    newddp, newzfg, depsbb, ddisbb, dc1,&
                    dc2, dtg, normm, normn)
!
        zimat = ipara(1)
        ncrit = ipara(2)
        newief = ipara(3)
        ier = ipara(4)
        newzef = newzfg(1)
        newzeg = newzfg(2)
!
        if (ier .gt. 0) goto 200
!
!     MISE A JOUR DES VARIABLE
        do 125 j = 1, 6
            nmnbn(j) = newnbn(j)
125     continue
!
        do 140 j = 1, 3
            do 130 i = 1, 2
                nmplas(i,j) = newpla(i,j)
130         continue
140     continue
!
        do 160 j = 1, 2
            do 150 i = 1, 2
                nmdpla(i,j) = newdpl(i,j)
                nmddpl(i,j) = newddp(i,j)
150         continue
160     continue
!
        nmzef = newzef
        nmzeg = newzeg
        nmief = newief
!
!        DO 170, J = 1,2
!          NMPROX(J) = NEWPRO(J)
! 170    CONTINUE
!
        do 180 j = 1, 6
            depsp(j) = depsp(j) + depsbb(j)
180     continue
!
        ddissi = ddissi + ddisbb
!
!     CALCUL DES CRITERES DE PLASTICITE F
        f1 = fplass(nmnbn,nmplas,1)
        f2 = fplass(nmnbn,nmplas,2)
!
        if (f1 .lt. nmzef .and. f2 .lt. nmzef .and. (f1 .gt. -nmzef .or. f2 .gt. -nmzef)) then
!
!     NMNBN PROCHE DE LA SURFACE DE PLASTICITE
            bbok = .true.
            goto 200
        endif
!
!     CALCUL DU PREDICTEUR ELASTIQUE ET DU NOMBRE DE CRITERES ACTIVES
!        NCRNEW = CRITNU(ZIMAT,NMNBN,NMDPLA,DEPSLO,DTG,NORMM)
        ncrnew = critnu(zimat,nmnbn,depslo,dtg,normm)
!
!     SI NCRNEW = 0 CELA VEUT DIRE QUE MBACKM EST DANS LE VOLUME ELAST
!     ON UTILISE LE CRITERE DU PAS PRECEDENT
!     POUR CALCULER LA COURBURE PLASTIQUE
!     C EST POURQUOI NCRIT N EST PAS MIS A JOUR
        if (ncrnew .gt. 0) then
            ncrit = ncrnew
        endif
190 end do
!
200 continue
!
end subroutine

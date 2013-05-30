subroutine chrp3d(ppp, siepin, siepoo, iop)
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
!--------------------------------------------------------
! ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
!-------------------------------------------------------
!   ------------------------------------------------------------------
!       CHGT DE REPERE POUR LES CONTRAINTES ET LES DEFORMATIONS
!         ECRITES SOUS FORME VECTORIELLE EN 3D
!                                             H. BUNG      02-93
!   ------------------------------------------------------------------
!   PPP      : MATRICE DE PASSAGE
!   SIEPIN   : VECTEUR DE CONTRAINTES OU DE DEFORMATIONS EN ENTREE
!   SIEPOO   : VECTEUR DE CONTRAINTES OU DE DEFORMATIONS EN SORTIE
!   IOP            ENTREE     SORTIE
!        = 0       SIG_G      SIG_L
!        = 1       SIG_L      SIG_G
!        = 2       EPS_G      EPS_L
!        = 3       EPS_L      EPS_G
!--    RAPPEL
!     [SIG] = [ S_11, S_22, S_33,   S_12,   S_23,   S_13 ]
!     [EPS] = [ E_11, E_22, E_33, 2*E_12, 2*E_23, 2*E_13 ]
    implicit none
!---   VARIABLES GLOBALES
    include 'asterfort/assert.h'
    include 'asterfort/dr3gl1.h'
    include 'asterfort/dr3gl2.h'
    real(kind=8) :: ppp(3, 3), siepin(*), siepoo(*)
    integer :: iop
!---   VARIABLES LOCALES
    real(kind=8) :: ain(9), aoo(9)
    integer :: llm(9), llv(6), ldef(6), ik, ikk, kop
    data llm/1,4,6,4,2,5,6,5,3/
    data llv/1,5,9,2,6,3/
    data ldef/2,3,4,6,7,8/
!----    PASSAGE VERS LA FORME MATRICIELLE
    do 10 ik = 1, 9
        ain(ik) = siepin(llm(ik))
10  end do
!
    if (iop .ge. 2) then
!---     TENSEUR DE DEFORMATIONS
        do 20 ik = 1, 6
            ikk = ldef(ik)
            ain(ikk) = 0.5d0*ain(ikk)
20      continue
    endif
!
    kop = mod(iop,2)
    if (kop .eq. 0) then
!        ENTREE : GLOBAL --> SORTIE : LOCAL
        call dr3gl1(ppp, ain, aoo)
    endif
    if (kop .eq. 1) then
!        ENTREE : LOCAL --> SORTIE : GLOBAL
        call dr3gl2(ppp, aoo, ain)
    endif
    call assert(kop.eq.0.or.kop.eq.1)
!
!----    PASSAGE VERS LA FORME VECTORIELLE
    do 30 ik = 1, 6
        siepoo(ik) = aoo(llv(ik))
30  end do
!----    POUR LES DEFORMATIONS
    if (iop .ge. 2) then
        siepoo(4) = 2.d0*siepoo(4)
        siepoo(5) = 2.d0*siepoo(5)
        siepoo(6) = 2.d0*siepoo(6)
    endif
end subroutine

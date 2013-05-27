subroutine dxefg2(pgl, sigt)
    implicit none
    include 'jeveux.h'
    include 'asterfort/dxmat1.h'
    include 'asterfort/elref5.h'
    include 'asterfort/jevech.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: pgl(3, 3), sigt(1)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
! --- EFFORTS GENERALISES N, M, V D'ORIGINE THERMIQUE AUX POINTS
! --- D'INTEGRATION POUR LES ELEMENTS COQUES A FACETTES PLANES :
! --- DKTG DUS :
! ---  .A UN CHAMP DE TEMPERATURES SUR LE PLAN MOYEN DONNANT
! ---        DES EFFORTS DE MEMBRANE
! ---  .A UN GRADIENT DE TEMPERATURES DANS L'EPAISSEUR DE LA COQUE
!     ------------------------------------------------------------------
!     IN  PGL(3,3)     : MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE
!                        LOCAL
!     OUT SIGT(1)      : EFFORTS  GENERALISES D'ORIGINE THERMIQUE
!                        AUX POINTS D'INTEGRATION
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: multic, ipg, somire, i
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3)
    real(kind=8) :: tmoypg, tsuppg, tinfpg
    real(kind=8) :: t2ev(4), t2ve(4), t1ve(9)
    integer :: icodre(56)
    character(len=4) :: fami
    character(len=10) :: phenom
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: igau, indith, iret1, iret, irett, iretm, jcara
    integer :: jcou, jmate
    real(kind=8) :: coe1, coe2, epais, tref
!-----------------------------------------------------------------------
    fami = 'RIGI'
    call elref5(' ', fami, ndim, nno, nnos,&
                npg, ipoids, icoopg, ivf, idfdx,&
                idfd2, jgano)
!
    call r8inir(32, 0.d0, sigt, 1)
!
    call jevech('PMATERC', 'L', jmate)
    call rccoma(zi(jmate), 'ELAS', 1, phenom, icodre)
!
! --- RECUPERATION DE LA TEMPERATURE DE REFERENCE ET
! --- DE L'EPAISSEUR DE LA COQUE
!     --------------------------
!
    call jevech('PCACOQU', 'L', jcara)
    epais = zr(jcara)
!
    call rcvarc(' ', 'TEMP_MIL', 'REF', fami, 1,&
                1, tref, iret1)
!
!
! --- CALCUL DES COEFFICIENTS THERMOELASTIQUES DE FLEXION,
! --- MEMBRANE, MEMBRANE-FLEXION
!     ----------------------------------------------------
!
    call dxmat1('RIGI', epais, df, dm, dmf,&
                pgl, indith, t2ev, t2ve, t1ve,&
                npg)
    if (indith .eq. -1) goto 30
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION
!     -----------------------------------
    do 20 igau = 1, npg
!
!  --      TEMPERATURES SUR LES FEUILLETS MOYEN, SUPERIEUR ET INFERIEUR
!  --      AU POINT D'INTEGRATION COURANT
!          ------------------------------
        call rcvarc(' ', 'TEMP_INF', '+', fami, igau,&
                    1, tinfpg, iret)
        call rcvarc(' ', 'TEMP_SUP', '+', fami, igau,&
                    1, tsuppg, iret)
        call rcvarc(' ', 'TEMP_MIL', '+', fami, igau,&
                    1, tmoypg, iretm)
        if (iret .eq. 0 .and. iretm .ne. 0) tmoypg=(tinfpg+tsuppg)/2.d0
!
        somire = iret
        if (somire .eq. 0) then
            if (iret1 .eq. 1) then
                call u2mess('F', 'CALCULEL_31')
            else
!
!  --      LES COEFFICIENTS SUIVANTS RESULTENT DE L'HYPOTHESE SELON
!  --      LAQUELLE LA TEMPERATURE EST PARABOLIQUE DANS L'EPAISSEUR.
!  --      ON NE PREJUGE EN RIEN DE LA NATURE DU MATERIAU.
!  --      CETTE INFORMATION EST CONTENUE DANS LES MATRICES QUI
!  --      SONT LES RESULTATS DE LA ROUTINE DXMATH.
!          ----------------------------------------
                coe1 = (tsuppg+tinfpg+4.d0*tmoypg)/6.d0 - tref
                coe2 = (tsuppg-tinfpg)/epais
!
                sigt(1+8* (igau-1)) = coe1* ( dm(1,1)+dm(1,2)) + coe2* (dmf(1,1)+dmf(1,2) )
                sigt(2+8* (igau-1)) = coe1* ( dm(2,1)+dm(2,2)) + coe2* (dmf(2,1)+dmf(2,2) )
                sigt(3+8* (igau-1)) = coe1* ( dm(3,1)+dm(3,2)) + coe2* (dmf(3,1)+dmf(3,2) )
                sigt(4+8* (igau-1)) = coe2* ( df(1,1)+df(1,2)) + coe1* (dmf(1,1)+dmf(1,2) )
                sigt(5+8* (igau-1)) = coe2* ( df(2,1)+df(2,2)) + coe1* (dmf(2,1)+dmf(2,2) )
                sigt(6+8* (igau-1)) = coe2* ( df(3,1)+df(3,2)) + coe1* (dmf(3,1)+dmf(3,2) )
            endif
        endif
20  end do
!
30  continue
!
end subroutine

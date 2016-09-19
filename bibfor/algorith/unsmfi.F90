subroutine unsmfi(imate, phi, cs, t, tbiot,&
                  aniso, ndim, phenom)
!
    implicit none
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! --- CALCUL DU MODULE DE BIOT CS --------------------------------------
! ======================================================================
#include "asterfort/rcvala.h"
#include "asterfort/utmess.h"
    integer :: nelas2, nelas1, nelas3, nelas4, ndim
    integer :: dim3
    parameter  ( dim3=3 )
    parameter  ( nelas1=2 )
    parameter  ( nelas2=5 )
    parameter  ( nelas3=3 )
    parameter  ( nelas4=7 )
    real(kind=8) :: elas2(nelas2), elas1(nelas1), elas4(nelas4)
    real(kind=8) :: s(6, 6)
    real(kind=8) :: young1, young3, nu12, nu21, nu13, nu31, nu32, nu23, g13
    real(kind=8) :: youngs, nus, biot1, biot3, delta, young2, g12, m33
    real(kind=8) :: val1(dim3), k0, eps, vt(1)
    character(len=8) :: ncra1(nelas1)
    character(len=8) :: ncra2(nelas2)
    character(len=8) :: ncra3(nelas3)
    character(len=8) :: ncra4(nelas4)
    integer :: icodr1(nelas1)
    integer :: icodr2(nelas2)
    integer :: icodr3(nelas3)
    integer :: icodr4(nelas4)
    integer :: imate, aniso, i, j
    real(kind=8) :: phi, cs, t, tbiot(6), kron(6), skron(6)
    real(kind=8) :: m11, m12, m13, ks
    character(len=16) :: phenom
    parameter  ( eps = 1.d-21 )
! =====================================================================
! --- DONNEES POUR RECUPERER LES CARACTERISTIQUES MECANIQUES ----------
! =====================================================================
    data ncra1/'E','NU'/
    data ncra2/'E_L','E_N','NU_LT','NU_LN','G_LN'/
    data ncra3/'BIOT_L','BIOT_N','BIOT_T'/
    data ncra4/'E_L','E_N','E_T','NU_LT','NU_LN','NU_TN',&
     &            'G_LT'/
! =====================================================================
! --- DEFINITION DU SYMBOLE DE KRONECKER UTILE POUR LA SUITE ----------
! =====================================================================
    do i = 1, 3
        kron(i) = 1.d0
    end do
    do i = 4, 6
        kron(i) = 0.d0
    end do
    do i = 1, 6
        skron(i)=0.d0
    end do
! =====================================================================
! --- CALCUL CAS ISOTROPE ---------------------------------------------
! =====================================================================
    if (aniso .eq. 0) then
! =====================================================================
! --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
! =====================================================================
        vt(1)=t
        call rcvala(imate, ' ', 'ELAS', 1, 'TEMP',&
                    vt, 2, ncra1(1), elas1(1), icodr1,&
                    0)
        youngs = elas1(1)
        nus = elas1(2)
        k0 = youngs / 3.d0 / (1.d0-2.d0*nus)
        cs = (tbiot(1)-phi)*(1.0d0-tbiot(1)) / k0
    else 
        if (aniso .eq. 1) then
! =====================================================================
! --- CALCUL CAS ISOTROPE TRANSVERSE ----------------------------------
! =====================================================================
            if (phenom .eq. 'ELAS') then
                vt(1)=t
                call rcvala(imate, ' ', 'ELAS', 1, 'TEMP',&
                            vt, 2, ncra1(1), elas1(1), icodr1,&
                            0)
                young1 = elas1(1)
                young3 = elas1(1)
                nu12 = elas1(2)
                nu13 = elas1(2)
                g13 = young1/(2*(1.d0+nu12))
            else if (phenom.eq.'ELAS_ISTR') then
                vt(1)=t
                call rcvala(imate, ' ', 'ELAS_ISTR', 1, 'TEMP',&
                            vt, 5, ncra2(1), elas2(1), icodr2,&
                            0)
                young1 = elas2(1)
                young3 = elas2(2)
                nu12 = elas2(3)
                nu13 = elas2(4)
                g13 = elas2(5)
            endif
! A rajouter pour l orthotropie
!
            nu31 = nu13*young3/young1
!
            vt(1)=0.d0
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        vt, 2, ncra3(1), val1(1), icodr3,&
                        0)
            biot1 = val1(1)
            biot3 = val1(2)
!
! ON FIXE ARBITRAIREMENT LE PARAMETRE NUS A FIXE 0.3
!
            nus=0.3d0
            m11=young1*(young3-young1*nu13*nu13)/((1.d0+nu12)* (young3-&
        young3*nu12-2.d0*young1*nu13*nu13))
            m12=young1*(young3*nu12*(1.+nu13)+young1*nu13*nu13)/((1.d0+nu12)*&
        (young3-young3*nu12-2.d0*young1*nu13*nu13))
            m13=young1*young3*nu13/(young3-young3*nu12-2.d0* young1*nu13*&
        nu13)
!
            if (abs(1.d0-biot1) .gt. eps) then
                ks=(m11+m12+m13)/(3.d0*(1.d0-biot1))
            else if (abs(1.d0-biot3).gt.eps) then
                m33=young1*young1*(1.d0-nu12)/ (young3-young3*nu12-2.d0*&
            young1*nu13*nu13)
                ks=(2*m13+m33)/(3.d0*(1.d0-biot3))
            else
! MATERIAU INCOMPRESSIBLE
                cs = 0.d0
                goto 999
            endif
!
!
        else if (aniso.eq.2) then
            if (ndim .ne. 2) then
                call utmess('F', 'ALGORITH17_36')
            endif
! =====================================================================
! --- CALCUL CAS ORTHOTROPIE 2D ----------------------------------
! =====================================================================
            if (phenom .eq. 'ELAS') then
                vt(1)=t
                call rcvala(imate, ' ', 'ELAS', 1, 'TEMP',&
                            vt, 2, ncra1(1), elas1(1), icodr1,&
                            0)
                young1 = elas1(1)
                young2 = elas1(1)
                young3 = elas1(1)
                nu12 = elas1(2)
                nu13 = elas1(2)
                nu23 = elas1(2)
                g12 = young1/(2*(1.d0+nu12))
            else if (phenom.eq.'ELAS_ORTH') then
                vt(1)=t        
                call rcvala(imate, ' ', 'ELAS_ORTH', 1, 'TEMP',&
                            vt, 7, ncra4(1), elas4(1), icodr4,&
                            0)
                young1 = elas4(1)
                young3 = elas4(2)
                young2 = elas4(3)
                nu12 = elas4(4)
                nu13 = elas4(5)
                nu23 = elas4(6)
                g12 = elas4(7)
            endif
            vt(1)=0.d0
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        vt, 3, ncra3(1), val1(1), icodr3,&
                        0)
            biot1 = val1(1)
            biot3 = val1(2)
!
! ON FIXE ARBITRAIREMENT LE PARAMETRE NUS A FIXE 0.3
!
            nus=0.3d0
!
            nu21 = nu12*young2/young1
            nu31 = nu13*young3/young1
            nu32 = nu23*young3/young2
            delta = 1.d0-nu23*nu32-nu31*nu13-nu21*nu12-2.d0*nu23*nu31*nu12
!
            m11=(1.d0 - nu23*nu32)*young1/delta
            m12=(nu21 + nu31*nu23)*young1/delta
            m13=(nu31 + nu21*nu32)*young1/delta
!
            if (abs(1.d0-biot1) .gt. eps) then
                ks=(m11+m12+m13)/(3.d0*(1.d0-biot1))
            else if (abs(1.d0-biot3).gt.eps) then
                m33=young1*young1*(1.d0-nu12)/ (young3-young3*nu12-2.d0*&
            young1*nu31*nu31)
                ks=(2*m13+m33)/(3.d0*(1.d0-biot3))
            else
! MATERIAU INCOMPRESSIBLE
                cs = 0.d0
                goto 999
            endif
        endif
!
        youngs=ks*(3.d0*(1.d0-2.d0*nus))
!
! CALCUL DE LA MATRICE DE SOUPLESSE DE LA MATRICE SOLIDE
!
        do i = 1, 6
            do j = 1, 6
                s(i,j)=0.d0
            end do
        end do
!
        s(1,1)=1.d0/youngs
        s(2,2)=1.d0/youngs
        s(3,3)=1.d0/youngs
        s(1,2)=-nus/youngs
        s(1,3)=-nus/youngs
        s(2,1)=-nus/youngs
        s(2,3)=-nus/youngs
        s(3,1)=-nus/youngs
        s(3,2)=-nus/youngs
        s(4,4)=2.d0*(1.d0+nus)/youngs
        s(5,5)=2.d0*(1.d0+nus)/youngs
        s(6,6)=2.d0*(1.d0+nus)/youngs
!
! CALCUL DU MODULE DE BIOT DANS LE CAS ISOTROPE TRANSVERSE
!
        cs=0.d0
        do i = 1, 6
            do j = 1, 6
                skron(i)=skron(i)+s(i,j)*kron(j)
            end do
        end do
        do i = 1, 6
            cs=cs+(tbiot(i)-phi*kron(i))*skron(i)
        end do
    endif
! ======================================================================
999 continue
! ======================================================================
end subroutine

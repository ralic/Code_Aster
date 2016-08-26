subroutine mbxchg(option,fami,nddl,nno,ncomp,kpg, npg,iepsin,itemps,ipoids,igeom,&
                  imate,ipesa,ivectu,icontm,vff,dff,alpha,beta)
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
!
    implicit none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/mbcine.h"
#include "asterfort/mbrigi.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/terefe.h"
#include "asterfort/verift.h"
!
    character(len=16) :: option
    character(len=4) :: fami
    integer :: nddl, nno, ncomp, npg
    integer :: kpg
    integer :: ipoids, igeom, imate, ipesa,iepsin,itemps
    integer :: ivectu, icontm
    real(kind=8) :: dff(2, nno), alpha, beta, vff(nno)
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE DE CHARGEMENT :
!                                  - CHAR_MECA_EPSI_R
!                                  - CHAR_MECA_EPSI_F
!                                  - CHAR_MECA_PESA_R
!                                  - CHAR_MECA_TEMP_R
!                                  - FORC_NODA
!                                  - REFE_FORC_NODA
!                          POUR LES MEMBRANES EN PETITES DEFORMATIONS
! ----------------------------------------------------------------------
! IN  OPTION       OPTION DE CALCUL
! IN  FAMI         NOM DE LA FAMILLE DE POINTS DE GAUSS :
!                  'RIGI','MASS',..
! IN  NDDL         NOMBRE DE DERGES DE LIBERTE AUX NOEUDS
! IN  NNO          NOMBRE DE NOEUDS
! IN  NCOMP        NOMBRE DE COMPOSANTS DANS LES VECTEURS COLONNES
!                  DE CONTRAINTE ET DEFORMATION
! IN  KPG          INCREMENT SUR LA BOUCLE DES PTS DE GAUSS
! IN  NPG          NOMBRE DE POINT DE GAUSS
! IN  IEPSIN       ADRESSE DANS ZR DU TABLEAU PEPSINR
! IN  ITEMPS       ADRESSE DANS ZR DU TABLEAU PTEMPSR
! IN  IPOIDS       ADRESSE DANS ZR DU TABLEAU POIDS
! IN  IGEOM        ADRESSE DANS ZR DU TABLEAU PGEOMER
! IN  IMATE        ADRESSE DANS ZI DU TABLEAU PMATERC
! IN  IPESA        ADRESSE DANS ZR DU TABLEAU PPESANR
! IN  IVECTU       ADRESSE DANS ZR DU TABLEAU PVECTUR
! IN  ICONTM       ADRESSE DANS ZR DU TABLEAU PCONMR
! IN  VFF          VALEURS DES FONCTIONS DE FORME
! IN  DFF          DERIVEE DES F. DE FORME
! IN  ALPHA, BETA  ANGLES NAUTIQUES ORIENTANT LE COMPORTEMENT
!                        ORTHOTROPE DE LA MEMBRANE (EN RADIAN)
!     
! OUT ***          ***
! ----------------------------------------------------------------------
!  
    integer :: i, n, c, cc, ier
    integer :: codres(2)
    real(kind=8) :: b(3, 3, 9), jac
    real(kind=8) :: rig(3, 3), rho(1)
    real(kind=8) :: epsthe, epsref, sgmref, sig(3)
    character(len=8) :: nompar(4)
    real(kind=8) :: valpar(4)
    real(kind=8) :: xgau, ygau, zgau, epsinif(3)
    
!
! --- CALCUL DE LA MATRICE "B" :
!              DEPL NODAL --> DEFORMATIONS MEMBRANAIRES ET JACOBIEN
!
        call mbcine(nno, zr(igeom), dff, alpha, beta,&
                    b, jac)
!
! - BRANCHEMENT DES DIFFERENTES OPTIONS
!
        if ((option.eq.'FORC_NODA') .or. (option.eq.'CHAR_MECA_TEMP_R') .or.&
            (option(1:15).eq.'CHAR_MECA_EPSI_')) then
!
! - FORC_NODA : IL SUFFIT DE RECOPIER SIGMA
!
            if (option .eq. 'FORC_NODA') then
                do c = 1, ncomp
                    sig(c) = zr(icontm+(kpg-1)*ncomp+c-1)
                end do
!
! - CHAR_MECA_EPSI_R : SIG = RIG*EPSIN
!
            else if (option.eq.'CHAR_MECA_EPSI_R') then
!
                call mbrigi(fami, kpg, imate, rig)
!
                call r8inir(3, 0.d0, sig, 1)
                do c = 1, ncomp
                    do cc = 1, ncomp
                        sig(c) = sig(c) + zr(iepsin+cc-1)*rig(cc,c)
                    end do
                end do
!
            else if (option.eq.'CHAR_MECA_EPSI_F') then
!
                call mbrigi(fami, kpg, imate, rig)
!
                call r8inir(3, 0.d0, sig, 1)
                
                nompar(1) = 'X'
                nompar(2) = 'Y'
                nompar(3) = 'Z'
                nompar(4) = 'INST'
                valpar(4) = zr(itemps)
                xgau = 0.d0
                ygau = 0.d0
                zgau = 0.d0
!
                do i = 1, nno
                    xgau = xgau + vff(i)*zr(igeom-1+1+3*(i-1))
                    ygau = ygau + vff(i)*zr(igeom-1+2+3*(i-1))
                    zgau = zgau + vff(i)*zr(igeom-1+3+3*(i-1))
                enddo
!
                valpar(1) = xgau
                valpar(2) = ygau
                valpar(3) = zgau
!
                call fointe('FM', zk8(iepsin), 4, nompar, valpar, epsinif(1), ier)
                call fointe('FM', zk8(iepsin+1), 4, nompar, valpar, epsinif(2), ier)
                call fointe('FM', zk8(iepsin+2), 4, nompar, valpar, epsinif(3), ier)
!
                do c = 1, ncomp
                    do cc = 1, ncomp
                        sig(c) = sig(c) + epsinif(cc)*rig(cc,c)
                    end do
                end do
                
                write(*,*) 'TOTO OOOOOOOOOOOOOOOOOOOOOOOOOOOOO'
!
! - CHAR_MECA_TEMP_R : SIG = RIG*EPSTHE
!
            else if (option.eq.'CHAR_MECA_TEMP_R') then
!
                call verift(fami, kpg, 1, '+', zi(imate),&
                            epsth_=epsthe)
!
                call mbrigi(fami, kpg, imate, rig)
!
                call r8inir(3, 0.d0, sig, 1)
                do c = 1, ncomp
                    sig(c) = epsthe*(rig(1,c)+rig(2,c))
                end do
!
            endif
!
            do n = 1, nno
                do i = 1, nddl
                    do c = 1, ncomp
                        zr(ivectu+(n-1)*nddl+i-1)=zr(ivectu+(n-1)*&
                        nddl+i-1) +b(c,i,n)*sig(c)*zr(ipoids+kpg-1)*&
                        jac
                    end do
                end do
            end do
!
! - REFE_FORC_NODA : ON CALCULE DES FORCES DE REFERENCE
!
        else if (option.eq.'REFE_FORC_NODA') then
!
            call terefe('EPSI_REFE', 'MEMBRANE', epsref)
            if (epsref .eq. r8vide()) then
                ASSERT(.false.)
            endif
!
            call mbrigi(fami, kpg, imate, rig)
!
!         ON CALCULE UN ORDRE DE GRANDEUR DE LA CONTRAINTE MEMBRANAIRE
            sgmref = epsref*(rig(1,1) + rig(2,2))/2.d0
            ASSERT(sgmref.gt.0.d0)
!
            do n = 1, nno
                do i = 1, nddl
                    zr(ivectu+(n-1)*nddl+i-1) = zr(ivectu+(n-1)*nddl+ i-1) + sgmref*sqrt(abs(jac)&
                                                )/npg
                end do
            end do
!
! - CHAR_MECA_PESA_R
!
        else if (option.eq.'CHAR_MECA_PESA_R') then
            call rcvalb(fami, kpg, 1, '+', zi(imate),&
                        ' ', 'ELAS_MEMBRANE', 0, ' ', [0.d0],&
                        1, 'RHO', rho, codres, 1)
            do n = 1, nno
                do i = 1, nddl
                    zr(ivectu+(n-1)*nddl+i-1) = zr(&
                                                ivectu+(n-1)*nddl+ i-1) + rho(1)*zr(ipesa)* zr(ip&
                                                &esa+i) *vff(n)*zr( ipoids+kpg-1&
                                                )*jac
                end do
            end do
        endif
        
end subroutine

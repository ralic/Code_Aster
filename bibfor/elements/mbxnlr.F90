subroutine mbxnlr(option,fami,nddl,nno,ncomp,kpg,ipoids,igeom,&
                  imate,ideplm,ideplp,ivectu,icontp,&
                  imatuu,dff,alpha,beta,&
                  vecteu,matric)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/mbcine.h"
#include "asterfort/mbrigi.h"
#include "asterfort/r8inir.h"
#include "asterfort/verift.h"
!
    character(len=16) :: option
    character(len=4) :: fami
    integer :: nddl, nno, ncomp
    integer :: kpg
    integer :: ipoids, igeom, imate, ideplm, ideplp
    integer :: ivectu, icontp, imatuu
    real(kind=8) :: dff(2, nno), alpha, beta
    aster_logical :: vecteu, matric
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE COMPORTEMENT
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
! IN  IPOIDS       ADRESSE DANS ZR DU TABLEAU POIDS
! IN  IGEOM        ADRESSE DANS ZR DU TABLEAU PGEOMER
! IN  IMATE        ADRESSE DANS ZI DU TABLEAU PMATERC
! IN  IDEPLM       ADRESSE DANS ZR DU TABLEAU PDEPLMR
! IN  IDEPLP       ADRESSE DANS ZR DU TABLEAU PDEPLPR
! IN  IVECTU       ADRESSE DANS ZR DU TABLEAU PVECTUR
! IN  ICONTP       ADRESSE DANS ZR DU TABLEAU PCONTPR
! IN  IMATUU       ADRESSE DANS ZR DU TABLEAU PMATUUR
! IN  DFF          DERIVEE DES F. DE FORME
! IN  ALPHA, BETA  ANGLES NAUTIQUES ORIENTANT LE COMPORTEMENT
!                        ORTHOTROPE DE LA MEMBRANE (EN RADIAN)
! IN  VECTEU       BOOL: 1 SI FULL_MECA OU RAPH_MECA
! IN  MATRIC       BOOL: 1 SI FULL_MECA OU RIGI_MECA
!     
! OUT ***          ***
! ----------------------------------------------------------------------
!  
    integer :: i, j, j1, n, m, c, cc, kk, kkd
    real(kind=8) :: b(3, 3, 9), jac
    real(kind=8) :: epsm(3), deps(3), epsth(3), epsthe, sigp(3), tmp, rig(3, 3)
!
! - CALCUL DE LA MATRICE "B" :
!   DEPL NODAL --> DEFORMATIONS MEMBRANAIRES ET JACOBIEN
!
    call mbcine(nno, zr(igeom), dff, alpha, beta,&
                b, jac)
!
! - RIGI_MECA : ON DONNE LA RIGIDITE ELASTIQUE
!
    if (option .eq. 'RIGI_MECA') then
!
        call mbrigi(fami, kpg, imate, rig)
!
! - RAPH_MECA, FULL_MECA*, RIGI_MECA_* : ON PASSE PAR LA LDC 1D
!
    elseif ((option .eq.'RAPH_MECA').or. (option(1:9)&
    .eq.'FULL_MECA').or. (option(1:10).eq.'RIGI_MECA_')) then
!
! ---   CALCUL DE LA DEFORMATION MEMBRANAIRE DANS LE REPERE LOCAL
        call r8inir(3, 0.d0, epsm, 1)
        call r8inir(3, 0.d0, deps, 1)
        do n = 1, nno
            do i = 1, nddl
                do c = 1, ncomp
                    epsm(c)=epsm(c)+b(c,i,n)*zr(ideplm+(n-1)*nddl+i-1)
                    deps(c)=deps(c)+b(c,i,n)*zr(ideplp+(n-1)*nddl+i-1)
                end do
            end do
        end do
!
        call verift(fami, kpg, 1, '+', zi(imate),&
                    epsth_=epsthe)
        call r8inir(3, 0.d0, epsth, 1)
        epsth(1) = epsthe
        epsth(2) = epsthe
!
        call mbrigi(fami, kpg, imate, rig)
!
        call r8inir(3, 0.d0, sigp, 1)
        do c = 1, ncomp
            do cc = 1, ncomp
                sigp(c) = sigp(c) + (epsm(cc)+deps(cc)-epsth(cc)) *rig(cc,c)
            end do
        end do
        
!
        if ((option .eq.'RAPH_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
            do c = 1, ncomp
                zr(icontp+(kpg-1)*ncomp+c-1)=sigp(c)
            end do
        endif
!
    endif
!
! - RANGEMENT DES RESULTATS
!
    if (vecteu) then
        do n = 1, nno
            do i = 1, nddl
                do c = 1, ncomp
                    zr(ivectu+(n-1)*nddl+i-1)=zr(ivectu+(n-1)*&
                    nddl+i-1) +b(c,i,n)*sigp(c)*zr(ipoids+kpg-1)*&
                    jac
                end do
            end do
        end do
    endif
!
    if (matric) then
        do n = 1, nno
            do i = 1, nddl
                kkd = (nddl*(n-1)+i-1) * (nddl*(n-1)+i) /2
                do j = 1, nddl
                    do m = 1, n
                        if (m .eq. n) then
                            j1 = i
                        else
                            j1 = nddl
                        endif
!
! ---------------       RIGIDITE ELASTIQUE
                        tmp = 0.d0
                        do c = 1, ncomp
                            do cc = 1, ncomp
                                tmp = tmp + b(cc,i,n)*rig(cc,c)*b( c,j,m) *zr(ipoids+kpg-1)*j&
                                      &ac
                            end do
                        end do
!
! ---------------       STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                        if (j .le. j1) then
                            kk = kkd + nddl*(m-1)+j
                            zr(imatuu+kk-1) = zr(imatuu+kk-1) + tmp
                        endif
                    end do
                end do
            end do
        end do        
    endif
    
        
end subroutine

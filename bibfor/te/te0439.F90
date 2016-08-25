subroutine te0439(option, nomte)
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
! aslint: disable=W0104
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/mbcine.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/vecma.h"
#include "asterfort/lteatt.h"
#include "blas/ddot.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DE L'OPTION MASS_MECA
!                          POUR LES MEMBRANES EN DYNAMIQUE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: codres(2)
    character(len=4) :: fami
    integer :: nno, npg, i, imatuu, ndim, nnos, jgano
    integer :: ipoids, ivf, idfde, igeom, imate, icacoq, icompo
    integer :: kpg, n, j, kkd, k
    integer :: kk, nddl, l
    real(kind=8) :: dff(2, 9)
    real(kind=8) :: vff(9), b(3, 3, 9), jac, rho(1)
    real(kind=8) :: alpha, beta, h
    real(kind=8) :: a(3, 3, 9, 9), coef
    real(kind=8) :: diag(3, 9), wgt, alfam(3), somme(3)
    aster_logical :: ldiag, grdef
!
!
    call jevech('PCOMPOR', 'L', icompo)
    
    grdef = (zk16 ( icompo + 2 )(1:9) .eq. 'GROT_GDEP')

    ldiag = (option(1:10).eq.'MASS_MECA_')
!
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
    fami = 'MASS'
    call elrefe_info(fami=fami, ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
    call r8inir(9*9*3*3, 0.d0, a, 1)
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCACOQU', 'L', icacoq)
!
! PARAMETRES EN SORTIE
!
    call jevech('PMATUUR', 'E', imatuu)
!
    nddl = 3
!
! - DIRECTION DE REFERENCE POUR UN COMPORTEMENT ANISOTROPE
!
    alpha = zr(icacoq+1) * r8dgrd()
    beta = zr(icacoq+2) * r8dgrd()
    
! - EPAISSEUR (VALABLE UNIQUEMENT POUR GROT_GDEP)
!
     
    
    if (grdef) then
        h = zr(icacoq)
    else
        h = 1.d0
    endif
    
!
! - CALCUL POUR CHAQUE POINT DE GAUSS : ON CALCULE D'ABORD LA
!      CONTRAINTE ET/OU LA RIGIDITE SI NECESSAIRE PUIS
!      ON JOUE AVEC B
!
    wgt = 0.d0
    do kpg = 1, npg
!
! - MISE SOUS FORME DE TABLEAU DES VALEURS DES FONCTIONS DE FORME
!   ET DES DERIVEES DE FONCTION DE FORME
!
        do n = 1, nno
            vff(n) =zr(ivf+(kpg-1)*nno+n-1)
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
        enddo
!
! - MASS_MECA
!

    if (grdef) then
        call rcvalb(fami, kpg, 1, '+', zi(imate),&
               ' ', 'ELAS', 0, ' ', [0.d0],&
               1, 'RHO', rho, codres, 1)
    else
        call rcvalb(fami, kpg, 1, '+', zi(imate),&
               ' ', 'ELAS_MEMBRANE', 0, ' ', [0.d0],&
               1, 'RHO', rho, codres, 1)
    endif
        
!
! - CALCUL DE LA MATRICE "B" : DEPL NODAL -> EPS11 ET DU JACOBIEN
!
        call mbcine(nno, zr(igeom), dff, alpha, beta,&
                    b, jac)
!
        wgt = wgt + rho(1)*zr(ipoids+kpg-1)*jac*h
!
        do n = 1, nno
            do i = 1, n
                coef = rho(1)*zr(ipoids+kpg-1)*jac*vff(n)*vff(i)*h
                a(1,1,n,i) = a(1,1,n,i) + coef
                a(2,2,n,i) = a(2,2,n,i) + coef
                a(3,3,n,i) = a(3,3,n,i) + coef
            enddo
        enddo
!
    end do
!
! - RANGEMENT DES RESULTATS
! -------------------------
    if (ldiag) then
!
!-- CALCUL DE LA TRACE EN TRANSLATION SUIVANT X
!
        call r8inir(3*9, 0.d0, diag, 1)
        call r8inir(3, 0.d0, somme, 1)
        do i = 1, 3
            do j = 1, nno
                somme(i) = somme(i) + a(i,i,j,j)
            enddo
            alfam(i) = wgt/somme(i)
        enddo
!
!-- CALCUL DU FACTEUR DE DIAGONALISATION
!
!        ALFA = WGT/TRACE
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
        do j = 1, nno
            do i = 1, 3
                diag(i,j) = a(i,i,j,j)*alfam(i)
            enddo
        enddo
!
        a(:,:,:,:) = 0.d0
        do k = 1, 3
            do i = 1, nno
                a(k,k,i,i) = diag(k,i)
            enddo
        enddo
    endif
!
!
    do k = 1, nddl
        do l = 1, nddl
            do i = 1, nno
                kkd = ((nddl*(i-1)+k-1)* (nddl*(i-1)+k))/2
                do j = 1, i
                    kk = kkd + nddl * (j-1) + l
                    zr(imatuu+kk-1) = a(k,l,i,j)
                enddo
            enddo
        enddo
    enddo
!
end subroutine

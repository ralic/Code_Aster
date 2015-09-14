subroutine te0435(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/codere.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/mbcine.h"
#include "asterfort/mbrigi.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/verift.h"
#include "blas/dcopy.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE COMPORTEMENT :
!                                  - FULL_MECA
!                                  - FULL_MECA_ELAS
!                                  - RAPH_MECA
!                                  - RIGI_MECA
!                                  - RIGI_MECA_ELAS
!                                  - RIGI_MECA_TANG
!                                  - RIGI_MECA_IMPLEX
!                          POUR LES MEMBRANES
!    - ARGUMENTS :
!        DONNEES :      OPTION       -->  OPTION DE CALCUL
!                       NOMTE        -->  NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    character(len=4) :: fami
    integer :: nddl, nno, nnos, npg, ndim, ncomp, nvari
    integer :: i, j, j1, n, m, c, cc, kpg, kk, kkd, iret, cod(9)
    integer :: ipoids, ivf, idfde, jgano, jtab(7)
    integer :: igeom, icacoq, imate, icompo, icarcr
    integer :: iinstm, iinstp, icontm, ideplm, ideplp, ivarim, ivarix
    integer :: ivectu, icontp, ivarip, jcret, imatuu, icontx
    real(kind=8) :: dff(2, 8), alpha, beta, b(3, 3, 8), jac
    real(kind=8) :: epsm(3), deps(3), epsth(3), epsthe, sigp(3), tmp, rig(3, 3)
    aster_logical :: vecteu, matric
!
! - BOOLEEN UTILES
!
    vecteu = ((option(1:9).eq.'FULL_MECA').or. (option .eq.'RAPH_MECA'))
    matric = ((option(1:9).eq.'FULL_MECA').or. (option(1:9).eq.'RIGI_MECA'))
!
! - NOMBRE DE COMPOSANTES DES TENSEURS
!
    ncomp = 3
    nddl = 3
!
! - FONCTIONS DE FORME ET POINTS DE GAUSS
!
    fami = 'RIGI'
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCACOQU', 'L', icacoq)
!
    if (option .eq. 'RIGI_MECA') then
        call jevech('PMATERC', 'L', imate)
!
        elseif ((option(1:9) .eq.'FULL_MECA').or. (option .eq.'RAPH_MECA')&
    .or. (option(1:10).eq.'RIGI_MECA_')) then
        call jevech('PMATERC', 'L', imate)
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PCARCRI', 'L', icarcr)
!
        call jevech('PINSTMR', 'L', iinstm)
        call jevech('PINSTPR', 'L', iinstp)
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PDEPLPR', 'L', ideplp)
        call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                    itab=jtab)
        nvari = max(jtab(6),1)*jtab(7)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PVARIMP', 'L', ivarix)
    endif
!
! - PARAMETRES EN SORTIE
!
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        call jevech('PCODRET', 'E', jcret)
!       ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
        call dcopy(npg*nvari, zr(ivarix), 1, zr(ivarip), 1)
    endif
!
    if ((option(1:4).eq.'FULL') .or. (option(1:4).eq.'RIGI')) then
        call jevech('PMATUUR', 'E', imatuu)
    endif
!
! - PARAMETRES EN SORTIE SUPPLEMENTAIE POUR LA METHODE IMPLEX
    if (option .eq. 'RIGI_MECA_IMPLEX') then
        call jevech('PCONTXR', 'E', icontx)
! ------ INITIALISATION DE LA CONTRAINTE INTERPOLE CONTX=CONTM
        call dcopy(npg*ncomp, zr(icontm), 1, zr(icontx), 1)
    endif
!
! - INITIALISATION CODES RETOURS
!
    do kpg = 1, npg
        cod(kpg)=0
    end do
!
! - DIRECTION DE REFERENCE POUR UN COMPORTEMENT ANISOTROPE
!
    alpha = zr(icacoq) * r8dgrd()
    beta = zr(icacoq+1) * r8dgrd()
!
! - DEBUT DE LA BOUCLE SUR LES POINTS DE GAUSS
!
    do kpg = 1, npg
!
! --- MISE SOUS FORME DE TABLEAU DES VALEURS ET DES DERIVEES
!     DES FONCTIONS DE FORME
!
        do n = 1, nno
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
        end do
!
! --- CALCUL DE LA MATRICE "B" :
!              DEPL NODAL --> DEFORMATIONS MEMBRANAIRES ET JACOBIEN
!
        call mbcine(nno, zr(igeom), dff, alpha, beta,&
                    b, jac)
!
! --- RIGI_MECA : ON DONNE LA RIGIDITE ELASTIQUE
!
        if (option .eq. 'RIGI_MECA') then
!
            call mbrigi(fami, kpg, imate, rig)
!
! --- RAPH_MECA, FULL_MECA*, RIGI_MECA_* : ON PASSE PAR LA LDC 1D
!
            elseif ((option .eq.'RAPH_MECA').or. (option(1:9)&
        .eq.'FULL_MECA').or. (option(1:10).eq.'RIGI_MECA_')) then
!
!         CALCUL DE LA DEFORMATION MEMBRANAIRE DANS LE REPERE LOCAL
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
! --- RANGEMENT DES RESULTATS
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
!                 RIGIDITE ELASTIQUE
                            tmp = 0.d0
                            do c = 1, ncomp
                                do cc = 1, ncomp
                                    tmp = tmp + b(cc,i,n)*rig(cc,c)*b( c,j,m) *zr(ipoids+kpg-1)*j&
                                          &ac
                                end do
                            end do
!
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (j .le. j1) then
                                kk = kkd + nddl*(m-1)+j
                                zr(imatuu+kk-1) = zr(imatuu+kk-1) + tmp
                            endif
                        end do
                    end do
                end do
            end do
        endif
!
! - FIN DE LA BOUCLE SUR LES POINTS DE GAUSS
    end do
!
    if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9).eq.'RAPH_MECA')) then
        call codere(cod, npg, zi(jcret))
    endif
!
end subroutine

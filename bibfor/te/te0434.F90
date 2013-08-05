subroutine te0434(option, nomte)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W0104
    implicit none
#include "jeveux.h"
!
#include "asterc/r8dgrd.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/mbcine.h"
#include "asterfort/mbrigi.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/terefe.h"
#include "asterfort/verift.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE CHARGEMENT :
!                                  - CHAR_MECA_EPSI_R
!                                  - CHAR_MECA_PESA_R
!                                  - CHAR_MECA_TEMP_R
!                                  - FORC_NODA
!                                  - REFE_FORC_NODA
!                          POUR LES MEMBRANES
!    - ARGUMENTS :
!        DONNEES :      OPTION       -->  OPTION DE CALCUL
!                       NOMTE        -->  NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    integer :: codres(2)
    character(len=4) :: fami
    character(len=8) :: materi
    integer :: nddl, nno, nnos, npg, ndim, ncomp
    integer :: i, n, kpg, c, cc, iret
    integer :: ipoids, ivf, idfde, jgano
    integer :: igeom, icacoq, imate, icontm, ipesa, iepsin, ivectu
    real(kind=8) :: dff(2, 8), vff(8), b(3, 3, 8), jac
    real(kind=8) :: alpha, beta, rho, rig(3, 3)
    real(kind=8) :: epsthe, epsref, sgmref, sig(3)
!
    materi = ' '
!
! - NOMBRE DE COMPOSANTES DES TENSEURS
!
    ncomp = 3
    nddl = 3
!
! - FONCTIONS DE FORME ET POINTS DE GAUSS
!
    fami = 'RIGI'
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCACOQU', 'L', icacoq)
!
    if (option .eq. 'FORC_NODA') then
        call jevech('PCONTMR', 'L', icontm)
!
    else if (option.eq.'REFE_FORC_NODA') then
        call jevech('PMATERC', 'L', imate)
!
    else if (option.eq.'CHAR_MECA_EPSI_R') then
        call jevech('PMATERC', 'L', imate)
        call jevech('PEPSINR', 'L', iepsin)
!
    else if (option.eq.'CHAR_MECA_PESA_R') then
        call jevech('PMATERC', 'L', imate)
        call jevech('PPESANR', 'L', ipesa)
!
    else if (option.eq.'CHAR_MECA_TEMP_R') then
        call jevech('PMATERC', 'L', imate)
!
    endif
!
! - PARAMETRES EN SORTIE
!
    call jevech('PVECTUR', 'E', ivectu)
!
! - DIRECTION DE REFERENCE POUR UN COMPORTEMENT ANISOTROPE
!
    alpha = zr(icacoq) * r8dgrd()
    beta = zr(icacoq+1) * r8dgrd()
!
! - DEBUT DE LA BOUCLE SUR LES POINTS DE GAUSS
!
    do 800 kpg = 1, npg
!
! --- MISE SOUS FORME DE TABLEAU DES VALEURS ET DES DERIVEES
!     DES FONCTIONS DE FORME
!
        do 110 n = 1, nno
            vff(n) =zr(ivf+(kpg-1)*nno+n-1)
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
110      continue
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
            (option.eq.'CHAR_MECA_EPSI_R')) then
!
! - FORC_NODA : IL SUFFIT DE RECOPIER SIGMA
!
            if (option .eq. 'FORC_NODA') then
                do 120 c = 1, ncomp
                    sig(c) = zr(icontm+(kpg-1)*ncomp+c-1)
120              continue
!
! - CHAR_MECA_EPSI_R : SIG = RIG*EPSIN
!
            else if (option.eq.'CHAR_MECA_EPSI_R') then
!
                call mbrigi(fami, kpg, imate, rig)
!
                call r8inir(3, 0.d0, sig, 1)
                do 130 c = 1, ncomp
                    do 130 cc = 1, ncomp
                        sig(c) = sig(c) + zr(iepsin+cc-1)*rig(cc,c)
130                  continue
!
! - CHAR_MECA_TEMP_R : SIG = RIG*EPSTHE
!
            else if (option.eq.'CHAR_MECA_TEMP_R') then
!
                call verift(fami, kpg, 1, '+', zi(imate),&
                            materi, 'ELAS_MEMBRANE', 1, epsthe, iret)
!
                call mbrigi(fami, kpg, imate, rig)
!
                call r8inir(3, 0.d0, sig, 1)
                do 140 c = 1, ncomp
                    sig(c) = epsthe*(rig(1,c)+rig(2,c))
140              continue
!
            endif
!
            do 150 n = 1, nno
                do 150 i = 1, nddl
                    do 150 c = 1, ncomp
                        zr(ivectu+(n-1)*nddl+i-1)=zr(ivectu+(n-1)*&
                        nddl+i-1) +b(c,i,n)*sig(c)*zr(ipoids+kpg-1)*&
                        jac
150                  continue
!
! - REFE_FORC_NODA : ON CALCULE DES FORCES DE REFERENCE
!
        else if (option.eq.'REFE_FORC_NODA') then
!
            call terefe('EPSI_REFE', 'MEMBRANE', epsref)
            if (epsref .eq. r8vide()) ASSERT(.false.)
!
            call mbrigi(fami, kpg, imate, rig)
!
!         ON CALCULE UN ORDRE DE GRANDEUR DE LA CONTRAINTE MEMBRANAIRE
            sgmref = epsref*(rig(1,1) + rig(2,2))/2.d0
            ASSERT(sgmref.gt.0.d0)
!
            do 200 n = 1, nno
                do 200 i = 1, nddl
                    zr(ivectu+(n-1)*nddl+i-1) = zr(ivectu+(n-1)*nddl+ i-1) + sgmref*sqrt(abs(jac)&
                                                )/npg
200              continue
!
! - CHAR_MECA_PESA_R
!
        else if (option.eq.'CHAR_MECA_PESA_R') then
            call rcvalb(fami, kpg, 1, '+', zi(imate),&
                        ' ', 'ELAS_MEMBRANE', 0, ' ', 0.d0,&
                        1, 'RHO', rho, codres, 1)
            do 300 n = 1, nno
                do 300 i = 1, nddl
                    zr(ivectu+(n-1)*nddl+i-1) = zr(&
                                                ivectu+(n-1)*nddl+ i-1) + rho*zr(ipesa)*zr(ipesa+&
                                                &i) *vff(n)*zr( ipoids+kpg-1&
                                                )*jac
300              continue
        endif
!
! - FIN DE LA BOUCLE SUR LES POINTS DE GAUSS
800  end do
!
end subroutine

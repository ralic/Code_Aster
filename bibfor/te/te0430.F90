subroutine te0430(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cargri.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/nmgrib.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecael.h"
#include "asterfort/terefe.h"
#include "asterfort/utmess.h"
#include "asterfort/lteatt.h"
#include "asterfort/verift.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE CHARGEMENT :
!                                  - CHAR_MECA_EPSI_R
!                                  - CHAR_MECA_PESA_R
!                                  - CHAR_MECA_TEMP_R
!                                  - FORC_NODA
!                                  - REFE_FORC_NODA
!                          POUR LES GRILLES MEMBRANES EXCENTREES OU NON
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: codres(2)
    character(len=4) :: fami
    character(len=8) :: nomres(2), materi
    integer :: nddl, nno, npg, i, kpg, n, ndim, nnos, jgano
    integer :: ipoids, ivf, idfde, igeom, imate, icontm, ivectu, iret
    integer :: ipesa, iepsin, iadzi, iazk24
    real(kind=8) :: dff(2, 8), vff(8), b(6, 8), p(3, 6), jac, epsthe, epsref
    real(kind=8) :: dir11(3), densit, pgl(3, 3), distn, vecn(3)
    real(kind=8) :: sig, rho(1), valres(2)
    logical :: lexc
!
! - BOOLEEN POUR LES GRILLES EXCENTREES
!
    lexc = (lteatt('CODMOD','GRC'))
    materi = ' '
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
!
    fami = 'RIGI'
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
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
! - LECTURE DES CARACTERISTIQUES DE GRILLE ET
!   CALCUL DE LA DIRECTION D'ARMATURE
!
    call cargri(lexc, densit, distn, dir11)
!
! - SI EXCENTREE : RECUPERATION DE LA NORMALE ET DE L'EXCENTREMENT
!
    if (lexc) then
!
        if (nomte .eq. 'MEGCTR3') then
            call dxtpgl(zr(igeom), pgl)
        else if (nomte.eq.'MEGCQU4') then
            call dxqpgl(zr(igeom), pgl, 'S', iret)
        endif
!
        do 8 i = 1, 3
            vecn(i)=distn*pgl(3,i)
  8     continue
        nddl=6
!
    else
        nddl = 3
    endif
!
! - DEBUT DE LA BOUCLE SUR LES POINTS DE GAUSS
!
    do 800 kpg = 1, npg
!
! - MISE SOUS FORME DE TABLEAU DES VALEURS DES FONCTIONS DE FORME
!   ET DES DERIVEES DE FONCTION DE FORME
!
        do 11 n = 1, nno
            vff(n) =zr(ivf+(kpg-1)*nno+n-1)
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
 11     continue
!
! - CALCUL DE LA MATRICE "B" : DEPL NODAL --> EPS11 ET DU JACOBIEN
!
        call nmgrib(nno, zr(igeom), dff, dir11, lexc,&
                    vecn, b, jac, p)
!
! - BRANCHEMENT DES DIFFERENTES OPTIONS
!
        if ((option.eq.'FORC_NODA') .or. (option.eq.'CHAR_MECA_TEMP_R') .or.&
            (option.eq.'CHAR_MECA_EPSI_R')) then
!
! - FORC_NODA : IL SUFFIT DE RECOPIER SIGMA
!
            if (option .eq. 'FORC_NODA') then
                sig = zr(icontm+kpg-1)
!
! - CHAR_MECA_EPSI_R : SIG = E*EPSIN
!
            else if (option.eq.'CHAR_MECA_EPSI_R') then
                nomres(1) = 'E'
                call rcvalb(fami, kpg, 1, '+', zi(imate),&
                            ' ', 'ELAS', 0, ' ', [0.d0],&
                            1, nomres, valres, codres, 1)
                sig=valres(1)*zr(iepsin)
!
! - CHAR_MECA_TEMP_R : SIG = E*EPSTHE
!
            else if (option.eq.'CHAR_MECA_TEMP_R') then
                call verift(fami, kpg, 1, '+', zi(imate),&
                            materi, 'ELAS', iret, epsth=epsthe)
                if (iret .ne. 0) then
                    call tecael(iadzi, iazk24)
                    call utmess('F', 'CALCULEL2_81', sk=zk24(iazk24-1+3))
                endif
                nomres(1) = 'E'
                call rcvalb(fami, kpg, 1, '+', zi(imate),&
                            ' ', 'ELAS', 0, ' ', [0.d0],&
                            1, nomres, valres, codres, 1)
                sig=valres(1)*epsthe
            endif
!
            do 100 n = 1, nno
                do 100 i = 1, nddl
                    zr(ivectu+(n-1)*nddl+i-1)=zr(ivectu+(n-1)*nddl+i-&
                    1) +b(i,n)*sig*zr(ipoids+kpg-1)*jac*densit
100             continue
!
! - REFE_FORC_NODA : ON CALCULE DES FORCES DE REFERENCE
!      (N'EST VALABLE QUE POUR LES GRILLES MEMBRANES)
!
        else if (option.eq.'REFE_FORC_NODA') then
!
            call terefe('EPSI_REFE', 'GRILLE', epsref)
            if (epsref .eq. r8vide()) ASSERT(.false.)
            if (lexc) ASSERT(.false.)
!
            nomres(1) = 'E'
            call rcvalb(fami, kpg, 1, '+', zi(imate),&
                        ' ', 'ELAS', 0, ' ', [0.d0],&
                        1, nomres, valres, codres, 1)
            sig=valres(1)*epsref
!
            do 110 n = 1, nno
                do 110 i = 1, nddl
                    zr(ivectu+(n-1)*nddl+i-1) = zr(ivectu+(n-1)*nddl+ i-1) + sig*sqrt(abs(jac)&
                                                )*densit/npg
110             continue
!
! - CHAR_MECA_PESA_R
!
        else if (option.eq.'CHAR_MECA_PESA_R') then
            call rcvalb(fami, kpg, 1, '+', zi(imate),&
                        ' ', 'ELAS', 0, ' ', [0.d0],&
                        1, 'RHO', rho, codres, 1)
            do 130 n = 1, nno
                do 130 i = 1, 3
                    zr(ivectu+(n-1)*nddl+i-1)=zr(ivectu+(n-1)*nddl+i-&
                    1)+ rho(1)*zr(ipoids+kpg-1)*zr(ipesa)*zr(ipesa+i)*&
                    vff(n)*densit*jac
130             continue
        endif
!
! - FIN DE LA BOUCLE SUR LES POINTS DE GAUSS
800 end do
!
end subroutine

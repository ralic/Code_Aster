subroutine te0431(option, nomte)
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
#include "asterc/r8dgrd.h"
#include "asterc/r8nnem.h"
#include "asterfort/cargri.h"
#include "asterfort/codere.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/nmco1d.h"
#include "asterfort/nmgrib.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "blas/dcopy.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE COMPORTEMENT :
!                                  - FULL_MECA
!                                  - FULL_MECA_ELAS
!                                  - RAPH_MECA
!                                  - RIGI_MECA
!                                  - RIGI_MECA_ELAS
!                                  - RIGI_MECA_TANG
!                          POUR LES GRILLES MEMBRANES EXCENTREES OU NON
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: codres(2)
    character(len=4) :: fami
    character(len=8) :: nomres(2)
    integer :: nddl, nno, nnos, npg, ndim, i, j, j1, n, m, kpg, kk, kkd, lgpg
    integer :: cod(9)
    integer :: imatuu, ipoids, ivf, idfde, igeom, imate, icamas, icontm, ivarim
    integer :: jgano, jtab(7), jcret, ideplm, ideplp, icompo, icarcr, iret
    integer :: ivectu, icontp, ivarip, ivarix
    real(kind=8) :: dff(2, 8), b(6, 8), p(3, 6), jac
    real(kind=8) :: dir11(3), densit, pgl(3, 3), distn, vecn(3)
    real(kind=8) :: epsm, deps, sigm, sig, tmp, rig, valres(2)
    real(kind=8) :: angmas(3)
    logical :: vecteu, matric, lexc
!
! - BOOLEEN UTILES
!
    vecteu = ((option(1:9).eq.'FULL_MECA').or. (option .eq.'RAPH_MECA'))
    matric = ((option(1:9).eq.'FULL_MECA').or. (option(1:9).eq.'RIGI_MECA'))
    lexc = (nomte(1:4).eq.'MEGC')
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
!
    fami = 'RIGI'
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
!
    if (option .eq. 'RIGI_MECA') then
        call jevech('PMATERC', 'L', imate)
!
        elseif ((option(1:9) .eq.'FULL_MECA').or. (option .eq.'RAPH_MECA')&
    .or. (option(1:10).eq.'RIGI_MECA_')) then
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PCARCRI', 'L', icarcr)
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PDEPLPR', 'L', ideplp)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PMATERC', 'L', imate)
        call tecach('OON', 'PVARIMR', 'L', 7, jtab,&
                    iret)
        lgpg = max(jtab(6),1)*jtab(7)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PVARIMP', 'L', ivarix)
!
! --- ORIENTATION DU MASSIF
!
        call tecach('NNN', 'PCAMASS', 'L', 1, icamas,&
                    iret)
        call r8inir(3, r8nnem(), angmas, 1)
        if (icamas .gt. 0) then
            if (zr(icamas) .gt. 0.d0) then
                angmas(1) = zr(icamas+1)*r8dgrd()
                angmas(2) = zr(icamas+2)*r8dgrd()
                angmas(3) = zr(icamas+3)*r8dgrd()
            endif
        endif
    endif
!
! - PARAMETRES EN SORTIE
!
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        call jevech('PCODRET', 'E', jcret)
!
! --- ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
!
        call dcopy(npg*lgpg, zr(ivarix), 1, zr(ivarip), 1)
    else
        ivarip=1
    endif
!
    if ((option(1:4).eq.'FULL') .or. (option(1:4).eq.'RIGI')) then
        call jevech('PMATUUR', 'E', imatuu)
    endif
!
! - INITIALISATION CODES RETOURS
!
    do 1955 kpg = 1, npg
        cod(kpg)=0
1955  end do
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
 8      continue
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
! --- MISE SOUS FORME DE TABLEAU DES VALEURS DES FONCTIONS DE FORME
!     ET DES DERIVEES DE FONCTION DE FORME
!
        do 11 n = 1, nno
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
11      continue
!
! --- CALCUL DE LA MATRICE "B" : DEPL NODAL --> EPS11 ET DU JACOBIEN
!
        call nmgrib(nno, zr(igeom), dff, dir11, lexc,&
                    vecn, b, jac, p)
!
! --- RIGI_MECA : ON DONNE LA RIGIDITE ELASTIQUE
!
        if (option .eq. 'RIGI_MECA') then
            nomres(1) = 'E'
            call rcvalb(fami, kpg, 1, '+', zi(imate),&
                        ' ', 'ELAS', 0, ' ', [0.d0],&
                        1, nomres, valres, codres, 1)
            rig=valres(1)
!
! --- RAPH_MECA, FULL_MECA*, RIGI_MECA_* : ON PASSE PAR LA LDC 1D
!
            elseif ((option .eq.'RAPH_MECA').or. (option(1:9)&
        .eq.'FULL_MECA').or. (option(1:10).eq.'RIGI_MECA_')) then
            sigm = zr(icontm+kpg-1)
!
!         CALCUL DE LA DEFORMATION DEPS11
            epsm=0.d0
            deps=0.d0
            do 20 i = 1, nno
                do 20 j = 1, nddl
                    epsm=epsm+b(j,i)*zr(ideplm+(i-1)*nddl+j-1)
                    deps=deps+b(j,i)*zr(ideplp+(i-1)*nddl+j-1)
20              continue
!
            call nmco1d(fami, kpg, 1, zi(imate), zk16(icompo),&
                        option, epsm, deps, angmas, sigm,&
                        zr(ivarim+(kpg-1)*lgpg), sig, zr( ivarip+(kpg-1)*lgpg), rig, cod(kpg))
!
            if ((option .eq.'RAPH_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
                zr(icontp+kpg-1)=sig
            endif
!
        endif
!
! --- RANGEMENT DES RESULTATS
!
        if (vecteu) then
            do 100 n = 1, nno
                do 100 i = 1, nddl
                    zr(ivectu+(n-1)*nddl+i-1)=zr(ivectu+(n-1)*nddl+i-&
                    1) +b(i,n)*sig*zr(ipoids+kpg-1)*jac*densit
100              continue
        endif
!
        if (matric) then
            do 200 n = 1, nno
                do 200 i = 1, nddl
                    kkd = (nddl*(n-1)+i-1) * (nddl*(n-1)+i) /2
                    do 200 j = 1, nddl
                        do 200 m = 1, n
                            if (m .eq. n) then
                                j1 = i
                            else
                                j1 = nddl
                            endif
!
!                 RIGIDITE ELASTIQUE
                            tmp=b(i,n)*rig*b(j,m)*zr(ipoids+kpg-1)*&
                            jac*densit
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (j .le. j1) then
                                kk = kkd + nddl*(m-1)+j
                                zr(imatuu+kk-1) = zr(imatuu+kk-1) + tmp
                            endif
200                      continue
        endif
!
! - FIN DE LA BOUCLE SUR LES POINTS DE GAUSS
800  end do
!
    if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9).eq.'RAPH_MECA')) then
        call codere(cod, npg, zi(jcret))
    endif
!
end subroutine

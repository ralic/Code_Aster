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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8nnem.h"
#include "asterfort/cargri.h"
#include "asterfort/codere.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/nmco1d.h"
#include "asterfort/nmgrib.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/lteatt.h"
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
!                                  - RIGI_MECA_IMPLEX
!                          POUR LES GRILLES MEMBRANES EXCENTREES OU NON
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: codres(2)
    character(len=4) :: fami
    character(len=16) :: nomres(2)
    integer :: nddl, nno, nnos, npg, ndim, i, j, j1, n, m, kpg, kk, kkd, lgpg
    integer :: cod(9)
    integer :: imatuu, ipoids, ivf, idfde, igeom, imate, icamas, icontm, ivarim
    integer :: jgano, jtab(7), jcret, ideplm, ideplp, icompo, icarcr, iret
    integer :: ivectu, icontp, ivarip, ivarix, icontx
    real(kind=8) :: dff(2, 8), b(6, 8), p(3, 6), jac
    real(kind=8) :: dir11(3), densit, pgl(3, 3), distn, vecn(3)
    real(kind=8) :: epsm, deps, sigm, sig, tmp, rig, valres(2)
    real(kind=8) :: angmas(3)
    aster_logical :: vecteu, matric, lexc
!
! - BOOLEEN UTILES
!
    vecteu = ((option(1:9).eq.'FULL_MECA').or. (option .eq.'RAPH_MECA'))
    matric = ((option(1:9).eq.'FULL_MECA').or. (option(1:9).eq.'RIGI_MECA'))
    lexc = (lteatt('CODMOD','GRC'))
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
!
    fami = 'RIGI'
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
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
        call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                    itab=jtab)
        lgpg = max(jtab(6),1)*jtab(7)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PVARIMP', 'L', ivarix)
!
! --- ORIENTATION DU MASSIF
!
        call tecach('NNN', 'PCAMASS', 'L', iret, iad=icamas)
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
! - PARAMETRES EN SORTIE SUPPLEMENTAIE POUR LA METHODE IMPLEX    
    if (option .eq. 'RIGI_MECA_IMPLEX') then
        call jevech('PCONTXR', 'E', icontx)
! ------ INITIALISATION DE LA CONTRAINTE INTERPOLE CONTX=CONTM        
        call dcopy(npg, zr(icontm), 1, zr(icontx), 1)      
    endif
!
    if ((option(1:4).eq.'FULL') .or. (option(1:4).eq.'RIGI')) then
        call jevech('PMATUUR', 'E', imatuu)
    endif
!
! - INITIALISATION CODES RETOURS
!
    do kpg = 1, npg
        cod(kpg)=0
    end do
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
        do i = 1, 3
            vecn(i)=distn*pgl(3,i)
        enddo
        nddl=6
!
    else
        nddl = 3
    endif
!
! - DEBUT DE LA BOUCLE SUR LES POINTS DE GAUSS
!
    do kpg = 1, npg
!
! --- MISE SOUS FORME DE TABLEAU DES VALEURS DES FONCTIONS DE FORME
!     ET DES DERIVEES DE FONCTION DE FORME
!
        do n = 1, nno
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
        enddo
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
            do i = 1, nno
                do j = 1, nddl
                    epsm=epsm+b(j,i)*zr(ideplm+(i-1)*nddl+j-1)
                    deps=deps+b(j,i)*zr(ideplp+(i-1)*nddl+j-1)
                enddo
            enddo
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
            do n = 1, nno
                do i = 1, nddl
                    zr(ivectu+(n-1)*nddl+i-1)=zr(ivectu+(n-1)*nddl+i-&
                    1) +b(i,n)*sig*zr(ipoids+kpg-1)*jac*densit
                enddo
            enddo
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
                            tmp=b(i,n)*rig*b(j,m)*zr(ipoids+kpg-1)*&
                            jac*densit
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (j .le. j1) then
                                kk = kkd + nddl*(m-1)+j
                                zr(imatuu+kk-1) = zr(imatuu+kk-1) + tmp
                            endif
                        enddo
                    enddo
                enddo
            enddo
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

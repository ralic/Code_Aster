subroutine te0432(option, nomte)
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
#include "asterfort/cargri.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/nmgrib.h"
#include "asterfort/pmavec.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/vecma.h"
#include "asterfort/lteatt.h"
#include "blas/ddot.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!                          POUR LES GRILLES MEMBRANES EXCENTREES OU NON
!                          EN DYNAMIQUE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: codres(2)
    character(len=4) :: fami
    character(len=3) :: stopz
    integer :: nno, npg, i, imatuu, ndim, nnos, jgano
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: iret, iretd, iretv
    integer :: kpg, n, j, kkd, m, k
    integer :: kk, nddl
    integer :: iacce, ivect, l, nvec, ivite, ifreq, iecin, idepl
    real(kind=8) :: dff(2, 8), p(3, 6), tref
    real(kind=8) :: dir11(3), vff(8), b(6, 8), jac, rho(1)
    real(kind=8) :: densit, vecn(3)
    real(kind=8) :: distn, pgl(3, 3), masdep(48)
    real(kind=8) :: aexc(3, 3, 8, 8), a(6, 6, 8, 8), coef, matv(1176)
    real(kind=8) :: matp(48, 48)
    real(kind=8) :: diag(3, 8), wgt, alfam(3), somme(3), masvit(48), ecin
    logical :: lexc, ldiag
!
!
    lexc = (lteatt('CODMOD','GRC'))
    ldiag = (option(1:10).eq.'MASS_MECA_')
!
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
    fami = 'MASS'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    call rcvarc(' ', 'TEMP', 'REF', fami, 1,&
                1, tref, iret)
    call r8inir(8*8*6*6, 0.d0, a, 1)
    call r8inir(8*8*3*3, 0.d0, aexc, 1)
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    if (option .eq. 'MASS_MECA') then
!
    else if (option.eq.'M_GAMMA') then
        call jevech('PACCELR', 'L', iacce)
    else if (option.eq.'ECIN_ELEM') then
        stopz='ONO'
        call tecach(stopz, 'PVITESR', 'L', iretv, iad=ivite)
        if (iretv .ne. 0) then
            call tecach(stopz, 'PDEPLAR', 'L', iretd, iad=idepl)
            if (iretd .eq. 0) then
                call jevech('POMEGA2', 'L', ifreq)
            else
                call utmess('F', 'ELEMENTS2_1', sk=option)
            endif
        endif
    endif
!
! PARAMETRES EN SORTIE
!
    if (option(1:9) .eq. 'MASS_MECA') then
        call jevech('PMATUUR', 'E', imatuu)
    else if (option.eq.'M_GAMMA') then
        call jevech('PVECTUR', 'E', ivect)
    else if (option.eq.'ECIN_ELEM') then
        call jevech('PENERCR', 'E', iecin)
    endif
!
!
! - INITIALISATION CODES RETOURS
!       DO 1955 KPG=1,NPG
!          COD(KPG)=0
! 1955  CONTINUE
!
!
! - LECTURE DES CARACTERISTIQUES DE GRILLE ET
!   CALCUL DE LA DIRECTION D'ARMATURE
!
    call cargri(lexc, densit, distn, dir11)
!
!
! --- SI EXCENTREE : RECUPERATION DE LA NORMALE ET DE L'EXCENTREMENT
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
!
        nddl=6
!
    else
!
        nddl = 3
!
    endif
!
! - CALCUL POUR CHAQUE POINT DE GAUSS : ON CALCULE D'ABORD LA
!      CONTRAINTE ET/OU LA RIGIDITE SI NECESSAIRE PUIS
!      ON JOUE AVEC B
!
    wgt = 0.d0
    do 800 kpg = 1, npg
!
! - MISE SOUS FORME DE TABLEAU DES VALEURS DES FONCTIONS DE FORME
!   ET DES DERIVEES DE FONCTION DE FORME
!
        do 11 n = 1, nno
            vff(n) =zr(ivf+(kpg-1)*nno+n-1)
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
11      continue
!
! - MASS_MECA
!
        call rcvalb(fami, kpg, 1, '+', zi(imate),&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, 'RHO', rho, codres, 1)
!
!
! - CALCUL DE LA MATRICE "B" : DEPL NODAL -> EPS11 ET DU JACOBIEN
!
        call nmgrib(nno, zr(igeom), dff, dir11, lexc,&
                    vecn, b, jac, p)
        wgt = wgt + rho(1)*zr(ipoids+kpg-1)*jac*densit
!
        do 130 n = 1, nno
            do 130 i = 1, n
                coef = rho(1)*zr(ipoids+kpg-1)*jac*densit*vff(n)*vff(i)
                a(1,1,n,i) = a(1,1,n,i) + coef
                a(2,2,n,i) = a(2,2,n,i) + coef
                a(3,3,n,i) = a(3,3,n,i) + coef
130          continue
!
        if (lexc) then
            do 135 i = 1, 3
                do 135 j = 1, 3
                    do 135 n = 1, nno
                        do 135 m = 1, n
                            aexc(i,j,n,m) = a(i,j,n,m)
135                      continue
            call r8inir(8*8*6*6, 0.d0, a, 1)
            do 140 i = 1, 6
                do 140 j = 1, 6
                    do 140 n = 1, nno
                        do 140 m = 1, n
                            do 140 k = 1, 3
                                a(i,j,n,m) = a(i,j,n,m)+p(k,i)*p(k,j) *aexc(k,k,n,m)
140                          continue
        endif
!
800  end do
!
! - RANGEMENT DES RESULTATS
! -------------------------
    if (ldiag) then
!
!-- CALCUL DE LA TRACE EN TRANSLATION SUIVANT X
!
        call r8inir(3*8, 0.d0, diag, 1)
        call r8inir(3, 0.d0, somme, 1)
        do 180 i = 1, 3
            do 181 j = 1, nno
                somme(i) = somme(i) + a(i,i,j,j)
181          continue
            alfam(i) = wgt/somme(i)
180      continue
!
!-- CALCUL DU FACTEUR DE DIAGONALISATION
!
!        ALFA = WGT/TRACE
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
        do 190 j = 1, nno
            do 190 i = 1, 3
                diag(i,j) = a(i,i,j,j)*alfam(i)
190          continue
!
        do 195 k = 1, nddl
            do 195 l = 1, nddl
                do 195 i = 1, nno
                    do 195 j = 1, nno
                        a(k,l,i,j) = 0.d0
195                  continue
        do 196 k = 1, 3
            do 196 i = 1, nno
                a(k,k,i,i) = diag(k,i)
196          continue
        if (nddl .eq. 6) then
            do 197 i = 1, nno
                a(4,4,i,i) = a(4,4,i,i) * alfam(1)
                a(5,5,i,i) = a(4,4,i,i) * alfam(2)
                a(6,6,i,i) = a(4,4,i,i) * alfam(3)
197          continue
        endif
    endif
!
!
    if (option(1:9) .eq. 'MASS_MECA') then
        do 200 k = 1, nddl
            do 200 l = 1, nddl
                do 200 i = 1, nno
                    kkd = ((nddl*(i-1)+k-1)* (nddl*(i-1)+k))/2
                    do 200 j = 1, i
                        kk = kkd + nddl * (j-1) + l
                        zr(imatuu+kk-1) = a(k,l,i,j)
200                  continue
!
    else if (option.eq.'M_GAMMA'.or. option.eq.'ECIN_ELEM') then
        nvec = nddl*nno*(nddl*nno+1)/2
        do 210 k = 1, nvec
            matv(k) = 0.0d0
210      continue
        do 220 k = 1, nddl
            do 220 l = 1, nddl
                do 220 i = 1, nno
                    kkd = ((nddl*(i-1)+k-1)* (nddl*(i-1)+k))/2
                    do 220 j = 1, i
                        kk = kkd + nddl* (j-1) + l
                        matv(kk) = a(k,l,i,j)
220                  continue
        call vecma(matv, nvec, matp, nddl*nno)
        if (option .eq. 'M_GAMMA') then
            call pmavec('ZERO', nddl*nno, matp, zr(iacce), zr(ivect))
        else if (option.eq.'ECIN_ELEM') then
            if (iretv .eq. 0) then
                call pmavec('ZERO', nddl*nno, matp, zr(ivite), masvit)
                ecin = .5d0*ddot(nddl*nno,zr(ivite),1,masvit,1)
            else
                call pmavec('ZERO', nddl*nno, matp, zr(idepl), masdep)
                ecin = .5d0*ddot(nddl*nno,zr(idepl),1,masdep,1)*zr( ifreq)
            endif
            zr(iecin) = ecin
        endif
!
    endif
!
end subroutine

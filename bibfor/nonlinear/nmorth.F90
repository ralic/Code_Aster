subroutine nmorth(fami, kpg, ksp, ndim, phenom,&
                  imate, poum, deps, sigm, option,&
                  angmas, sigp, vip, dsidep)
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterc/r8vide.h"
#include "asterfort/d1ma3d.h"
#include "asterfort/d1mamc.h"
#include "asterfort/dmat3d.h"
#include "asterfort/dmatmc.h"
#include "asterfort/lteatt.h"
#include "asterfort/matrot.h"
#include "asterfort/r8inir.h"
#include "asterfort/u2mess.h"
#include "asterfort/utpslg.h"
#include "asterfort/verift.h"
    character(len=*) :: fami, poum
    integer :: kpg, ksp, ndim, imate
    real(kind=8) :: deps(6), sigm(6), sigp(6)
    real(kind=8) :: angmas(3), dsidep(6, 6), p(3, 3), vip
    character(len=16) :: option, phenom
!
!  IN    FAMI   : FAMILLE DE POINT DE GAUSS
!  IN    KPG    : NUMERO DU POINT DE GAUSS
!  IN    KSP    : NUMERO DU SOUS POINT DE GAUSS
!  IN    NDIM   : DIMENSION DU PROBLEME
!  IN    PHENOM : PHENOMENE (ELAS_ORTH OU ELAS_ISTR)
!  IN    TYPMOD : TYPE DE MODELISATION
!  IN    IMATE  : ADRESSE DU MATERIAU
!  IN    EPSM   : DEFORMATION A L INSTANT T-
!  IN    DESPS  : INCREMENT DE DEFORMATION
!  IN    SIGM   : CONTRAINTE A L INSTANT T-
!  IN    OPTION : OPTION A CALCULER
!  IN    ANGMAS : ANGLE DU REPERE LOCAL D ORTHOTROPIE
!  OUT   SIGP   : CONTRAINTE A L INSTANT T+
!  OUT   VIP    : VARIABLE INTERNE (NECESSAIRE
!                 CAR IL EN EXISTE FORCEMENT UNE)
!  OUT   DSIDEP : MATRICE DE RIGIDITE TANGENTE
!
!     VARIABLE LOCALE
    real(kind=8) :: rbid, repere(7), hookf(36), mkooh(36), xyzgau(3)
    real(kind=8) :: valres(3), deplth(6), depgth(6), depstr(6)
    real(kind=8) :: depsme(6), rac2, vepst1(6), vepst2(6), epsm2(6)
    integer :: iret, nbsigm, i, j
    character(len=2) :: k2bid
    character(len=8) :: materi
    logical :: vrai
!
    k2bid = '  '
    materi = ' '
!
    rac2=sqrt(2.d0)
    nbsigm=ndim*2
    call r8inir(36, 0.d0, dsidep, 1)
!
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        do 1 i = 1, nbsigm
            if (i .le. 3) then
                depstr(i)=deps(i)
            else
                depstr(i)=deps(i)*rac2
            endif
 1      end do
    endif
!
    if (angmas(1) .eq. r8vide()) call u2mess('F', 'ALGORITH8_20')
!
    repere(1)=1.d0
    repere(2)=angmas(1)
!
    vrai = .false.
    if (fami .eq. 'PMAT') then
!        ON VIENT DE OP0033
        repere(3)=angmas(2)
        repere(4)=angmas(3)
        vrai = .true.
    else
        if (lteatt(' ','DIM_TOPO_MAILLE','3')) then
            repere(3)=angmas(2)
            repere(4)=angmas(3)
            vrai = .true.
        else if (lteatt(' ','C_PLAN','OUI')) then
            vrai = .true.
        else if (lteatt(' ','D_PLAN','OUI')) then
            vrai = .true.
        else if (lteatt(' ','AXIS','OUI')) then
            vrai = .true.
        endif
    endif
!
    if (.not.vrai) call u2mess('F', 'ALGORITH8_22')
!
    do 2 i = 1, nbsigm
        depgth(i)=0.d0
 2  end do
!
!     MATRICES TANGENTES
!
    if (fami .eq. 'PMAT') then
!        ON VIENT DE OP0033
        if (option .eq. 'RIGI_MECA_TANG') then
            call dmat3d(fami, imate, rbid, '-', kpg,&
                        ksp, repere, xyzgau, hookf)
        else
            call d1ma3d(fami, imate, rbid, '-', kpg,&
                        ksp, repere, xyzgau, mkooh)
            call dmat3d(fami, imate, rbid, '+', kpg,&
                        ksp, repere, xyzgau, hookf)
        endif
!
    else
        if (option .eq. 'RIGI_MECA_TANG') then
            call dmatmc(fami, k2bid, imate, rbid, '-',&
                        kpg, ksp, repere, xyzgau, nbsigm,&
                        hookf)
        else
            call d1mamc(fami, imate, rbid, '-', kpg,&
                        ksp, repere, xyzgau, nbsigm, mkooh)
            call dmatmc(fami, k2bid, imate, rbid, '+',&
                        kpg, ksp, repere, xyzgau, nbsigm,&
                        hookf)
        endif
    endif
!
    if (option .eq. 'RIGI_MECA_TANG' .or. option .eq. 'FULL_MECA') then
        do 10 i = 1, nbsigm
            do 20 j = 1, nbsigm
                dsidep(i,j)=hookf(nbsigm*(j-1)+i)
20          continue
10      continue
    endif
!
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
!
!
        if (phenom .eq. 'ELAS_ORTH') then
!
            call verift(fami, kpg, ksp, poum, imate,&
                        materi, 'ELAS_ORTH', 3, valres, iret)
            deplth(1) = valres(1)
            deplth(2) = valres(2)
            deplth(3) = valres(3)
!
!
        else if (phenom.eq.'ELAS_ISTR') then
!
! RECUPERATION DES PARAMETRES MATERIAUX A L INSTANT -
!
            call verift(fami, kpg, ksp, poum, imate,&
                        materi, 'ELAS_ISTR', 2, valres, iret)
            deplth(1) = valres(1)
            deplth(2) = valres(1)
            deplth(3) = valres(2)
!
        endif
!
! INCREMENT DE DEFORMATIONS D ORIGINE THERMIQUE DANS LE REPERE LOCAL
!
        deplth(4)=0.d0
        deplth(5)=0.d0
        deplth(6)=0.d0
!
! RECUPERATION DE LA MATRICE DE PASSAGE
        call matrot(angmas, p)
!
! PASSAGE DU TENSEUR DES DEFORMATIONS THERMIQUES DANS LE REPERE GLOBAL
!
        vepst1(1)=deplth(1)
        vepst1(2)=deplth(4)
        vepst1(3)=deplth(2)
        vepst1(4)=deplth(5)
        vepst1(5)=deplth(6)
        vepst1(6)=deplth(3)
        call utpslg(1, 3, p, vepst1, vepst2)
!
        depgth(1)=vepst2(1)
        depgth(2)=vepst2(3)
        depgth(3)=vepst2(6)
        depgth(4)=vepst2(2)
        depgth(5)=vepst2(4)
        depgth(6)=vepst2(5)
! CALCUL DES DEFORMATIONS MECANIQUES
! ATTENTION LES TERMES EXTRA DIAGONAUX DE DEFORMATIONS THERMIQUES
! DOIVENT ETRE MULTIPLIES PAR DEUX POUR ETRE CONFORME AVEC
! LA MATRICE DE RIGIDITE ISSU DE DMATMC (ET DONC AVEC DEPSTR AUSSI)
!
        do 30 i = 1, nbsigm
            if (i .le. 3) then
                depsme(i)=depstr(i)-depgth(i)
            else
                depsme(i)=depstr(i)-2.d0*depgth(i)
            endif
30      continue
!
! CONTRAINTE A L ETAT +
        do 55 i = 4, nbsigm
            sigm(i)=sigm(i)/rac2
55      continue
! MODIFICATIOn DE SIGM POUR PRENDRE EN COMPTE LA VARIATION DE
! COEF ELASTIQUES AVEC LA TEMPERATURE
!
        do 40 i = 1, nbsigm
            epsm2(i)=0.d0
            do 50 j = 1, nbsigm
                epsm2(i)=epsm2(i)+mkooh(nbsigm*(j-1)+i)*sigm(j)
50          continue
40      continue
!
        do 60 i = 1, nbsigm
            sigp(i)=0.d0
            do 70 j = 1, nbsigm
                sigp(i)=sigp(i)+hookf(nbsigm*(j-1)+i)*(depsme(j)+&
                epsm2(j))
70          continue
60      continue
!
! PAS DE VARIABLE INTERNE POUR CE COMPORTEMENT
        vip=0.d0
!
! REMISE AU FORMAT ASTER DES VALEURS EXTRA DIAGONALES
        do 80 i = 4, nbsigm
            sigp(i)=sigp(i)*rac2
80      continue
    endif
!
    if (option .eq. 'RIGI_MECA_TANG' .or. option .eq. 'FULL_MECA') then
        do 67 i = 1, 6
            do 67 j = 4, 6
                dsidep(i,j) = dsidep(i,j)*sqrt(2.d0)
67          continue
        do 68 i = 4, 6
            do 68 j = 1, 6
                dsidep(i,j) = dsidep(i,j)*sqrt(2.d0)
68          continue
    endif
!
end subroutine

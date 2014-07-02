subroutine asmcyc(cmass, ndim, soumat, beta, ni,&
                  nj, na, axok, liax, nbliax,&
                  libid)
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
!
!***********************************************************************
!    P. RICHARD     DATE 14/03/91
!-----------------------------------------------------------------------
!  BUT:  ASSEMBLER LES MATRICES MASSE COMPLEXES RELATIVES
!   A UN PROBLEME CYCLIQUE SOUS MATRICES ET DEPHASAGE BETA
!
!
! METHODE:
!         TOUTES LES SOUS-MATRICES POSSIBLES SONT PASSEE  EN REVUE
!         PAR APPEL A DES ROUTINES D'ASSEMBLAGE SPECIALISEES
!         QUI TESTENT L'EXISTENCE DE LA SOUS-MATRICE ET NE FONT
!         RIEN SI ELLE N'EXISTE PAS
!         CHAQUE METHODE: CRAIG-BAMPTON (CB), MAC-NEAL (MN), ET
!                         CRAIG-BAMPTON HARMONIQUE (CBH) Y
!                         TROUVE SON COMPTE
!
!-----------------------------------------------------------------------
!
! CMASS    /O/: MATRICE DE MASSE COMPLEXE
! NDIM     /O/: DIMENSION DES MATRICES COMPLEXES
! SOUMAT   /I/: NOM K24 DE LA FAMILLE DES SOUS-MATRICES
! BETA     /I/: ANGLE DE PEPHASAGE EN RADIANS
! NI       /I/: NOMBRE DE MODE PROPRES DE LA BASE
! NJ       /I/: NOMBRE DE DDL GENERALISES DE DROITE
! NA       /I/: NOMBRE DE DDL GENERALISES AXE
! AXOK     /I/: FLAG ASSEMBLAGE TERMES RELATIFS DDL AXE
! LIAX     /I/: LISTE DES NUMERO DDL AXE A ASSEMBLER
! NBLIAX   /I/: NOMBRE DES DDL AXE A ASSEMBLER (<=NA)
! LIBID    /I/: LISTE BIDON LIBID(I)=I DE DIM >=MAX(NI,NJ)
!
!
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/acyel1.h"
#include "asterfort/acyel2.h"
#include "asterfort/acyel4.h"
#include "asterfort/acyelt.h"
!-----------------------------------------------------------------------
    integer :: i, ia, id, na, nbliax, ndim, ni
    integer :: nj
    real(kind=8) :: beta
    character(len=24) :: soumat
    complex(kind=8) :: cmass(*)
    integer :: libid(*), liax(nbliax)
    aster_logical :: axok, vrai, faux
!-----------------------------------------------------------------------
    data vrai,faux /.true._1,.false./
!-----------------------------------------------------------------------
!
!
!-----------------MISE A ZERO DES MATRICES COMPLEXES--------------------
!
    do 10 i = 1, ndim*(ndim+1)/2
        cmass(i)=dcmplx(0.d0,0.d0)
 10 end do
!
!
!--------------DETERMINATION DES DIMENSIONS DES SOUS-MATRICES-----------
!
!
! DEBUT DES DDL GENERALISES DE DROITE
!
    id=ni+1
!
! DEBUT DES DDL GENERALISES DE AXE
!
    ia=ni+nj+1
!
!--------------ASSEMBLAGE DES TERMES STANDARDS--------------------------
!
!  POUR TOUTES LES METHODES
!
    call acyelt(soumat, 'M0II', ni, cmass, ndim,&
                1, 1, 1.d0)
!
!  POUR CRAIG-BAMPTON ET CRAIG-BAMPTON HARMONIQUE
!
    call acyelt(soumat, 'M0JJ', nj, cmass, ndim,&
                id, id, 1.d0)
!
    call acyel2(soumat, 'M0IJ', ni, nj, faux,&
                libid, ni, libid, nj, cmass,&
                ndim, 1, id, 1.d0)
!
    call acyel4(soumat, 'MPLUSJJ', nj, nj, faux,&
                libid, nj, libid, nj, cmass,&
                ndim, id, id, beta)
!
    call acyel4(soumat, 'MPLUSIJ', ni, nj, faux,&
                libid, ni, libid, nj, cmass,&
                ndim, 1, id, beta)
!
!  POUR MAC-NEAL
!    RIEN
!
!
!-------------------------CAS DE PRESENCE DE DDL AXE--------------------
!
    if (axok) then
!
!
!  POUR TOUTES LES METHODES
!       RIEN
!
!
!  POUR CRAIG-BAMPTON ET CRAIG-BAMPTON HARMONIQUE
!
        call acyel2(soumat, 'M0IA', ni, na, vrai,&
                    libid, ni, liax, nbliax, cmass,&
                    ndim, 1, ia, 1.d0)
        call acyel2(soumat, 'M0AJ', na, nj, vrai,&
                    liax, nbliax, libid, nj, cmass,&
                    ndim, ia, id, 1.d0)
        call acyel1(soumat, 'M0AA', na, na, vrai,&
                    liax, nbliax, liax, nbliax, cmass,&
                    ndim, ia, ia, 1.d0)
        call acyel4(soumat, 'MPLUSIA', ni, na, vrai,&
                    libid, ni, liax, nbliax, cmass,&
                    ndim, 1, ia, beta)
        call acyel4(soumat, 'MPLUSAJ', na, nj, vrai,&
                    liax, nbliax, libid, nj, cmass,&
                    ndim, ia, id, beta)
        call acyel4(soumat, 'MPLUSJA', nj, na, vrai,&
                    libid, nj, liax, nbliax, cmass,&
                    ndim, id, ia, beta)
        call acyel4(soumat, 'MPLUSAA', na, na, vrai,&
                    liax, nbliax, liax, nbliax, cmass,&
                    ndim, ia, ia, beta)
!
!  POUR MAC-NEAL
!
!    RIEN
!
    endif
!
!
end subroutine

subroutine pcstru(n, in, ip, icpl, icpc,&
                  icpd, icpcx, icplx, niveau, complt,&
                  lca, imp, ier)
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
! aslint: disable=W1304
    implicit none
!
!----------------------------------------------------------------------
!   ENTREE
!   N          : TAILLE DE A
!   IN,IP      : MATRICE D'ENTREE FORMAT SYMETRIQUE
!   LCA        : LONGUEUR MAXI MATRICE FACTORISEE
!
!   SORTIE
!   ICPL,ICPC  : MATRICE APRES REMPLISSAGE FORMAT SYMETRIQUE
!   COMPLT    : FALSE OU TRUE
!   IER        : =0 TAILLE LCA SUFFISANTE
!              : =NN TAILLE LCA INSUFFISANTE IL FAUT NN
!
!   TRAVAIL
!   ICPD       : POINTEUR SUR LA DIAG DE LU
!   ICPCX      : IDEM ICPC
!   ICPLX      : IDEM ICPL
!----------------------------------------------------------------------
!----------------------------------------------------------------------
#include "jeveux.h"
!
#include "asterfort/jedetr.h"
#include "asterfort/pcdiag.h"
#include "asterfort/pcfalu.h"
#include "asterfort/pcfull.h"
#include "asterfort/pcinfe.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: n, in(n)
    integer(kind=4) :: ip(*), icpc(*)
    integer :: icpl(0:n), icpd(n)
    integer :: icplx(0:n), icpcx(*)
!
    logical(kind=1) :: complt
!----------------------------------------------------------------------
!
!     TDEB = SECOND()
!     WRITE (6,1500)
!     WRITE (6,1000) NIVEAU,LCA
!     NCOEF = IN(N)
!     WRITE (6,*) ' TAILLE INITIALE ',NCOEF
! IN-IP---> IPL-IPC
! =================
!-----------------------------------------------------------------------
    integer :: i, ier, imp,  k, k1, k2
    integer :: kk, lca, niv, niveau, nz
    integer, pointer :: ind(:) => null()
!-----------------------------------------------------------------------
    AS_ALLOCATE(vi=ind, size=n)
    call pcfalu(n, in, ip, icpl, icpc,&
                ind, imp)
!
! INITIALISATION
    ier = 0
    complt = .false.
    call pcdiag(n, icpl, icpc, icpd)
!
! BOUCLE SUR LES NIVEAUX
! ======================
    do 10 niv = 1, niveau
        nz = icpl(n)
        if (niv .lt. niveau) then
            call pcfull(n, icpl, icpc, icpd, icplx,&
                        icpcx, ind, lca, ier)
        else
            call pcinfe(n, icpl, icpc, icpd, icplx,&
                        icpcx, ind, lca, ier)
        endif
!
        if (ier .gt. 0) goto 50
!
        call pcdiag(n, icpl, icpc, icpd)
        if (icpl(n) .eq. nz) then
!         WRITE (6,4000) NIV
            complt = .true.
            goto 20
        endif
10  end do
!
20  continue
!     WRITE(6,*)' MATRICE APRES FACTORISATION SYMBOLIQUE'
!
! ICPL,ICPC FORMAT LU ---> FORMAT SYMETRIQUE
! ================================
    icpc(1) = 1
    kk = 1
    do 40 i = 2, n
!                  ATTENTION ICPL(0:N)
        icpl(i-2) = kk
        k1 = icpl(i-1) + 1
        k2 = icpd(i)
        do 30 k = k1, k2
            kk = kk + 1
            icpc(kk) = icpc(k)
30      continue
!   TERME DIAG
        kk = kk + 1
        icpc(kk) = int(i, 4)
40  end do
    icpl(n-1) = kk
!
!     TFIN = SECOND() - TDEB
!     WRITE (6,*) ' S-P PCSTRU FIN NORMALE'
!     WRITE (6,*) ' TAILLE FINALE APRES REMPLISSAGE ',ICPL(N)
!     WRITE (6,*) ' DUREE  ',TFIN
    goto 60
!
50  continue
!     TFIN = SECOND() - TDEB
!     WRITE (6,*) ' S-P PCSTRU FIN ANORMALE !!! '
!     WRITE (6,*) ' DUREE  ',TFIN
60  continue
!
    AS_DEALLOCATE(vi=ind)
!
end subroutine

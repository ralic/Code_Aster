subroutine vppgen(lmasse, lamor, lraide, masseg, amorg,&
                  raideg, vect, neq, nbvect, iddl)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mrmult.h"
#include "asterfort/vecini.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
    integer :: lmasse, lamor, lraide, neq, nbvect, iddl(*)
    real(kind=8) :: masseg(*), amorg(*), raideg(*), vect(neq, *)
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CALCUL DES PARAMETRES MODAUX :
!            MASSE, AMORTISSEMENT ET RAIDEUR GENERALISES
!     ------------------------------------------------------------------
! IN  LMASSE : IS : DESCRIPTEUR NORMALISE DE LA MATRICE DE MASSE
!            = 0  ON NE CALCULE PAS LA MASSE GENERALISEE
! IN  LAMOR  : IS : DESCRIPTEUR NORMALISE DE LA MATRICE D'AMORTISSEMENT
!            = 0  ON NE CALCULE PAS L'AMORTISSEMENT GENERALISE
! IN  LRAIDE : IS : DESCRIPTEUR NORMALISE DE LA MATRICE DE RIGIDITE
!            = 0  ON NE CALCULE PAS LA RIGIDITE GENERALISEE
!     ------------------------------------------------------------------
!     REMARQUE : ON FAIT LES CALCULS VECTEURS APRES VECTEURS
!              : C'EST PLUS LONG MAIS PAS DE PB DE TAILLE MEMOIRE
!     ------------------------------------------------------------------
!
!
    real(kind=8) :: rzero
    character(len=24) :: vecaux, vecau1
    integer :: ieq, ivect, laux, laux1
!-----------------------------------------------------------------------
    data  vecaux/'&&VPPGEN.VECTEUR.AUX0'/
    data  vecau1/'&&VPPGEN.VECTEUR.AUX1'/
!     ------------------------------------------------------------------
    call jemarq()
    call wkvect(vecaux, 'V V R', neq, laux)
    call wkvect(vecau1, 'V V R', neq, laux1)
    laux = laux - 1
    laux1= laux1- 1
!
    rzero=0.d0
!     ------------------------------------------------------------------
!     ----------------- CALCUL DE LA MASSE GENERALISEE -----------------
!     ------------------------------------------------------------------
    if (lmasse .ne. 0) then
        do 100 ivect = 1, nbvect
            call mrmult('ZERO', lmasse, vect(1, ivect), zr(laux+1), 1,&
                        .false.)
            masseg(ivect) = ddot(neq,vect(1,ivect),1,zr(laux+1),1)
100      continue
    endif
!     ------------------------------------------------------------------
!     --------------- CALCUL DE L'AMORTISSEMENT GENERALISE -------------
!     ------------------------------------------------------------------
    if (lamor .ne. 0) then
        do 200 ivect = 1, nbvect
            call mrmult('ZERO', lamor, vect(1, ivect), zr(laux+1), 1,&
                        .false.)
            amorg(ivect) = ddot(neq,vect(1,ivect),1,zr(laux+1),1)
200      continue
    else
        call vecini(nbvect, rzero, amorg)
    endif
!     ------------------------------------------------------------------
!     ---------------- CALCUL DE LA RAIDEUR GENERALISEE ----------------
!     ------------------------------------------------------------------
    if (lraide .ne. 0) then
        do 300 ivect = 1, nbvect
            do 310 ieq = 1, neq
                zr(laux1+ieq)= vect(ieq,ivect)*iddl(ieq)
310          continue
            call mrmult('ZERO', lraide, zr(laux1+1), zr(laux+1), 1,&
                        .false.)
            raideg(ivect) = ddot(neq,zr(laux+1),1,zr(laux1+1),1)
300      continue
    endif
!     ------------------------------------------------------------------
    call jedetr(vecaux)
    call jedetr(vecau1)
!     ------------------------------------------------------------------
    call jedema()
end subroutine

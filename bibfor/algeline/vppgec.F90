subroutine vppgec(lmasse, lamor, lraide, masseg, amorg,&
                  raideg, vect, neq, nbvect, iddl)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mcmult.h"
#include "asterfort/wkvect.h"
    integer :: lmasse, lamor, lraide, neq, nbvect, iddl(*)
    real(kind=8) :: masseg(*), amorg(*), raideg(*)
    complex(kind=8) :: vect(neq, *)
!     ------------------------------------------------------------------
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
! ======================================================================
!     CALCUL DES PARAMETRES MODAUX POUR DES VECTEURS COMPLEXES :
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
    complex(kind=8) :: cval
    character(len=24) :: vecaux, vecau1
    integer :: ieq, ivect, laux, laux1
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data  vecaux/'&&VPPGEN.VECTEUR.AUX0'/
    data  vecau1/'&&VPPGEN.VECTEUR.AUX1'/
!     ------------------------------------------------------------------
    call jemarq()
    call wkvect(vecaux, 'V V C', neq, laux)
    call wkvect(vecau1, 'V V C', neq, laux1)
    laux = laux - 1
    laux1= laux1- 1
!     ------------------------------------------------------------------
!     ----------------- CALCUL DE LA MASSE GENERALISEE -----------------
!     ------------------------------------------------------------------
    if (lmasse .ne. 0) then
        do 100 ivect = 1, nbvect
            call mcmult('ZERO', lmasse, vect(1, ivect), zc(laux+1), 1,&
                        .false._1)
            cval = dcmplx(0.d0,0.d0)
            do 110 ieq = 1, neq
                cval = cval + dconjg(vect(ieq,ivect)) * zc(laux+ieq)
110          continue
            masseg(ivect) = dble(cval)
100      continue
    endif
!     ------------------------------------------------------------------
!     --------------- CALCUL DE L'AMORTISSEMENT GENERALISE -------------
!     ------------------------------------------------------------------
    if (lamor .ne. 0) then
        do 200 ivect = 1, nbvect
            call mcmult('ZERO', lamor, vect(1, ivect), zc(laux+1), 1,&
                        .false._1)
            cval = dcmplx(0.d0,0.d0)
            do 210 ieq = 1, neq
                cval = cval + dconjg(vect(ieq,ivect)) * zc(laux+ieq)
210          continue
            amorg(ivect) = dble(cval)
200      continue
    else
        do 220 ivect = 1, nbvect
            amorg(ivect) = 0.d0
220      continue
    endif
!     ------------------------------------------------------------------
!     ---------------- CALCUL DE LA RAIDEUR GENERALISEE ----------------
!     ------------------------------------------------------------------
    if (lraide .ne. 0) then
        do 300 ivect = 1, nbvect
            do 310 ieq = 1, neq
                zc(laux1+ieq) = vect(ieq,ivect)*iddl(ieq)
310          continue
            call mcmult('ZERO', lraide, zc(laux1+1), zc(laux+1), 1,&
                        .false._1)
            cval = dcmplx(0.d0,0.d0)
            do 320 ieq = 1, neq
                cval = cval + vect(ieq,ivect)*zc(laux+ieq)*iddl(ieq)
320          continue
            raideg(ivect) = dble(cval)
300      continue
    endif
!     ------------------------------------------------------------------
    call jedetr(vecaux)
    call jedetr(vecau1)
!     ------------------------------------------------------------------
    call jedema()
end subroutine

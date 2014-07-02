subroutine xmmco2(ndim, nno, nnos, nnol, ddls,&
                  ddlm, dsidep, p, r, nfh,&
                  jac, ffp, ffc, pla, singu,&
                  nfiss, jheafa, jfisno, ifa, ncomph,&
                  ifiss, rr, mmat)
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/indent.h"
#include "asterfort/matini.h"
#include "asterfort/promat.h"
#include "asterfort/transp.h"
    integer :: ndim, nno, nfh, ddls, singu
    real(kind=8) :: mmat(216, 216), dsidep(6, 6)
    real(kind=8) :: ffp(27), jac
    real(kind=8) :: p(3, 3), rr
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
!
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DES MATRICES DE COHESION
! ELEMENT COHESIF MIXTE
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
! IN  DSIDEP : MATRICE TANGENTE BASE LOCALE
! IN  PP     :
! IN  P      : MATRICE PROJECTION PLAN TANGENT
! IN  ND     : DIRECTION NORMALE
! IN  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
! IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
! IN  FFP    : FONCTIONS DE FORME DE L'ELEMENT PARENT
! IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
! IN  RR     : DISTANCE AU FOND DE FISSURE
! IN  TAU1   : PREMIERE DIRECTION TANGENTE
! IN  AM     :
! I/O MMAT   : MATRICE ELEMENTAITRE DE CONTACT/FROTTEMENT
!
!
    integer :: i, j, ddlm, ifa, ifh, ifiss, in, jfh, jfisno, jheafa, jn
    integer :: k, l, ncomph, nfiss, nnol, nnos, pla(27), pli, plj
    real(kind=8) :: au(3, 3), coefi, coefj, dside2(3, 3), ffc(8), pdotal(3, 3)
    real(kind=8) :: ffi, ffj, r, temp(3, 3), unity(3, 3), ptr(3, 3)
    real(kind=8) :: alocal(3, 3)
    aster_logical :: lmultc
!
!
! ----------------------------------------------------------------------
!
!     INITIALISATIONS
!
    call matini(3, 3, 0.d0, unity)
    call matini(3, 3, 0.d0, alocal)
    call matini(3, 3, 0.d0, ptr)
    call matini(3, 3, 0.d0, pdotal)
    call matini(3, 3, 0.d0, au)
    call matini(3, 3, 0.d0, dside2)
    call matini(3, 3, 0.d0, temp)
!
!   MATRICE -ID+R DSIDEP
!
    do 3 i = 1, ndim
        unity(i,i) = 1.d0
  3 end do
!
    do 1 i = 1, ndim
        do 2 j = 1, ndim
            dside2(i,j) = dsidep(i,j)
            alocal(i,j) = -unity(i,j) + r*dside2(i,j)
  2     continue
  1 end do
!
! MATRICE [P]T[ALOCAL]
!
    call transp(p, 3, ndim, ndim, ptr,&
                3)
!
    call promat(ptr, 3, ndim, ndim, alocal,&
                3, ndim, ndim, pdotal)
!
! MATRICE TANGENTE EN BASE FIXE [P]T [DSIDEP] [P]
!
    call promat(ptr, 3, ndim, ndim, alocal,&
                3, ndim, ndim, temp)
    call promat(temp, 3, ndim, ndim, p,&
                3, ndim, ndim, au)
!
! ON STOCKE DANS LA MATRICE ELEMENTAIRE
!
    coefi = 2
    coefj = 2
    lmultc = nfiss.gt.1
!
    do 10 i = 1, nnol
!
        pli=pla(i)
        ffi=ffc(i)
!
        do 11 k = 1, ndim
!
            do 20 j = 1, nno
                call indent(j, ddls, ddlm, nnos, jn)
                do 21 jfh = 1, nfh
                    if (lmultc) then
                        coefj = zi(&
                                jheafa-1+ncomph*(&
                                nfiss*(ifiss-1) +zi(jfisno-1+nfh*(j-1)+jfh)-1)+2*ifa) - zi( jheaf&
                                &a-1+ncomph*(nfiss*(ifiss-1) +zi(jfisno-1+ nfh*(j-1)+jfh)-1&
                                )+2*ifa-1&
                                )
                    endif
                    do 22 l = 1, ndim
!
! INDICES INVERSES MATRICE INTERFACE
!
                        mmat(pli-1+k,jn+ndim*jfh+l)=mmat(pli-1+k,jn+&
                        ndim*jfh+l) - coefj * ffi * ffp(j) * pdotal(l,&
                        k) * jac
!
! INDICES MEME ORDRE MATRICE EQUILIBRE
!
                        mmat(jn+ndim*jfh+l,pli-1+k)=mmat(jn+ndim*jfh+&
                        l,pli-1+k) - coefj * ffi * ffp(j) * pdotal(l,&
                        k) * jac
 22                 continue
!
 21             continue
                do 23 l = 1, singu*ndim
                    mmat(pli-1+k,jn+ndim*(1+nfh)+l) = mmat(&
                                                      pli-1+k,&
                                                      jn+ ndim*(1+nfh)+l) - coefj * ffi * ffp(j) &
                                                      &* rr * pdotal(l,&
                                                      k&
                                                      ) * jac
!
                    mmat(jn+ndim*(1+nfh)+l,pli-1+k)= mmat(jn+ndim*(1+&
                    nfh)+l,pli-1+k) - coefj * ffi * ffp(j) * rr *&
                    pdotal(l,k) * jac
 23             continue
!
 20         continue
 11     continue
 10 continue
!
! -- MATRICE VENANT S AJOUTER A LA RAIDEUR
!
    do 140 i = 1, nno
        call indent(i, ddls, ddlm, nnos, in)
        do 141 j = 1, nno
            call indent(j, ddls, ddlm, nnos, jn)
            do 148 ifh = 1, nfh
                if (lmultc) then
                    coefi = zi(&
                            jheafa-1+ncomph*(&
                            nfiss*(ifiss-1) +zi(jfisno-1+nfh*(i-1)+ifh)-1)+2*ifa) - zi(jheafa- 1+&
                            &ncomph*(nfiss*(ifiss-1) +zi(jfisno-1+nfh*(i-1)+ ifh)-1&
                            )+2*ifa-1&
                            )
                endif
                do 149 jfh = 1, nfh
                    if (lmultc) then
                        coefj = zi(&
                                jheafa-1+ncomph*(&
                                nfiss*(ifiss-1) +zi(jfisno-1+nfh*(j-1)+jfh)-1)+2*ifa) - zi( jheaf&
                                &a-1+ncomph*(nfiss*(ifiss-1) +zi(jfisno-1+ nfh*(j-1)+jfh)-1&
                                )+2*ifa-1&
                                )
                    endif
                    do 142 k = 1, ndim
                        do 143 l = 1, ndim
                            mmat(in+ndim*ifh+k,jn+ndim*jfh+l) =&
                            mmat(in+ndim*ifh+k,jn+ndim*jfh+l) -&
                            coefi*coefj*r*au(k,l)*ffp(i)*ffp(j)*jac
143                     continue
!
                        do 144 l = 1, singu*ndim
                            mmat(in+ndim+k,jn+ndim*(1+nfh)+l) =&
                            mmat(in+ndim+k,jn+ndim*(1+nfh)+l) -&
                            4.d0*ffp(i)*ffp(j)*rr*r*au(k,l)*jac
144                     continue
142                 continue
149             continue
148         continue
!
            do 145 k = 1, singu*ndim
                do 146 l = 1, nfh*ndim
                    mmat(in+ndim*(1+nfh)+k,jn+ndim+l) = mmat(&
                                                        in+ndim*(1+nfh)+k,&
                                                        jn+ndim+l) - 4.d0*ffp(i)*ffp(j)*rr*r*au( &
                                                        &k,&
                                                        l&
                                                        )*jac
146             continue
!
                do 147 l = 1, singu*ndim
                    mmat(in+ndim*(1+nfh)+k,jn+ndim*(1+nfh)+l) =&
                    mmat(in+ndim*(1+nfh)+k,jn+ndim*(1+nfh)+l) -&
                    4.d0*ffp(i)*ffp(j)*rr*rr*r*au(k,l) *jac
147             continue
145         continue
!
141     continue
140 continue
!
! -- MATRICE D INTERFACE : EXPRESSION DIRECTE
!
    do 30 i = 1, nnol
!
        pli=pla(i)
        ffi=ffc(i)
        do 31 k = 1, ndim
!
            do 40 j = 1, nnol
!
                plj=pla(j)
                ffj=ffc(j)
                do 41 l = 1, ndim
!
                    mmat(pli-1+k,plj-1+l) = mmat(pli-1+k,plj-1+l) - ffj * dside2(k,l)*ffi * jac
 41             continue
 40         continue
 31     continue
 30 continue
end subroutine

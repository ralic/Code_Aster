subroutine wp5vec(opt, nbfreq, nbvect, neq, vp,&
                  vecp, mxresf, resufi, resufr, vauc)
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
#include "asterfort/wpordc.h"
    character(len=*) :: opt
    integer :: nbfreq, nbvect, neq, resufi(mxresf, *), mxresf
    complex(kind=8) :: vecp(neq, *), vauc(2*neq, *), vp(*)
    real(kind=8) :: resufr(mxresf, *)
!     -----------------------------------------------------------------
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
!     RESTITUTION DES VALEURS PROPRES ET DES MODES DU PB QUADRATIQUE
!     AVEC MATRICE DE RAIDEUR COMPLEXE
!     -----------------------------------------------------------------
! IN  OPT    : K : OPTION : 'CENTRE' OU 'PLUS_PETITE'
! IN  NBFREQ : I : NOMBRE DE MODES DEMANDES
! IN  NBVECT : I : NOMBRE DE VECTEURS DE LANCZOS
! IN  NEQ    : I : TAILLE DES MATRICES DU PB QUADRATIQUE
! VAR VP     : C : VALEURS PROPRE DU PB QUADRATIQUE
! OUT VECP   : C : MODES DU PB QUADRATIQUE
! IN  VAUC   : C : MODES DU PB QUADRATIQUE COMPLET
! OUT RESUFR : C : TABLEAU DE POST-TRAITEMENT
!     -----------------------------------------------------------------
!
    real(kind=8) :: am, om
    integer :: i, j, k, iadind
!
    call jemarq()
!
    call wkvect('&&WP5VEC.INDIC.PART.VP', 'V V I', nbvect, iadind)
!
! --- 4. TRI (DANS LE SPECTRE ET DE PRESENTATION) DES VALEURS PROPRES-
!
    do 1 j = 1, nbvect
        zi(iadind + j-1) = -2
 1  end do
    do 2 j = 1, nbvect
        if (zi(iadind + j-1) .eq. -2) then
            if (dimag(vp(j)) .gt. 0.d0) then
                zi(iadind + j-1) = 0
            else
                zi(iadind + j-1) = 1
            endif
        endif
 2  end do
!
    if (zi(iadind + nbvect-1) .eq. -2) then
        zi(iadind + nbvect-1) = 0
    endif
!
!
! --- 1.3. ELIMINATION DES CONJUGUES (OPERATEUR REEL) -- COMPACTAGE --
    k = 1
    do 4 j = 1, nbvect
        if (zi(iadind + j-1) .eq. 0) then
            if (k .ne. j) then
                vp(k) = vp(j)
                zi(iadind + k-1) = zi(iadind + j-1)
                do 5, i = 1, neq, 1
                vecp(i,k) = vecp(i,j)
                vauc(i,k) = vauc(i,j)
                vauc(i+neq,k) = vauc(i+neq,j)
 5              continue
            endif
            k = k + 1
        endif
 4  end do
!
!
!     ---------- FIN DE PARTITION TEST ET ELIMINATION -----------------
!     ----------    AU NIVEAU DE L' OPERATEUR REEL    -----------------
!
! --- 4. TRI (DANS LE SPECTRE ET DE PRESENTATION) DES VALEURS PROPRES-
    call wpordc(1, dcmplx(0.d0, 0.d0), vp, vecp, nbfreq,&
                neq)
!
! --- 5. PREPARATION DE RESUFR
    do 30 j = 1, nbfreq
        am = dble(vp(j))*dble(vp(j))
        om = dimag(vp(j))*dimag(vp(j))
        resufi(j,1) = j
        resufr(j,2) = om
        resufr(j,3) = -dble(vp(j))/sqrt(om + am)
30  end do
!
! --- 6. DESTRUCTION OJB TEMPORAIRE
!
    call jedetr('&&WP5VEC.INDIC.PART.VP')
!
    call jedema()
end subroutine

subroutine vppfac(lmasse, masgen, vect, neq, nbvect,&
                  mxvect, masmod, facpar)
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8miem.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mrmult.h"
#include "asterfort/pteddl.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
!
    integer :: lmasse, neq, nbvect, mxvect
    real(kind=8) :: masgen(*), vect(neq, *)
    real(kind=8) :: masmod(mxvect, *), facpar(mxvect, *)
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
!            FACTEUR DE PARTICIPATION ET MASSE MODALE UNITAIRE
!     ------------------------------------------------------------------
! IN  LMASSE : IS : DESCRIPTEUR NORMALISE DE LA MATRICE DE MASSE
!     ------------------------------------------------------------------
!     PRESUME L'EXECUTION PREALABLE DE VPPGEN : CALCUL DES PARAMETRES
!     MODAUX.
!     ------------------------------------------------------------------
!
!
    integer :: lddl, laux1, laux2, iddl, ia, ieq, ivect, mxddl, neq1
    parameter     ( mxddl=6 )
    character(len=8) :: nomddl(mxddl)
    character(len=14) :: nume
    character(len=19) :: masse
    character(len=24) :: posddl, vecau1, vecau2
    real(kind=8) :: rmin, rmax, raux, rval
!     ------------------------------------------------------------------
    data nomddl / 'DX      ', 'DY      ', 'DZ      ' ,&
     &              'DRX     ', 'DRY     ', 'DRZ     ' /
!
!     ------------------------------------------------------------------
    data  posddl/'&&VPPFAC.POSITION.DDL'/
    data  vecau1/'&&VPPFAC.VECTEUR.AUX1'/
    data  vecau2/'&&VPPFAC.VECTEUR.AUX2'/
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     ------ RECUPERATION DES POSITIONS DES PARAMETRES DE LAGRANGE -----
!     ------------------------------------------------------------------
!
    call jemarq()
    masse = zk24(zi(lmasse+1))
    call dismoi('NOM_NUME_DDL', masse, 'MATR_ASSE', repk=nume)
!
    call wkvect(posddl, 'V V I', neq*mxddl, lddl)
    call pteddl('NUME_DDL', nume, mxddl, nomddl, neq,&
                zi(lddl))
!
!     ------------------------------------------------------------------
!     ----------------- CREATION DE VECTEURS DE TRAVAIL ----------------
!     ------------------------------------------------------------------
!
    call wkvect(vecau1, 'V V R', neq, laux1)
    call wkvect(vecau2, 'V V R', neq, laux2)
!
!     ------------------------------------------------------------------
!     ----------- CALCUL DE  FREQ * MASSE * UNITAIRE_DIRECTION ---------
!     ------------------------------------------------------------------
    neq1=neq-1
    rmin=100.d0*r8miem()
    rmax=0.01d0*r8maem()
    do iddl = 1, 3
        ia = (iddl-1)*neq
        do ieq = 0, neq1
            zr(laux1+ieq) = zi(lddl+ia+ieq)
        end do
        call mrmult('ZERO', lmasse, zr(laux1), zr(laux2), 1,&
                    .false.)
        do ivect = 1, nbvect
            rval = ddot(neq,vect(1,ivect),1,zr(laux2),1)
            raux = masgen(ivect)
            if (abs(raux) .lt. rmin) then
                masmod(ivect,iddl) = rmax
                facpar(ivect,iddl) = rmax
            else
                raux=rval/raux
                masmod(ivect,iddl) = rval * raux
                facpar(ivect,iddl) = raux
            endif
        end do
    end do
!
!     ------------------------------------------------------------------
!     ----------------- DESTRUCTION DES VECTEURS DE TRAVAIL ------------
!     ------------------------------------------------------------------
!
    call jedetr(posddl)
    call jedetr(vecau1)
    call jedetr(vecau2)
!
    call jedema()
end subroutine

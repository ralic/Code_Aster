subroutine xcalfev_wrap(ndim, nnop, basloc, stano, he,&
                        lsn, lst, geom, kappa, mu, ff, fk,&
                        dfdi, dkdgl, face, elref, nnop2, ff2, dfdi2)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/xcalfev.h"
#include "asterfort/iselli.h"
#include "asterfort/xellin.h"
#include "asterfort/elref1.h"
#include "asterfort/reeref.h"
#include "asterfort/is_enr_line.h"
!
    integer :: ndim, nnop, stano(*)
    real(kind=8) :: he, lsn(*), basloc(*), fk(27,3,3), lst(*)
    real(kind=8) :: kappa, ff(*), geom(*), mu
    real(kind=8), optional :: dkdgl(27,3,3,3)
    real(kind=8), optional :: dfdi(nnop,ndim)
    character(len=4), optional :: face
    character(len=8), optional :: elref
    integer, optional :: nnop2
    real(kind=8), optional :: ff2(:), dfdi2(:,:)
!
!
!     BUT:  CALCUL DES FONCTIONS D'ENRICHISSEMENT <VECTORIEL> EN UN POINT DE GAUSS
!            DANS LA BASE <GLOBALE>
!
! IN  HE      : VALEUR DE LA FONCTION HEAVYSIDE CSTE LE SS-ELT
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE
! IN  KA, MU  : PARAMETRES MATERIAU /
!                 LES FONCTIONS ASYMPTOTIQUES NE SONT VALABLES QUE POUR UN MATERIAU ELASTIQUE
!                 CE N EST PAS GENANT DE LES UTILISEES POUR MODELISER UN AUTRE COMPORTEMENT
!                 ELLES NE TRANSPORTENT PLUS ALORS UNE INFORMATION SIGNIFICATIVE POUR LE MODELE
! IN  FF      : FONCTIONS DE FORMES DE L ELEMENT PARENT
! IN  DFDI    : DERIVEES DES FONCTIONS DE FORMES DE L ELEMENT PARENT
! IN  FACE    : LE COTE DE LA FACETTE TRAITEE "MAIT" <MAITRE> OU "ESCL" <ESCLAVE>
! IN  ELREF   : CET ARGUMENT N EST PAS UTILE A L INTERIEUR DES TE 
! IN  NNOP2   : NOMBRE NOEUDS SOMMETS
! IN  FF2     : FONCTIONS DE FORMES LINEAIRES /
!                 CET ARGUMENT DOIT ETRE RENSEIGNE POUR LES ELEMENTS QUADRATIQUES
! IN  DFDI2   : DERIVEES DES FONCTIONS DE FORMES LINEAIRES /
!                 CET ARGUMENT DOIT ETRE RENSEIGNE POUR LES ELEMENTS QUADRATIQUES
!
! OUT FK      : VALEURS DES FONCTIONS D'ENRICHISSEMENT <VECTORIEL> DANS LA BASE <GLOBALE>
! OUT DKDGL   : DERIVEES DES FONCTIONS D'ENRICHISSEMENT <VECTORIEL> DANS LA BASE <GLOBALE>
!
!----------------------------------------------------------------
!
    aster_logical :: lderiv
    integer :: ino, nnop_lin, j
    character(len=4) :: fac2
    character(len=8) :: elrefp, elrefp_lin
    real(kind=8) :: ff_lin(8), dfdi_lin(8,3), xe_lin(ndim), xg(ndim)
!----------------------------------------------------------------
!
    ASSERT(ndim.eq.2.or.ndim.eq.3)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    PREPARATION DES ARGUMENTS DE XCALFEV
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    fac2=' '
    if (present(face)) fac2=face
!
    if (.not.present(dkdgl)) then
      lderiv=.false.
    else
      lderiv=.true.
      ASSERT(present(dfdi))
      dkdgl(:,:,:,:)=0.d0
    endif
!
    if (present(elref)) then
      elrefp=elref
      if (iselli(elrefp)) goto 10
      ASSERT(present(nnop2).and.present(ff2))
      nnop_lin=nnop2
      ff_lin(1:nnop_lin)=ff2(1:nnop_lin)
      if (.not.lderiv) goto 10
      ASSERT(present(dfdi2))
      dfdi_lin(1:nnop_lin,1:ndim)= dfdi2(1:nnop_lin,1:ndim)
    else
      call elref1(elrefp)
      if (iselli(elrefp)) goto 10
      xg(:)=0.
      do ino=1, nnop
        do j=1, ndim
          xg(j)=xg(j)+ff(ino)*geom(ndim*(ino-1)+j)
        enddo
      enddo
      call xellin(elrefp, nnop, elrefp_lin, nnop_lin)
      if (.not.lderiv) then
        call reeref(elrefp_lin, nnop_lin, geom, xg, ndim, xe_lin,&
                          ff_lin(1:nnop_lin))
      else
        call reeref(elrefp_lin, nnop_lin, geom, xg, ndim, xe_lin,&
                          ff_lin(1:nnop_lin), dfdi=dfdi_lin(1:nnop_lin,1:ndim))
      endif  
    endif
!
10  continue
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    APPEL A XCALFEV
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (.not.iselli(elrefp).and.is_enr_line()) then
!      
       if (lderiv) then
          call xcalfev(elrefp, ndim, nnop, basloc, stano, he,&
                    lsn, lst, geom, kappa, mu, ff, fk,&
                    dfdi, dkdgl, face=fac2, nnop_lin=nnop_lin,&
                    ff_lin=ff_lin, dfdi_lin=dfdi_lin)
       else     
          call xcalfev(elrefp, ndim, nnop, basloc, stano, he,&
                    lsn, lst, geom, kappa, mu, ff, fk,&
                    face=fac2, nnop_lin=nnop_lin,&
                    ff_lin=ff_lin)
        endif
!
     else
!
       if (lderiv) then
          call xcalfev(elrefp, ndim, nnop, basloc, stano, he,&
                    lsn, lst, geom, kappa, mu, ff, fk,&
                    dfdi, dkdgl, face=fac2)
       else
          call xcalfev(elrefp, ndim, nnop, basloc, stano, he,&
                    lsn, lst, geom, kappa, mu, ff, fk,&
                    face=fac2)
       endif
!
     endif
!
end subroutine

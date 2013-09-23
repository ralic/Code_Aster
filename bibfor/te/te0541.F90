subroutine te0541(option, nomte)
!
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/xbsig.h"
#include "asterfort/xsigth.h"
#include "asterfort/xteddl.h"
#include "asterfort/xteini.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! FONCTION REALISEE:  CALCUL DE L'OPTION CHAR_MECA_TEMP_R POUR LES
!                     ÉLÉMENTS MECA X-FEM
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano, igeom, ivectu
    integer :: jpintt, jcnset, jheavt, jlonch, jbaslo, jlsn, jlst
    integer :: jpmilt, ddlm, nfiss, jfisno, ideplm, icontt
    integer :: nfh, ddlc, nfe, ibid, ddls, nbsig, nddl, jstno
    integer :: contac, nnom, singu, itab(1)
    integer :: iret, k, itemps
    logical :: lbid
    real(kind=8) :: rbid
    character(len=8) :: enr, elref
    character(len=16) :: compor(4)
! ----------------------------------------------------------------------
!
!     CARACTERISTIQUES DU TYPE D'ELEMENT : GEOMETRIE ET INTEGRATION
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    call elref1(elref)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                nnom, ddls, nddl, ddlm, nfiss,&
                contac)
!
!     NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
    nbsig = nbsigm()
!
!     RECUPERATION DES XHAMPS IN ET OUT
    call tecach('ONO', 'PCOMPOR', 'L', iret, iad=itab(1))
    do 100 k = 1, 4
        if (iret .eq. 0) then
            compor(k) = zk16(itab(1)-1+k)
        else
            compor(k) = ' '
        endif
100  end do
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PVECTUR', 'E', ivectu)
    call jevech('PCONTRT', 'E', icontt)
!
!     PARAMETRES PROPRES A X-FEM
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASLOR', 'L', jbaslo)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PSTANO', 'L', jstno)
!     PROPRE AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
    call teattr(nomte, 'S', 'XFEM', enr, ibid)
    if ((ibid.eq.0) .and. (nomte(3:4).ne.'AX') .and.&
        (enr.eq.'XH' .or.enr.eq.'XHT'.or.enr.eq.'XT'.or.enr.eq.'XHC') .and. ndim .le. 2) &
    call jevech('PPMILTO', 'L', jpmilt)
    if (nfiss .gt. 1) call jevech('PFISNO', 'L', jfisno)
!
!     CALCUL DES CONTRAINTES THERMIQUES
    call xsigth(ndim, nno, nfh, igeom, zi(jlonch),&
                zr(itemps), nbsig, zr(icontt))
!
!     CALCUL DU VECTEUR \INT BT*SIGMA_THERMIQUE
    call xbsig(option, ndim, nno, nfh, nfe,&
               ddlc, ddlm, igeom, compor, jpintt,&
               zi(jcnset), zi(jheavt), zi(jlonch), zr(jbaslo), zr(icontt),&
               nbsig, ideplm, zr(jlsn), zr(jlst), ivectu,&
               jpmilt, nfiss, jfisno)
!
!     POUR LES DDLS HEAVISIDE ENRICHIS A TORT
    call xteddl(ndim, nfh, nfe, ddls, nddl,&
                nno, nnos, zi(jstno), .false., lbid,&
                option, nomte, rbid, zr(ivectu), ddlm,&
                nfiss, jfisno)
!
!
end subroutine

subroutine te0541(option, nomte)
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
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/ltequa.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/xbsig.h"
#include "asterfort/xsigth.h"
#include "asterfort/xteddl.h"
#include "asterfort/xteini.h"
#include "asterfort/lteatt.h"
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
    integer :: jpmilt, ddlm, nfiss, jfisno, icontt
    integer :: nfh, ddlc, nfe, ibid, ddls, nbsig, nddl, jstno, jheavn, imate
    integer :: contac, nnom, singu, itab(1)
    integer :: iret, k, itemps
    aster_logical :: lbid
    character(len=8) :: enr, elref
    character(len=16) :: compor(4)
! ----------------------------------------------------------------------
!
!     CARACTERISTIQUES DU TYPE D'ELEMENT : GEOMETRIE ET INTEGRATION
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
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
!     RECUPERATION DES CHAMPS IN ET OUT
    call tecach('ONO', 'PCOMPOR', 'L', iret, iad=itab(1))
    do k = 1, 4
        if (iret .eq. 0) then
            compor(k) = zk16(itab(1)-1+k)
        else
            compor(k) = ' '
        endif
    end do
!     X-FEM AVEC VARC TEMP UNIQUEMENT EN HPP
    ASSERT(compor(3).eq.'PETIT' .or. compor(3).eq.' ')
!
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
    call teattr('S', 'XFEM', enr, ibid)
    if (enr(1:2).eq.'XH') call jevech('PHEA_NO', 'L', jheavn)
    if ((ibid.eq.0) .and. ltequa(elref,enr))&
    call jevech('PPMILTO', 'L', jpmilt)
    if (nfiss .gt. 1) call jevech('PFISNO', 'L', jfisno)
    if (nfe.gt.0) call jevech('PMATERC', 'L', imate)
!
!     CALCUL DES CONTRAINTES THERMIQUES
    call xsigth(ndim, zi(jlonch), zr(itemps), nbsig, zr(icontt))
!
!     CALCUL DU VECTEUR \INT BT*SIGMA_THERMIQUE
    call xbsig(ndim, nno, nfh, nfe, ddlc,&
               ddlm, igeom, compor, jpintt, zi(jcnset),&
               zi(jheavt), zi(jlonch), zr(jbaslo), zr(icontt), nbsig,&
               ibid, zr(jlsn), zr(jlst), ivectu, jpmilt,&
               nfiss, jheavn, jstno, imate)
!
!     POUR LES DDLS HEAVISIDE ENRICHIS A TORT
    call xteddl(ndim, nfh, nfe, ddls, nddl,&
                nno, nnos, zi(jstno), .false._1, lbid,&
                option, nomte, ddlm, nfiss, jfisno,&
                vect=zr(ivectu))
!
!
end subroutine

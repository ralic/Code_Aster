subroutine te0542(option, nomte)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/iselli.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/teattr.h"
#include "asterfort/terefe.h"
#include "asterfort/xbsig.h"
#include "asterfort/xbsir.h"
#include "asterfort/xbsir2.h"
#include "asterfort/xteddl.h"
#include "asterfort/xteini.h"
#include "asterfort/lteatt.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! FONCTION REALISEE:  CALCUL DES OPTION FORC_NODA ET REFE_FORC_NODA
!                     POUR LES ÉLÉMENTS MECA X-FEM
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano, igeom, ivectu
    integer :: jpintt, jcnset, jheavt, jlonch, jbaslo, icontm, jlsn, jlst
    integer :: jpmilt, ddlm, nfiss, jfisno, ideplm, icompo, jheavn
    integer :: nfh, ddlc, nfe, ibid, ddls, nbsig, nddl, jstno
    integer :: contac, nnom, singu
    aster_logical :: lbid
    real(kind=8) :: sigref(1), depref
    character(len=8) :: enr, elref
! DEB ------------------------------------------------------------------
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
    call elref1(elref)
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                nnom, ddls, nddl, ddlm, nfiss,&
                contac)
!
! ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
!      -----------------------------------------
    nbsig = nbsigm()
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PVECTUR', 'E', ivectu)
!
!     PARAMÈTRES PROPRES À X-FEM
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASLOR', 'L', jbaslo)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
!     PROPRE AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
    call teattr('S', 'XFEM', enr, ibid)
    if (enr(1:2).eq.'XH'.or. enr(1:2).eq.'XT') call jevech('PHEA_NO', 'L', jheavn)
    if ((ibid.eq.0) .and.&
        (enr.eq.'XH' .or.enr.eq.'XHT'.or.enr.eq.'XT'.or.enr.eq.'XHC') .and. .not.iselli(elref)) &
    call jevech('PPMILTO', 'L', jpmilt)
    if (nfiss .gt. 1) call jevech('PFISNO', 'L', jfisno)
!
    if (option .eq. 'FORC_NODA') then
!      --------------------
! VECTEUR SECOND MEMBRE DONNE EN ENTREE
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PSTANO', 'L', jstno)
!       CALCUL DU VECTEUR DES FORCES INTERNES (BT*SIGMA)
        call xbsig(ndim, nno, nfh, nfe, ddlc,&
                   ddlm, igeom, zk16( icompo), jpintt, zi(jcnset),&
                   zi(jheavt), zi(jlonch), zr(jbaslo), zr(icontm), nbsig,&
                   ideplm, zr(jlsn), zr(jlst), ivectu, jpmilt,&
                   nfiss, jheavn)
!
        call xteddl(ndim, nfh, nfe, ddls, nddl,&
                    nno, nnos, zi(jstno), .false._1, lbid,&
                    option, nomte, ddlm, nfiss, jfisno,&
                    vect=zr(ivectu))
!
    else if (option.eq.'REFE_FORC_NODA') then
!
! --- ON RECUPERE CONTRAINTE ET SAUT DE DEPLACEMENT DE REFERENCE
!
        call terefe('SIGM_REFE', 'MECA_INTERFACE', sigref(1))
        call terefe('DEPL_REFE', 'MECA_INTERFACE', depref)
!
! --- ON COMMENCE PAR CALCULER LES CONTRIBUTIONS VOLUMIQUES
!
        call xbsir(ndim, nno, nfh, nfe, ddlc,&
                   ddlm, igeom, zk16(icompo), jpintt, zi(jcnset),&
                   zi(jheavt), zi(jlonch), zr(jbaslo), sigref, nbsig,&
                   ideplm, zr(jlsn), zr(jlst), ivectu, jpmilt,&
                   nfiss, jheavn)
!
! --- SI ELEMENT DE CONTACT, ON Y AJOUTE LES CONTRIBUTIONS SURFACIQUES
! --- NOTAMMENT CELLE POUR LES EQUATIONS DUALES
!
        if (enr .eq. 'XHC') then
            call xbsir2(elref, contac, ddlc, ddlm, ddls,&
                        igeom, jheavn, jlst, ivectu, singu,&
                        nddl, ndim, nfe, nfh, nfiss,&
                        nno, nnom, nnos, depref, sigref(1))
        endif
    else
        ASSERT(.false.)
    endif
! FIN ------------------------------------------------------------------
!
end subroutine

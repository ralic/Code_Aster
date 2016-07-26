subroutine te0536(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/jevech.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/xrigel.h"
#include "asterfort/xteddl.h"
#include "asterfort/xteini.h"
    character(len=16) :: option, nomte
!
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
!    - FONCTION REALISEE:  CALCUL DE L'OPTION "RIGI_MECA_GE" POUR LES
!                          ELEMENTS X-FEM
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    character(len=8) :: lag, enr, elrefp
    integer :: jgano, nno, npg, imatuu, ndim
    integer :: ipoids, ivf, idfde, igeom, icont
    integer :: nnos
    integer :: jpintt, jcnset, jheavt, jlonch, jbaslo, jlsn, jlst, jstno, jpmilt
    integer :: nfh, ddlc, nddl, nnom, nfe, ibid, ddls, ddlm, nfiss, jfisno
    integer :: jheavn, ncompn, heavn(27,5), jtab(7), iret, ifh, ino, imate
!
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
    call elref1(elrefp)
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!      FAMI='RIGI'
!     MATNS MAL DIMENSIONNEE
    ASSERT(nno.le.27)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, ibid, ddlc,&
                nnom, ddls, nddl, ddlm, nfiss,&
                ibid)
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTRR', 'L', icont)
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASLOR', 'L', jbaslo)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PSTANO', 'L', jstno)
    call teattr('S', 'XFEM', enr, ibid)
    if (ibid .eq. 0 .and. (enr.eq.'XH'.or.enr.eq.'XHC')&
        .and. .not.iselli(elrefp)) call jevech('PPMILTO', 'L', jpmilt)
    if (nfiss .gt. 1) call jevech('PFISNO', 'L', jfisno)
!   IL Y A UN PB DANS LA LECTURE DU MATERIAU POUR CETTE OPTION
!     ON PREND POUR LE MOMENT UN MATERIAU ARBITRAIRE
     imate=0
!    if (nfe.gt.0) call jevech('PMATERC', 'L', imate)
!
    call jevech('PMATUUR', 'E', imatuu)
!
!     RECUPERATION DE LA DEFINITION DES DDLS HEAVISIDES
    if (enr(1:2).eq.'XH') then
      call jevech('PHEA_NO', 'L', jheavn)
      call tecach('OOO', 'PHEA_NO', 'L', iret, nval=7,&
                itab=jtab)
       ncompn = jtab(2)/jtab(3)
       ASSERT(ncompn.eq.5)
       do ino = 1, nno
         do ifh = 1 , ncompn
           heavn(ino,ifh) = zi(jheavn-1+ncompn*(ino-1)+ifh)
         enddo
       enddo
    endif
!   PRECAUTION :: ON BLOQUE LE MULTIHEAVISIDE POUR L INSTANT CAR FISNO DOIT ETRE 
!                   PRIS EN COMPTE POUR UTILISER HEAVN => A FAIRE
    ASSERT(nfiss.eq.1)
!
    call xrigel(nno, nfh*ndim, nfe, ddlc,&
                igeom, jpintt, zi(jcnset), zi(jheavt), zi(jlonch),&
                zr(jbaslo), zr(jlsn), zr(jlst), zr(icont), zr(imatuu),&
                jpmilt, heavn, jstno, imate)
!
!
!     SUPPRESSION DES DDLS SUPERFLUS
    call teattr('C', 'XLAG', lag, ibid)
    if (ibid .eq. 0 .and. lag .eq. 'ARETE') then
        nno = nnos
    endif
    call xteddl(ndim, nfh, nfe, ddls, nddl,&
                nno, nnos, zi(jstno), .false._1, .true._1,&
                option, nomte, ddlm,&
                nfiss, jfisno, mat=zr(imatuu))
!
!
end subroutine

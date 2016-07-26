subroutine te0566(nomopt, nomte)
! aslint: disable=W0104
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/elrfvf.h"
#include "asterfort/indent.h"
#include "asterfort/iselli.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/xcalc_heav.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalfev_wrap.h"
#include "asterfort/xkamat.h"
#include "asterfort/lteatt.h"
#include "asterfort/xteini.h"
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: patrick.massin at edf.fr
!
!    - FONCTION REALISEE:  CALCUL DU CHAMP DE DEPLACEMENT RECOMPOSE AUX
!                          POINTS DE GAUSS DES SOUS-ELEMENTS X-FEM
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!     ------------------------------------------------------------------
    character(len=16) :: nomte, nomopt
!
    integer :: nbnomx, nfissmx
    parameter (nbnomx=27, nfissmx=4)
!
    integer :: ndim, nnop, nnops, nspg, irese, nse
    integer :: nfh, nfe, singu, ddlc, nnom, ddls, nddl, ddlm, ddlg
    integer :: nfiss, contac
    integer :: jdeplno, jxpg, jheavt, jlonch, jbaslo, jlsn, jlst, jheavn, jdeplpg, jstno, imate
    integer :: jtab(7), jtab2(7), iret, ncomp, ncompn
    integer :: ise, kpg, ipg, i, ino, ifiss, ifh, alp, igeom
    integer :: he(nfissmx), dec(nbnomx), heavn(nbnomx, 5)
    integer :: hea_se
    integer :: iadzi, iazk24
    real(kind=8) :: xg(3), ug(3)
    real(kind=8) :: ff(nbnomx)
    real(kind=8) :: fk(27,3,3)
    character(len=8) :: elrese(6), fami(6), elrefp
    real(kind=8) :: ka, mu
    aster_logical :: axi
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','T10'/
    data    fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!     ------------------------------------------------------------------
!
!
    call tecael(iadzi, iazk24)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                nnom, ddls, nddl, ddlm, nfiss,&
                contac)
    axi=lteatt('AXIS','OUI')
!   champs IN
    call jevech('PDEPLNO', 'L', jdeplno)
    call jevech('PXFGEOM', 'L', jxpg)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASLOR', 'L', jbaslo)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    if (nfh.gt.0) call jevech('PHEA_NO', 'L', jheavn)
    if (nfe.gt.0) then 
       call jevech('PSTANO', 'L', jstno)
       call jevech('PMATERC', 'L', imate)
       call jevech('PGEOMER', 'L', igeom)
    endif
!   champ OUT
    call jevech('PDEPLPG', 'E', jdeplpg)
    call tecach('OOO', 'PDEPLPG', 'E', iret, nval=7,&
                itab=jtab2)
!
!     NOMBRE DE COMPOSANTES DE PHEAVTO (DANS LE CATALOGUE)
    call tecach('OOO', 'PHEAVTO', 'L', iret, nval=2,&
                itab=jtab)
    ncomp = jtab(2)
!
!   carateristisques de l'element parent
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nnop, nnos=nnops)
    call elref1(elrefp)
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NPG
    if (.not.iselli(elrefp)) then
        irese=3
    else
        irese=0
    endif
    call elrefe_info(elrefe=elrese(ndim+irese),fami=fami(ndim+irese),&
                     npg=nspg)
!
!     RECUPERATION DE LA DEFINITION DES DDL HEAVISIDES
    if (nfh.gt.0) then
       call tecach('OOO', 'PHEA_NO', 'L', iret, nval=7,&
                 itab=jtab)
       ncompn = jtab(2)/jtab(3)
       ASSERT(ncompn.eq.5)
       do ino = 1, nnop
          do ifh = 1 , ncompn
             heavn(ino,ifh) = zi(jheavn-1+ncompn*(ino-1)+ifh)
          enddo
       enddo
    endif
!
! DECALAGES CALCULES EN AMONT: PERF
!
    do ino = 1, nnop
        call indent(ino, ddls, ddlm, nnops, dec(ino))
    end do
!
!   calcul du nombre de ddl par point de Gauss, pour le champ OUT
    ddlg=jtab2(2)/jtab2(3)
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=zi(jlonch-1+1)
!
!       BOUCLE SUR LES NSE SOUS-ELEMENTS
    do ise = 1, nse
!
!       FONCTION HEAVISIDE CSTE POUR CHAQUE FISSURE SUR LE SS-ELT
       he=0
       do ifiss = 1, nfiss
           he(ifiss) = zi(jheavt - 1 + ncomp*(ifiss - 1) + ise)
       end do
!                   
!       CALCUL DE L IDENTIFIANT DU SS ELEMENT
       hea_se=xcalc_code(nfiss, he_inte=[he])
!                   
!       BOUCLE SUR LES POINTS DE GAUSS
       do kpg = 1, nspg
!         calcul de l'indice du point de Gauss courant dans la liste des
!         points de Gausse de la famille XFEM
          ipg=nspg*(ise-1)+kpg
!         recuperation des coordonnees du point de Gauss courant
          xg=0.d0
          do i=1, ndim
             xg(i)=zr(jxpg - 1 + ndim*(ipg - 1) + i)
          enddo
!
!         evaluation des fonctions de forme au point de Gauss courant
          call elrfvf(elrefp, xg, nbnomx, ff, nnop)         
!
!       FONCTION D'ENRICHISSEMENT AU POINT DE GAUSS ET LEURS DÉRIVÉES
        if (nfe .gt. 0) then
            call xkamat(zi(imate), ndim, axi, ka, mu)
            call xcalfev_wrap(ndim, nnop, zr(jbaslo), zi(jstno), real(he(1),8),&
                         zr(jlsn),  zr(jlst), zr(igeom), ka, mu, ff, fk)
        endif
!
!         recomposition du champ de deplacement au point de Gauss courant
          ug=0.d0
          do ino = 1, nnop
!            ddl classiques
             do i=1, ndim
                ug(i) = ug(i) + ff(ino)*zr(jdeplno - 1 + dec(ino) + i)
             enddo
!            ddl Heaviside
             do ifh = 1, nfh
                do i=1, ndim
                   ug(i) = ug(i) + ff(ino)*zr(jdeplno-1 + dec(ino) + ndim*ifh + i)&
                                          *xcalc_heav(heavn(ino,ifh),hea_se,heavn(ino,5))
                enddo
             enddo
!            ddl crack-tip
             do alp=1, ndim*nfe
                 do i=1, ndim
                   ug(i) = ug(i) + fk(ino,alp,i)*zr(jdeplno-1+dec(ino)+ndim*(nfh+1)+alp)
                 enddo
             enddo
          enddo
!
!         stockage du champ de deplacement recompose (DX, DY, DZ)
          do i=1, ndim
             zr(jdeplpg - 1 + ddlg*(ipg-1) + i) = ug(i)
          enddo
       enddo
!
    enddo
!
!
end subroutine

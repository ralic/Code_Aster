subroutine te0559(option, nomte)
    implicit none
!
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
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
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN FLUX INJECTE DANS UNE FISSURE
!          POUR UN MODELE COUPLE HM-XFEM
!
!          OPTION : 'CHAR_MECA_FLUX_F'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/confac.h"
#include "asterfort/dismoi.h"
#include "asterfort/elelin.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/elrfvf.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/reeref.h"
#include "asterfort/teattr.h"
#include "asterfort/tecael.h"
#include "asterfort/vecini.h"
#include "asterfort/xelrex.h"
#include "asterfort/xhmini.h"
#include "asterfort/xinter.h"
#include "asterfort/xmifis.h"
#include "jeveux.h"
!
    character(len=8) :: elrefp, elrefc, enr, enr2
    character(len=16) :: nomte, option
    integer :: ndim, nnop, nnops, iret, nfiss, nfh, ddld, ddlp, ddlm, ddlc
    integer :: jlsn, itemps, ifluxf, igeom, jheano, ires, nbnomx
    integer :: iadzi, iazk24, i, ndime, j, k, ia , ib, im, n(3), ifiss
    integer :: nbf, f(6,8), ibid2(12,3), ibid, contac, nddls
    integer :: nbar, ar(12,3), ninter, exit(2), nlag
    integer :: nnof, npgf, ipoidf, ivff, idfdef, ipgf, lact(4), nlact
    real(kind=8) :: lsna, lsnb, lsnm, minlsn, inref(3) , inter(3)
    real(kind=8) :: tplus, deltat, valpar(4), flux, pinref(9), lsn(27)
    real(kind=8) :: ff(27), pinter(9), miref(3), mifis(3), rbid(1)
    real(kind=8) :: dxdk, dydk, dzdk, jac, xg(3), xe(3), phi, x(81)
    character(len=8) :: typma, typfa, nompar(4), elc, fpg
    parameter   (nbnomx = 27)
    aster_logical :: pre1
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
!
    call jemarq()
!     ELEMENT DE REFERENCE PARENT
    call elref1(elrefp)
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nnop,nnos=nnops)
    call xelrex(elrefp, nnop, x)
!
    ASSERT(ndim.eq.2.or.ndim.eq.3)
!
    call teattr('S', 'XFEM', enr, iret)
    ASSERT(enr(1:2).eq. 'XH')
!
    call teattr('C', 'MODTHM', enr2, iret)
    pre1=(iret.eq.0)
    ASSERT(pre1)
! 
    ASSERT(option(1:16).eq.'CHAR_MECA_FLUX_F')
!
    if (enr(3:3).ne.'C' .and. enr(4:4).ne.'C') go to 100
!
    call jevech('PLSN', 'L', jlsn)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PFLUXF', 'L', ifluxf)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ires)
!
    call xhmini(nomte, nfh, ddld, ddlm, ddlp, nfiss, ddlc, contac)
    ndime=ndim
    nddls=ddld+ddlp+ddlc
    call elelin(contac, elrefp, elrefc, ibid, ibid)
    if (contac.eq.3) then
       nlag = 1
    else if (contac.eq.2) then
       nlag = 3
    else
       ASSERT(.false.)
    endif
!
    if (nfiss .gt. 1) then
       call jevech('PHEAVNO', 'L', jheano)
    endif
!
    call tecael(iadzi, iazk24, noms=0)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
!
    do ifiss = 1, nfiss
       call vecini(27, 0.d0, lsn)
       do i = 1, nnop
          lsn(i) = zr(jlsn-1+(i-1)*nfiss+ifiss)
       end do
       call vecini(3, 0.d0, inter)
       call vecini(3, 0.d0, inref)
       if (ndim.eq.2) then
          call conare(typma, ar, nbar)
          do i = 1, nbar
             ia = ar(i,1)
             ib = ar(i,2)
             lsna = lsn(ia)
             lsnb = lsn(ib)
             minlsn = min(lsna,lsnb)
             if (lsna*lsnb.le.0.d0 .and. minlsn.lt.0.d0) then
!         L'ARETE EST COUPEE, ON CHERCHE LA POSTION DU POINT D'INTERSECTION AVEC
!         LSN=0
                im = ar(i,3)
                lsnm=lsn(im)
                if (lsna.eq.0.d0) then
                   do j = 1, ndim
                      inter(j) = zr(igeom-1+(ia-1)*ndim+j)
                      inref(j) = x((ia-1)*ndim+j)
                   end do
                elseif (lsnb.eq.0.d0) then
                   do j = 1, ndim
                      inter(j) = zr(igeom-1+(ib-1)*ndim+j)
                      inref(j) = x((ib-1)*ndim+j)
                   end do
                elseif (lsnm.eq.0.d0) then
                   do j = 1, ndim
                      inter(j) = zr(igeom-1+(im-1)*ndim+j)
                      inref(j) = x((im-1)*ndim+j)
                   end do
                else
                   call xinter(ndim, ndime, elrefp, zr(igeom), lsn, ia, ib,&
                               im, rbid, rbid, lsna, lsnb, lsnm, inref, inter)
                endif
!         ON EVALUE LA FONCTION AU POINT D'INTERSECTION
                tplus = zr(itemps)
                deltat = zr(itemps+1)
                nompar(1) = 'X'
                valpar(1) = inter(1)
                nompar(2) = 'Y'
                valpar(2) = inter(2)
                nompar(3) = 'INST'
                valpar(3) = tplus
                call fointe('FM', zk8(ifluxf+0), 3, nompar, valpar,&
                            flux, iret)
!         SI LE FLUX EST POSITIF, ON MET LES CLS SUR CETTE ARETE
                if (flux.gt.0.d0) then
                   call elrfvf(elrefc, inref, nbnomx, ff, nnops)
                   if (nfiss.gt.1) then
                      zr(ires-1+(ia-1)*nddls+(ndim+1)*(1+nfh)+(zi(jheano-1+(ia-1)*nfiss+ifiss)-1)&
                      *(nlag*ndim+3)+1)=zr(ires-1+(ia-1)*nddls+(ndim+1)*(1+nfh)+(zi(jheano-1+&
                      (ia-1)*nfiss+ifiss)-1)*(nlag*ndim+3)+1)-flux*deltat*ff(ia)
                      zr(ires-1+(ib-1)*nddls+(ndim+1)*(1+nfh)+(zi(jheano-1+(ia-1)*nfiss+ifiss)-1)&
                      *(nlag*ndim+3)+1)=zr(ires-1+(ib-1)*nddls+(ndim+1)*(1+nfh)+(zi(jheano-1+&
                      (ia-1)*nfiss+ifiss)-1)*(nlag*ndim+3)+1)-flux*deltat*ff(ib)
                   else
                      zr(ires-1+(ia-1)*nddls+(ndim+1)*(1+nfh)+1)=&
                      zr(ires-1+(ia-1)*nddls+(ndim+1)*(1+nfh)+1)-flux*deltat*ff(ia)
                      zr(ires-1+(ib-1)*nddls+(ndim+1)*(1+nfh)+1)=&
                      zr(ires-1+(ib-1)*nddls+(ndim+1)*(1+nfh)+1)-flux*deltat*ff(ib)
                   endif
                   go to 200
                endif
             endif
          end do
       elseif(ndim.eq.3) then
          call confac(typma, ibid2, ibid, f, nbf, quad='OUI')
          do i = 1, nbf
             typfa = 'TRIA6'
             ninter = 0
             lact(1:4) = 0
             if (f(i,7).gt.0.d0) typfa = 'QUAD8'
             call conare(typfa, ar, nbar)
             do k = 1, nbar
                ia  = f(i,ar(k,1))
                ib  = f(i,ar(k,2))
                lsna = lsn(ia)
                lsnb = lsn(ib)
                if (lsna.eq.0.d0) then
                   ninter = ninter+1
                   lact(ar(k,1))=1
                   do j = 1, ndim
                      pinter((ninter-1)*ndim+j)=zr(igeom-1+(ia-1)*ndim+j)
                      pinref((ninter-1)*ndim+j)=x((ia-1)*ndim+j)
                   end do
                elseif (lsna*lsnb.lt.0.d0) then
                   ninter = ninter+1
                   lact(ar(k,1))=1
                   lact(ar(k,2))=1
                   im = f(i,ar(k,3))
                   lsnm=lsn(im)
                   call xinter(ndim, ndime, elrefp, zr(igeom), lsn, ia, ib,&
                               im, rbid, rbid, lsna, lsnb, lsnm, inref, inter)
                   do j = 1, ndim
                      pinter((ninter-1)*ndim+j)=inter(j)
                      pinref((ninter-1)*ndim+j)=inref(j)
                   end do
                endif
             end do
             ASSERT(ninter.eq.0 .or. ninter.eq.1 .or. ninter.ge.2)
             if (ninter.eq.2) then
!         LA FACE EST COUPEE, ON EVALUE LA FONCTION AUX POINTS D'INTERSECTION
                tplus = zr(itemps)
                deltat = zr(itemps+1)
                nompar(1) = 'X'
                nompar(2) = 'Y'
                nompar(3) = 'Z'
                nompar(4) = 'INST'
                valpar(4) = tplus
                do k = 1, 2
                   valpar(1) = pinter((k-1)*ndim+1)
                   valpar(2) = pinter((k-1)*ndim+2)
                   valpar(3) = pinter((k-1)*ndim+3)
                   call fointe('FM', zk8(ifluxf+0), 4, nompar, valpar,&
                               flux, iret)
                   if (flux.le.0.d0) go to 300
                end do
!         LE FLUX EST POSITIF, ON MET LES CLS SUR CETTE FACE
!         ON CHERCHE LA POSTION DU POINT MILIEU SUR LA COURBE LSN=0
                n(1) = f(i,1)
                n(2) = f(i,2)
                n(3) = f(i,3)
                exit(1:2) = 0
                call xmifis(ndim, ndime, elrefp, zr(igeom), lsn,&
                            n, 1, 2, pinref, miref, mifis,rbid, exit, .true._1)
                do j = 1, ndim
                   pinter(2*ndim+j)=mifis(j)
                   pinref(2*ndim+j)=miref(j)
                end do
                elc = 'SE3'
                fpg = 'MASS'
                call elrefe_info(elrefe=elc,fami=fpg,nno=nnof,&
                                 npg=npgf,jpoids=ipoidf,jvf=ivff,jdfde=idfdef)
!         BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
                jac = 0.d0
                do ipgf = 1, npgf
                   dxdk = 0.d0
                   dydk = 0.d0
                   dzdk = 0.d0
                   call vecini(ndim, 0.d0, xg)
                   do k = 1, nnof
                      dxdk = dxdk + pinter(ndim*(k-1)+1)*zr(idfdef-1+(ipgf-1)*nnof+k)
                      dydk = dydk + pinter(ndim*(k-1)+2)*zr(idfdef-1+(ipgf-1)*nnof+k)
                      dzdk = dzdk + pinter(ndim*(k-1)+3)*zr(idfdef-1+(ipgf-1)*nnof+k)
                      do j = 1, ndim
                         xg(j) = xg(j)+pinter(ndim*(k-1)+j)*zr(ivff-1+(ipgf-1)*nnof+k)
                      end do
                   end do
                   jac = sqrt(dxdk**2+dydk**2+dzdk**2)*zr(ipoidf-1+ipgf)
                   call reeref(elrefc, nnops, zr(igeom), xg, ndim, xe, ff)
                   nlact = 0
                   do k = 1, nbar
                      nlact = nlact +lact(k)
                   end do
                   phi = 0.d0
                   do k = 1, nbar
                      if (lact(k).eq.0) then
                         phi = phi+ff(f(i,k))/nlact
                      endif
                   end do
                   do k = 1, nbar
                      if (nfiss.gt.1) then
                         zr(ires-1+(f(i,k)-1)*nddls+(ndim+1)*(1+nfh)+(zi(jheano-1+(f(i,k)-1)&
                         *nfiss+ifiss)-1)*(nlag*ndim+3)+1)=zr(ires-1+(f(i,k)-1)*nddls+(ndim+1)*&
                         (1+nfh)+(zi(jheano-1+(f(i,k)-1)*nfiss+ifiss)-1)*(nlag*ndim+3)+1)-&
                         flux*deltat*lact(k)*(ff(f(i,k))+phi)*jac
                      else
                         zr(ires-1+(f(i,k)-1)*nddls+(ndim+1)*(1+nfh)+1)=&
                         zr(ires-1+(f(i,k)-1)*nddls+(ndim+1)*(1+nfh)+1)-&
                         flux*deltat*lact(k)*(ff(f(i,k))+phi)*jac
                      endif
                   end do
                end do
                go to 200
             endif
300          continue
          end do
       endif
!
200 continue
!
    end do
!
100 continue
!
    call jedema()
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
end subroutine

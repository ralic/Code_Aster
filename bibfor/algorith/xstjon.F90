subroutine xstjon(elrefp, ndim, joncno, jlsn, igeom, nfiss, nfisc, fisco, nnops,&
                  txlsn, n, c)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "blas/ddot.h"
#include "asterfort/conare.h"
#include "asterfort/confac.h"
#include "asterfort/provec.h"
#include "asterfort/reeref.h"
#include "asterfort/tecael.h"
#include "asterfort/vecini.h"
#include "asterfort/xelrex.h"
#include "asterfort/xnormv.h"
    character(len=8) :: elrefp
    integer :: ndim, joncno, jlsn, nfiss, nfisc, nnops, fisco(*), igeom
    real(kind=8) :: txlsn(28)
    integer, intent(in), optional :: n
    real(kind=8), intent(in), optional :: c(ndim)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!             DETERMINER LES NOEUDS SOMMETS DE L'ELEMENT PARENT
!             QUI VOIENT UNE JONCTION (POUR LE CONTACT MULTI)
!                      
!     ENTREE  N     : NUMERO DU NOEUD DANS L'ELEMENT PARENT SI LE POINT
!                     D'INTERSECTION EST CONFONDU AVEC UN NOEUD
!             C     : COORDONNES DE REFERENCE DU POINT D'INTERSECTION
!
!     SORTIE  PTJONC: LE POINT CONSIDERE EST-IL UN POINT DE JONCTION?
!
!---------------------------------------------------------------------
!
    integer :: i, iadzi, iazk24, ft(12, 3), nbft, f(6, 8), nbf
    integer :: ar(12, 3), nbar, iar, nno, ino
    real(kind=8) :: xref(81), u(3), v(3), w(3), norme, normal(3), cridist
    real(kind=8) :: cref(ndim), ff(27), val
    character(len=8) :: typma
    aster_logical :: jonc, arete , face
    parameter(cridist=1.d-8)
!
!---------------------------------------------------------------------
!     DEBUT
!---------------------------------------------------------------------
!
    call tecael(iadzi, iazk24, noms=0)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
    call xelrex(elrefp, nno, xref)
!
    jonc = .false.
    arete = .false.
    face = .false.
!
    if (present(n)) then
       if (n.le.nnops) then
          do i = 1, nfisc
             if (zr(jlsn-1+(n-1)*nfiss+fisco(2*i-1)).eq.0.d0) then
                zi(joncno-1+n) = 1
             endif
          end do
       elseif (n.le.nno) then
          call conare(typma, ar, nbar)
          do i = 1, nfisc
             if (zr(jlsn-1+(n-1)*nfiss+fisco(2*i-1)).eq.0.d0) then
                do iar = 1, nbar
                   if (n.eq.ar(iar,3)) then
                      zi(joncno-1+ar(iar,1)) = 1
                      zi(joncno-1+ar(iar,2)) = 1
                   endif
                end do
             endif
          end do
       elseif (n.gt.nno .and. n.lt.1000) then
          do i = 1, nfisc
             if (txlsn((n-nno-1)*nfiss+fisco(2*i-1)).eq.0.d0) then
                zi(joncno-1+n) = 1
             endif
          end do
       elseif (n.lt.2000) then
          call reeref(elrefp, nno, zr(igeom), c, ndim,&
                      cref, ff)
          do i = 1, nfisc
             val = 0.d0
             do ino = 1, nno
                val = val + zr(jlsn-1+(ino-1)*nfiss+fisco(2*i-1))*ff(ino)
             end do
             if (abs(val).lt.cridist) jonc = .true.
          end do
       elseif (n.lt.3000) then
          call reeref(elrefp, nno, zr(igeom), c, ndim,&
                      cref, ff)
          do i = 1, nfisc
             val = 0.d0
             do ino = 1, nno
                val = val + zr(jlsn-1+(ino-1)*nfiss+fisco(2*i-1))*ff(ino)
             end do
             if (abs(val).lt.cridist) jonc = .true.
          end do
       endif
    else
       jonc = .true.
    endif
!
    if (jonc) then
       call conare(typma, ar, nbar)
       do iar = 1, nbar
          call vecini(3, 0.d0, u)
          call vecini(3, 0.d0, v)
          do i = 1, ndim
             u(i) = xref((ar(iar,2)-1)*ndim+i)-xref((ar(iar,1)-1)*ndim+i)
             v(i) = c(i)-xref((ar(iar,1)-1)*ndim+i)
          end do
          call xnormv(ndim, u, norme)
          call xnormv(ndim, v, norme)
          call provec(u, v, normal)
          call xnormv(ndim, normal, norme)
          if (norme.lt.cridist) then
             zi(joncno-1+ar(iar,1)) = 1
             zi(joncno-1+ar(iar,2)) = 1
             arete = .true.
          endif
       end do
       if (ndim.eq.3) then
          call confac(typma, ft, nbft, f, nbf)
          do iar = 1, nbf
             call vecini(3, 0.d0, u)
             call vecini(3, 0.d0, v)
             do i = 1, ndim
                u(i) = xref((f(iar,2)-1)*ndim+i)-xref((f(iar,1)-1)*ndim+i)
                v(i) = xref((f(iar,3)-1)*ndim+i)-xref((f(iar,1)-1)*ndim+i)
                w(i) = c(i)-xref((f(iar,1)-1)*ndim+i)
             end do
             call xnormv(ndim, u, norme)
             call xnormv(ndim, v, norme)
             call xnormv(ndim, w, norme)
             call provec(u, v, normal)
             call xnormv(ndim, normal, norme)
             if (abs(ddot(ndim,w,1,normal,1)).lt.cridist) then
                zi(joncno-1+f(iar,1)) = 1
                zi(joncno-1+f(iar,2)) = 1
                zi(joncno-1+f(iar,3)) = 1
                if (f(iar,4).ge.1) then
                   zi(joncno-1+f(iar,4)) = 1
                endif
                face = .true.
             endif
          end do
       endif
    endif
!
    if (jonc .and. .not.arete .and. .not.face) then
       do i  = 1, nnops
          zi(joncno-1+i) = 1
       end do
    endif
!---------------------------------------------------------------------
!     FIN
!---------------------------------------------------------------------
end subroutine

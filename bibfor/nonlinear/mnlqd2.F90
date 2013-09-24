subroutine mnlqd2(ind, imat, neq, ninc, nd,&
                  nchoc, h, hf, parcho, xcdl,&
                  adime, xvect, xtemp)
! aslint: disable=W1306

    implicit none
!
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
! ----------------------------------------------------------------------
!
!     MODE_NON_LINE -- MATRICE JACOBIENNE (Q(V,E_I))
!     -    -                -            -   -
! ----------------------------------------------------------------------
!
! CALCUL PARTIELLE DE  LA MATRICE JACOBIENNE POUR UN CERTAIN
!                                                       VECTEUR SOLUTION
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
!
#include "jeveux.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mnlaft.h"
#include "asterfort/mrmult.h"
    integer :: ind, imat(2), neq, ninc, nd, nchoc, h, hf
    character(len=14) :: parcho, xcdl, adime, xvect, xtemp
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    real(kind=8) :: q1(ninc-1), temp1(neq), temp2(neq), temp3(2*hf+1)
    real(kind=8) :: temp4(2*hf+1), jeu, alpha, coef
    integer :: iddl, i, nddl, iraid, inddl, ijmax
    integer :: ijeu, icdl, iadim, itemp, k, ivec, nt, ih, puismax
    integer :: ineqs, ityp, neqs, deb, hind, ddl, nddlx, nddly
    logical :: stp
!
    call jemarq()
!
    puismax=int(dlog(4.d0*dble(hf)+1.d0)/dlog(2.d0)+1.d0)
    nt = 2**puismax
    q1(1:ninc-1)=0.d0
    temp1(1:neq)=0.d0
    temp2(1:neq)=0.d0
    temp3(1:2*hf+1)=0.d0
    temp4(1:2*hf+1)=0.d0
    stp=.true.
!
    call jeveuo(parcho//'.RAID', 'L', iraid)
    call jeveuo(parcho//'.NDDL', 'L', inddl)
    call jeveuo(parcho//'.JEU', 'L', ijeu)
    call jeveuo(parcho//'.JEUMAX', 'L', ijmax)
    call jeveuo(parcho//'.NEQS', 'L', ineqs)
    call jeveuo(parcho//'.TYPE', 'L', ityp)
    call jeveuo(xcdl, 'L', icdl)
    call jeveuo(adime, 'L', iadim)
    call jeveuo(xvect, 'L', ivec)
    call jeveuo(xtemp, 'E', itemp)
    call dscal(ninc-1, 0.d0, zr(itemp), 1)
! ----------------------------------------------------------------------
! --- INCONNUE DU SYSTEME DYNAMIQUE i.e. ND+1:ND*(2*H+1)
! ----------------------------------------------------------------------
    if (ind .le. nd*(2*h+1) .and. ind .gt. nd) then
        ih=int((ind-1)/nd)
        iddl=ind-nd*int((ind-1)/nd)
        i=0
        do 10 k = 1, neq
            if (zi(icdl-1+k) .eq. 0) then
                i=i+1
                if (i .eq. iddl) then
                    temp1(k)=1.d0
                endif
            endif
10      continue
        call mrmult('ZERO', imat(2), temp1, temp2, 1,&
                    .false.)
        i=0
        do 20 k = 1, neq
            if (zi(icdl-1+k) .eq. 0) then
                i=i+1
                if (ih .le. h) then
                    coef=real(ih)*real(ih)
                else
                    coef=real(ih-h)*real(ih-h)
                endif
                q1(ih*nd+i)=-zr(ivec-1+ninc-2)*coef*temp2(k)/zr(iadim+&
                1)
            endif
20      continue
        if (ih .le. h) then
            q1((h+ih)*nd+iddl)=real(ih)*zr(ivec-1+ninc-3)
        else
            q1((ih-h)*nd+iddl)=-real(ih)*zr(ivec-1+ninc-3)
        endif
    endif
! ----------------------------------------------------------------------
! --- EQUATIONS SUPPLEMENTAIRES
! ----------------------------------------------------------------------
    neqs=0
    deb=nd*(2*h+1)
    do 60 i = 1, nchoc
        alpha=zr(iraid-1+i)/zr(iadim-1+1)
        jeu=zr(ijeu-1+i)/zr(ijmax)
        if (zk8(ityp-1+i)(1:7) .eq. 'BI_PLAN') then
            nddl=zi(inddl-1+6*(i-1)+1)
            if ((ind.le.nd*(2*h+1)) .or. ((ind.gt.deb).and.(ind.le.(deb+ (2*hf+1))))) then
! ---     (F/ALPHA-XG))
                call dscal(2*hf+1, 0.d0, temp4, 1)
                call dcopy(2*hf+1, zr(ivec-1+deb+1), 1, temp4, 1)
                call dscal(2*hf+1, 1.d0/alpha, temp4, 1)
                call daxpy(h+1, -1.d0/jeu, zr(ivec-1+nddl), nd, temp4,&
                           1)
                call daxpy(h, -1.d0/jeu, zr(ivec-1+nd*(h+1)+nddl), nd, temp4(hf+2),&
                           1)
            endif
            if (ind .le. nd*(2*h+1)) then
                hind=int((ind-1)/nd)
!            WRITE(6,*) 'HIND',HIND
                ddl=ind-nd*hind
! ---     -(F/ALPHA-XG)*(F/ALPHA-XG))
                if (ddl .eq. nddl) then
                    call dscal(2*hf+1, 0.d0, temp3, 1)
                    if (hind .le. h) then
                        temp3(hind+1)=-1.d0/jeu
                    else
                        temp3(hf+1+hind-h)=-1.d0/jeu
                    endif
!              WRITE(6,*) 'TEMP3',TEMP3(1:2*HF+1)
!              WRITE(6,*) 'TEMP4',TEMP4(1:2*HF+1)
                    call mnlaft(temp4, temp3, hf, nt,&
                                q1(deb+(2*hf+1)+1))
                    call dscal(2*hf+1, -1.d0, q1(deb+(2*hf+1)+1), 1)
!              WRITE(6,*) 'Q1',IND,DEB,Q1(DEB+(2*HF+1)+1:DEB+2*(2*HF+1))
                endif
            else if ((ind.gt.deb).and.(ind.le.(deb+(2*hf+1)))) then
! ---     -(F/ALPHA-XG)*(F/ALPHA-XG))
                call dscal(2*hf+1, 0.d0, temp3, 1)
                temp3(ind-deb)=1.d0/alpha
                call mnlaft(temp4, temp3, hf, nt,&
                            q1(deb+(2*hf+1)+1))
                call dscal(2*hf+1, -1.d0, q1(deb+(2*hf+1)+1), 1)
                else if((ind.gt.(deb+2*hf+1).and.ind.le.(deb+4*hf+2)))&
            then
! ---     -F*Z
                call dscal(2*hf+1, 0.d0, temp3, 1)
                call dscal(2*hf+1, 0.d0, temp4, 1)
                temp3(ind-deb-(2*hf+1))=-1.d0
                call dcopy(2*hf+1, zr(ivec-1+deb+1), 1, temp4, 1)
                call mnlaft(temp4, temp3, hf, nt,&
                            q1(deb+1))
            endif
        else if (zk8(ityp-1+i)(1:6).eq.'CERCLE') then
            nddlx=zi(inddl-1+6*(i-1)+1)
            nddly=zi(inddl-1+6*(i-1)+2)
            if (ind .le. nd*(2*h+1)) then
                hind=int((ind-1)/nd)
                ddl=ind-nd*hind
                if ((ddl.eq.nddlx) .or. (ddl.eq.nddly)) then
                    call dscal(2*hf+1, 0.d0, temp4, 1)
                    if (hind .le. h) then
                        temp4(hind+1)=1.d0/jeu
                    else
                        temp4(hf+1+hind-h)=1.d0/jeu
                    endif
! ---         FX*R - FN*([UX]/JEU)
! ---         FY*R - FN*([UY]/JEU)
                    call dscal(2*hf+1, 0.d0, temp3, 1)
                    call dcopy(2*hf+1, zr(ivec+deb+3*(2*hf+1)), 1, temp3, 1)
                    if (ddl .eq. nddlx) then
                        call mnlaft(temp3, temp4, hf, nt,&
                                    q1(deb+1))
                        call dscal(2*hf+1, -1.d0, q1(deb+1), 1)
                    else if (ddl.eq.nddly) then
                        call mnlaft(temp3, temp4, hf, nt,&
                                    q1(deb+(2*hf+ 1)+1))
                        call dscal(2*hf+1, -1.d0, q1(deb+(2*hf+1)+1), 1)
                    endif
! ---         R*R - ([UX]/JEU)^2 - ([UY]/JEU)^2
                    call dscal(2*hf+1, 0.d0, temp3, 1)
                    call dcopy(h+1, zr(ivec-1+ddl), nd, temp3, 1)
                    call dcopy(h, zr(ivec-1+nd*(h+1)+ddl), nd, temp3(hf+ 2), 1)
                    call dscal(2*hf+1, 1.d0/jeu, temp3, 1)
                    call mnlaft(temp3, temp4, hf, nt,&
                                q1(deb+2*(2*hf+1)+ 1))
                    call dscal(2*hf+1, -1.d0, q1(deb+2*(2*hf+1)+1), 1)
                endif
                else if(ind.gt.deb+2*(2*hf+1).and.ind.le.deb+3*(2*hf+1))&
            then
                call dscal(2*hf+1, 0.d0, temp4, 1)
                temp4(ind-deb-2*(2*hf+1))=1.d0
! ---       FX*[R] - FN*(UX/JEU)
                call dscal(2*hf+1, 0.d0, temp3, 1)
                call dcopy(2*hf+1, zr(ivec+deb), 1, temp3, 1)
                call mnlaft(temp3, temp4, hf, nt,&
                            q1(deb+1))
! ---       FY*[R] - FN*(UY/JEU)
                call dscal(2*hf+1, 0.d0, temp3, 1)
                call dcopy(2*hf+1, zr(ivec+deb+(2*hf+1)), 1, temp3, 1)
                call mnlaft(temp3, temp4, hf, nt,&
                            q1(deb+(2*hf+1)+1))
! ---       R*[R] - (UX/JEU)^2 - (UY/JEU)^2
                call dscal(2*hf+1, 0.d0, temp3, 1)
                call dcopy(2*hf+1, zr(ivec+deb+2*(2*hf+1)), 1, temp3, 1)
                call mnlaft(temp3, temp4, hf, nt,&
                            q1(deb+2*(2*hf+1)+1))
                else if(ind.gt.deb+3*(2*hf+1).and.ind.le.deb+4*(2*hf+1))&
            then
                call dscal(2*hf+1, 0.d0, temp4, 1)
                temp4(ind-deb-3*(2*hf+1))=1.d0
! ---       (FN/ALPHA - R)*[FN]
                call dscal(2*hf+1, 0.d0, temp3, 1)
                call daxpy(2*hf+1, -1.d0, zr(ivec+deb+2*(2*hf+1)), 1, temp3,&
                           1)
                call daxpy(2*hf+1, 1.d0/alpha, zr(ivec+deb+3*(2*hf+1)), 1, temp3,&
                           1)
                call mnlaft(temp3, temp4, hf, nt,&
                            q1(deb+3*(2*hf+1)+1))
            endif
        else if (zk8(ityp-1+i)(1:4).eq.'PLAN') then
            nddl=zi(inddl-1+6*(i-1)+1)
            call dscal(2*hf+1, 0.d0, temp3, 1)
            call dscal(2*hf+1, 0.d0, temp4, 1)
            if (ind .gt. deb .and. ind .le. deb+(2*hf+1)) then
! ---       (F/ALPHA - XG)*[F]
                call dcopy(h+1, zr(ivec-1+nddl), nd, temp3(1:h+1), 1)
                call dcopy(h, zr(ivec-1+nd*(h+1)+nddl), nd, temp3(hf+2: hf+h+1), 1)
                call dscal(2*hf+1, -1.d0, temp3, 1)
                call daxpy(2*hf+1, 1.d0/alpha, zr(ivec+deb), 1, temp3,&
                           1)
                temp4(ind-deb)=1.d0
                call mnlaft(temp3, temp4, hf, nt,&
                            q1(deb+1))
            endif
        endif
        neqs=neqs+zi(ineqs-1+i)
        deb=deb+zi(ineqs-1+i)*(2*hf+1)
60  continue
!
! ----------------------------------------------------------------------
! --- GAMMA1 i.e. ND*(2*H+1)+2*NCHOC(2*HF+1)+1
! --- GAMMA2 i.e. ND*(2*H+1)+2*NCHOC(2*HF+1)+2
! ----------------------------------------------------------------------
    if (ind .eq. ninc) then
        q1(ninc-3)=-1.d0*zr(ivec-1+ninc-1)
        q1(ninc-2)=-1.d0*zr(ivec-1+ninc)
    endif
! ----------------------------------------------------------------------
! --- EQUATION DE PHASE i.e. ND*(2*H+1)+2*NCHOC(2*HF+1)+3
! ----------------------------------------------------------------------
    do 70 k = 1, h
        if (ind .eq. (nd*(h+k)+1)) then
            q1(ninc-1)=k*zr(ivec-1+ninc)
        endif
70  continue
!
    call dcopy(ninc-1, q1, 1, zr(itemp), 1)
!
    call jedema()
!
end subroutine

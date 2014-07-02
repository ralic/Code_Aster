subroutine mnlldr(ind, imat, neq, ninc, nd,&
                  nchoc, h, hf, parcho, xcdl,&
                  adime, xtemp)
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
!     MODE_NON_LINE -- MATRICE JACOBIENNE (L(E_I))
!     -    -                -            -   -
! ----------------------------------------------------------------------
!
! CALCUL LA MATRICE JACOBIENNE POUR UN CERTAIN VECTEUR SOLUTION
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
!
#include "asterf_types.h"
#include "jeveux.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
#include "blas/dcopy.h"
#include "blas/dscal.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
    integer :: ind, imat(2), neq, ninc, nd, nchoc, h, hf
    character(len=14) :: parcho, xcdl, adime, xtemp
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    real(kind=8) :: jeu, eta, alpha
    integer :: il, itemp1, itemp2
    integer :: deb, ddl, j, i, nddl
    integer :: icdl, iadim, itemp, k
    integer :: ncmp, hind, hfind, nddlx, nddly
    aster_logical :: stp
    integer, pointer :: vnddl(:) => null()
    integer, pointer :: neqs(:) => null()
    real(kind=8), pointer :: vjeu(:) => null()
    integer, pointer :: vncmp(:) => null()
    real(kind=8), pointer :: raid(:) => null()
    real(kind=8), pointer :: reg(:) => null()
    real(kind=8), pointer :: jeumax(:) => null()
    character(len=8), pointer :: type(:) => null()
    real(kind=8), pointer :: orig(:) => null()
!
    call jemarq()
!
    call wkvect('&&mnlldr.l', 'V V R', ninc-1, il)
    call wkvect('&&mnlldr.temp1', 'V V R', neq, itemp1)
    call wkvect('&&mnlldr.temp2', 'V V R', neq, itemp2)
    stp=.true.
!
    call jeveuo(parcho//'.RAID', 'L', vr=raid)
    call jeveuo(parcho//'.REG', 'L', vr=reg)
    call jeveuo(parcho//'.NDDL', 'L', vi=vnddl)
    call jeveuo(parcho//'.JEU', 'L', vr=vjeu)
    call jeveuo(parcho//'.JEUMAX', 'L', vr=jeumax)
    call jeveuo(parcho//'.NCMP', 'L', vi=vncmp)
    call jeveuo(parcho//'.NEQS', 'L', vi=neqs)
    call jeveuo(parcho//'.TYPE', 'L', vk8=type)
    call jeveuo(parcho//'.ORIG', 'L', vr=orig)
    call jeveuo(xcdl, 'L', icdl)
    call jeveuo(adime, 'L', iadim)
    call jeveuo(xtemp, 'E', itemp)
    call dscal(ninc-1, 0.d0, zr(itemp), 1)
! ----------------------------------------------------------------------
! --- INCONNUE DU SYSTEME DYNAMIQUE i.e. 1:ND*(2*H+1)
! ----------------------------------------------------------------------
    hind=int((ind-1)/nd)
    ddl=ind-nd*hind
    if (ind .le. nd*(2*h+1)) then
        call dscal(neq, 0.d0, zr(itemp1), 1)
        call dscal(neq, 0.d0, zr(itemp2), 1)
        i=0
        do 10 k = 1, neq
            if (zi(icdl-1+k) .eq. 0) then
                i=i+1
                if (i .eq. ddl) then
                    zr(itemp1-1+k)=1.d0
                endif
            endif
 10     continue
        call mrmult('ZERO', imat(1), zr(itemp1), zr(itemp2), 1,&
                    .false._1)
        i=0
        do 20 k = 1, neq
            if (zi(icdl-1+k) .eq. 0) then
                i=i+1
                zr(il-1+hind*nd+i)=zr(itemp2-1+k)/zr(iadim)
            endif
 20     continue
    else if (ind.le.(ninc-4)) then
        deb=nd*(2*h+1)
        do 30 i = 1, nchoc
            jeu=vjeu(i)/jeumax(1)
            ncmp=vncmp(i)
            do 31 j = 1, ncmp
                nddl=vnddl(6*(i-1)+j)
                if (ind .gt. deb+(j-1)*(2*hf+1) .and. ind .le. deb+j*(2* hf+1)) then
                    hfind=ind-deb-(j-1)*(2*hf+1)-1
                    if (hfind .le. h) then
                        zr(il-1+nd*hfind+nddl)=jeu
                    else if (hfind.ge.(hf+1).and.hfind.le.(hf+h)) then
                        zr(il-1+nd*(hfind-hf+h)+nddl)=jeu
                    endif
                endif
 31         continue
            deb=deb+neqs(i)*(2*hf+1)
 30     continue
    endif
! ----------------------------------------------------------------------
! --- EQUATIONS SUPPLEMENTAIRES POUR DEFINIR LA FORCE NON-LINEAIRE
! ----------------------------------------------------------------------
    deb=nd*(2*h+1)
    do 110 i = 1, nchoc
        alpha=raid(i)/zr(iadim-1+1)
        eta=reg(i)
        jeu=vjeu(i)/jeumax(1)
        if (type(i)(1:7) .eq. 'BI_PLAN') then
            nddl=vnddl(6*(i-1)+1)
            if (ind .le. nd*(2*h+1)) then
                if (ddl .eq. nddl) then
                    if (hind .le. h) then
                        zr(il-1+deb+hind+1)=-eta/jeu
                    else
                        zr(il-1+deb+(hf+1)+(hind-h))=-eta/jeu
                    endif
                endif
            else if ((ind.gt.deb).and.(ind.le.(deb+2*(2*hf+1)))) then
                zr(il-1+ind)=1.d0
            endif
        else if (type(i)(1:6).eq.'CERCLE') then
            nddlx=vnddl(6*(i-1)+1)
            nddly=vnddl(6*(i-1)+2)
! ---     +2*ORIG1*UX + 2*ORIG2*UY
            if (ind .le. nd*(2*h+1)) then
                if (hind .le. h) then
                    if (ddl .eq. nddlx) then
                        zr(il-1+deb+2*(2*hf+1)+hind+1)=2*orig(1+3*(i-1))/jeu**2
                    else if (ddl.eq.nddly) then
                        zr(il-1+deb+2*(2*hf+1)+hind+1)=2*orig(1+3*(i-1)+1)/jeu**2
                    endif
                else
                    if (ddl .eq. nddlx) then
                        zr(il-1+deb+2*(2*hf+1)+(hf+1)+(hind-h))= 2*orig(1+3*(i-1))/jeu**2
                    else if (ddl.eq.nddly) then
                        zr(il-1+deb+2*(2*hf+1)+(hf+1)+(hind-h))= 2*orig(1+3*(i-1)+1)/jeu**2
                    endif
                endif
            endif
            if (ind .gt. (deb+3*(2*hf+1)) .and. ind .le. (deb+4*(2*hf+1))) then
! ---     +ORIG1*FN
                zr(il-1+deb+(ind-deb-3*(2*hf+1)))=orig(1+3*(i-1))/jeu
! ---     +ORIG2*FN
                zr(il-1+deb+(2*hf+1)+(ind-deb-3*(2*hf+1)))=orig(1+3*(i-1)+1)/jeu
! ---     FN
                zr(il-1+ind)=1.d0
            endif
        else if (type(i)(1:4).eq.'PLAN') then
! ---     F
            if (ind .gt. deb .and. ind .le. (deb+(2*hf+1))) then
                zr(il-1+ind)=1.d0
            endif
        endif
        deb=deb+neqs(i)*(2*hf+1)
110 continue
! ----------------------------------------------------------------------
! --- AUTRES EQUATIONS
! ----------------------------------------------------------------------
! --- GAMMA1
    if (ind .eq. ninc-3) then
        zr(il-1+ninc-3) = 1.d0
    endif
! --- GAMMA2
    if (ind .eq. ninc-2) then
        zr(il-1+ninc-2) = 1.d0
    endif
!
    call dcopy(ninc-1, zr(il), 1, zr(itemp), 1)
!
    call jedetr('&&mnlldr.l')
    call jedetr('&&mnlldr.temp1')
    call jedetr('&&mnlldr.temp2')
!
    call jedema()
!
end subroutine

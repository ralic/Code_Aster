subroutine te0324(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
! aslint: disable=W0104
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/provec.h"
#include "blas/ddot.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'AMOR_MECA'
!                                OU 'RIGI_MECA_HYST'
!        POUR TOUS LES TYPES D'ELEMENTS (SAUF LES ELEMENTS DISCRETS)
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nbres, nbpar
    parameter         ( nbres=4 )
    parameter         ( nbpar=3 )
!
    integer :: iret, nbval, nbddl
    integer :: i, j, irigi, jma, i2, i3
    integer :: iresu, ins, irns, ivari
    integer :: idresu(5), idrigi(2), idgeo(5)
    integer :: igeom
    integer :: ndim, nno, nnos
!
    real(kind=8) :: valres(nbres), valpar(nbpar)
!
    real(kind=8) :: x(4), y(4), z(4), c1(3), c2(3), c3(3)
    real(kind=8) :: a(3), b(3), surf, surf2    
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres)
    logical :: ljfr
    
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos)
!
!     -- RECUPERATION DES CHAMPS PARAMETRES ET DE LEURS LONGUEURS:
!     ------------------------------------------------------------
    ins=0
    irns=0
    if (option .eq. 'AMOR_MECA') then
       call tecach('NNO', 'PRIGIEL', 'L', ins, iad=idrigi(1))
       if (ins .eq. 0) then
          call tecach('ONN', 'PMATUUR', 'E', iret, nval=5,&
                       itab=idresu)
       else
          call tecach('NNN', 'PMATUNS', 'E', irns, nval=5,&
                       itab=idresu)
          if (irns .ne. 0) call tecach('ONN','PMATUUR','E',iret,5,&
                                       itab=idresu)
       endif
    else if (option.eq.'RIGI_MECA_HYST') then
       call tecach('ONN', 'PMATUUC', 'E', iret, nval=5, itab=idresu)
    else
       ASSERT(.false.)
    endif
    nbval= idresu(2)
!
    ljfr=.false.
    call tecach('NNN', 'PMATERC', 'L', iret, iad=jma)
    if ((jma.eq.0) .or. (iret.ne.0)) goto 1
    nomres(1) = 'K_N'
    nomres(2) = 'AMOR_NOR'
    nomres(3) = 'AMOR_TAN'
    nomres(4) = 'COEF_AMO'    
    valres(1) = 0.d0
    valres(2) = 0.d0
    valres(3) = 0.d0
    valres(4) = 0.d0    
    call rcvala(zi(jma), ' ', 'JOINT_MECA_FROT', 0, ' ',&
                [valpar], 4, nomres, valres, icodre, 0)
    if (icodre(1).eq.0) then
       ljfr = .true.
    endif 
!
!
    call tecach('ONN', 'PGEOMER', 'L', iret, nval=5,&
                itab=idgeo)
    igeom=idgeo(1)
    do i=1,nnos
      x(i) = zr(igeom+3*(i-1)+1-1)
      y(i) = zr(igeom+3*(i-1)+2-1)
      z(i) = zr(igeom+3*(i-1)+3-1)
    end do
    a(1) = x(3) - x(1)
    a(2) = y(3) - y(1)
    a(3) = z(3) - z(1)
    if (nnos .eq. 3) then
      b(1) = x(2) - x(1)
      b(2) = y(2) - y(1)
      b(3) = z(2) - z(1)
    else if (nnos .eq. 4) then
      b(1) = x(4) - x(2)
      b(2) = y(4) - y(2)
      b(3) = z(4) - z(2)
    else
      ASSERT(.false.)
    endif
    call provec(a, b, c1)
    surf=ddot(3,c1,1,c1,1)
    c1(1)=c1(1)/sqrt(surf)
    c1(2)=c1(2)/sqrt(surf)
    c1(3)=c1(3)/sqrt(surf)
    surf = sqrt(surf)*0.5d0
    if (nnos .eq. 3) then
      surf2=ddot(3,b,1,b,1)
      c2(1)=b(1)/sqrt(surf2)
      c2(2)=b(2)/sqrt(surf2)
      c2(3)=b(3)/sqrt(surf2)
    else if (nnos .eq. 4) then
      surf2=ddot(3,a,1,a,1)
      c2(1)=a(1)/sqrt(surf2)
      c2(2)=a(2)/sqrt(surf2)
      c2(3)=a(3)/sqrt(surf2)          
    endif
    call provec(c1, c2, c3)
!
!
!     -- CALCUL PROPREMENT DIT :
!     --------------------------
    iresu= idresu(1)
    irigi= idrigi(1)
    if (option .eq. 'AMOR_MECA') then
       if (ljfr) then
          if (ins.eq.0) then
             call tecach('ONN', 'PRIGINS', 'L', irns, iad=idrigi(1))
             call tecach('ONN', 'PRIGIEL', 'L', iret, nval=2, itab=idrigi)
             nbddl = int(-1.0d0+sqrt(1.0d0+8.d0*dble(idrigi(2))))/2
             nbval=idrigi(2)
             call tecach('ONN', 'PMATUUR', 'E', iret, nval=2, itab=idresu)
          else
             call tecach('ONN', 'PRIGINS', 'L', iret, nval=2, itab=idrigi)
             call tecach('NNN', 'PMATUNS', 'E', irns, nval=5, itab=idresu)
             if (irns .ne. 0) then
               call tecach('ONN', 'PMATUUR', 'E', iret, 5, itab=idresu)
               nbddl = int(-1.0d0+sqrt(1.0d0+8.d0*dble(idresu(2))))/2
             else
               nbddl = int(sqrt(dble(idresu(2))))          
             endif
             nbval=idresu(2)
          endif
          irigi= idrigi(1)
          iresu= idresu(1)
          call jevech('PVARIPG', 'L', ivari)
          if (irigi.ne.0) then
             if (ins.ne.0 .and. irns.ne.0) then
              do i3=1,nnos
               do j=1,3
                 i=3*(i3-1)+j
                 i2=i+3*nnos
                 zr(iresu-1+i*(i+1)/2)=valres(2)*c1(j)**2+&
                  valres(3)*c2(j)**2+valres(3)*c3(j)**2
                 zr(iresu-1+i*(i+1)/2)=zr(iresu-1+i*(i+1)/2)*surf/nnos
                 if (nnos.eq.3 .and. nno.eq.3) then
                   if (zr(ivari-1+7) .ge. 0.d0) then
                     zr(iresu-1+i*(i+1)/2)=zr(iresu-1+i*(i+1)/2)*valres(4)
                   endif
                 else if (nnos.eq.3 .and. nno.eq.6) then
                   if (zr(ivari-1+7+18*(i3-1)) .ge. 0.d0) then
                     zr(iresu-1+i*(i+1)/2)=zr(iresu-1+i*(i+1)/2)*valres(4)
                   endif
                 else if (nnos.eq.4) then
                   if (zr(ivari-1+7+18*(i3-1)) .ge. 0.d0) then
                     zr(iresu-1+i*(i+1)/2)=zr(iresu-1+i*(i+1)/2)*valres(4)
                   endif
                 endif     
                 zr(iresu-1+i2*(i2+1)/2)=zr(iresu-1+i*(i+1)/2)
                 zr(iresu-1+i+i2*(i2-1)/2)=-zr(iresu-1+i*(i+1)/2)
               end do
              end do
             else
               do i = 1, nbval
                zr(iresu-1+i)=zr(irigi-1+i)*valres(2)/valres(1)
               end do                 
             endif
          endif
          goto 1
       endif
    endif
1   continue
end subroutine

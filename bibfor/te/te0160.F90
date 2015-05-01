subroutine te0160(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!    - ELEMENT:  MECABL2
!      OPTION : 'FULL_MECA'   'RAPH_MECA'   'RIGI_MECA_TANG'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
! --------------------------------------------------------------------------------------------------
!
! aslint: disable=W0104
    implicit none
    character(len=16) :: option, nomte
!
#include "jeveux.h"
#include "asterfort/biline.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/matvec.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
!
    integer ::          icodre(2)
    real(kind=8) ::     valres(2)
    character(len=16) :: nomres(2)
!
    integer :: nno, kp, ii, jj, imatuu
    integer :: ipoids, ivf, igeom, imate, jcret
    integer :: icompo, idepla, ideplp, idfdk, imat, iyty, jefint, ivarip
    integer :: jgano, kk, lsect, lsigma, ndim, nelyty, nnos, nordre, npg
    real(kind=8) :: aire, coef, coef1, coef2, demi
    real(kind=8) :: etraction, epsth, ecompress, ecable
    real(kind=8) :: green, jacobi, nx, ytywpq(9), w(9)
    real(kind=8) :: preten, r8bid
!
! --------------------------------------------------------------------------------------------------
!
    demi = 0.5d0
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
        npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
    call jevete('&INEL.CABPOU.YTY', 'L', iyty)
!   3 efforts par noeud
    nordre = 3*nno
!
! --------------------------------------------------------------------------------------------------
!   parametres en entree
    call jevech('PCOMPOR', 'L', icompo)
    if (zk16(icompo+3) (1:9) .eq. 'COMP_INCR') then
        call utmess('F', 'ELEMENTS3_36')
    endif
    if (zk16(icompo) (1:5) .ne. 'CABLE') then
        call utmess('F', 'ELEMENTS3_37', sk=zk16(icompo))
    endif
    if (zk16(icompo+1) .ne. 'GROT_GDEP') then
        call utmess('F', 'ELEMENTS3_38', sk=zk16(icompo+1))
    endif
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    nomres(1) = 'E'
    nomres(2) = 'EC_SUR_E'
    r8bid = 0.0d0
    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                ' ', 'ELAS', 0, '  ', [r8bid],&
                1, nomres, valres, icodre, 1)
    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                ' ', 'CABLE', 0, '  ', [r8bid],&
                1, nomres(2), valres(2), icodre(2), 1)
    etraction = valres(1)
    ecompress = etraction*valres(2)
    ecable = etraction
    call jevech('PCACABL', 'L', lsect)
    aire = zr(lsect)
    preten = zr(lsect+1)
    call jevech('PDEPLMR', 'L', idepla)
    call jevech('PDEPLPR', 'L', ideplp)
! --------------------------------------------------------------------------------------------------
!   parametres en sortie
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:14) .eq. 'RIGI_MECA_TANG') then
        call jevech('PMATUUR', 'E', imatuu)
    endif
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PVECTUR', 'E', jefint)
        call jevech('PCONTPR', 'E', lsigma)
        call jevech('PVARIPR', 'E', ivarip)
    endif
!
    do ii = 1, 3*nno
        w(ii) = zr(idepla-1+ii) + zr(ideplp-1+ii)
    enddo
    do kp = 1, npg
        call verift('RIGI', kp, 1, '+', zi(imate),epsth=epsth)
        kk = (kp-1)*nordre*nordre
        jacobi = sqrt(biline(nordre,zr(igeom),zr(iyty+kk),zr(igeom)))
        green = biline(nordre, w, zr(iyty+kk), zr(igeom))+ demi*biline( nordre, w, zr(iyty+kk), w)
        green = green/jacobi**2
        nx = etraction*aire*green
        if (abs(nx) .lt. 1.d-6) then
            nx = preten
        else
            nx = nx - etraction*aire*epsth
        endif
!       le cable a un module plus faible en compression qu'en traction
!       le module de compression peut meme etre nul.
        if (nx .lt. 0.d0) then
            nx = nx*ecompress/etraction
            ecable = ecompress
        endif
!
        coef1 = ecable*aire*zr(ipoids-1+kp)/jacobi**3
        coef2 = nx*zr(ipoids-1+kp)/jacobi
        call matvec(nordre, zr(iyty+kk), 2, zr(igeom), w, ytywpq)
        if (option(1:9).eq.'FULL_MECA' .or. option(1:14).eq.'RIGI_MECA_TANG') then
            nelyty = iyty - 1 - nordre + kk
            imat = imatuu - 1
            do ii = 1, nordre
                nelyty = nelyty + nordre
                do jj = 1, ii
                    imat = imat + 1
                    zr(imat) = zr(imat) + coef1*ytywpq(ii)*ytywpq(jj) + coef2*zr(nelyty+jj)
                enddo
            enddo
        endif
        if (option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RAPH_MECA') then
            coef = nx*zr(ipoids-1+kp)/jacobi
            do ii = 1, nordre
                zr(jefint-1+ii) = zr(jefint-1+ii) + coef*ytywpq(ii)
            enddo
            zr(lsigma-1+kp) = nx
            zr(ivarip+kp-1) = 0.0d0
        endif
    enddo
!
    if (option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = 0
    endif
end subroutine

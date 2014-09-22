subroutine te0086(option, nomte)
!
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
!
    implicit none
    character(len=16) :: option, nomte
!
! --------------------------------------------------------------------------------------------------
!
!    ELEMENT MECABL2
!       OPTION : 'RIGI_MECA_GE'
!
!       si pas de champe DEPL en entrée alors MATGEOM = 0
!
! --------------------------------------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/biline.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/matvec.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
!
    integer ::          icodre(2)
    real(kind=8) ::     valres(2)
    character(len=16) :: nomres(2)
!
    integer ::      nno, kp, ii, jj, imatuu, ipoids, ivf, igeom, imate,iforce
    integer ::      ideplp, idfdk, imat, iyty
    integer ::      jgano, kk, lsect, ndim, nelyty, nnos, iret, nbval, nordre, npg
    real(kind=8) :: aire, coef1, coef2, demi, etraction, ecompress, ecable, nx, jacobi
    real(kind=8) :: ytywpq(9), w(9)
    real(kind=8) :: r8bid
!
! --------------------------------------------------------------------------------------------------
    demi = 0.5d0
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
        npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
    call jevete('&INEL.CABPOU.YTY', 'L', iyty)
!   3 efforts par noeud
    nordre = 3*nno
!
! --------------------------------------------------------------------------------------------------
!   Parametres en sortie
    call jevech('PMATUUR', 'E', imatuu)
    nbval = nordre*(nordre+1)/2
    zr(imatuu:imatuu-1+nbval)= 0.0d0
! --------------------------------------------------------------------------------------------------
!   Parametres en entree : si pas de DEPL MATGEOM=0
    call tecach('ONO', 'PDEPLPR', 'L', iret, iad=ideplp)
    if (iret .ne. 0) goto 999
!
! --------------------------------------------------------------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTRR', 'L', iforce)
    call jevech('PMATERC', 'L', imate)
!
    nomres(1) = 'E'
    nomres(2) = 'EC_SUR_E'
    r8bid = 0.0d0
    call rcvalb('RIGI', 1, 1, '+', zi(imate), ' ', 'ELAS',  0, '  ', [r8bid],&
                1, nomres, valres, icodre, 1)
    call rcvalb('RIGI', 1, 1, '+', zi(imate), ' ', 'CABLE', 0, '  ', [r8bid],&
                1, nomres(2), valres(2), icodre(2), 1)
    etraction = valres(1)
    ecompress = etraction*valres(2)
    ecable = etraction
    call jevech('PCACABL', 'L', lsect)
    aire = zr(lsect)
!
    imat = imatuu - 1
    do ii = 1, 3*nno
        w(ii) = zr(ideplp-1+ii)
    enddo
    do kp = 1, npg
        kk = (kp-1)*nordre*nordre
        jacobi = sqrt(biline(nordre,zr(igeom),zr(iyty+kk),zr(igeom)))
!
        nx = zr(iforce-1+kp)
!       Le cable a un module plus faible en compression qu'en traction
!       Le module de compression peut même être nul.
        if ( nx .lt. 0.0d0 ) then
            ecable = ecompress
        endif
!
        coef1 = ecable*aire*zr(ipoids-1+kp)/jacobi**3
        coef2 = nx*zr(ipoids-1+kp)/jacobi
        call matvec(nordre, zr(iyty+kk), 2, zr(igeom), w, ytywpq)
        nelyty = iyty - 1 - nordre + kk
        do ii = 1, nordre
            nelyty = nelyty + nordre
            do jj = 1, ii
                imat = imat + 1
                zr(imat) = zr(imat) + coef1*ytywpq(ii)*ytywpq(jj) + coef2*zr(nelyty+jj)
            enddo
        enddo
    enddo
!
999 continue
end subroutine

subroutine xlsjon(ino, jlsn, nfiss, jfisco, jonc_no)
    implicit none
!
#include "jeveux.h"
#include "asterfort/jevech.h"
    integer :: ino, jlsn, nfiss, jfisco
    real(kind=8) :: jonc_no(nfiss)
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
!       CALCUL DE LA LSN D'UN NOEUD DE L'ELEMENT PARENT EN TENANT COMPTE
!       DES JONCTIONS POUR CHAQUE FISSURE
!
!     ENTREE
!       INO      : NUMERO DU NOEUD COURANT DE L'ELT PARENT
!       NFISS    : NOMBRE DE FISSURE DANS LE SUPPORT DU NOEUD
!     SORTIE
!       jonc_no    : FONCTION HEAVISIDE AU NOEUD POUR CHAQUE FISSURE EN TENANT COMPTE
!                  DES JONCTIONS
!
    integer :: nfimax
    parameter (nfimax=10)
    integer :: ifiss, i, nfisc, ifisc
    integer :: fisco(2*nfimax), fisc(2*nfimax)
    real(kind=8) :: ljonc(nfimax+1)
!
!.....................................................................
!
!
    if (nfiss.eq.1) then
!
      if (zr(jlsn-1+ino) .lt. 0.d0) then
         jonc_no(1) = -1.d0
      else 
         jonc_no(1) = +1.d0
      endif   
!
    else
!
      fisc(1:2*nfimax)=0
      fisco(1:2*nfimax)=0
      if (nfiss.gt.1) fisco(1:2*nfiss)=zi(jfisco:(jfisco+2*nfiss-1))
      jonc_no(1:nfiss)=0.d0
      ljonc(1:nfimax)=0.d0
      do ifiss = 1, nfiss
!   REMPLISSAGE DE FISC POUR CHAQUE FISSURE
       fisc(1:2*nfiss)=0
       ifisc = ifiss
       nfisc = 0
 80    continue
       if (fisco(2*ifisc-1) .gt. 0) then
          nfisc = nfisc+1
          fisc(2*(nfisc-1)+2) = fisco(2*ifisc)
          ifisc = fisco(2*ifisc-1)
          fisc(2*(nfisc-1)+1) = ifisc
          goto 80
       endif
!
       do i = 1, nfisc
          ljonc(i) = zr(jlsn-1+(ino-1)*nfiss+fisc(2*i- 1))
       end do
       ljonc(nfisc+1) = zr(jlsn-1+(ino-1)*nfiss+ ifiss)
!
!   MISE A ZERO POUR LA FONCTION JONCTION AU NIVEAU DU BRANCHEMENT
       do i = 1, nfisc
           if (fisc(2*i)*ljonc(i) .gt. 0.d0) goto 300
       end do
!
       if (ljonc(nfisc+1) .lt. 0.d0) then
           jonc_no(ifiss) = -1.d0
       else if (ljonc(nfisc+1).gt.0.d0) then
           jonc_no(ifiss) = +1.d0
       endif
300    continue
      end do
!
    endif
!
!
end subroutine

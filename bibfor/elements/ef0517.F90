subroutine ef0517(nomte)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mess.h"
    character(len=16) :: nomte
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     EFGE_ELNO
! ----------------------------------------------------------------------
!
    integer :: nc
    real(kind=8) :: zero
!
    real(kind=8) :: fl(14), d1b3(2, 3), ksi1, tmax(2), tmin(2)
    real(kind=8) :: sigfib
!
    integer :: nbfib, kp, adr, ncomp, i, cara, ne, jacf, ncarfi
    integer :: icgp, icontn, npg
    integer :: istrxr
!
    parameter(zero=0.d+0)
! ----------------------------------------------------------------------
!
!
!     --------------------------------------
    if (nomte .eq. 'MECA_POU_D_TGM') then
!
        call jevech('PCONTRR', 'L', icgp)
        call jevech('PSTRXRR', 'L', istrxr)
        call jevech('PEFFORR', 'E', icontn)
!       --- RECUPERATION DES CARACTERISTIQUES DES FIBRES
        call jevech('PNBSP_I', 'L', i)
        nbfib=zi(i)
        call jevech('PFIBRES', 'L', jacf)
        ncarfi=3
!
!       --- NOMBRE DE POINT DE GAUSS
!
!       ON PROJETTE AVEC LES FCTS DE FORME
!       SUR LES NOEUDS DEBUT ET FIN DE L'ELEMENT
!       POUR LE POINT 1
        ksi1=-sqrt(5.d0/3.d0)
        d1b3(1,1)=ksi1*(ksi1-1.d0)/2.0d0
        d1b3(1,2)=1.d0-ksi1*ksi1
        d1b3(1,3)=ksi1*(ksi1+1.d0)/2.0d0
!       POUR LE POINT 2
        ksi1=sqrt(5.d0/3.d0)
        d1b3(2,1)=ksi1*(ksi1-1.d0)/2.0d0
        d1b3(2,2)=1.d0-ksi1*ksi1
        d1b3(2,3)=ksi1*(ksi1+1.d0)/2.0d0
!
        nc=7
        npg = 3
        ncomp=18
!
!
! --- CALCUL DES FORCES INTEGREES
        do 20 i = 1, nc
            fl(i)=zero
            fl(i+nc)=zero
            do 10 kp = 1, npg
                adr=istrxr+ncomp*(kp-1)+i-1
                fl(i)=fl(i)+zr(adr)*d1b3(1,kp)
                fl(i+nc)=fl(i+nc)+zr(adr)*d1b3(2,kp)
10          continue
20      continue
!
! !!!   A CAUSE DE LA PLASTIFICATION DE LA SECTION LES EFFORTS
!          N,MFY,MFZ DOIVENT ETRE RECALCULES POUR LES NOEUDS 1 ET 2
        fl(1)=zero
        fl(5)=zero
        fl(6)=zero
        fl(1+nc)=zero
        fl(5+nc)=zero
        fl(6+nc)=zero
!
!       POUR LES NOEUDS 1 ET 2
!          CALCUL DES CONTRAINTES
!          CALCUL DES EFFORTS GENERALISES A PARTIR DES CONTRAINTES
        do 50 ne = 1, 2
            do 40 i = 1, nbfib
                sigfib=zero
                do 30 kp = 1, npg
                    adr=icgp+nbfib*(kp-1)+i-1
                    sigfib=sigfib+zr(adr)*d1b3(ne,kp)
30              continue
                if (i .eq. 1) then
                    tmax(ne)=sigfib
                    tmin(ne)=sigfib
                else
                    if (sigfib .gt. tmax(ne)) tmax(ne)=sigfib
                    if (sigfib .lt. tmin(ne)) tmin(ne)=sigfib
                endif
                adr=nc*(ne-1)
                cara=jacf+(i-1)*ncarfi
                fl(1+adr)=fl(1+adr)+sigfib*zr(cara+2)
                fl(5+adr)=fl(5+adr)+sigfib*zr(cara+2)*zr(cara+1)
                fl(6+adr)=fl(6+adr)-sigfib*zr(cara+2)*zr(cara)
40          continue
50      continue
!
        do 60 i = 1, nc
            zr(icontn+i-1)=fl(i)
60      continue
        zr(icontn+(nc+1)-1)=tmax(1)
        zr(icontn+(nc+2)-1)=tmin(1)
        do 70 i = (nc+1), 2*nc
            zr(icontn+2+i-1)=fl(i)
70      continue
        zr(icontn+2*(nc+1)+1-1)=tmax(2)
        zr(icontn+2*(nc+1)+2-1)=tmin(2)
!
!
    else if (nomte.eq.'MECA_POU_D_EM') then

        nc=6
        ncomp=18
        npg = 2
        call jevech('PSTRXRR', 'L', istrxr)
        call jevech('PEFFORR', 'E', icontn)
        do kp = 1,npg
            do i = 1,nc
                zr(icontn-1+nc*(kp-1)+i) = zr(istrxr-1+ncomp*(kp-1)+i)
            end do
        end do
!
    else
        call assert(.false.)
    endif
!
end subroutine

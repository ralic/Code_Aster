subroutine nmceai(numedd, depdel, deppr1, deppr2, depold,&
                  sdpilo, rho, eta, isxfe, f,&
                  indic)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: indic
    character(len=24) :: numedd
    character(len=19) :: sdpilo, depdel, depold, deppr1, deppr2
    real(kind=8) :: eta, rho, f
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE - SELECTION PARAMETRE)
!
! CALCUL DU PARAMETRE DE SELECTION DE TYPE ANGL_INCR_DEPL
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUME_DDL
! IN  SDPILO : SD PILOTAGE
! IN  DEPDEL : INCREMENT DE DEPLACEMENT DEPUIS DEBUT PAS DE TEMPS
! IN  DEPOLD : INCREMENT DE DEPLACEMENT PAS DE TEMPS PRECEDENT
! IN  DEPPR1 : INCREMENT DE DEPLACEMENT K-1.F_DONNE
! IN  DEPPR2 : INCREMENT DE DEPLACEMENT K-1.F_PILO
! IN  RHO    : PARAMETRE DE RECHERCHE LINEAIRE
! IN  ETA    : PARAMETRE DE PILOTAGE
! IN  ISXFE  : INDIQUE SI LE MODELE EST UN MODELE XFEM
! OUT F      : VALEUR DU CRITERE
! OUT INDIC  : 0 CRITERE NON UTILISABLE
!              1 CRITERE UTILISABLE
!
!
!
!
    real(kind=8) :: sca, nodup, coef, nodup1, nodup2
    integer :: jdepde, jdu0, jdu1, jdepol
    integer :: neq, i, j
    character(len=19) :: profch, chapil, chapic, selpil
    integer :: jcoee, jcoef, ideeq, jplsl
    real(kind=8) :: dn, dc, dp, da
    logical :: isxfe
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    if (isxfe) then
        chapil = sdpilo(1:14)//'.PLCR'
        call jeveuo(chapil(1:19)//'.VALE', 'L', jcoef)
        chapic = sdpilo(1:14)//'.PLCI'
        call jeveuo(chapic(1:19)//'.VALE', 'L', jcoee)
    else
!
! --- ACCES VECTEUR DE SELCTION CMP DX/DY/DZ
!
        selpil = sdpilo(1:14)//'.PLSL'
        call jeveuo(selpil(1:19)//'.VALE', 'L', jplsl)
    endif
!
!
! --- INITIALISATIONS--------------------------------
!
    sca = 0.d0
    nodup = 0.d0
    nodup1 = 0.d0
    nodup2 = 0.d0
    f = 0.d0
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    call dismoi('PROF_CHNO', depdel, 'CHAM_NO', repk=profch)
    call jeveuo(profch(1:19)//'.DEEQ', 'L', ideeq)
!
! --- ACCES AUX VECTEURS SOLUTIONS
!
    call jeveuo(depdel(1:19)//'.VALE', 'L', jdepde)
    call jeveuo(deppr1(1:19)//'.VALE', 'L', jdu0)
    call jeveuo(deppr2(1:19)//'.VALE', 'L', jdu1)
    call jeveuo(depold(1:19)//'.VALE', 'L', jdepol)
!
!
! --- CALCUL DE L'ANGLE
!
    if (isxfe) then
        do i = 1, neq
            if (zi(ideeq-1+2*i) .gt. 0) then
                if (zr(jcoee+i-1) .eq. 0.d0) then
                    sca = sca + zr(jdepol+i-1)* zr(jcoef+i-1)**2*(zr( jdepde+i-1) + rho*zr(jdu0+i&
                          &-1) + eta*zr(jdu1+i-1))
                    nodup1 = nodup1 + zr(jcoef+i-1)**2*(zr(jdepde+i-1) + rho*zr(jdu0+i-1) + eta*z&
                             &r(jdu1+i-1))**2
                    nodup2 = nodup2 + zr(jcoef+i-1)**2*zr(jdepol+i-1) **2
                else
                    da = 0.d0
                    dn = 0.d0
                    dc = 0.d0
                    dp = 0.d0
                    do j = i+1, neq
                        if (zr(jcoee+i-1) .eq. zr(jcoee+j-1)) then
                            da = da + zr(jcoef+i-1)*zr(jdepol+i-1)+ zr(jcoef+j-1)*zr(jdepol+j-1)
                            dn = dn + zr(jcoef+i-1)*zr(jdepde+i-1)+ zr(jcoef+j-1)*zr(jdepde+j-1)
                            dc = dc + zr(jcoef+i-1)*zr(jdu0-1+i)+ zr(jcoef+j-1)*zr(jdu0-1+j)
                            dp = dp + zr(jcoef+i-1)*zr(jdu1-1+i)+ zr(jcoef+j-1)*zr(jdu1-1+j)
                        endif
                    end do
                    sca = sca + da*(dn+rho*dc+eta*dp)
                    nodup1 = nodup1 + (dn+rho*dc+eta*dp)**2
                    nodup2 = nodup2 + da**2
                endif
            endif
        end do
        nodup = nodup1*nodup2
    else
        do i = 1, neq
            coef = zr(jplsl-1+i)
            sca = sca + (zr(jdepol+i-1)*(zr(jdepde+i-1) + rho*zr(jdu0+ i-1) + eta*zr(jdu1+i-1))&
                  )*coef
            nodup = nodup + ( zr(jdepde+i-1) + rho*zr(jdu0+i-1) + eta*zr(jdu1+i-1))**2
        end do
    endif
!
    if (nodup .eq. 0.d0) then
        indic = 0
        f = 0.d0
    else
        indic = 1
        f = sca / sqrt(nodup)
    endif
    f = -f
!
    call jedema()
end subroutine

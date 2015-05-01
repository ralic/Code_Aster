subroutine cavini(ndim, nno, geom, vim, npg,&
                  lgpg, imate)
!
! ======================================================================
! ======================================================================
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
! ======================================================================
! CAVINI :
! CALCUL DES CONTRAINTES DE RUPTURE POUR MODELE ENDO_HETEROGENE
! VIM(3,GG) = CONTRAINTE D AMORCAGE AU PT DE GAUSS GG
! VIM(4,GG) = CONTRAINTE DE PROPAGATION AU PT DE GAUSS GG
!
    implicit none
#include "asterfort/carand.h"
#include "asterfort/casurf.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
    integer :: ndim, nno, npg, lgpg, imate, zz, zzz, zzzz, nono, nitert, ntirmx
    real(kind=8) :: geom(1:ndim, 1:nno)
    real(kind=8) :: vim(1:lgpg, 1:npg), gr
    real(kind=8) :: lc(1), mm, echp, ki, epai, ct1, ct2, randd, surff
    integer :: icodre(5)
    integer :: k2(1), kpg, spt
    character(len=16) :: nomres(5)
    character(len=8) :: fami, poum
    real(kind=8) :: valres(5), sa, sp, sc
!
!
    nitert = 0
567  continue
!
    nono=0
! RMQ NICO : INITIALISATION DE LA CONTRAINTE D AMORCAGE
! SI NON PRECISEE
!
    call casurf(ndim, nno, geom, surff)
    nomres(1) = 'SY'
    nomres(2) = 'WEIBULL'
    nomres(3) = 'KI'
    nomres(4) = 'EPAI'
    nomres(5) = 'GR'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, imate,&
                ' ', 'ENDO_HETEROGENE', 0, ' ', [0.d0],&
                5, nomres, valres, icodre, 1)
    call rcvalb(fami, kpg, spt, poum, imate,&
                ' ', 'NON_LOCAL', 0, ' ', [0.d0],&
                1, 'LONG_CARA', lc, k2, 1)
!  FACTEUR D ECHELLE
    echp = valres(1)
!  MODULE DE WEIBULL
    mm = valres(2)
! EPAISSEUR
!
    ki = valres(3)
    epai = valres(4)
! GRAINE
    gr = valres(5)
!
    if (gr .gt. 0.d0) then
!
! GRAINE NON NULLE : TIRAGE UNIQUE
! ON NE VERIFIE LE SEUIL QU'UNE SEULE FOIS
        ntirmx=1
    else
        ntirmx=25
    endif
!
    if (vim(3,1) .lt. 0.0001d0) then
        call carand(randd, gr)
        ct1=0.d0
        ct1=0.d0-log(1.d0-randd)
        sa=0.d0
        sa=echp*((lc(1)**3.d0)**(1.d0/mm))/ ((surff*epai)**(1.d0/mm))*(&
        ct1**(1.d0/mm))
        do 5,zz=1,npg
        vim(3,zz)=sa
 5      continue
    endif
!  INITIALISATION DE LA CONTRAINTE DE PROPAGATION
! SI NON PRECISEE
    if (vim(4,1) .lt. 0.0001d0) then
!
! TENACITE
!
        ct2=0.d0
        ct2=0.5736d0
        sp=0.d0
        sp=ct2*((ki**2.d0/(3.1416d0*lc(1)))**(0.5d0))
        do 6,zzz=1,npg
        vim(4,zzz)=sp
 6      continue
    endif
!
!  VERIFICATION DE LA COHERENCE DES DEUX SEUILS
    sc = ((2.d0)**(0.5d0))*vim(4,1)
    if (sc .gt. vim(3,1)) then
        do 7,zzzz=1,npg
        vim(3,zzzz)=0.d0
        vim(4,zzzz)=0.d0
 7      continue
        nono=1
        nitert=nitert+1
    endif
!
!
! ON N AUTORISE NTIRMX TIRAGES SINON L'UTILISATEUR DOIT
! REVOIR L'INITIALISATION DE SES SEUILS
    if ((nono .eq. 1) .and. (nitert.lt.ntirmx)) then
        goto 567
    else if ((nono .eq. 1).and.(nitert.ge.ntirmx)) then
        call utmess('F', 'COMPOR2_14')
    endif
!
end subroutine

subroutine lcdedi(fami, kpg, ksp, nmat, materd,&
                  materf, tempd, tempf, tref, depst,&
                  epsdt, depsm, epsdm)
    implicit none
!       ----------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ----------------------------------------------------------------
!       RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE
!       POUR TENIR COMPTE DES CONTRAINTES THERMIQUES :
!       (DILATATION ISOTROPE POUR LE MOMENT !!)
!
!       ON RETIRE       - A DEPST, L INCREMENT DE DEFORMATION DUE
!                         A LA DILATATION(TEMP)
!                       - ET A EPSDT , LA DEFORMATION DE DILATATION A T
!
!       POUR OBTENIR    - L'INCREMENT DE DEFORMATION MECANIQUE DEPSM
!                       - ET LA DEFORMATION MECANIQUE A T      EPSDM
!
!       ON A SIG = HOOK EPSE  = HOOK ( EPST - EPSP - EPSTH )
!                             = HOOK ( EPST - EPSP ) - HOOK EPSTH
!       DONC            SIG   = SIGM                 + SIGTH
!       AVEC            SIGTH = - HOOK EPSTH
!                             = - HOOK ALPHA ( T - TREF ) I
!       OU   EN PRENANT EPS   = EPST - EPSTH
!                       SIG   = HOOK ( EPS - EPSP )
!
!       ON PEUT DONC - SOIT TRAVAILLER AVEC EPST ET AJOUTER SIGTH APRES
!                    - SOIT TRAVAILLER AVEC EPS = EPST - EPSTH
!                      CE QUI EST FAIT ICI
!       ----------------------------------------------------------------
!       IN      NMAT    DIMENSION  DE MATER
!               MATERD  COEFFICIENTS MATERIAU A T
!               MATERF  COEFFICIENTS MATERIAU A T+DT
!               TD      TEMPERATURE DEBUT INCREMENT
!               TF      TEMPERATURE FIN INCREMENT
!               TR      TEMPERATURE DE REFERENCE
!               DEPST   INCREMENT DE DEFORMATION TOTALE
!               EPSDT   DEFORMATION TOTALE A T
!       OUT     DEPSM   INCREMENT DE DEFORMATION MECANIQUE
!               EPSDM   DEFORMATION MECANIQUE A T
!       ----------------------------------------------------------------
#include "asterc/iisnan.h"
#include "asterc/r8vide.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
    integer :: kpg, ksp, ndt, ndi, nmat, k, iret
    character(len=*) :: fami
    real(kind=8) :: td, tf, tr, tempd, tempf, tref
    real(kind=8) :: epsdt(6), depst(6)
    real(kind=8) :: epsdm(6), depsm(6), alphfn, alphfl, alphft
    real(kind=8) :: alphad, alphaf, alphdl, alphdt, alphdn
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
!       ----------------------------------------------------------------
    common /tdim/   ndt  , ndi
!       ----------------------------------------------------------------
    if (iisnan(tref) .eq. 0) then
        if (tref .eq. r8vide()) then
            call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                        ksp, td, iret)
            call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                        ksp, tf, iret)
            call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                        ksp, tr, iret)
        else
            td=tempd
            tf=tempf
            tr=tref
        endif
    else
        td=tempd
        tf=tempf
        tr=tref
    endif
!
    if ((iisnan(tf).eq.0) .and. (iisnan(td).eq.0)) then
        if (iisnan(tr) .ne. 0) then
            call utmess('F', 'CALCULEL_31')
        else
            if (materd(nmat,1) .eq. 0) then
                alphad = materd(3,1)
                alphaf = materf(3,1)
                do 110 k = 1, ndi
                    depsm(k) = depst(k) - ( alphaf*(tf-tr) - alphad*( td-tr))
                    epsdm(k) = epsdt(k) - ( alphad*(td-tr) )
110              continue
!
                do 111 k = ndi+1, ndt
                    depsm(k) = depst(k)
                    epsdm(k) = epsdt(k)
111              continue
!
            else if (materd(nmat,1).eq.1) then
!
                alphdl = materd(73,1)
                alphdt = materd(74,1)
                alphdn = materd(75,1)
!
                alphfl = materf(73,1)
                alphft = materf(74,1)
                alphfn = materf(75,1)
!
                depsm(1) = depst(1) - ( alphfl*(tf-tr) - alphdl*(td- tr))
                depsm(2) = depst(2) - ( alphft*(tf-tr) - alphdt*(td- tr))
                depsm(3) = depst(3) - ( alphfn*(tf-tr) - alphdn*(td- tr))
!
                epsdm(1) = epsdt(1) - ( alphdl*(td-tr) )
                epsdm(2) = epsdt(2) - ( alphdt*(td-tr) )
                epsdm(3) = epsdt(3) - ( alphdn*(td-tr) )
!
                do 112 k = 4, 6
                    depsm(k) = depst(k)
                    epsdm(k) = epsdt(k)
112              continue
            endif
!
        endif
    else
        alphad = materd(3,1)
        alphaf = materf(3,1)
        do 113 k = 1, ndt
            depsm(k) = depst(k)
            epsdm(k) = epsdt(k)
113      continue
    endif
end subroutine

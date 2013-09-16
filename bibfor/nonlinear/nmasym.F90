subroutine nmasym(fami, kpg, ksp, icodma, option,&
                  xlong0, a, tmoins, tplus, dlong0,&
                  effnom, vim, effnop, vip, klv,&
                  fono)
! ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ------------------------------------------------------------------
    implicit none
#include "asterfort/nm1das.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
    integer :: kpg, ksp, neq, nbt, nvar, icodma
    parameter   (neq = 6,nbt = 21,nvar=4)
!
    character(len=*) :: fami, option
    real(kind=8) :: xlong0, a, syc, syt, etc, ett, cr
    real(kind=8) :: e, dlong0, tmoins, tplus
    real(kind=8) :: effnom, vim(nvar)
    real(kind=8) :: effnop, vip(nvar), fono(neq), klv(nbt)
! -------------------------------------------------------------------
!
!    TRAITEMENT DE LA RELATION DE COMPORTEMENT -ELASTOPLASTICITE-
!    ECROUISSAGE ISOTROPE ASYMETRIQUE LINEAIRE - VON MISES-
!    POUR UN MODELE BARRE ELEMENT MECA_BARRE
!
! -------------------------------------------------------------------
! IN  :
!       XLONG0 : LONGUEUR DE L'ELEMENT DE BARRE AU REPOS
!       A      : SECTION DE LA BARRE
!       TMOINS : INSTANT PRECEDENT
!       TPLUS  : INSTANT COURANT
!       XLONGM : LONGEUR DE L'ELEMENT AU TEMPS MOINS
!       DLONG0 : INCREMENT D'ALLONGEMENT DE L'ELEMENT
!       EFFNOM : EFFORT NORMAL PRECEDENT
!       OPTION : OPTION DEMANDEE (R_M_T,FULL OU RAPH_MECA)
!
! OUT : EFFNOP : CONTRAINTE A L'INSTANT ACTUEL
!       VIP    : VARIABLE INTERNE A L'INSTANT ACTUEL
!       FONO   : FORCES NODALES COURANTES
!       KLV    : MATRICE TANGENTE
!
!----------VARIABLES LOCALES
!
    real(kind=8) :: sigm, deps, dsdem, dsdep, sigp, xrig
    integer :: nbpar, nbres, kpg1, spt
!
    real(kind=8) :: valpar, valres(4)
    integer :: icodre(4)
    character(len=8) :: nompar, nomela, nomasl(4), famil, poum
    data nomela / 'E' /
    data nomasl / 'SY_C', 'DC_SIGM_','SY_T','DT_SIGM_' /
!
!----------INITIALISATIONS
!
    call r8inir(nbt, 0.d0, klv, 1)
    call r8inir(neq, 0.d0, fono, 1)
!
!----------RECUPERATION DES CARACTERISTIQUES
!
    deps = dlong0/xlong0
    sigm    =effnom/a
!
! --- CARACTERISTIQUES ELASTIQUES
!
    nbres = 2
    nbpar = 0
    nompar = '  '
    valpar = 0.d0
    famil='FPG1'
    kpg1=1
    spt=1
    poum='+'
    call rcvalb(famil, kpg1, spt, poum, icodma,&
                ' ', 'ELAS', 0, nompar, [valpar],&
                1, nomela, valres, icodre, 1)
    e = valres(1)
!
! --- CARACTERISTIQUES ECROUISSAGE LINEAIRE ASYMETRIQUE
!
!
!JMP  NBRES = 5
    nbres = 4
    nbpar = 0
    call rcvalb(fami, 1, 1, '+', icodma,&
                ' ', 'ECRO_ASYM_LINE', nbpar, nompar, [valpar],&
                nbres, nomasl, valres, icodre, 1)
    syc = valres(1)
    etc = valres(2)
    syt = valres(3)
    ett = valres(4)
!JMP    CR     = VALRES(5) MODELE DE RESTAURATION PAS AU POINT
!
    cr = 0.d0
!
!
    call nm1das(fami, kpg, ksp, e, syc,&
                syt, etc, ett, cr, tmoins,&
                tplus, icodma, sigm, deps, vim,&
                sigp, vip, dsdem, dsdep)
    effnop=sigp*a
!
! --- CALCUL DU COEFFICIENT NON NUL DE LA MATRICE TANGENTE
!
    if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
!
        if (option(11:14) .eq. 'ELAS') then
            xrig=e*a/xlong0
        else
            if (option(1:14) .eq. 'RIGI_MECA_TANG') then
                xrig= dsdem*a/xlong0
            else
                xrig= dsdep*a/xlong0
            endif
        endif
        klv(1) = xrig
        klv(7) = -xrig
        klv(10) = xrig
    endif
!
! --- CALCUL DES FORCES NODALES
!
    fono(1) = -effnop
    fono(4) = effnop
!
! -------------------------------------------------------------
!
end subroutine

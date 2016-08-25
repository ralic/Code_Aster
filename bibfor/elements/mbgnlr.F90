subroutine mbgnlr(option,vecteu,matric,nno,ncomp,imate,icompo,dff,alpha,beta,h,&
                  preten,igeom,ideplm,ideplp,kpg,fami,ipoids,icontp,ivectu,imatuu)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
#include "asterfort/r8inir.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "jeveux.h"
#include "asterfort/mbhenh.h"
#include "asterfort/mbhesv.h"
#include "asterfort/mbpk2c.h"
#include "asterfort/mbtgin.h"
#include "asterfort/mbvfie.h"
#include "asterfort/subaco.h"
#include "asterfort/sumetr.h"
#include "asterfort/subacv.h"
#include "asterfort/utmess.h"
!
    character(len=4) :: fami
    character(len=16) :: option
    integer :: nno, ncomp, kpg
    integer :: imate, icompo, igeom, ideplm, ideplp, ipoids, icontp, ivectu
    integer :: imatuu
    real(kind=8) :: dff(2, nno), alpha, beta, h, preten
    aster_logical :: vecteu, matric
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE COMPORTEMENT :
!                            - FULL_MECA
!                            - RAPH_MECA
!                            - RIGI_MECA_TANG
!                          POUR LES MEMBRANES EN GRANDES DEFORMATIONS
! ----------------------------------------------------------------------
! IN  OPTION            OPTION DE CALCUL
!     NOMTE             NOM DU TYPE ELEMENT
!     VECTEU            BOOL: 1 SI FULL_MECA OU RAPH_MECA
!     MATRIC            BOOL: 1 SI FULL_MECA OU RIGI_MECA
!     NNO               NOMBRE DE NOEUDS
!     NCOMP             NOMBRE DE COMPOSANTS DANS LES VECTEURS COLONNES
!                       DE CONTRAINTE ET DEFORMATION
!     IMATE             ADRESSE DANS ZI DU TABLEAU PMATERC
!     ICOMPO            ADRESSE DANS ZK16 DU TABLEAU PCOMPOR
!     DFF               DERIVEE DES F. DE FORME
!     ALPHA, BETA       ANGLES DEF. LA BASE DE L'ÉCRITURE DES CONTRAINTES
!     H                 EPAISSEUR DE LA MEMBRANE
!     PRETEN            PRECONTRAINTES
!     IGEOM             ADRESSE DANS ZR DU TABLEAU PGEOMER
!     IDEPLM            ADRESSE DANS ZR DU TABLEAU PDEPLMR
!     IDEPLP            ADRESSE DANS ZR DU TABLEAU PDEPLPR
!     KPG               NUMERO DU POINT DE GAUSS DANS LA BOUCLE
!     FAMI              NOM DE LA FAMILLE DE POINTS DE GAUSS :
!                       'RIGI','MASS',..
!     IPOIDS            ADRESSE DANS ZR DU TABLEAU POIDS
!     ICONTP            ADRESSE DANS ZR DU TABLEAU PCONTPR
!     IVECTU            ADRESSE DANS ZR DU TABLEAU PMATUUR
!     IMATUN            ADRESSE DANS ZR DU TABLEAU PMATUNS
!
! OUT ***          ***
! ----------------------------------------------------------------------
! aslint: disable=W1504
!
    integer :: n, nn, c, incm
    real(kind=8) :: posdef(3*nno)
    real(kind=8) :: covaini(3, 3), metrini(2, 2), jacini, cnvaini(3, 2), aini(2, 2)
    real(kind=8) :: covadef(3, 3), metrdef(2, 2), jacdef, cnvadef(3, 2), adef(2, 2)
    real(kind=8) :: sigpk2(2,2), dsigpk2(2, 2, 2, 2), sighca(3), sigpk2temp(3)
    real(kind=8) :: ktgt(3*nno,3*nno)
    real(kind=8) :: vecfie(3*nno)
    
    
! ON S'ASSURE QUE L'OPTION UTILISEE EST PRISE EN COMPTE
!
    if ((option .ne.'FULL_MECA').and. (option .ne.'RAPH_MECA')&
                       .and. (option.ne.'RIGI_MECA_TANG')) then
        call utmess('F', 'MEMBRANE_5')
    end if
    
! - CALCUL DES COORDONNEES COVARIANTES ET CONTRAVARIANTES DE LA SURFACE INITIALE
    call subaco(nno, dff, zr(igeom), covaini)
    call sumetr(covaini, metrini, jacini)
    call subacv(covaini, metrini, jacini, cnvaini, aini)
   
! - CALCUL DES COORDONNEES COVARIANTES ET CONTRAVARIANTES DE LA SURFACE DEFORMEE
!   
    
    do n = 1, 3*nno
            posdef(n) = zr(igeom+n-1) + zr(ideplm+n-1) + zr(ideplp+n-1)
    end do
    
    call subaco(nno, dff, posdef, covadef)
    call sumetr(covadef, metrdef, jacdef)
    
    call subacv(covadef, metrdef, jacdef, cnvadef, adef)
    
   
! - ON APPELLE LA LDC HYPERELASTIQUE NEO-HOOKEENE
! -- ON OBTIENT LE CONTRAINTES A L'ITERATION DE NEWTON (i-1) (SIGPK2: TENSEUR SYM) 
!
    if (zk16 (icompo)(1:16).eq.'ELAS_HYP_MEMB_SV') then
        call mbhesv(imate,kpg,fami,aini,metrini,metrdef,sigpk2,dsigpk2)
    elseif (zk16 (icompo)(1:16).eq.'ELAS_HYP_MEMB_NH') then
        call mbhenh(imate,kpg,fami,aini,adef,jacini,jacdef,sigpk2,dsigpk2)
    else
        ASSERT(.false.)
    endif
    
! --- SI LA NORME EUCLIDIENNE DE SIGPK2 EST NULLE, ON APPLIQUE DES PRECONTRAINTES

    if (sqrt(sigpk2(1,1)**2+2*sigpk2(1,2)**2+sigpk2(2,2)**2).lt.1.0d-6) then
        sigpk2(1,1) = sigpk2(1,1) + preten
        sigpk2(2,2) = sigpk2(2,2) + preten
    endif
    
    
! - ON CALCUL LA MATRICE TANGENTE ELEMENTAIRE DUE AUX EFFORTS INTERNES
!
    if ((option(1:9).eq.'FULL_MECA').or. (option(1:10).eq.'RIGI_MECA_')) then
    
        call mbtgin(nno,kpg,dff,sigpk2,dsigpk2,ipoids,h,covadef,ktgt)
        
    end if
    
    if ((option .eq.'RAPH_MECA').or. (option(1:9).eq.'FULL_MECA')) then
    
! - ON EN DEDUIT LES CONTRAINTES INTEGREES (SUR L'EPAISSEUR) DE CAUCHY
! -- SIGMA_CAUCHY = (SIGMA11, SIGMA22, SIGMA12)
        
        sigpk2temp(1) = sigpk2(1,1)
        sigpk2temp(2) = sigpk2(2,2)
        sigpk2temp(3) = sigpk2(1,2)
        
       
        call mbpk2c(0 ,alpha, beta, h,covaini,jacini,jacdef,sigpk2temp,sighca)
        
! - CALCUL DU VECTEUR FORCE INTERNE ELEMENTAIRE
!    
        call mbvfie(nno,kpg,dff,sigpk2,ipoids,h,covadef,vecfie)
        
    end if
    
! - RANGEMENT DES RESULTATS
!
    if (vecteu) then
        do n = 1, 3*nno
            zr(ivectu + n - 1) = zr(ivectu + n - 1) + vecfie(n)*jacini
        end do
        do c = 1, ncomp
            zr(icontp+(kpg-1)*ncomp+c-1)=sighca(c)
        end do
    endif
    
    
    
    if (matric) then
        incm = 0
        do n = 1, 3*nno
            do nn = 1, n
                  incm = incm + 1
                  zr(imatuu+incm-1) = zr(imatuu+incm-1) + ktgt(n,nn)*jacini
            end do
        end do
    endif
    
    
end subroutine

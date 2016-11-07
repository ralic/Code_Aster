subroutine lc0059(fami, kpg, ksp, imate,&
                  compor, crit, instam, instap, neps, epsm,&
                  deps, nsig, sigm, vim, option, angmas,&
                  sigp, vip, tmin, tpin, trefin, tampon,&
                  typmod, icomp, nvi, dsidep, codret)

!
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

!!!
!!! MODELE LKR ROUTINE PRINCIPALE
!!!

! ======================================================================
! VARIABLES INTERNES DU MODELE :
!         1.  RXIP      : VARIABLE D ECROUISSAGE MECA. PLASTIQUE
!         2.  RGAMMAP   : DISTORSION PLASTIQUE
!         3.  RXIVP     : VARIABLE D ECROUISSAGE DU MECANISME 
!                         VISCOPLASTIQUE
!         4.  RGAMMAVP  : DISTORSION VISCOPLASTIQUE
!         5.  RINDICDIL : INDICATEUR DE DILATANCE : 1 SI DIL    0 SINON
!         6.  INDIVISC  : INDICATEUR DE VISCO.    : 1 SI VISCO. 0 SINON
!         7.  INDIPLAS  : INDICATEUR DE PLAST.    : 1 SI PLAST. 0 SINON
!         8.  RDVME     : DEF. VOL. ELAS. MECA.
!         9.  RDVTE     : DEF. VOL. ELAS. THER.
!         10. RDVPL     : DEF. VOL. PLAS.
!         11. RDVVP     : DEF. VOL. VISCOPLAS.
!         12. DOMAINES  : DOMAINE EN FONCTION DES VALEURS DE XIP
!                          ELAS                      ---> DOMAINE = 0
!                          PLAS PRE-PIC              ---> DOMAINE = 1
!                          PLAS POST-PIC AVT CLIVAGE ---> DOMAINE = 2
!                          PLAS POST-PIC AP CLIVAGE  ---> DOMAINE = 3
! ======================================================================
    
    implicit none
     
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/plasti.h"
#include "asterfort/rcvarc.h"
#include "asterfort/srcomp.h"
#include "asterfort/utlcal.h"
    
    !!!
    !!! Variables
    !!!
    
    integer :: imate, kpg, ksp, codret, icomp, nvi
    integer :: neps, nsig, iret1, iret2, iret3
    real(kind=8) :: crit(*), angmas(*), instam, instap, tampon(*)
    real(kind=8) :: epsm(neps), deps(neps), sigm(nsig), sigp(nsig)
    real(kind=8) :: vim(nvi), vip(nvi)
    real(kind=8) :: dsidep(6,6), tmin, tpin, trefin, tm, tp, tref
    character(len=16) :: compor(*), option, algo
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    
    !!!
    !!! Cas sans thm : initialisation a r8vide de r8bid passe en argument pour
    !!!                tmin, tpin et trefin
    !!!
    
    if (trefin .eq. r8vide()) then
        !!! on verifie si on fait appel a affe_varc dans le fichier de commande
        call rcvarc(' ', 'TEMP', '-', fami, kpg, ksp, tm, iret1)
        call rcvarc(' ', 'TEMP', '+', fami, kpg, ksp, tp, iret2)
        call rcvarc(' ', 'TEMP', 'REF', fami, kpg, ksp, tref, iret3)
        !!! en l'absence de varc, on affecte 0 aux temperatures
        if (iret1.eq.1 .or. iret2.eq.1 .or. iret3.eq.1) then
            tm   = 0.d0
            tp   = 0.d0
            tref = 0.d0
        endif
    
    !!!
    !!! Cas thm : initialisation de tmin, tpin et trefin dans calcme
    !!!
    
    else
        tm   = tmin
        tp   = tpin
        tref = trefin
    endif
    
    !!!
    !!! Choix de l'algorithme d'integration
    !!!
    
    call utlcal('VALE_NOM', algo, crit(6))
    
    !!! explicite
    if ((algo(1:10).eq.'SPECIFIQUE') .or. (option(1:14).eq.'RIGI_MECA_TANG')) then
        
        call srcomp(typmod, imate, instam, instap, tm, tp, tref, deps, sigm, vim,&
                    option, sigp, vip, dsidep, codret, nvi)
     
    !!! implicite
    else
        
        call plasti(fami, kpg, ksp, typmod, imate,&
                    compor, crit, instam, instap, tm,&
                    tp, tref, epsm, deps, sigm,&
                    vim, option, angmas, sigp, vip,&
                    dsidep, icomp, nvi, tampon, codret)
    
    endif

end subroutine

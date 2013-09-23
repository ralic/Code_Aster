subroutine acplcr(nbvec,jvectn, jvectu, jvectv, nbordr,&
                  kwork, sompgw, jrwork, tspaq, ipg, dectau,nommet, &
                  jvecpg, jnorma,rayon,jresun, jdtaum,jtauma,&
                  jsgnma,jdsgma )

    implicit   none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/raycir.h"
#include "asterfort/taurlo.h"
!
    integer :: nbvec, jvectn, jvectu, jvectv, nbordr, kwork
    integer :: sompgw, jrwork, tspaq, ipg, jvecpg, jnorma
    logical :: rayon
    integer :: dectau, jresun, jdtaum,jtauma,jsgnma,jdsgma
    character(len=16) ::nommet
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
! person_in_charge: van-xuan.tran at edf.fr
! ---------------------------------------------------------------------
! BUT: CONSTRUIRE LES COMPOSANTES u ET v DU VECTEUR DE CISAILLEMENT TAU
!      DANS LE REPERE LOCAL PERPENDICULAIRE AU VECTEUR NORMAL, POUR
!      TOUS LES VECTEURS NORMAUX A TOUS LES NUMEROS D'ORDRE.
! ----------------------------------------------------------------------
! ARGUMENTS :
!     NBVEC   : IN  : NOMBRE DE VECTEURS NORMAUX.
!     JVECTN  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS NORMAUX.
!     JVECTU  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS u DU PLAN DE CISAILLEMENT.
!     JVECTV  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS v DU PLAN DE CISAILLEMENT.
!     NBORDR  : IN  : NOMBRE DE NUMEROS D'ORDRE.
!     KWORK   : IN  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET DE
!                               MAILLES ;
!                     KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
!                               MAILLES.
!     SOMPGW  : IN  : SOMME DES POINTS DE GAUSS DES N MAILLES PRECEDANT
!                     LA MAILLE COURANTE.
!     JRWORK  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                     L'HISTORIQUE DES TENSEURS DES CONTRAINTES
!                     ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
!                     DU <<PAQUET>> DE MAILLES.
!     TSPAQ   : IN  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
!                     COURANT.
!     IPG     : IN  : IEME POINT DE GAUSS.
!     JVECPG  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                     LES COMPOSANTES u ET v DU VECTEUR TAU
!                     (CISAILLEMENT), POUR TOUS LES NUMEROS
!                     D'ORDRE DE CHAQUE VECTEUR NORMAL.
!     jnorma  : IN  : ADRESS DU VECTEUR NORMAL
!     rayon   : IN : LOGICAL POUR VOIR SI LA METHODE CIRCONSCRITE EST
!                     NECESSAIR  
!     jresun  : IN:  POUR RAYCIR
!     jdtaum  : OUT: ADRESSS D_TAU OU D_GAMMA POUR TOUTE ORIETATION
!     jtauma  : OUT: ADRESSS TAU_MAX OUGAMMA_MAX POUR TOUTE ORIETATION
!     jsgnma  : OUT: ADRESSS D_SIGN OU D_EPSN POUR TOUTE ORIETATION
!     jdsgma  : OUT: ADRESSS SIGN_MAX OU EPSN_MAX POUR TOUTE ORIETATION
! ----------------------------------------------------------------------
    integer :: ivect, iordr, n
    real(kind=8) :: norm, cutau, cvtau
    real(kind=8) ::epsilo, tau, dnomin, dnomax

!     ------------------------------------------------------------------

!    call jemarq()

    epsilo = 1.0d-7

    call taurlo(nbvec, jvectn, jvectu, jvectv, nbordr,&
                kwork, sompgw, jrwork, tspaq, ipg,dectau,&
                jvecpg, jnorma)

!  CDU RAYON CIRCONSCRIT
!
    if (rayon) then
        call raycir(jvecpg, jdtaum, jresun, nbordr, nbvec,&
            nommet)
    endif

! SHEAR MAX
    n = 0
    do 10 ivect = 1, nbvec
        zr(jtauma+ivect-1) = r8prem()
        do 20 iordr = 1, nbordr
            n = n + 1
            cutau = zr( jvecpg + (n-1)*2 ) 
            cvtau = zr( jvecpg + (n-1)*2 + 1 ) 
            tau = sqrt(cutau**2 +cvtau**2)
            if ((tau .gt. epsilo) .and. & 
                ((tau-zr(jtauma+ivect)) .gt. epsilo)) then
                zr(jtauma+ivect-1) = tau
            endif
20      continue
!
10  end do


!! AMPLITUDE NORMAL MAX    
    do 12 ivect = 1, nbvec      
        dnomin = zr(jnorma)
        dnomax = zr(jnorma)
        do 22 iordr = 1, nbordr
            norm = zr( jnorma -1 +iordr+(ivect-1)*nbordr)
            if ((dnomin - norm) .gt. epsilo) then
                dnomin = norm
            endif
            if ((dnomax - norm) .lt. epsilo) then
                dnomax = norm
            endif 
22     continue

       zr(jdsgma+ivect-1) = (dnomax - dnomin)/2
       zr(jsgnma+ivect-1) = dnomax
  
!
12 end do

end subroutine

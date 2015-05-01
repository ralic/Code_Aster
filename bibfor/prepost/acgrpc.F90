subroutine acgrpc(nbordr, kwork,&
                  sompgw, jrwork, tspaq, ipg, &
                  nommet, forcri,nompar, vanocr, respc,vnmax)
    implicit   none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/loisem.h"
#include "asterc/lor8em.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/acgrcr.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedisp.h"
#include "asterfort/raycir.h"
#include "asterfort/taurlo.h"
#include "asterfort/vecnuv.h"
#include "asterfort/wkvect.h"
#include "asterfort/utmess.h"
    integer :: nbordr, kwork
    integer :: sompgw, jrwork, tspaq, ipg
    character(len=16) :: nommet, forcri
    character(len=8) :: nompar(35)
    real(kind=8) :: respc(24),vnmax(6), vanocr(23)

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
! ---------------------------------------------------------------------
! BUT: POUR LA FATIGUE A AMPLITUDE CONSTANTE
!      DETERMINER LE PLAN DES MAX DES TAU_MAX ET CALCULER DES GRANDEURS
!
!
! REMARQUE: CETTE SUBROUTINE EST APPLICABLE POUR UN NOEUD OU IPG EGALE
!           A 1 ET SOMPGW = SOMNOW,JVECPG = JVECNO
! ----------------------------------------------------------------------
! ARGUMENTS :
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
!    NOMMET     IN    NOM DE METHOD D'APPROCHEMENT DE CERCLE ("CERCLE
!                     EXACT" ET "CERCLE APPROCHE")
!    VALA       IN    VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
!    COEFPA     IN    COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
!   VRSESU      OUT   TABLEAU DES RESULTATS (GRANDEURS ET DOMMAGE).
!                     POUR L'INSTANT, LA DIMENSION DE VRESU EST 24
! ----------------------------------------------------------------------
    integer :: i, j, k, n
    integer :: nbvec, dim, jvpg2
    integer :: jvecn2, jvecu2, jvecv2
    integer :: ideb, ngam 
    integer :: tneces, tdisp(1), jvecno, jnorm2
    integer :: tab2(18),jvectn, jvectu, jvectv
    integer :: vali(2), tnecno, jnorma
!
    real(kind=8) :: dgam, dphi, tab1(18)
    real(kind=8) :: epsilo, gamma, pi
    real(kind=8) :: gammam, phim, dgam2, dphi2, phi0
    real(kind=8) :: nxm(2), nym(2), nzm(2)

!
!-----------------------------------------------------------------------
    data  tab1/ 180.0d0, 60.0d0, 30.0d0, 20.0d0, 15.0d0, 12.857d0,&
     &             11.25d0, 10.588d0, 10.0d0, 10.0d0, 10.0d0, 10.588d0,&
     &             11.25d0, 12.857d0, 15.0d0, 20.0d0, 30.0d0, 60.0d0 /
!
    data  tab2/ 1, 3, 6, 9, 12, 14, 16, 17, 18, 18, 18, 17, 16, 14,&
     &           12, 9, 6, 3 /
!
!-----------------------------------------------------------------------
    !     ------------------------------------------------------------------
!
!234567
!
!
    epsilo = 1.0d-7
    pi = r8pi()
! PROJECTION DE L'HISTORIQUE DU STRESS ET STRAIN DANS UN PLAN.
!
! CONSTRUCTION DU VECTEUR CONTENANT DELTA_TAU_MAX
! CONSTRUCTION DU VECTEUR CONTENANT LA VALEUR DU POINTEUR PERMETTANT
!              DE RETROUVER LE VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX
!
!      call wkvect('&&ACGRPC.DTAU_MAX', 'V V R', 209, jdtaum)
!      call wkvect('&&ACGRPC.RESU_N', 'V V I', 209, jresun)
! !
! CONSTRUCTION DU VECTEUR NORMAL SUR UNE DEMI SPHERE
! CONSTRUCTION DU VECTEUR U DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
! CONSTRUCTION DU VECTEUR V DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
!
    call wkvect('&&ACGRPC.VECT_NORMA', 'V V R', 630, jvectn)
    call wkvect('&&ACGRPC.VECT_TANGU', 'V V R', 630, jvectu)
    call wkvect('&&ACGRPC.VECT_TANGV', 'V V R', 630, jvectv)

    tneces = 209*nbordr*2
    tnecno = 209*nbordr
    call jedisp(1, tdisp)
    tdisp(1) = (tdisp(1) * loisem()) / lor8em()
    if (tdisp(1) .lt. tneces) then
        vali (1) = tdisp(1)
        vali (2) = tneces
        call utmess('F', 'PREPOST5_8', ni=2, vali=vali)
    else
        call wkvect('&&ACGRPC.VECTNO', 'V V R', tneces, jvecno)
        call wkvect('&&ACGRPC.VECT_NOR', 'V V R', tnecno, jnorma)
    endif
!
    dgam = 10.0d0
!
    n = 0
    k = 1
    ideb = 1
    dim = 627
    do 300 j = 1, 18
        gamma=(j-1)*dgam*(pi/180.0d0)
        dphi=tab1(j)*(pi/180.0d0)
        phi0=dphi/2.0d0
        ngam=tab2(j)
!
        call vecnuv(ideb, ngam, gamma, phi0, dphi,&
                    n, k, dim, zr( jvectn), zr(jvectu),&
                    zr(jvectv))
!
300  end do


    do  21 i = 1, 24
        respc(i) = 0.0d0
21  continue

!!!!IDENTIFIER LE PLAN DE MAXIMUM DE GRANDEUR CRITIQUE
    nbvec = 209

    call acgrcr(nbvec, jvectn, jvectu, jvectv, nbordr, kwork,&
                  sompgw, jrwork, tspaq, ipg, &
                  nommet, jvecno, jnorma, forcri,nompar,vanocr, respc,vnmax)


!!!REFINEMENT DE PLAN CRITIQUE
    call wkvect('&&ACGRPC.VECT_NORMA2', 'V V R', 27, jvecn2)
    call wkvect('&&ACGRPC.VECT_TANGU2', 'V V R', 27, jvecu2)
    call wkvect('&&ACGRPC.VECT_TANGV2', 'V V R', 27, jvecv2)
!

    call wkvect('&&ACGRPC.VECTPG2', 'V V R', 18*nbordr, jvpg2)
    call wkvect('&&ACGRPC.VECTNO2', 'V V R', 9*nbordr, jnorm2)

    dim = 27
!
    do 441 k = 1, 2

        nxm(k) = vnmax(1+(k-1)*3)
        nym(k) = vnmax(2+(k-1)*3)
        nzm(k) = vnmax(3+(k-1)*3)

        gammam = atan2(sqrt(abs(1.0d0-nzm(k)**2)),nzm(k))
        if (gammam .lt. 0.0d0) then
            gammam = gammam + pi
        endif
!
        if ((abs(nym(k)) .lt. epsilo) .and. (abs(nxm(k)) .lt. epsilo)) then
            phim = 0.0d0
        else
            phim = atan2(abs(nym(k)),nxm(k))
        endif
        if (phim .lt. 0.0d0) then
            phim = phim + pi
        endif
!
        if (abs(gammam) .lt. epsilo) then
            gamma = 5.0d0*(pi/180.0d0)
            dphi2 = 60.0d0*(pi/180.0d0)
            phi0 = 0.0d0
            n = 0
!
            call vecnuv(1, 6, gamma, phi0, dphi2,&
                        n, 1, dim, zr( jvecn2), zr(jvecu2),&
                        zr(jvecv2))
!
            gamma = 0.0d0
            phi0 = pi
!
            call vecnuv(1, 1, gamma, phi0, dphi2,&
                        n, 1, dim, zr( jvecn2), zr(jvecu2),&
                        zr(jvecv2))
!
            nbvec = 7

            call acgrcr(nbvec, jvecn2, jvecu2, jvecv2, nbordr, kwork,&
                  sompgw, jrwork, tspaq, ipg, &
                  nommet, jvpg2, jnorm2, forcri,nompar,vanocr, respc,vnmax)

        else
            dgam2 = 2.0d0*(pi/180.0d0)
            dphi2 = dgam2/sin(gammam)
            n = 0
            do 461 j = 1, 3
                gamma = gammam + (j-2)*dgam2
!
                call vecnuv(1, 3, gamma, phim, dphi2,&
                            n, 2, dim, zr(jvecn2), zr(jvecu2),&
                            zr(jvecv2))
!
461          continue
!
            nbvec = 9

            call acgrcr(nbvec, jvecn2, jvecu2, jvecv2, nbordr, kwork,&
                  sompgw, jrwork, tspaq, ipg, &
                  nommet, jvpg2, jnorm2, forcri,nompar,vanocr, respc,vnmax)

        endif

441 continue


    call jedetr('&&ACGRPC.DTAU_MAX')
    call jedetr('&&ACGRPC.RESU_N')
    call jedetr('&&ACGRPC.VECT_NORMA')
    call jedetr('&&ACGRPC.VECT_TANGU')
    call jedetr('&&ACGRPC.VECT_TANGV')
    call jedetr('&&ACGRPC.VECTNO')
    call jedetr('&&ACGRPC.VECT_NOR')

!     call jedetr('&&ACGRPC.VECT_NORMA1')
!     call jedetr('&&ACGRPC.VECT_TANGU1')
!     call jedetr('&&ACGRPC.VECT_TANGV1')
!     call jedetr('&&ACGRPC.VECTPG1')

    call jedetr('&&ACGRPC.VECT_NORMA2')
    call jedetr('&&ACGRPC.VECT_TANGU2')
    call jedetr('&&ACGRPC.VECT_TANGV2')
    call jedetr('&&ACGRPC.VECTPG2')
    call jedetr('&&ACGRPC.VECTNO2')


end subroutine

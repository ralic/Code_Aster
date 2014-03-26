subroutine lcpivm(fami, kpg, ksp, mate, compor,&
                  carcri, instam, instap, fm, df,&
                  vim, option, taup, vip, dtaudf,&
                  iret)
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
!
! aslint: disable=
    implicit none
#include "asterfort/calcdp.h"
#include "asterfort/ecpuis.h"
#include "asterfort/gdsmci.h"
#include "asterfort/gdsmhy.h"
#include "asterfort/gdsmin.h"
#include "asterfort/gdsmtg.h"
#include "asterfort/lcpima.h"
#include "asterfort/lcpitg.h"
#include "asterfort/nmcri6.h"
#include "asterfort/rcfonc.h"
#include "asterfort/zerofr.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    integer :: mate, iret, kpg, ksp
    character(len=16) :: compor, option
    character(len=*) :: fami
    real(kind=8) :: instam, instap
    real(kind=8) :: df(3, 3), fm(3, 3)
    real(kind=8) :: vim(8), vip(8)
    real(kind=8) :: taup(6), dtaudf(6, 3, 3)
    real(kind=8) :: carcri(*)
!
! ----------------------------------------------------------------------
!       INTEGRATION DE LA LOI DE COMPORTEMENT PLASTIQUE ISOTROPE
!              EN GRANDES DEFORMATIONS DE TYPE SIMO-MIEHE
!              AINSI QUE SA VERSION VISQUEUSE (LOI SINH)
! ----------------------------------------------------------------------
!
! IN  MATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR : COMPORTEMENT
! IN  CARCRI : PARAMETRES POUR L INTEGRATION DE LA LOI DE COMMPORTEMENT
!                 CARCRI(1) = NOMBRE D ITERATIONS
!                 CARCRI(3) = PRECISION SUR LA CONVERGENCE
! IN  INSTAM : INSTANT PRECEDENT
! IN  INSTAP : INSTANT COURANT
! IN  DF     : INCREMENT DU GRADIENT DE LA TRANSFORMATION
! IN  FM     : GRADIENT DE LA TRANSFORMATION A L INSTANT PRECEDENT
! IN  VIM    : VARIABLES INTERNES A L INSTANT DU CALCUL PRECEDENT
!                 VIM(1)=P (DEFORMATION PLASTIQUE CUMULEE)
!                 VIM(2)=INDICATEUR DE PLASTICITE
!                          0 : ELASTIQUE  1: PLASTIQUE
!                 TRE/3 AVEC E=(ID-BE)/2.D0 EST STOCKE DANS :
!                 VIP(3) POUR LA PLASTICITE OU
!                 VIP(1) POUR L ELASTICITE
! IN  OPTION : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! OUT TAUP   : CONTRAINTES DE KIRCHHOFF A L'INSTANT ACTUEL
! OUT VIP    : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DTAUDF : DERIVEE DE TAU PAR RAPPORT A DF  * (F)T
! OUT IRET   : CODE RETOUR DE  L'INTEGRATION DE LA LDC
!               IRET=0 => PAS DE PROBLEME
!               IRET=1 => DJ<0 ET INTEGRATION IMPOSSIBLE
! ----------------------------------------------------------------------
!  COMMON MATERIAU POUR VON MISES
!
    integer :: jprol, jvale, nbval, itmx
    real(kind=8) :: pm, young, nu, mu, unk, troisk, cother, sigy
    real(kind=8) :: sigm0, epsi0, dt, coefm, rpm, pente, apui, npui
    character(len=1) :: poum
    real(kind=8) :: xap, precr, rprim
!
    common /lcpim/&
     &          pm,young,nu,mu,unk,troisk,cother,&
     &          sigm0,epsi0,dt,coefm,rpm,pente,&
     &          apui,npui,sigy,jprol,jvale,nbval
! ----------------------------------------------------------------------
! COMMON GRANDES DEFORMATIONS SIMO - MIEHE
!
    integer :: ind(3, 3), ind1(6), ind2(6)
    real(kind=8) :: kr(6), rac2, rc(6), id(6, 6)
    real(kind=8) :: bem(6), betr(6), dvbetr(6), eqbetr, trbetr
    real(kind=8) :: jp, dj, jm, dfb(3, 3), mutrbe, tauteq
    real(kind=8) :: djdf(3, 3), dbtrdf(6, 3, 3)
!
    common /rconm6/mutrbe,tauteq
!
    common /gdsmc/&
     &            bem,betr,dvbetr,eqbetr,trbetr,&
     &            jp,dj,jm,dfb,&
     &            djdf,dbtrdf,&
     &            kr,id,rac2,rc,ind,ind1,ind2
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    logical :: resi, rigi, elas
    integer :: i, ij, line, n
    real(kind=8) :: dp, seuil
    real(kind=8) :: rp, pentep, airerp
    real(kind=8) :: em(6), ep(6), trtau, dvbe(6)
!
! ----------------------------------------------------------------------
!
!
! 1 - INITIALISATION
!-----------------------------------------------------------------------
!
!    DONNEES DE CONTROLE DE L'ALGORITHME
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
    elas = option(11:14).eq.'ELAS'
    call gdsmin()
!
!    LECTURE DES VARIABLES INTERNES (DEFORMATION PLASTIQUE CUMULEE ET
!                                   -DEFORMATION ELASTIQUE)
    pm=vim(1)
    call dcopy(6, vim(3), 1, em, 1)
    call dscal(3, rac2, em(4), 1)
!
!    CALCUL DES ELEMENTS CINEMATIQUES
    call gdsmci(fm, df, em)
!
!    CARACTERISTIQUES MATERIAU
    if (resi) then
        poum='+'
    else
        poum='-'
    endif
    call lcpima(fami, kpg, ksp, poum, mate,&
                compor, instam, instap, carcri, taup,&
                vim)
!
! 2 - RESOLUTION
!-----------------------------------------------------------------------
    if (resi) then
        seuil = mu*eqbetr - rpm
!
        if (seuil .le. 0.d0) then
            dp = 0.d0
            line = 0
!
        else
            line = 1
            if (compor .eq. 'VMIS_ISOT_LINE') then
                dp = seuil/(pente+mu*trbetr)
!
            else if (compor .eq. 'VMIS_ISOT_PUIS') then
                tauteq=mu*eqbetr
                mutrbe=mu*trbetr
                call ecpuis(young, sigy, apui, 1.d0/npui, pm,&
                            0.d0, rp, rprim)
                xap = (tauteq - rp)/mutrbe
                precr = carcri(3) * sigy
                itmx = nint(carcri(1))
!
                call zerofr(0, 'DEKKER', nmcri6, 0.d0, xap,&
                            precr, itmx, dp, iret, n)
                if (iret .ne. 0) goto 999
                call ecpuis(young, sigy, apui, 1.d0/npui, pm,&
                            dp, rp, rprim)
                pente=rprim
            else if (compor.eq.'VMIS_ISOT_TRAC') then
                call rcfonc('E', 1, jprol, jvale, nbval,&
                            e = young* trbetr/3, nu = nu, p = pm, rp = rp,&
                            rprim = pente, airerp = airerp, sieleq = mu*eqbetr, dp = dp)
            else
! CAS VISQUEUX : CALCUL DE DP PAR RESOLUTION DE
!  FPLAS - (R'+MU TR BEL)DP - PHI(DP) = 0
                call calcdp(carcri, seuil, dt, pente, mu*trbetr,&
                            sigm0, epsi0, coefm, dp, iret)
! DANS LE CAS NON LINEAIRE ON VERFIE QUE L ON A LA BONNE PENTE
                if (compor(10:14) .eq. '_TRAC') then
                    call rcfonc('V', 1, jprol, jvale, nbval,&
                                p = pm+dp, rp = rp, rprim = pentep)
                    do i = 1, nbval
                        if (abs(pente-pentep) .le. 1.d-3) then
                            goto 20
                        else
                            pente=pentep
                            seuil = mu*eqbetr - (rp-pente*dp)
                            call calcdp(carcri, seuil, dt, pente, mu* trbetr,&
                                        sigm0, epsi0, coefm, dp, iret)
                            call rcfonc('V', 1, jprol, jvale, nbval,&
                                        p = vim(1)+dp, rp = rp, rprim = pentep)
                        endif
                    end do
20                  continue
                endif
            endif
        endif
!
! 4 - MISE A JOUR DES CHAMPS
! 4.1 - CONTRAINTE
!
        call dcopy(6, dvbetr, 1, dvbe, 1)
        if (line .eq. 1) call dscal(6, 1-dp*trbetr/eqbetr, dvbe, 1)
!
        trtau = (troisk*(jp**2-1) - 3.d0*cother*(jp+1.d0/jp)) / 2.d0
!
        do ij = 1, 6
            taup(ij) = mu*dvbe(ij) + trtau/3.d0*kr(ij)
        end do
!
! 4.2 - CORRECTION HYDROSTATIQUE A POSTERIORI
!
        do  ij = 1, 6
            ep(ij)=(kr(ij)-jp**(2.d0/3.d0)*(dvbe(ij)+trbetr/3.d0*kr(&
            ij))) /2.d0
        end do
        call gdsmhy(jp, ep)
!
! 4.3 - P, DEFORMATION ELASTIQUE ET INDICE DE PLASTICITE
!
        vip(1) = pm+dp
        vip(2) = line
        call dcopy(6, ep, 1, vip(3), 1)
        call dscal(3, 1.d0/rac2, vip(6), 1)
    endif
!
! 5 - CALCUL DE LA MATRICE TANGENTE
!
    if (rigi) then
        if (.not. resi) then
            dp = 0.d0
            line = nint(vim(2))
            call dcopy(6, dvbetr, 1, dvbe, 1)
        endif
!
        if (elas) line = 0
!
        call gdsmtg()
        call lcpitg(compor, df, line, dp, dvbe,&
                    dtaudf)
    endif
999 continue
end subroutine

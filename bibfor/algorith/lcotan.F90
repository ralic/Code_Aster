subroutine lcotan(opt, angmas, etatd, etatf, fami,&
                  kpg, ksp, loi, mod, imat,&
                  nmat, materd, materf, epsd, deps,&
                  sigd, sigf, nvi, vind, vinf,&
                  drdy, vp, vecp, theta, dt,&
                  devg, devgii, timed, timef, comp,&
                  nbcomm, cpmono, pgl, nfs, nsg,&
                  toutms, hsr, nr, itmax, toler,&
                  typma, dsde, codret)
! aslint: disable=W1504
    implicit none
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
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!
!     CALCUL DE L'OPERATEUR TANGENT = DS/DE(T+DT) OU DS/DE(T)
!     CONVENTION :
!                 SUFFIXE D : DEBUT DU PAS DE TEMPS
!                 SUFFIXE F : FIN DU PAS DE TEMPS
!     ==================================================================
!     ARGUMENTS
!
!     IN FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!        KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!        IMAT    ADRESSE DU MATERIAU CODE
!        COMP    COMPORTEMENT DE L ELEMENT
!                COMP(1) = RELATION DE COMPORTEMENT (ROUSSELIER.)
!                COMP(2) = NB DE VARIABLES INTERNES
!                COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
!        CRIT    CRITERES  LOCAUX
!                CRIT(1) = NOMBRE D ITERATIONS MAXI (ITER_INTE_MAXI)
!                CRIT(3) = TOLERANCE DE CONVERGENCE(RESI_INTE_RELA)
!                CRIT(4) = THETA
!                CRIT(5) = ITER_INTE_PAS (UTILISE PAR REDECE EN AMONT)
!                CRIT(6) = ALGO_INTE(NEWTON, NEWTON_PERT, NEWTON_RELI)
!        TIMED   INSTANT T
!        TIMEF   INSTANT T+DT
!        TEMPD   TEMPERATURE A T           POUR LA THM
!        TEMPF   TEMPERATURE A T+DT        POUR LA THM
!        TREF    TEMPERATURE DE REFERENCE  POUR LA THM
!        CES PARAMETRES DE TEMPERATURE NE SONT PAS PRIS EN COMPTE EN
!        MECANIQUE PURE (ON UTILISE LES VARIABLES DE COMMANDES)
!
!        EPSDT   DEFORMATION TOTALE A T
!        DEPST   INCREMENT DE DEFORMATION TOTALE
!        SIGD    CONTRAINTE A T
!        VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!        OPT     OPTION DE CALCUL
!                        'RIGI_MECA_TANG'> DSDE(T)
!                        'FULL_MECA'     > DSDE(T+DT), SIGF, VINF
!                        'RAPH_MECA'     > SIGF, VINF
!        ANGMAS  ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
!                +  0 SI NAUTIQUIES OU 2 SI EULER
!                + LES 3 ANGLES D'EULER
!     OUT
!        SIGF    CONTRAINTE A T+DT
!        VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!        DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!        CODRET  CODE RETOUR =0 OK, =1 => REDECOUPAGE DU PAS DE TEMPS
!
#include "asterfort/burjpl.h"
#include "asterfort/lchbvp.h"
#include "asterfort/lcjela.h"
#include "asterfort/lcjpla.h"
#include "asterfort/lcjplc.h"
    integer :: nmat, nsg, nfs, nbcomm(nmat, 3)
!
    character(len=*) :: fami
    character(len=7) :: etatd, etatf
    character(len=8) :: mod, typma
    character(len=16) :: comp(*), opt, loi
    character(len=24) :: cpmono(5*nmat+1)
!
    integer :: imat, ndt, ndi, nr, nvi, itmax, kpg, ksp, codret, k, l
    real(kind=8) :: toler, materd(nmat, 2), materf(nmat, 2), angmas(3)
    real(kind=8) :: vind(*), vinf(*), timed, timef, epsd(9), deps(9), sigd(6)
    real(kind=8) :: sigf(6)
    real(kind=8) :: theta, dt, devg(6), devgii, vp(3), vecp(3, 3), dsde(6, *)
    real(kind=8) :: pgl(3, 3)
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg), drdy(*)
!     ----------------------------------------------------------------
    common /tdim/   ndt  , ndi
!     ----------------------------------------------------------------
!     OPTIONS 'FULL_MECA' ET 'RIGI_MECA_TANG' = CALCUL DE DSDE
!     ----------------------------------------------------------------
!     EVALUATION DU JACOBIEN DSDE A (T+DT) POUR 'FULL_MECA'
!     ET CALCUL ELASTIQUE    ET   A (T)    POUR 'RIGI_MECA_TANG'
!     ----------------------------------------------------------------
!
    codret=0
!
!     MATRICE TANGENTE DE PREDICTION
!
    if (opt(1:9) .eq. 'RIGI_MECA') then
!
        if ((loi.eq.'LAIGLE') .or. (loi.eq.'BETON_BURGER_FP')) then
!
            call lcjela(loi, mod, nmat, materd, vind,&
                        dsde)
!
        else if ((etatd.eq.'PLASTIC').and.(loi.eq.'MONOCRISTAL')) then
!
            call lcjplc(loi, mod, angmas, imat, nmat,&
                        materf, timed, timef, comp, nbcomm,&
                        cpmono, pgl, nfs, nsg, toutms,&
                        hsr, nr, nvi, epsd, deps,&
                        itmax, toler, sigd, vind, sigd,&
                        vind, dsde, drdy, opt, codret)
            if (codret .ne. 0) goto 9999
!
        else if ((etatd.eq.'PLASTIC').and.(typma.eq.'VITESSE ')) then
            if ((loi(1:10).eq.'HOEK_BROWN') .or. (loi(1:14) .eq.'HOEK_BROWN_EFF')) then
! ---              HOEK-BROWN : CALCUL DES VALEURS ET VECTEURS PROPRES
! ---                           DU DEVIATEUR ELASTIQUE
                call lchbvp(sigd, vp, vecp)
            endif
            call lcjpla(fami, kpg, ksp, loi, mod,&
                        nr, imat, nmat, materd, nvi,&
                        deps, sigd, vind, dsde, sigd,&
                        vind, vp, vecp, theta, dt,&
                        devg, devgii, codret)
            if (codret .ne. 0) goto 9999
!
        else
!
!           CAS GENERAL : ELASTICITE LINEAIRE ISOTROPE OU ANISOTROPE
            call lcjela(loi, mod, nmat, materd, vind,&
                        dsde)
!
        endif
!
!
    else if (opt(1:9) .eq. 'FULL_MECA') then
!
        if (etatf .eq. 'ELASTIC') then
            if (loi(1:15) .eq. 'BETON_BURGER_FP') then
                call burjpl(nmat, materf, nr, drdy, dsde)
            else
                call lcjela(loi, mod, nmat, materf, vinf,&
                            dsde)
            endif
!
        else if (etatf .eq. 'PLASTIC') then
!   --->    ELASTOPLASTICITE ==>  TYPMA = 'VITESSE '
!   --->    VISCOPLASTICITE  ==>  TYPMA = 'COHERENT '
            if (typma .eq. 'COHERENT') then
                call lcjplc(loi, mod, angmas, imat, nmat,&
                            materf, timed, timef, comp, nbcomm,&
                            cpmono, pgl, nfs, nsg, toutms,&
                            hsr, nr, nvi, epsd, deps,&
                            itmax, toler, sigf, vinf, sigd,&
                            vind, dsde, drdy, opt, codret)
                if (codret .ne. 0) goto 9999
            else if (typma .eq. 'VITESSE ') then
                call lcjpla(fami, kpg, ksp, loi, mod,&
                            nr, imat, nmat, materd, nvi,&
                            deps, sigf, vinf, dsde, sigd,&
                            vind, vp, vecp, theta, dt,&
                            devg, devgii, codret)
                if (codret .ne. 0) goto 9999
            endif
        endif
!
    endif
!
! -   MODIFICATION EN CONTRAINTE PLANES POUR TENIR COMPTE DE
!     SIG3=0 ET DE LA CONSERVATION DE L'ENERGIE
    if (mod(1:6) .eq. 'C_PLAN') then
        do 136 k = 1, ndt
            if (k .eq. 3) goto 136
            do 137 l = 1, ndt
                if (l .eq. 3) goto 137
                dsde(k,l) = dsde(k,l) - 1.d0/dsde(3,3)*dsde(k,3)*dsde( 3,l)
137          continue
136      continue
    endif
!
9999  continue
end subroutine

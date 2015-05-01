subroutine mazu1d(ee, mazars, sigm, varm, epsm,&
                  deps, esout, sigp, varp, option)
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
! person_in_charge: jean-luc.flejou at edf.fr
! ----------------------------------------------------------------------
!
!              LOI DE MAZARS UNILATERALE EN 1D
!
! ----------------------------------------------------------------------
    implicit none
#include "asterf_types.h"
    character(len=*) :: option
    real(kind=8) :: ee, sigm, epsm, deps, esout, sigp
    real(kind=8) :: mazars(*), varm(*), varp(*)
!
! ----------------------------------------------------------------------
!  IN :
!     EE       : MODULE D'YOUNG INITIAL
!     MAZARS   : LES COEFFICIENTS DE LA LOI, DANS CET ORDRE
!                    EPSD0,BETA,AC,BC,AT,BT,SIGM_ELS,EPSI_ELU,NU
!     SIGM     : CONTRAINTE A L'INSTANT MOINS
!     VARM     : VARIABLES INTERNES A L'INSTANT MOINS
!     EPSM     : DEFORMATION TOTALE A L'INSTANT MOINS
!     DEPS     : INCREMENT DE DEFORMATION TOTALE
!     OPTION   : FULL_MECA,     :  MAT  VI  SIG  :  RIGI  RESI
!                RAPH_MECA      :       VI  SIG  :        RESI
!                RIGI_MECA_TANG :  MAT           :  RIGI
!
!  OUT :
!     ESOUT    : MODULE SECANT OU TANGENT
!     SIGP     : CONTRAINTE A L'INSTANT PLUS
!     VARP     : VARIABLES INTERNES A L'INSTANT PLUS
!
! --- ------------------------------------------------------------------
!     VARIABLES INTERNES
!        1  -> ICELS  : CRITERE SIGMA
!        2  -> ICELU  : CRITERE EPSI
!        3  -> IDOMM  : ENDOMMAGEMENT
!        4  -> IEPSQT : VALEUR DE EPSEQT DE TRACTION
!        5  -> IEPSQC : VALEUR DE EPSEQT DE COMPRESSION
!        6  -> IRSIGM : FACTEUR DE TRIAXIALITE EN CONTRAINTE
!        7  -> ITEMP  : TEMPERATURE MAXIMALE ATTEINTE PAR LE MATERIAU
!        8  -> IDISSD : DISSIPATION D'ENDOMMAGEMENT
! --- ------------------------------------------------------------------
!     INDEX DES VARIABLES INTERNES
    integer :: icels, icelu
    parameter (icels=1,icelu=2)
    integer :: idomm, iepsqt, iepsqc, irsigm, itemp, idissd
    parameter (idomm=3,iepsqt=4,iepsqc=5,irsigm=6,itemp=7,idissd=8)
! --- ------------------------------------------------------------------
    aster_logical :: rigi, resi
!
    integer :: indxvp
    real(kind=8) :: grdexp, rac2
    parameter (grdexp=200.0d0,rac2=1.4142135623731d+00)
!
    real(kind=8) :: epsela, dommag, ddmdeq, dommt, dommc, dommtc
    real(kind=8) :: epsd0, ac, bc, at, bt, nu, sgels, epelu, xx1
    real(kind=8) :: aa, bb, rr, deqtep, epseq, rtemp, epsqt, epsqc, epsqtc
! --- ------------------------------------------------------------------
!
!     RIGI_MECA_TANG ->       DSIDEP       -->  RIGI
!     FULL_MECA      ->  SIG  DSIDEP  VIP  -->  RIGI  RESI
!     RAPH_MECA      ->  SIG          VIP  -->        RESI
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    rigi = .true.
!
! --- ------------------------------------------------------------------
! --- CARACTERISTIQUES MATERIAUX
    epsd0 = mazars(1)
!       KK    = MAZARS(2)
    ac = mazars(3)
    bc = mazars(4)
    at = mazars(5)
    bt = mazars(6)
    sgels = mazars(7)
    epelu = mazars(8)
    nu = mazars(9)
! --- ------------------------------------------------------------------
!     CALCUL DES ENDOMMAGEMENTS PRECEDENTS : TRACTION, COMPRESSION
    epsqt = varm(iepsqt)
    if (epsqt .gt. epsd0) then
!        CALCUL DE L'ENDOMMAGEMENT
        dommag = 1.0d0 - (epsd0*(1.0d0-at)/epsqt)
!        IL FAUT EVITER QUE LE CALCUL PLANTE DANS EXP(RTEMP)
        rtemp = bt*(epsqt-epsd0)
        if (rtemp .le. grdexp) dommag = dommag - at*exp(-rtemp)
        dommt = min(max(dommag,0.0d0),0.99999d0 )
    else
        dommt = 0.0d0
    endif
    epsqc = varm(iepsqc)
    if (epsqc .gt. epsd0) then
!        CALCUL DE L'ENDOMMAGEMENT
        dommag = 1.0d0 - (epsd0*(1.0d0-ac)/epsqc)
!        IL FAUT EVITER QUE LE CALCUL PLANTE DANS EXP(RTEMP)
        rtemp = bc*(epsqc-epsd0)
        if (rtemp .le. grdexp) dommag = dommag - ac*exp(-rtemp)
        dommc = min(max(dommag,0.0d0),0.99999d0 )
    else
        dommc = 0.0d0
    endif
! --- ------------------------------------------------------------------
!     CALCUL DE LA DEFORMATION ELASTIQUE
!     C'EST LA SEULE QUI CONTRIBUE A FAIRE EVOLUER L'ENDOMMAGEMENT
    epsela = epsm + deps
!     DEFORMATION EQUIVALENTE
!     ENDOMMAGEMENT ET DEFORMATION EQUIVALENTE PRECEDENTS
    if (epsela .ge. 0.0d0) then
        epseq = abs(epsela)
        dommtc = dommt
        epsqtc = epsqt
        rr = 1.0d0
        deqtep = 1.0d0
        indxvp = iepsqt
        aa = at
        bb = bt
    else
        epseq = abs(epsela*rac2*nu)
        dommtc = dommc
        epsqtc = epsqc
        rr = 0.0d0
        deqtep = -rac2*nu
        indxvp = iepsqc
        aa = ac
        bb = bc
    endif
! --- ------------------------------------------------------------------
!     ENDOMMAGEMENT PRECEDENT
    dommag = dommtc
!     DERIVE DE L'ENDOMMAGEMENT PAR RAPPORT A EPSI
    ddmdeq = 0.0d0
! --- ------------------------------------------------------------------
!     CALCUL DES CONTRAINTES ET VARIABLES INTERNES
!     RESI = OPTIONS FULL_MECA ET RAPH_MECA
! --- ------------------------------------------------------------------
    if (resi) then
!        MISE A JOUR DES VARIP : PAR DEFAUT ELLES NE VARIENT PAS
        varp(iepsqt) = epsqt
        varp(iepsqc) = epsqc
!        PROGRESSION DE L'ENDOMMAGEMENT
        if ((epseq.gt.epsd0) .and. (epseq.gt.epsqtc)) then
!           CALCUL DE L'ENDOMMAGEMENT
            dommag = 1.0d0 - (epsd0*(1.0d0-aa)/epseq)
            ddmdeq = epsd0*(1.0d0-aa)/epseq**2
!           IL FAUT EVITER QUE LE CALCUL PLANTE DANS EXP(RTEMP)
            rtemp = bb*(epseq-epsd0)
            if (rtemp .le. grdexp) then
                dommag = dommag - aa*exp(-rtemp)
                ddmdeq = ddmdeq + aa*bb*exp(-rtemp)
            endif
            ddmdeq = ddmdeq*deqtep
            if (dommag .le. dommtc) then
                dommag = dommtc
                ddmdeq = 0.0d0
            endif
            if (dommag .gt. 0.99999d0) then
                dommag = 0.99999d0
                ddmdeq = 0.0d0
            endif
        endif
!        CALCUL DES CONTRAINTES
        sigp = ee*epsela*(1.0d0-dommag)
!        CORRESPOND AUX CRITERES ELS, ELU DANS LE CAS NON-LINEAIRE
        varp(icels) = sigp/sgels
        varp(icelu) = epsela*sqrt(1.0d0 + 2.0d0*nu*nu)/epelu
!        MISE A JOUR DES VARIABLES INTERNES
        varp(idomm) = dommag
        varp(indxvp) = max( epseq, epsqtc )
        varp(irsigm) = rr
        varp(itemp) = 0.0d0
!        DISSIPATION IRREVERSIBLE
        xx1 = ee*(1.0d0-dommag)*deps
        varp(idissd) = varm(idissd) + (xx1-(sigp-sigm))*deps/2.0d0
    endif
!
! --- ------------------------------------------------------------------
!     MATRICE TANGENTE
    if (rigi) then
        esout = ee*(1.0d0-dommag) - ee*epsela*ddmdeq
    endif
end subroutine

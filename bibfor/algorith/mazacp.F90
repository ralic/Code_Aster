subroutine mazacp(option, ndimsi, epsm, deps, epsane,&
                  ee, mazars, varm, varp, sigp,&
                  dsidep)
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
    implicit none
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/bptobg.h"
#include "asterfort/diago3.h"
#include "asterfort/r8inir.h"
#include "asterfort/sgmxve.h"
    character(len=16) :: option
    integer :: ndimsi
    real(kind=8) :: epsm(*), deps(*), varm(*), varp(*), sigp(*), dsidep(6, 6)
    real(kind=8) :: epsane, mazars(*)
!
! --- ------------------------------------------------------------------
!
!  IN :
!     OPTION   : FULL_MECA RAPH_MECA RIGI_MECA_TANG
!     NDIMSI   : DIMENSION DES TENSEURS
!     EPSM     : DEFORMATION TOTALE INSTANT MOINS
!     DEPS     : INCREMENT DE DEFORMATION TOTALE
!     EPSANE   : DEFORMATION ANELASTIQUE : THER, SECH, HYDR
!     EE       : MODULE D'YOUNG INITIAL
!     MAZARS   : LES COEFFICIENTS DE LA LOI, DANS CET ORDRE
!                    EPSD0,K,AC,BC,AT,BT,SIGM_LIM,EPSI_LIM,NU
!     VARM      : VARIABLES INTERNES A L'INSTANT MOINS
!
!  OUT :
!     SIGP     : CONTRAINTE A L'INSTANT PLUS
!     VARP     : VARIABLES INTERNES A L'INSTANT PLUS
!     DSIDEP   : MATRICE TANGENTE
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
    integer :: idomm, iepsqt, iepsqc, irsigm, idissd
    parameter (idomm=3,iepsqt=4,iepsqc=5,irsigm=6,idissd=8)
! --- ------------------------------------------------------------------
    aster_logical :: rigi, resi
    aster_logical :: elas, prog
    integer :: ii, jj, ll
!
    real(kind=8) :: rac2, grdexp
    real(kind=8) :: ee, epsd0, kk, ac, bc, at, bt, nu, sgels, epelu
    real(kind=8) :: aa, bb, coeff, rr, gamma
!
    real(kind=8) :: tr(6), vecpe(3, 3), epseqt, epseqc, sigeqc, sigeqt, sigeq
    real(kind=8) :: epsplu(6), epspri(3), sigpri(6), epseq
    real(kind=8) :: trsiga, trsigt, trsigc, lambda, deuxmu, dommag, yyp, yy
    real(kind=8) :: rtemp, epsela(6), sigela(6)
!
    real(kind=8) :: kron(6)
    data       kron/1.0d0,1.0d0,1.0d0,0.0d0,0.0d0,0.0d0/
    data       grdexp,rac2/200.0d0,1.4142135623731d0/
!
! --------------------------------------------------------------------------------------------------
!
!   RIGI_MECA_TANG ->        DSIDEP        -->  RIGI
!   FULL_MECA      ->  SIGP  DSIDEP  VARP  -->  RIGI  RESI
!   RAPH_MECA      ->  SIGP          VARP  -->        RESI
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
!
!   caractéristiques matériaux
    epsd0 = mazars(1)
    kk = mazars(2)
    ac = mazars(3)
    bc = mazars(4)
    at = mazars(5)
    bt = mazars(6)
    sgels = mazars(7)
    epelu = mazars(8)
    nu = mazars(9)
!
    lambda = ee*nu/(1.0d0+nu)/(1.0d0 - 2.0d0*nu)
    deuxmu = ee/(1.0d0+nu)
!
! --------------------------------------------------------------------------------------------------
!   calcul de la déformation élastique
!   c'est la seule qui contribue à faire évoluer l'endommagement
    call r8inir(6, 0.0d0, epsela, 1)
    if (resi) then
        do ii = 1, ndimsi
            epsela(ii) = epsm(ii) + deps(ii) - epsane*kron(ii)
        enddo
    else
        do ii = 1, ndimsi
            epsela(ii) = epsm(ii) - epsane*kron(ii)
        enddo
    endif
!
! --------------------------------------------------------------------------------------------------
!   on est obligatoirement en contraintes planes
    epsela(3) = -nu*(epsela(1)+epsela(2))/(1.0d0-nu)
    do ii = 4, ndimsi
        epsela(ii) = epsela(ii)/rac2
    enddo
!
! --------------------------------------------------------------------------------------------------
!   on passe dans le repère propre de EPS :
!       VECPE  : vecteurs propres, matrice de passage BP vers base INIT
!       EPSPRI : valeurs propres
    call diago3(epsela, vecpe, epspri)
!
! --------------------------------------------------------------------------------------------------
!   calcul de :
!       EPSPLU = <EPSE>+ dans le repère initial
!       EPSEQT = sqrt( tr(<EPSE>+ * <EPSE>+)  )
!       EPSEQC = sqrt( tr(<EPSE>- * <EPSE>-)  )
!       EPSEQ  = sqrt( tr(<EPSE>  * <EPSE> )  )
    epseqt = 0.0d0
    epseqc = 0.0d0
    epseq = 0.0d0
    call r8inir(6, 0.d0, tr, 1)
    call r8inir(6, 0.d0, epsplu, 1)
    do ii = 1, 3
        epseq = epseq + (epspri(ii)**2)
        if (epspri(ii) .gt. 0.0d0) then
            epseqt = epseqt + (epspri(ii)**2)
            tr(ii) = epspri(ii)
        else
            epseqc = epseqc + (epspri(ii)**2)
        endif
    enddo
    epseqt = sqrt(epseqt)
    epseqc = sqrt(epseqc)
    epseq = sqrt(epseq)
!   passage base propre vers base initiale
    call bptobg(tr, epsplu, vecpe)
    do ii = 4, ndimsi
        epsplu(ii) = epsplu(ii)*rac2
    enddo
!
! --------------------------------------------------------------------------------------------------
!   calcul des contraintes élastiques
!   dans le repère principal de déformation ==> elles sont planes
    do ii = 1, 3
        sigpri(ii) = lambda*(epspri(1)+epspri(2)+epspri(3)) + deuxmu* epspri(ii )
    enddo
!   calcul de : |SIGMA| , SIGMA+ , SIGMA- , SIGEQT , SIGEQC , SIGEQ
    trsiga = 0.0d0
    trsigt = 0.0d0
    trsigc = 0.0d0
    sigeqt = 0.0d0
    sigeqc = 0.0d0
    sigeq = 0.0d0
    do ii = 1, 3
        trsiga = trsiga + abs(sigpri(ii))
        sigeq = sigeq + (sigpri(ii)**2)
        if (sigpri(ii) .lt. 0.0d0) then
            trsigc = trsigc + sigpri(ii)
            sigeqc = sigeqc + (sigpri(ii)**2)
        else
            trsigt = trsigt + sigpri(ii)
            sigeqt = sigeqt + (sigpri(ii)**2)
        endif
    enddo
    sigeqt = sqrt(sigeqt)
    sigeqc = sqrt(sigeqc)
    sigeq = sqrt(sigeq)
! --------------------------------------------------------------------------------------------------
!   calcul de R : 1 en traction pure, 0 en compression
    if (trsiga .gt. trsigt+r8prem()) then
        rr = trsigt / trsiga
    else
        rr = 1.0d0
    endif
    if (rr .lt. 0.00001d0) rr = 0.0d0
    if (rr .gt. 0.99999d0) rr = 1.0d0
! --------------------------------------------------------------------------------------------------
!   calcul de gamma 3D:[ 0.577... ; 1 ] CP:[ 0.707... ; 1 ]
    gamma = 1.0d0
    if ((abs(trsigc).gt.1.d-10) .and. (rr.eq.0.d0)) then
        gamma = sigeqc/abs(trsigc)
    endif
! --------------------------------------------------------------------------------------------------
!   variables internes précédentes
    dommag = varm(idomm)
    yy = varm(iepsqt)
! --------------------------------------------------------------------------------------------------
!   calcul des contraintes et variables internes
!   RESI = options FULL_MECA et RAPH_MECA
! --------------------------------------------------------------------------------------------------
    prog = .false.
    elas = .true.
    if (resi) then
        yyp = gamma*epseqt
        if ((yyp.gt.epsd0) .and. (yyp.gt.yy)) then
            yy = yyp
!           calcul de l'endommagement
            aa = 2.0d0*(rr*rr)*(at-2.0d0*kk*at+ac) - rr*(at-4.0d0*kk*at+3.0d0*ac) + ac
            bb = (rr*rr)*bt + (1.0d0-rr*rr)*bc
!           il faut éviter que le calcul plante dans l'évaluation
!           de exp(rtemp) si rtemp trop grand
            rtemp = bb*(yy-epsd0)
            dommag = 1.0d0 - epsd0*(1.0d0-aa)/yy
            if (rtemp .le. grdexp) dommag = dommag - (aa/exp(rtemp))
            dommag = min( max( varm(idomm), dommag ) , 0.99999d0 )
!
            prog = ( dommag .gt. varm(idomm) )
            elas = ( dommag .le. 0.0d0 )
        endif
!
!       calcul des contraintes dans le repère initial
        call r8inir(6, 0.0d0, sigp, 1)
        call r8inir(6, 0.0d0, tr, 1)
        tr(1) = sigpri(1)*(1.0d0-dommag)
        tr(2) = sigpri(2)*(1.0d0-dommag)
        tr(3) = sigpri(3)*(1.0d0-dommag)
!       passage base propre vers base initiale
        call bptobg(tr, sigp, vecpe)
        do ii = 4, ndimsi
            sigp(ii) = rac2*sigp(ii)
        enddo
!       correspond aux critères ELS, ELU dans le cas non-linéaire 1D
        varp(icels) = sigeq*sgmxve(3,sigpri)*(1.0d0-dommag)/sgels
        varp(icelu) = epseq*sgmxve(3,epspri)/epelu
!       mise à jour des variables internes
        varp(idomm) = dommag
        varp(iepsqt) = yy
        varp(iepsqc) = epseqc
        varp(irsigm) = rr
        varp(idissd) = 0.0d0
    endif
!
! --------------------------------------------------------------------------------------------------
!   calcul de la matrice tangente DSIDEP
!   RIGI = options RIGI_MECA_TANG et FULL_MECA
! --------------------------------------------------------------------------------------------------
    if (rigi) then
!       matrice élastique endommagée
        call r8inir(36, 0.0d0, dsidep, 1)
        lambda = lambda*(1.0d0-dommag)
        deuxmu = deuxmu*(1.0d0-dommag)
        dsidep(1,1) = lambda+deuxmu
        dsidep(2,2) = lambda+deuxmu
        dsidep(3,3) = lambda+deuxmu
        dsidep(1,2) = lambda
        dsidep(2,1) = lambda
        dsidep(1,3) = lambda
        dsidep(3,1) = lambda
        dsidep(2,3) = lambda
        dsidep(3,2) = lambda
        dsidep(4,4) = deuxmu
        dsidep(5,5) = deuxmu
        dsidep(6,6) = deuxmu
!       contribution de l'endommagement
        prog = .false.
        if ((.not.elas) .and. prog .and. (dommag.lt.0.99999d0)) then
            rtemp = bb*(yy-epsd0)
            coeff = epsd0*(1.0d0-aa)/yy**2
            if (rtemp .le. grdexp) coeff = coeff + aa*bb/exp(rtemp)
            coeff = coeff*gamma*gamma/yy
!
            call r8inir(6, 0.0d0, sigela, 1)
            call r8inir(6, 0.0d0, tr, 1)
            tr(1) = sigpri(1)
            tr(2) = sigpri(2)
            tr(3) = sigpri(3)
!           passage base propre vers base initiale
            call bptobg(tr, sigela, vecpe)
            do ii = 4, ndimsi
                sigela(ii) = rac2*sigela(ii)
            enddo
            do ii = 1, 6
                do jj = 1, 6
                    dsidep(ii,jj) = dsidep (ii,jj) - coeff*sigela(ii)* epsplu(jj)
                enddo
            enddo
        endif
!       correction contraintes planes
        do ii = 1, ndimsi
            if (ii .lt. 3) then
                do ll = 1, ndimsi
                    if (ll .lt. 3) then
                        dsidep(ii,ll) = dsidep(ii,ll) - dsidep(ii,3)*dsidep(3,ll)/dsidep(3,3)
                    endif
                enddo
            endif
        enddo
    endif
end subroutine
